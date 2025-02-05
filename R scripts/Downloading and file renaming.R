library(dplyr)
library(data.table)
library(NOOA)

# File path declaration 
input_data_path <- "../Data/Input/"
output_data_path <- "../Data/Output/"
recording_storage_path <- "../Recordings"
GBIF_dataset_name <- "GBIF_ort_hem_recordings_in_Europe_2025-02-05"

# Setting a 2-minute timeout to allow enough time to download heavy recordings 
options(timeout = 120)

# Checking which recordings have already been downloaded in case the loop has to be run more than once
already_downloaded_recordings <- list.files(recording_storage_path, recursive = TRUE, include.dirs = FALSE)

# Reading input CSVs
country_codes <- read.csv(paste0(input_data_path, "Country_codes.csv"))

target_species <- read.csv(paste0(input_data_path, "Orthopteran_and_cicada_species_in_temperate_Western_Europe.csv")) %>%
  filter(!(sound %in% c("No", "Unknown"))) %>%
  rename("subspecies_name" = "species") %>%
  rowwise() %>%
  mutate(species_name = paste(head(strsplit(subspecies_name, " ")[[1]], 2), collapse = " "))

GBIF_occurrences <- read.table(paste0(input_data_path, GBIF_dataset_name, "/occurrence.txt"), sep = "\t", fill = TRUE, quote = "", row.names = NULL, header = TRUE)
GBIF_multimedia <- read.table(paste0(input_data_path, GBIF_dataset_name, "/multimedia.txt"), sep = "\t", fill = TRUE, quote = "", row.names = NULL, header = TRUE)

GBIF_audio_data <- GBIF_multimedia %>%
  left_join(GBIF_occurrences %>%
              select(-type, -references, -publisher, -license, -rightsHolder),
            by = "gbifID") %>%
  filter(type == "Sound")

obsolete_hosting_platforms <- c("http://www.biologie.uni-ulm.de",
                                "http://www.tierstimmenarchiv.de",
                                "http://cettia-idf.fr",
                                "http://biocase.zfmk.de",
                                "http://media.snsb.info",
                                "")

# Filtering out recordings of other taxonomic groups and recordings with invalid links
filtered_GBIF_audio_data <- GBIF_audio_data %>%
  rowwise() %>%
  mutate(hosting_platform = paste(head(strsplit(identifier, "/")[[1]], 3), collapse = "/")) %>%
  ungroup() %>%
  filter(species %in% target_species$species_name,
         !(hosting_platform %in% obsolete_hosting_platforms)) %>%
  mutate(eventTime = case_when((is.na(eventTime) | eventTime == "") & nchar(eventDate) >= 16 ~ gsub(":", "h", substr(eventDate, 11, 15)),
                               TRUE ~ eventTime),
         eventDate = case_when(nchar(eventDate) >= 16 ~ substr(eventDate, 1, 10),
                               TRUE ~ eventDate),
         recordist_date_species = paste0(rightsHolder, "_", eventDate, "_", species),
         species = case_when(taxonRank == "SUBSPECIES" ~ paste(species, infraspecificEpithet),
                             TRUE ~ species))

n_recordings <- nrow(filtered_GBIF_audio_data)

final_metadata <- data.frame()

for (i in 1:n_recordings) {
  print(paste0("Processing recording ", i, " out of ", n_recordings))
  current_recording <- filtered_GBIF_audio_data[i,]
  
  tax_group <- current_recording$order
  species <- current_recording$species
  
  # Removing problematic characters from the author name
  author_name <- gsub("  ", " ", 
                      gsub("\\.", "", 
                           gsub(",", "", 
                               gsub("_", "-", 
                                    gsub("\"", "", 
                                         gsub("/", "", 
                                              gsub("\\|", "", 
                                                   gsub("Alena Fionina / Алёна Фионина", "Alena Fionina", 
                                                        gsub("Владимир Шлёнсков / Vladimir Shlyonskov", "Vladimir Shlyonskov", 
                                                             strsplit(case_when(current_recording$rightsHolder != "" ~ current_recording$rightsHolder,
                                                                                current_recording$recordedBy != "" ~ current_recording$recordedBy,
                                                                                TRUE ~ "Anonymous"), "\\(")[[1]][1])))))))))
  
  
  author_first_name_letters <- paste0(toupper(substr(author_name, 1, 1)), tolower(substr(author_name, 2, 2)))
  author_name_array <- strsplit(author_name, " ")[[1]]
  author_last_name_letters <- case_when(grepl(" ", author_name, fixed = TRUE) ~ paste0(toupper(substr(author_name_array[2], 1, 1)), tolower(substr(author_name_array[2], 2, 2))),
                                        TRUE ~ tolower(substr(author_name, 3, 4)))
  author_first_letters <- paste0(author_first_name_letters, author_last_name_letters)
  
  remarks <- current_recording$occurrenceRemarks
  download_link <- current_recording$identifier
  hosting_platform <- paste(head(strsplit(download_link, "/")[[1]], 3), collapse = "/")
  license <- paste0("CC-", toupper(strsplit(current_recording$license, "/")[[1]][5]))
  
  locality <- current_recording$locality
  
  latitude <- current_recording$decimalLatitude
  longitude <- current_recording$decimalLongitude
  GPS <- case_when(!is.na(latitude) & !is.na(longitude) & latitude != "" & longitude != "" ~ paste0(latitude, ", ", longitude),
                   TRUE ~ "")
  
  country_code <- current_recording$countryCode
  
  if (is.na(country_code) | country_code == "") {
    if (!is.na(latitude) & !is.na(longitude)) {
      country_name <- case_when(coords2country(latitude, longitude) == "UK" ~ "UNITED KINGDOM",
                TRUE ~ coords2country(latitude, longitude))
      country_code <- (country_codes %>%
                         filter(toupper(Name) == toupper(country_name)))$Code
    } else {
      country_code <- ""
    }
  }
  
  if (is.na(current_recording$eventDate) | current_recording$eventDate == "") {
    recording_date_code <- "XXXXXXXX"
    recording_date <- ""
  } else {
    recording_date <- current_recording$eventDate
    
    if (!grepl("\\D", recording_date)) { # Checking whether the date is in number format (corresponding to the number of days having passed since 1900)
      recording_date <- as.Date("1900-01-01") + as.numeric(recording_date) - 2
    } else {
      if (grepl("/", recording_date, fixed = TRUE)) {
        recording_date <- format(as.Date(recording_date, format = "%d/%m/%Y"), "%Y-%m-%d")
      } else {
        recording_date <- as.Date(recording_date, format = "%Y-%m-%d")
      }
    }
    recording_date <- as.character(recording_date)
  }
  
  possible_duplicates <- final_metadata %>%
    filter(author_name == .env$author_name,
           recording_date == .env$recording_date,
           species == .env$species,
           hosting_platform != .env$hosting_platform)
  
  # Not adding the recording if another recording by the same author, date and species has already been found in another platform
  if (nrow(possible_duplicates) == 0) {
    
    if (current_recording$eventTime != "") {
      recording_time <- substr(current_recording$eventTime, 1, 5)
      if (substr(recording_time, 1, 1) == "T") {
        recording_time <- paste0(substr(recording_time, 2, 5), "0")
      }
    } else {
      recording_time <- ""
    }
    
    recording_time <- gsub("h", ":", recording_time)
    
    id <- as.character(i)
    
    while (nchar(id) < 6) {
      id <- paste0("0", id)
    }
    
    unique_code = gsub("\\?", "-",
                       gsub("‡", "-",
                            gsub("'", "-", 
                                 gsub("\\(", "-", 
                                      gsub("‰", "-", 
                                           gsub("\\.", "-", 
                                                paste(tax_group, "GBIF", author_first_letters, country_code, recording_date, id, sep = "_")))))))
    
    original_file_name_array <- strsplit(current_recording$identifier, "/")[[1]]
    original_file_name <- strsplit(original_file_name_array[length(original_file_name_array)], "\\?")[[1]][1]
    file_extension <- paste0(".", strsplit(original_file_name, "\\.")[[1]][2])
    
    # Defining output folder hierarchy
    new_location_folder_base_path <- paste0(recording_storage_path, "/", tax_group, "/")
    new_location_folder_author_path <- paste0(new_location_folder_base_path, author_name, "/")
    
    file_location <- paste0(new_location_folder_author_path, species)
    file_name <- paste0(unique_code, file_extension)
    file_location_and_name <- paste0(file_location, "/", file_name)
    
    # Checking whether the recording already exists in case the loop has to be run more than once due downloading errors
    if(!file.exists(file_location_and_name)) {
      # Keeping the loop going even if a particular recording can't be downloaded
      tryCatch ({ 
        # Creating output folder hierarchy
        dir.create(file.path(recording_storage_path, tax_group), showWarnings = FALSE)
        dir.create(file.path(new_location_folder_base_path, author_name), showWarnings = FALSE)
        dir.create(file.path(new_location_folder_author_path, species), showWarnings = FALSE)
        
        download.file(download_link, file_location_and_name, mode = "wb")
        
        # Not adding the recording to the metadata data frame if it couldn't be downloaded
        final_metadata <- rbindlist(list(final_metadata, data.frame(file_name, species, tax_group, author_name, recording_date, recording_time, country_code, locality, GPS, file_location, remarks, download_link, original_file_name, license)), use.names = FALSE)
      }, error = function(e) {
        
      })
    } else {
      print("Recording already downloaded")
    }
  }
}

# Saving metadata
final_metadata_file_name <- file.path(paste0(output_data_path, "Automatically generated metadata.csv"))
write.csv(x=final_metadata, file=final_metadata_file_name, row.names = FALSE)

