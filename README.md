# GBIF recording download
Code to download all recordings uploaded on Xeno-canto, iNaturalist, observation.org, ZFMK and MinIO and to extract the corresponding metadata based on GBIF results (GBIF.org, 2025). To avoid duplicate downloads, the code ensures that recordings are not redownloaded if an entry from the same species, date, and author has already been retrieved from another platform. 

&nbsp;

Instructions:
1. Download the updated list of animal recordings available on online platforms based on the GBIF Occurrence Search tool, and replace the "GBIF_ort_hem_recordings_in_Europe_2025-02-05" folder under "Data/Input/" with the export results. The original folder contains all European orthopteran and cicada recordings having been uploaded before February 5, 2025.
2. Replace the file "Orthopteran_and_cicada_species_in_North_Central_and_temperate_Western_Europe.csv" in the "Data/Input/" folder with the list of species whose recordings you want to download. The original file contains a list of all sound-producing orthopteran and cicada species and subspecies present in North, Central and temperate Western Europe (Andorra, Austria, Belgium, Czechia, Denmark, Estonia, Finland, mainland France and Corsica, Germany, Ireland, Latvia, Lithuania, Luxembourg, Monaco, Netherlands, Norway, Poland, United Kingdom, Sweden and Switzerland).
3. Run the "Downloading and file renaming.R" script in the "R scripts/" folder.

&nbsp;

Requirements:

The code can be run on R ≥4.2. If the NOAA package cannot be installed through the standard install.packages command, the following command can be used instead:

*install.packages(“https://cran.r-project.org/src/contrib/noaa_1.0.0.tar.gz”, repos = NULL, type = “source”)*

&nbsp;

How to cite this code:

Funosas, D., Massol, E., Bas, Y., Schmidt, S., Arend, D., Gebhard, A., Barbaro, L., König, S., Font, R. C., Sannier, D., Deroussen, F., Sueur, J., Roesti, C., Trilar, T., Forstmeier, W., Roger, L., Matheu, E., Guzik, P., Barataud, J., Pelozuelo, L., Puissant, S., Mueller, S., Schuller, B., Montoya, J.M., Triantafyllopoulos, A., Cauchoix, M. (2025). ECOSoundSet: A finely annotated dataset for the automated acoustic identification of Orthoptera and Cicadidae in North, Central and temperate Western Europe (No. arXiv:2504.20776). arXiv. https://doi.org/10.48550/arXiv.2504.20776

&nbsp;

References:

GBIF.org (05 February 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.v7nwww
