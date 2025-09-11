library(dplyr)
library(tidyr)
library(purrr)
library(rvest)
library(httr)
library(jsonlite)
library(stringr)

#### GET THE FRENCH TEMPERATURES ####

##### first dowload the datas #####

#list all url to download
urls <- get_meteofrance_urls()
a__f_dowload_list_file(urls)


##### create a RDStable for each french department from ext data folder to gen rds folder #####

a__f_concatenate_gz_files(K_DIR_EXT_DATA,K_DIR_GEN_RDS,4)

##### create temperature indicators #####

# List all RDS files with prefix "Q_"
files <- list.files(K_DIR_GEN_RDS, pattern = "^Q_", full.names = TRUE)

# Initialize an empty list to store results
all_results <- list()

# Loop over files
for (file in files) {
  message("Processing: ", basename(file))
  
  # Load the departmental dataset
  temp <- readRDS(file)
  
  # Compute regression for this department
  res <- a__f_coef_linear_reg(temp, NUM_POSTE, AAAAMMJJ, TN)
  
  # Store result in the list
  all_results[[basename(file)]] <- res
}

# Combine all departmental results into one data frame
temperatures_minimales <- dplyr::bind_rows(all_results, .id = "source_file")

# Save the combined results
saveRDS(temperatures_minimales, file = file.path(K_DIR_GEN_RDS, "temperatures_minimales_all.RDS"))

message("✅ All results saved to temperatures_minimales_all.RDS")

##### plot french map #####

# df = your final dataframe with columns NUM_POSTE, LAT, LON, TN
stations_unique <- temperatures_minimales %>%
  select(NUM_POSTE, latitude, longitude) %>%  # keep only relevant columns
  distinct()                        # keep only one row per station

plot_french_stations(stations_unique, title = "Stations météo 5")

