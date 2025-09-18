library(dplyr)
library(tidyr)
library(purrr)
library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(readxl)
library(ggplot2)

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

#### GET THE FRENCH POPULATIONS ####

a__f_dowload_list_file(c("https://www.insee.fr/fr/statistiques/fichier/3698339/base-pop-historiques-1876-2022.xlsx"))

# Read Excel (first sheet)
df <- read_excel(file.path(K_DIR_EXT_DATA, "base-pop-historiques-1876-2022.xlsx"), skip=5, sheet = 1)

# Select years spaced by at least ten years
df<- df %>% select(CODGEO,
                   PTOT1876, PTOT1886, PTOT1896, PTOT1906, PTOT1926, PTOT1936, PTOT1954,
                   PSDC1968, PSDC1975, PSDC1990, PSDC1999,
                   PMUN2009,PMUN2019)

# Save as RDS
saveRDS(df, file.path(K_DIR_GEN_RDS, "populations_communes_historiques.rds"))

#### MERGE POPULATIONS AND TEMPERATURES ####

##### First, calculate regressions for each interval #####

# Define population reference years
pop_years <- c(1876, 1886, 1896, 1906, 1926, 1936, 1954,
               1968, 1975, 1990, 1999, 2009, 2019)

# Build consecutive intervals
intervals <- tibble(
  start = head(pop_years, -1),
  end   = tail(pop_years, -1)
)

# List all departmental RDS files with prefix "Q_"
files <- list.files(K_DIR_GEN_RDS, pattern = "^Q_", full.names = TRUE)

all_results <- list()

for (file in files) {
  message("Processing department file: ", basename(file))
  temp <- readRDS(file)
  
  # Add a year column derived from AAAAMMJJ (integer YYYYMMDD)
  temp <- temp %>%
    mutate(year = as.integer(AAAAMMJJ %/% 10000))
  
  # Loop over intervals
  dept_results <- map_dfr(seq_len(nrow(intervals)), function(i) {
    s <- intervals$start[i]
    e <- intervals$end[i]
    
    # Filter rows within the interval
    df_interval <- temp %>% filter(year >= s, year <= e)
    
    # Step 1: keep only stations that fully cover the interval
    complete_stations <- df_interval %>%
      group_by(NUM_POSTE) %>%
      summarise(
        min_year = min(year, na.rm = TRUE),
        max_year = max(year, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(min_year <= s, max_year >= e) %>%
      pull(NUM_POSTE)
    
    df_interval <- df_interval %>% filter(NUM_POSTE %in% complete_stations)
    
    # Step 2: remove rows where TN is NA
    df_interval <- df_interval %>% filter(!is.na(TN))
    
    # Step 3: keep only stations with at least 2000 valid observations
    valid_stations <- df_interval %>%
      group_by(NUM_POSTE) %>%
      summarise(n_obs = n(), .groups = "drop") %>%
      filter(n_obs >= 2000) %>%
      pull(NUM_POSTE)
    
    df_interval <- df_interval %>% filter(NUM_POSTE %in% valid_stations)
    
    # Skip if no station meets all criteria
    if (nrow(df_interval) == 0) {
      message("  Interval ", s, "-", e, ": no eligible station")
      return(NULL)
    } else {
      message("  Interval ", s, "-", e, ": ", length(unique(df_interval$NUM_POSTE)), " stations retained")
    }
    
    # Compute regression with your custom function
    res <- a__f_coef_linear_reg(
      dataframe    = df_interval,
      col_to_group = NUM_POSTE,
      date_col     = AAAAMMJJ,
      y            = TN
    )
    
    # Add interval and department metadata
    res %>%
      mutate(
        start_year = s,
        end_year   = e,
        department = basename(file)
      )
  })
  
  all_results[[basename(file)]] <- dept_results
}

# Combine results from all departments
temperatures_minimales_intervalles <- bind_rows(all_results, .id = "source_file")

# Save final results
saveRDS(
  temperatures_minimales_intervalles,
  file = file.path(K_DIR_GEN_RDS, "temperatures_minimales_intervalles_all.RDS")
)

message("✅ All interval results saved to temperatures_minimales_intervalles_all.RDS")

##### plot interval temperature on french map #####

plot_data <- temperatures_minimales_intervalles %>%
  mutate(
    trend = case_when(
      slope_p_value < 0.05 & slope_per_year > 0  ~ "increase",
      slope_p_value < 0.05 & slope_per_year < 0  ~ "decrease",
      TRUE                                       ~ "neutral"
    )
  )

unique_intervals <- plot_data %>% select(start_year, end_year) %>% distinct()

plots <- lapply(seq_len(nrow(unique_intervals)), function(i) {
  s <- unique_intervals$start_year[i]
  e <- unique_intervals$end_year[i]
  plot_interval_map(plot_data, s, e)
})

# To view one map, for example the first interval
plots[[1]]

#  save all to files
for(i in seq_along(plots)){
  ggsave(filename = paste0(K_DIR_GEN_PNG,"/temperature_map_", unique_intervals$start_year[i], "-", unique_intervals$end_year[i], ".png"),
         plot = plots[[i]], width = 8, height = 10)
}

#create a gif

create_gif_from_pngs(K_DIR_GEN_PNG, K_DIR_GEN_GIF,"temperature_map_", "temperature_gif")
