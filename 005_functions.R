library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(rvest)
library(httr)
library(jsonlite)
library(stringr)

#### find here every functions used in this project ####

# create a directory if not exists
a__f_createDir <- function(dirPath) {
  
  if (!dir.exists(dirPath)) dir.create(dirPath, recursive = TRUE)
  
  dirPath
}


# Function to retrieve all Meteo-France temperature file URLs
get_meteofrance_urls <- function() {
  # Retrieve the dataset metadata via its unique ID
  dataset_id <- "6569b51ae64326786e4e8e1a"
  base_api <- "https://www.data.gouv.fr/api/2/datasets/"
  
  # Get the number of resources
  dataset_info <- GET(paste0(base_api, dataset_id, "/")) %>%
    content() %>% 
    pluck("resources", "total")
  
  # Fetch all resource metadata in one JSON request
  resources <- GET(glue::glue("{base_api}{dataset_id}/resources/?page=1&page_size={dataset_info}&type=main")) %>%
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE) %>% 
    pluck("data") %>% 
    as_tibble()
  
  # Filter only the files containing "RR-T-Vent"
  meteo_resources <- resources %>%
    filter(str_detect(title, "RR-T-Vent")) %>%
    pull(url)
  
  return(meteo_resources)
}


# Download files from a URL list only if not already downloaded
a__f_dowload_list_file <- function(url_list) {
  for (url in url_list) {
    # Extract the file name from the URL
    split_the_url <- strsplit(url, "/")
    matrix_the_url <- matrix(unlist(split_the_url), ncol = 9, byrow = TRUE)
    file_name <- matrix_the_url[1, 9]
    
    # Full local path
    dest_file <- file.path(K_DIR_EXT_DATA, file_name)
    
    # Check if file already exists
    if (!file.exists(dest_file)) {
      message("Downloading: ", file_name)
      download.file(url, dest_file, mode = "wb")
    } else {
      message("Already exists, skipping: ", file_name)
    }
  }
}

# create a RDS containing datas of .gz. It concatenate files that have the same prefix

a__f_concatenate_gz_files <- function(folder_start, folder_destination, prefix_number) {
  
  files <- list.files(folder_start, pattern = ".gz")
  files <- sort(files)  # important: sort files
  
  first_name <- NULL
  total_table <- NULL
  
  for (file in files) {
    
    file_prefix <- substr(file, 1, prefix_number)
    print(paste("File:", file, "Prefix:", file_prefix))
    
    if(!identical(first_name, file_prefix)){
      
      # save previous table
      if(!is.null(first_name) && !is.null(total_table)){
        saveRDS(total_table, file = paste0(folder_destination, "/", first_name, ".RDS"))
      }
      
      print(paste("Switching from", first_name, "to", file_prefix))
      
      total_table <- read.table(gzfile(paste0(folder_start, "/", file)), sep = ";", header = TRUE)
      first_name <- file_prefix
    }
    else{
      # same prefix → append
      other_table <- read.table(gzfile(paste0(folder_start, "/", file)), sep = ";", header = TRUE)
      total_table <- rbind(total_table, other_table)
    }
    
  }
  
  # save the last table
  if(!is.null(total_table)){
    saveRDS(total_table, file = paste0(folder_destination, "/", first_name, ".RDS"))
  }
  
}

# create data containing coef of linear regression

a__f_coef_linear_reg <- function(dataframe, col_to_group, date_col, y) {
  
  # convert date_col (AAAAMMJJ as integer) into a proper Date
  dataframe <- dataframe %>%
    mutate(
      date = as.Date(as.character({{date_col}}), format = "%Y%m%d"),
      day_index = as.numeric(date - min(date, na.rm = TRUE))  # days since first date in dataset
    )
  
  # summary by group (station)
  resume <- dataframe %>%
    filter(!is.na({{y}})) %>%
    group_by({{col_to_group}}) %>%
    summarise(
      first_date = min(date, na.rm = TRUE),
      last_date  = max(date, na.rm = TRUE),
      n_obs      = n(),
      latitude   = first(LAT),
      longitude  = first(LON),
      altitude   = first(ALTI),
      .groups = "drop"
    )
  
  # regression per station
  coef_linear_model <- dataframe %>%
    filter(!is.na({{y}})) %>%
    group_by({{col_to_group}}) %>%
    nest() %>%
    mutate(
      model = map(data, ~lm(as.formula(paste(rlang::as_name(rlang::ensym(y)), "~ day_index")), 
                            data = ., na.action = na.exclude)),
      tidy_model = map(model, broom::tidy)
    ) %>%
    select(-data, -model) %>%
    unnest(cols = c(tidy_model)) %>%
    filter(term == "day_index") %>%  # keep only slope
    rename(slope_per_day = estimate,
           slope_se = std.error,
           slope_t = statistic,
           slope_p_value = p.value) %>%
    mutate(
      slope_per_year    = slope_per_day * 365.25,
      slope_per_century = slope_per_year * 100,
      slope_ci_lower = slope_per_day - 1.96 * slope_se,  # 95% CI
      slope_ci_upper = slope_per_day + 1.96 * slope_se
    ) %>%
    ungroup()
  
  # join descriptive info
  resultat <- coef_linear_model %>%
    left_join(resume, by = rlang::as_name(rlang::ensym(col_to_group)))
  
  return(resultat)
}

