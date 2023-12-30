pacman::p_load(tidyverse,
               googlesheets4,
               arrow)

data_path <- file.path(path.expand("~"), "data", "openalex")
sheet_url <- "https://docs.google.com/spreadsheets/d/1LBFHjPt4rj_9r0t0TTAlT68NwOtNH8Z21lBMsJDMoZg/edit#gid=575855905"
ss_info <- gs4_get(sheet_url)
data_concepts <- read_sheet(sheet_url)
data_concepts <- as.data.table(data_concepts)

file_name <- ss_info$name %>% 
  str_remove_all(., "[:punct:]") %>% 
  tolower() %>% 
  str_replace_all(., " ", "_")

write_parquet(data_concepts, file.path(data_path, paste0(file_name, ".parquet")),
              compression = "zstd")
