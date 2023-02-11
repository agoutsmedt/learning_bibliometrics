############## Loading packages ####################

package_list <- c("tidyverse",
                  "rvest",
                  "glue",
                  "here",
                  "pdftools")

for(package in package_list){
  if(package %in% installed.packages() == FALSE){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

################# Paths ####################             
data_path <- here(path.expand("~"),
                  "data",
                  "central_banks",
                  "scrap_bis")

bis_path <- "https://www.bis.org/"
cbspeeches_path <- paste0(bis_path, 
                          "doclist/cbspeeches.htm")

################# Scrapping metadata ########################

# Loading the list of speeches we already have
current_speeches_metadata <- readRDS(here(data_path, 
                                          "speeches_metadata_cleaned_updated.rds")) # %>%
#  mutate(date = lubridate::dmy(date))
last_date <- current_speeches_metadata$date %>% 
  max()

# We check manually what is the page of the last speeches we scrapped
nb_pages <- 25

new_data <- tribble(~date, ~title, ~description, ~speaker, ~url)

# giving name to columns for the merge in the loop
columns <- c("date", "title", "description", "speaker")

# Loop which depends on the pages we want to scrapp on "https://www.bis.org/doclist/cbspeeches.htm"
for(j in 6:nb_pages){
  html <- read_html(paste0(cbspeeches_path,
                           glue("?page={j}")))
  Sys.sleep(0.5)
  
  for(i in 1:10){ # the default value is 10 items per page
    values <- html %>% 
      html_element(xpath = glue("body/div[1]/table/tbody/tr[{i}]")) %>% 
      html_text2() %>%
      str_split("\\\n") %>%
      .[[1]] %>% 
      as_tibble %>% 
      mutate(value = str_remove_all(value, "\t|\r")) %>% 
      filter(value != "") 
    
    values <- values %>% 
      mutate(columns = columns[1:nrow(values)]) %>% 
      pivot_wider(names_from = columns, values_from = value)
    
    url <- html %>% 
      html_element(xpath = glue("body/div[1]/table/tbody/tr[{i}]/td[2]/div/a")) %>% 
      html_attr("href")
    
    values <- values %>% 
      mutate(url = paste0(bis_path, url))
    
    new_data <- new_data %>% 
      bind_rows(values)
    
  }
}

##################### Cleaning data #######################################

#' We use a function that make different operations to clean the data, notably the speakers,
#' to generate the link of the pdf, the identifier of the speech, etc...
source(here("scraping_scripts",
            "function_scraping_bis.R"))
data_cleaned <- cleaning_bis_metadata(new_data)

#' We now merge with already existing corpus and remove the doublons
#' 
updated_data <- current_speeches_metadata %>% 
  bind_rows(data_cleaned) %>% 
  distinct(file_name, .keep_all = TRUE) %>% 
  arrange(desc(date))

saveRDS(updated_data, here(data_path,
                           glue("speeches_metadata_cleaned_updated.rds")))

##################### Downloading PDFs #######################################
# we remove first the doublons
reduced_data <- data_cleaned %>% 
  filter(! file_name %in% current_speeches_metadata$file_name)

walk(reduced_data$file_name[1:nrow(reduced_data)], ~download.file(url = glue("https://www.bis.org/review/{.}.pdf"),
                                                        destfile = glue("~/data/central_banks/scrap_bis/pdf/{.}.pdf"),
                                                        method = "wininet",
                                                        mode = "wb"))
#download.file(url = "https://www.bis.org/review/r220513a.pdf",
 #             destfile = "~/data/central_banks/scrap_bis/pdf/r220513a.pdf",
  #            method = "wininet",
   #           mode = "wb")

####################### Extracting texts ##############################
pdf_files <- paste0(reduced_data$file_name, ".pdf")

# putting .txt in a repository just in case
saving_text <- function(file) {
  pdf_text(here(data_path, "pdf", file)) %>% 
    write_lines(paste0(data_path, "/txt/", file, ".txt"))
}
walk(pdf_files, ~ saving_text(.))

# creating a data frame
extracting_text <- function(file) {
  
  message(glue("{file}"))
  text_data <- pdf_text(here(data_path, "pdf", file)) %>%
    as_tibble() %>% 
    mutate(page = 1:n(),
           file = file) %>% 
    rename(text = value)
}

new_text <- map(pdf_files, ~extracting_text(.)) %>% 
  bind_rows()

former_text <- readRDS(here(data_path,
             glue("speeches_text_updated.rds")))

updated_text <- former_text %>% 
  bind_rows(new_text)

saveRDS(updated_text, here(data_path,
                       glue("speeches_text_updated.rds")))
