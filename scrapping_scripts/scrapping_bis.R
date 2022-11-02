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
                  "scrap_bis")

bis_path <- "https://www.bis.org/"
cbspeeches_path <- paste0(bis_path, 
                          "doclist/cbspeeches.htm")

################# Scrapping metadata ########################

# We calculate the number of pages for speeches data
nb_pages <- read_html(cbspeeches_path) %>% 
  html_element(xpath = "body/div[1]/div[2]/nav/div/div[2]/div/div[2]/span") %>% 
  html_text2() %>% 
  str_remove_all("1 of |,") %>% 
  as.integer()

# We load the data we have already saved
file <- list.files(data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "rds"))

data <- readRDS(here(data_path,
                     file[1]$value))

# If there is no data (meaning we have an error at readRDS()), we can start from the beginning
if(is.data.frame(data) == FALSE){
data <- tribble(~date, ~title, ~description, ~speaker, ~url)
}

# giving name to columns for the merge in the loop
columns <- c("date", "title", "description", "speaker")

# Loop which depends on the pages we want to scrapp on "https://www.bis.org/doclist/cbspeeches.htm"
for(j in 610:nb_pages){
html <- read_html(paste0(cbspeeches_path,
                  glue("?page={j}")))

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
  pivot_wider(value, names_from = columns, values_from = value)

url <- html %>% 
  html_element(xpath = glue("body/div[1]/table/tbody/tr[{i}]/td[2]/div/a")) %>% 
  html_attr("href")

values <- values %>% 
  mutate(url = paste0(bis_path, url))

data <- data %>% 
  bind_rows(values)
  
}
}

# We can now save the result of the last extraction
save_date <- Sys.Date() %>% 
  str_remove_all("-")

saveRDS(data, here(data_path,
                   glue("speeches_metadata_{save_date}.rds")))

##################### Cleaning data #######################################
data <- readRDS(here(data_path,
                     "speeches_metadata.rds"))

source(here("scrapping_scripts",
            "function_scrapping_bis.R"))
data_cleaned <- cleaning_bis_metadata(data)

saveRDS(data_cleaned, here(data_path,
                           glue("speeches_metadata_cleaned.rds")))

##################### Downloading PDFs #######################################

walk(data_cleaned$file_name[3045:17669], ~download.file(url = glue("https://www.bis.org/review/{.}.pdf"),
                                                 destfile = glue("~/data/scrap_bis/pdf/{.}.pdf"),
                                                 method = "wininet",
                                                 mode = "wb"))
download.file(url = "https://www.bis.org/review/r220513a.pdf",
              destfile = "~/data/scrap_bis/pdf/r220513a.pdf",
              method = "wininet",
              mode = "wb")

#problems with:
no_pdf <- c("r201222j",
            "r191008d",
            "r181115a",
            "r181017a",
            "r181017b")


####################### Extracting texts ##############################
pdf_files <- list.files(here(data_path, "pdf"))

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
  
bis_text <- map(pdf_files, ~extracting_text(.)) %>% 
  bind_rows()

saveRDS(bis_text, here(data_path,
                       glue("speeches_text.rds")))


pdf_text(here(data_path, "pdf", pdf_files[1])) %>% 
write_lines("test.txt")

############## scrapping missing pdf ######################                  
                