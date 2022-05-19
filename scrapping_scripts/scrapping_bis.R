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

data_cleaned <- data %>% 
  select(-`NA`, - value) %>% 
  mutate(pdf_link = str_replace(url, "htm$", "pdf"),
         file_name = str_extract(pdf_link, "(?<=\\/)r\\d+.+(?=\\.pdf)"),
         speaker = str_remove(speaker, "by "),
         speaker_from_title = str_extract(title, ".*(?=:)"),
         speaker_cleaned = ifelse(is.na(speaker), speaker_from_title, speaker),
         speech_type = str_extract(description, "(.+?)(?=, )") %>% 
           str_remove(" by .+"),
         cb_info = str_extract(description, "(?<=, ).+") %>% 
           str_extract("(.+?)(?=, )"),
         central_bank = str_extract(cb_info, "((C|c)entral |Reserve )?(B|b)ank of (the )?[A-Z][a-z]+( (of )?[A-Z][a-z]+( [A-Z][a-z]+)?)?")) 

central_banks <- c("Bundesbank",
                   "European Central Bank",
                   "ECB", # second name
                   "Swiss National Bank",
                   "Banque nationale suisse",
                   "Sveriges Ri(s)?k(s)?bank",
                   "Norges Bank",
                   "Danmarks National Bank",
                   "Netherlands Bank",
                   "Nederlandsche Bank", # second name
                   "Banco de Portugal", # second name
                   "Banco de España",
                   "Banque (de|of) France", # second name
                   "Czech National Bank",
                   "Austrian National( B|b)ank", # second name
                   "Oesterreichische Nationalbank", # third name
                   "Central Bank (\\&|and) Financial Services Authority of Ireland", # second name
                   "Bulgarian National Bank",
                   "Croati(a|o)n National Bank", # second name
                   "Centrale Bank van Curaçao en Sint Maarte",
                   "National Bank of the Republic of (North )?Macedonia",
                   "Central Bank of the United Arab Emirates",
                   "Bahrain Monetary Agency",
                   "Saudi Arabian Monetary Agency",
                   "South African Reserve Bank",
                   "Reserve Bank of india", # second name
                   "Hong Kong Monetary Authority",
                   "Hong Kong Monetary", # second name
                   "Monetary Authority of Singapore",
                   "Bangko Sentral ng Pilipinas",
                   "Bank Indonesia",
                   "Bank Negara Malaysia",
                   "Maldives Monetary Authority",
                   "Cayman Islands Monetary Authority",
                   "Monetary Authority of Macao",
                   "Mon\\. Authority of Macao", # second name
                   "Federal Reserve",
                   "Fed\\. Res(erve|\\.) System", # second name
                   "Eastern Caribbean Central Bank",
                   "Australian Reserve Bank",
                   "Bank for International Settlements") # not a central bank

data_cleaned <- data_cleaned %>% 
  mutate(central_bank = ifelse(is.na(central_bank), str_extract(description, paste0(central_banks, collapse = "|")), central_bank),
         central_bank = ifelse(is.na(central_bank), str_extract(description, "((C|c)entral |Reserve )?(B|b)ank of (the )?[A-Z][a-z]+( (of )?[A-Z][a-z]+( [A-Z][a-z]+)?)?"), central_bank),
         central_bank = ifelse(is.na(central_bank), str_extract(title, "((C|c)entral |Reserve )?(B|b)ank of (the )?[A-Z][a-z]+( (of )?[A-Z][a-z]+( [A-Z][a-z]+)?)?"), central_bank)) # for the very first years


# cleaning CBs to uniformise names

correct_name <- tribble(
  ~error, ~correction,
  "Austria", "Austrian National Bank",
  "Oesterreichische", "Austrian National Bank",
  "Banco de España", "Bank of Spain",
  "Banco de Portugal", "Bank of Portugal",
  "Bank of China", "Bank of China",
  "Bank of England", "Bank of England",
  "France", "Bank of France",
  "Bank of Japan", "Bank of Japan",
  "Bank of Lativa", "Bank of Latvia",
  "Malaysia", "Central Bank of Malaysia",
  "Sveriges", "Sveriges Riksbank",
  "Sweden", "Sveriges Riksbank",
  "Uganda", "Bank of Uganda",
  "Suisse", "Swiss National Bank",
  "Ireland", "Central Bank of Ireland",
  "Morocco", "Bank of Morocco",
  "Turkey", "Central Bank of the Republic of Turkey",
  "Russia", "Bank of Russia",
  "Nederlandsche Bank", "Netherlands Bank",
  "ECB", "European Central Bank",
  "Danmarks", "Bank of Denmark",
  "Hong Kong", "Hong Kong Monetary Authority",
  "Fed\\. Res(erve|\\.) System", "Federal Reserve",
  "Macao", "Monetary Authority of Macao",
  "Norges Bank", "Central Bank of Norway",
  "South Africa", "South African Reserve Bank",
  "New Zealand", "Reserve Bank of New Zealand",
  "India", "Reserve Bank of India",
  "Australia", "Reserve Bank of Australia"
)

for(i in 1:nrow(correct_name)){
data_cleaned <- data_cleaned %>% 
  mutate(central_bank = ifelse(str_detect(central_bank, correct_name$error[i]), correct_name$correction[i], central_bank))
}

data_cleaned <- data_cleaned %>% 
  mutate(central_bank = tolower(central_bank))
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
                