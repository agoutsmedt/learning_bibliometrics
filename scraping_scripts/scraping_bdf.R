#' The aim of this script is to scrap different contents on the Bank of France website:
#' 
#' - the economic bulletins
#' - the research papers:
#'   - the working papers
#'   - the discussion papers
#'   - the occasional papers


############## Loading packages ####################

package_list <- c("tidyverse",
                  "lubridate",
                  "rvest",
                  "RSelenium",
                  "glue",
                  "here",
                  "pdftools")

for(package in package_list){
  if(package %in% installed.packages() == FALSE){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}
source("helpers_function.R")

################# Paths ####################             
data_path <- here(path.expand("~"),
                  "data",
                  "central_banks",
                  "BdF")
bdf_pub_path <- "https://publications.banque-france.fr/liste-chronologique/"
#languages <- c("", "en/")
bdf_paths <- tribble(
  ~ document, ~ url, ~languages,
  "Interventions", "https://www.banque-france.fr/news/interventions", "fr",
  "Interventions", "https://www.banque-france.fr/en/news/interventions", "en",
  "Bulletin", str_c(bdf_pub_path, "le-bulletin-de-la-banque-de-france"), "fr",
  "Bulletin", str_c(bdf_pub_path, "le-bulletin-de-la-banque-de-france") %>% str_replace("liste", "en/liste"), "en",
  "Macroeconomic projections", str_c(ecb_pub_path, "macroeconomic-projections"), "en",
  "Documents de travail", str_c(ecb_pub_path, "documents-de-travail"), "en",
  "Eco notepad", "https://blocnotesdeleco.banque-france.fr/en", "en",
)

##############" scraping ###################

remDr <- rsDriver(browser = "firefox",
                  port = 4445L,
                  chromever = NULL)
browser <- remDr[["client"]]
browser$maxWindowSize()
all_data <- list()

# scraping interventions
for(lang in c("fr", "en")){
  url <- bdf_paths %>% 
    filter(document == "Interventions",
           languages == lang) %>% 
    pull(url)


  browser$navigate(url)
#  browser$findElement("id", "footer_tc_privacy_button_2")$clickElement()
  Sys.sleep(3)
  browser$findElement("css", "div.form-item.form-type-checkbox.form-item-filter-data-list-6886")$clickElement() #click gouverneur
  Sys.sleep(3)
  browser$findElement("css", "div.form-item.form-type-checkbox.form-item-filter-data-list-6887")$clickElement() #click sous-gouverneurs
  Sys.sleep(3)
#  browser$findElement("css", "div.form-item.form-type-checkbox.form-item-filter-data-list-459")$clickElement() #click audition
 # Sys.sleep(0.5)
  #browser$findElement("css", "div.form-item.form-type-checkbox.form-item-filter-data-list-6")$clickElement() #click discours
  #Sys.sleep(0.5)
  #browser$findElement("css", "div.form-item.form-type-checkbox.form-item-filter-data-list-458")$clickElement() #click Tribune
  #Sys.sleep(0.5)
  interventions_data <- list()
  for(i in 1:1000){
  page_html <- browser$getPageSource()[[1]] %>% 
    read_html()

  author <- page_html %>% 
    html_elements("div.rubrique.inter-rubrique") %>% 
    html_text2()
  
  title <- page_html %>% 
    html_elements("h3") %>% 
    html_elements("a") %>% 
    html_text2()
  
  url_link <- page_html %>% 
    html_elements("h3") %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    str_c("https://www.banque-france.fr", .)
  
  dates <- page_html %>% 
    html_elements("span.date") %>% 
    html_text2
  
  interventions_data[[paste(i)]] <- tibble(
    "author" = author,
    "title" = title,
    "url" = url_link,
    "date" = dates
  )
  
  out <- tryCatch({browser$findElement("css", "li.pager-next")$clickElement()
    Sys.sleep(3)
    "ok"},
                  error = function(e){message("No next page")
                    return(NA)})
  if(is.na(out)){break()}
  }
  all_data[[lang]] <- bind_rows(interventions_data) %>% 
    mutate(language = lang)
}
interventions <- bind_rows(all_data) %>% 
  mutate(document_type = str_extract(author, "[A-Z][a-z]+ ") %>% str_trim("both"),
         author = str_remove(author, "[A-Z][a-z]+ ")) %>% 
  filter(!str_detect(document_type, "Interview|Remarques"))

all_data <- list()
all_data[["Interventions"]] <- interventions

