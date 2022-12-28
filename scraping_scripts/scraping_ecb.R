#' The aim of this script is to scrap different contents on the ECB website:
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
                  "ECB")
ecb_pub_path <- "https://www.ecb.europa.eu/pub/"
ecb_paths <- tribble(
  ~ document, ~ url,
  "Economic bulletin", str_c(ecb_pub_path, "economic-bulletin/html/all_releases.en.html"),
  "Occasional papers", str_c(ecb_pub_path, "research/", "occasional-papers", "/html/index.en.html"),
  "Working papers", str_c(ecb_pub_path, "research/", "working-papers", "/html/index.en.html"),
  "Discussion papers", str_c(ecb_pub_path, "research/", "discussion-papers", "/html/index.en.html")
)

##############" scraping ###################

remDr <- rsDriver(browser = "firefox",
                  port = 4444L,
                  chromever = NULL)
browser <- remDr[["client"]]
browser$open()
browser$maxWindowSize()

all_data <- list()
for(doc in ecb_paths$document){
  browser$navigate(filter(ecb_paths, document == doc) %>% pull())
  Sys.sleep(1) 
  browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement()
  Sys.sleep(1) 
  
  
  page <- browser$getPageSource()[[1]] %>% 
    read_html()
  
  test_snippet <- page %>% # We test if that is a long page or not
    html_elements("div#snippet0.lazy-load.loaded")
  
  if(length(test_snippet) == 0){
    test_snippet <- FALSE
  } else {
    test_snippet <- TRUE
  }
  
  if(test_snippet == TRUE){ # If that is a long page, we scroll several times. We still need a more efficient code to measure the scrolling height
    scroll_height <- 1600
    for(i in 1:400) {   
      print(i)
      browser$executeScript(glue("window.scrollBy(0,{scroll_height});"))
      Sys.sleep(0.5)
    }
  }
  
  if(doc != "Economic bulletin"){
    buttons <- browser$findElements("css", "div.accordion")
    for(i in seq_along(buttons)){
      buttons[[i]]$clickElement()
      Sys.sleep(0.3)
    }
  }
  
  page <- browser$getPageSource()[[1]] %>% 
    read_html()
  
  dates <- page %>%
    html_elements("div.date") %>% 
    html_text
  
  code <- page %>% 
    html_elements("dd")
  data_bind <- list()
  for(i in 1:length(code)){
    code_unit <- code[i]
    title <- code_unit %>% 
      html_elements("div.title") %>% 
      html_elements("a") %>% 
      html_text() %>% 
      .[1]
    url <- code_unit %>% 
      html_elements("div.title") %>% 
      html_elements("a") %>% 
      html_attr("href") %>% 
      str_c("https://www.ecb.europa.eu", .) %>% 
      .[1]
    pdf_url <- code_unit %>% 
      html_elements("span.offeredLanguage") %>% 
      html_elements("a.pdf") %>% 
      html_attr("href") %>% 
      str_c("https://www.ecb.europa.eu", .) %>% 
      .[1]
    details_text <- code_unit %>% 
      html_elements("div.accordion") %>%
      html_elements("dl") %>% 
      html_text %>% 
      .[1]
    
    data_unique <- bind_cols("title" = title, 
                             "url" = url,
                             "pdf_url" = pdf_url,
                             "details_text" = details_text)
    
    data_bind[[paste(i)]] <- data_unique
  }
  
  data <- bind_rows(data_bind) %>% 
    filter(! is.na(title)) %>% 
    mutate(dates = dmy(dates),
           jel_codes = str_extract(details_text, "(?<=JEL Code).*"),
           abstract = ifelse(is.na(jel_codes),
                             str_extract(details_text, "(?<=Abstract).*"), 
                             str_extract(details_text, "(?<=Abstract).*") %>% 
                               str_remove(jel_codes)),
           document = doc,
           id = str_c(doc, "_", row_number()) %>% 
             str_replace(" ", "_")) 
  
  all_data[[doc]] <- data
}
ecb_doc <- bind_rows(all_data)
saveRDS(ecb_doc, here(data_path, "ecb_docs.rds"))

############# Saving documents ###########################
if(! "ecb_docs" %in% list.files(data_path)) dir.create(here(data_path, "ecb_docs"))

download_list <- ecb_doc %>% 
  filter(!is.na(pdf_url),
         (document == "Economic bulletin" | !is.na(details_text)))

walk2(download_list$pdf_url[1:nrow(download_list)],
      download_list$id[1:nrow(download_list)],
      ~slow_download(url = .x,
                     destfile = str_c(data_path, "/ecb_docs/", .y, ".pdf"),
                     sleep_time = 1))

