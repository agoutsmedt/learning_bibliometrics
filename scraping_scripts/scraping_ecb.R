# Loading packages and paths----

source("packages_and_data_path.R")

# Scraping the ECB website----

#' The aim of this script is to scrap different contents on the ECB website:
#' 
#' - the speeches
#' - the economic bulletins
#' - the research papers:
#'   - the working papers
#'   - the discussion papers
#'   - the occasional papers

## Paths----           
ecb_pub_path <- "https://www.ecb.europa.eu/pub/"

#' We create a file with the different paths. And we need also to set a number of iteration for scrolling
#' the page (that will be useful later, to adapt to the number of documents)
ecb_paths <- tribble(
  ~ document, ~ url, ~scroll_iteration,
  "Occasional papers", str_c(ecb_pub_path, "research/", "occasional-papers", "/html/index.en.html"), 80, # first type of working papers
  "Working papers", str_c(ecb_pub_path, "research/", "working-papers", "/html/index.en.html"), 600, # second type of working papers
  "Discussion papers", str_c(ecb_pub_path, "research/", "discussion-papers", "/html/index.en.html"), 10, # third type of working papers
  "Economic bulletin", str_c(ecb_pub_path, "economic-bulletin/html/all_releases.en.html"), NA,
  "Speech", "https://www.ecb.europa.eu/press/key/html/downloads.en.html", NA
)


## Launching the web browser----

session <- bow("https://www.ecb.europa.eu/")
remDr <- rsDriver(browser = "firefox",
                  port = 4444L,
                  chromever = NULL)
browser <- remDr[["client"]]

## Dowloading speeches table----

browser$navigate(filter(ecb_paths, document == "Speech")$url)
browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refuse cookies

# We click on the link towards the .csv table that will be stored in our download directory
browser$findElement("css", ".csv")$clickElement()

# We move the downloaded csv files towards the ECB data
download_files <- list.files("C:/Users/goutsmedt/Downloads")[str_which(list.files("C:/Users/goutsmedt/Downloads"), ".csv")]
speeches_in_csv <- tibble(files = download_files) %>% 
  bind_cols(file.info(paste0("C:/Users/goutsmedt/Downloads/", download_files))) %>% 
  arrange(desc(mtime)) %>% 
  slice(1) %>% 
  pull(files)
file.copy(paste0("C:/Users/goutsmedt/Downloads/", speeches_in_csv),
          here(ecb_data_path, "speeches_ecb.csv"),
          overwrite = TRUE)

ecb_speeches <- read_delim(here(ecb_data_path, "speeches_ecb.csv"), delim = "|") %>% 
  arrange(desc(date), speakers) %>% 
  mutate(document = "speech",
         id = paste0("s", str_remove_all(date, "-"), "_", 1:n()),
         .by = date)

#' We extract the identifier from the url. It should be permanent, allows to identify doublons 
#' (ie things published as a "working paper" and a "discussion paper") and identify documents that
#' are not papers, like database or annex, because they don't have an id.
ecb_doc <- read_rds(here(ecb_data_path, "ecb_docs.rds")) %>% 
  mutate(id = str_replace(id, "Economic_bulletin", "eb"),
         id = str_replace(id, "Occasional_papers", "op"), # first type of working papers
         id = str_replace(id, "Working_papers", "wp"), 
         id = str_replace(id, "Discussion_papers", "dp"), 
         id = str_remove(id, "_\\d+$")) %>% 
  rename(date = dates) %>% 
  mutate(id = paste0(id, str_remove_all(date, "-"), "_", 1:n()),
        .by = date) %>% 
  mutate(official_id = str_extract(url, "(ocp|op|eb|wp)\\d+(?=(~|\\.en|\\.pdf))"))
  
## Scraping metadata of working papers----
all_data <- list()
for(doc in ecb_paths$document[1:3]){
  browser$navigate(filter(ecb_paths, document == doc) %>% pull(url))
  Sys.sleep(session$delay) 
  browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refusing cookies
  Sys.sleep(1) 
  
  scroll_iteration <- ecb_paths %>% 
    filter(document == doc) %>% 
    pull(scroll_iteration)
  scroll_height <- 1600
  for(i in 1:scroll_iteration) {   
    print(i)
    browser$executeScript(glue("window.scrollBy(0,{scroll_height});"))
    Sys.sleep(0.4)
  }
  
  buttons <- browser$findElements("css", "div.accordion")
  for(i in seq_along(buttons)){
    buttons[[i]]$clickElement()
    Sys.sleep(0.4)
  }
  
  selectors <- tibble(type = c("number", "date", "title", "author"),
                      selector = c(".category", ".loaded > dt", ".category+ .title", "#lazyload-container ul"))
  metadata <- vector(mode = "list", length = nrow(selectors))
  
  for(i in 1:nrow(selectors)){
    type <- selectors$type[i]
    metadata[[type]] <- browser$findElements("css selector", selectors$selector[i]) %>% 
      map_chr(., ~.$getElementText()[[1]])
    message(glue("{type} has a length of {length(metadata[[type]])}"))
  }
  metadata <- bind_cols(metadata)
  
  supplementary_information <- browser$findElements("css selector", ".category , .content-box dt , .content-box dd") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    tibble(text = .) %>% 
    mutate(id = cumsum(str_detect(text, "No. \\d+$"))) %>% 
    mutate(number = str_extract(text, "No. \\d+"), # extract the identifier of the paper and apply it to all the rows of the document
           number = first(number), .by = id) %>% 
    mutate(info_type = lag(str_extract(text, "^Abstract$|^JEL Code$|^Network$"), 1)) %>%  # We identify Abstracts and other information
    filter(! is.na(info_type)) %>% 
    pivot_wider(names_from = info_type, values_from = text) %>% 
    select(-id)  
  
  url <-  browser$findElements("css selector", ".category+ .title a") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]])
  
  url_pdf <-  browser$findElements("css selector", ".pdf") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]]) %>% 
    .[!str_detect(., "ECB_(C|c)irculation_(M|m)odel")] # Some supplementary pdf to remove
  
  #' You cannot differentiate the authors in the information above as there is no delimiter. The easiest
  #' thinkable way to differentiate them is to extract the author individually (they are of course more authors than documents)
  #' and then to use the name to extract the authors. The author column will thus become a "list" column.  
  list_all_author <- browser$findElements("css selector", ".authors a") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    unique()
  
  metadata <- metadata %>% 
    left_join(supplementary_information) %>% 
    mutate(author = str_extract_all(author, paste0(list_all_author, collapse = "|")),
           document = doc,
           url = url,
           url_pdf = url_pdf)
  
  all_data[[paste0(doc)]] <- metadata
}

## Scraping metadata of Economic Bulletin----

browser$navigate(filter(ecb_paths, document == doc) %>% pull(url))
Sys.sleep(session$delay) 
browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refusing cookies
Sys.sleep(1) 

scroll_iteration <- ecb_paths %>% 
  filter(document == doc) %>% 
  pull(scroll_iteration)
scroll_height <- 1600
for(i in 1:scroll_iteration) {   
  print(i)
  browser$executeScript(glue("window.scrollBy(0,{scroll_height});"))
  Sys.sleep(0.4)
}

economic_bulletin_metadata <- tibble(
  date = browser$findElements("css selector", selectors$selector[1]) %>% 
    map_chr(., ~.$getElementText()[[1]]),
  title = browser$findElements("css selector", selectors$selector[2]) %>% 
    map_chr(., ~.$getElementText()[[1]])
)

url <-  browser$findElements("css selector", ".category+ .title a") %>% 
  map_chr(., ~.$getElementAttribute("href")[[1]])

url_pdf <-  browser$findElements("css selector", ".pdf") %>% 
  map_chr(., ~.$getElementAttribute("href")[[1]]) %>% 
  .[!str_detect(., "ECB_(C|c)irculation_(M|m)odel")] # Some supplementary pdf to remove

economic_bulletin_metadata <- economic_bulletin_metadata %>% 
  mutate(document = "Economic bulletin",
         url = url,
         url_pdf = url_pdf)

##' Merging all the data together
##' 

ecb_doc <- bind_rows(all_data) %>% 
  bind_rows(economic_bulletin_metadata) %>% 
  bind_rows(ecb_speeches)
  
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


