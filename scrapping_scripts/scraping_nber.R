#' The aim of this script is to scrap different contents on the NBER website. We
#' then apply OCR on the downloaded PDFs. 

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

################# Paths ####################             
data_path <- here(path.expand("~"),
                  "data",
                  "scrap_nber")
nber_path <- "https://www.nber.org"



################# Connecting to the Browser ########################
#' We use RSelenium in order to open NBER pages on our web browser.

remDr <- rsDriver(browser='firefox', port=4444L)
brow <- remDr[["client"]]
brow$open()

#' We create a function
wait_selenium <- function(browser = brow, wait_min = .5, wait_max = 1, value_to_search){
  webElemtest <- NULL
  while(is.null(webElemtest)){
    webElemtest <- tryCatch({browser$findElements(using = "css selector", 
                                                  value = value_to_search)},
                            error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
  }
  randsleep <- sample(seq(wait_min, wait_max, by = 0.001), 1)
  Sys.sleep(randsleep)
}

#' On the webpage of NBER digest docs, we extract the url leading to each edition of the
#' digest. 
digest_list <- list()
for(i in 1:4){
digest_path <- str_c(nber_path, 
                     glue("/digest-archives?page={i}&perPage=100"))
brow$navigate(digest_path)
wait_selenium(value_to_search = ".digest-card")

h <- brow$getPageSource()
h <- read_html(h[[1]]) %>% 
    html_elements(css = ".digest-card")

digest_list[[i]] <- tibble(url = h %>% 
                             html_elements(".digest-card__title") %>% 
                             html_elements("a") %>% 
                             html_attr("href"),
                           information = h %>% 
                             html_text2())
}

digest_list <- bind_rows(digest_list)

#' We extract now the url of the digest's pdf

digest_list <- digest_list %>% 
  mutate(pdf_url = NA)

for(i in 1:(nrow(digest_list) - 2)){# We remove the two last lines that are connected to other types of digest page
  digest_page <- str_c(nber_path, digest_list[i,]$url)
  brow$navigate(digest_page)
  wait_selenium(value_to_search = ".publication-promo__cta")
  h <- brow$getPageSource()
  h <- read_html(h[[1]]) %>% 
    html_elements(css = ".publication-promo__cta") %>% 
    html_element("a") %>% 
    html_attr("href")
  
  digest_list <- digest_list %>% 
    mutate(pdf_url = ifelse(url == digest_list[i,]$url,
                             h,
                            pdf_url))
  
}

digest_list <- digest_list %>% 
  slice(-309, -310) %>%  # removing the two last links that are for older digests
  mutate(information_cleaned = str_extract(information, "(?<=, ).+"),
         month = str_extract(information_cleaned, "^[A-Z][a-z]+"),
         year = str_extract(information_cleaned, "[:digit:]{4}$"),
         date = str_c(month, " ", year)) %>% 
  select(url, date, pdf_url)

#' Digests for older years are on two special pages with several pdf and so necessitate another method

special_page <- str_c(nber_path, "/nber-digest-issues-1979-1989")
brow$navigate(special_page)
h <- brow$getPageSource()
first_page_url <- read_html(h[[1]]) %>% 
  html_elements(css = ".embedded-entity") %>% 
  html_elements("span") %>% 
  html_elements("a") %>% 
  html_attr("href")

special_page_2 <- str_c(nber_path, "/digest/nber-digest-issues-1990-1996")
brow$navigate(special_page_2)
h <- brow$getPageSource()
second_page_url <- read_html(h[[1]]) %>% 
  html_elements(css = ".embedded-entity") %>% 
  html_elements("span") %>% 
  html_elements("a") %>% 
  html_attr("href")

special_links <- tibble(pdf_url = first_page_url) %>% 
  bind_rows(tibble(pdf_url = second_page_url)) %>% 
  mutate(pdf_url = str_remove(pdf_url, ".*\\.org"),
         date = str_extract(pdf_url, "[a-z]{3}[:digit:]{2}"),
         year = str_extract(date, "\\d+"),
         url = ifelse(between(year, 1990, 1996), 
                      "/digest/nber-digest-issues-1990-1996",
                      "/digest/nber-digest-issues-1979-1989")) %>% 
  select(url, date, pdf_url) %>% 
  unique()

#' We merge together the two lists, and we can now download the pdf
#' 
full_digest_list <- digest_list %>% 
  bind_rows(special_links)

#' we save:
#' `write_csv2(full_digest_list, here(data_path, "digest_list.csv"))`
download_list <- full_digest_list %>% 
  filter(!is.na(pdf_url))
walk(download_list$pdf_url[1:nrow(download_list)], ~download.file(url = str_c(nber_path, .),
                                                    destfile = str_c(data_path, "/digest_pdf/digest_", str_extract(., "[a-z]{3}[:digit:]{2}"), ".pdf"),
                                                    method = "wininet",
                                                    mode = "wb"))

"https://www2.nber.org/sites/default/files/2020-06/feb79.pdf"

#' We now focus on the reporter archives and we extract first the url of each reporter
#' edition.
#' 

reporter_list <- list()
for(i in 1:2){
  reporter_path <- str_c(nber_path, 
                       glue("/reporter-archives?page={i}&perPage=100"))
  brow$navigate(reporter_path)
  wait_selenium(value_to_search = ".digest-card")
  
  h <- brow$getPageSource()
  h <- read_html(h[[1]]) %>% 
    html_elements(css = ".digest-card")
  
  reporter_list[[i]] <- tibble(url = h %>% 
                                 html_elements(".digest-card__title") %>% 
                                 html_elements("a") %>% 
                                 html_attr("href"),
                               information = h %>% 
                                 html_text2())
}

reporter_list <- bind_rows(reporter_list)

#' We extract now the url of the pdf
reporter_list <- reporter_list %>% 
  mutate(pdf_url = NA)

for(i in 1:nrow(reporter_list)){# We remove the two last lines that are connected to other types of digest page
  reporter_page <- str_c(nber_path, reporter_list[i,]$url)
  brow$navigate(reporter_page)
  wait_selenium(value_to_search = ".publication-promo__cta")
  h <- brow$getPageSource()
  h <- read_html(h[[1]]) %>% 
    html_elements(css = ".publication-promo__cta") %>% 
    html_element("a") %>% 
    html_attr("href")
  
  reporter_list <- reporter_list %>% 
    mutate(pdf_url = ifelse(url == reporter_list[i,]$url,
                             h,
                             pdf_url))
}

#' we save:
#' `write_csv2(reporter_list, here(data_path, "reporter_list.csv"))`
#' 
download_list <- reporter_list %>% 
  filter(!is.na(pdf_url))

walk(download_list$pdf_url, ~download.file(url = str_c(nber_path, .),
                                               destfile = str_c(data_path, "/reporter_pdf/reporter_", str_extract(., "(?<=[:digit:]{4}-[:digit:]{2}\\/).+(?=.pdf)"), ".pdf"),
                                               method = "wininet",
                                               mode = "wb"))

#' We want to complete by running OCR on the pdfs as the documents extracted are not
#' searchable pdf. The issue is that I found no way to do it without using a bit the command line. 
#' The OCR implies to transform the pdf in a .png. There will be as many .png as the number
#' of pages in the pdf. Then, we apply text recognition with tesseract to each .png. We
#' obtain one-page .pdf. Finally, we merge together all the one-page pdf that were initially
#' in the same document.

path_Tesseract <- "C:/Users/goutsmedt/AppData/Local/Tesseract-OCR/tesseract.exe"
digest_path <- str_c(data_path, "/digest_pdf/")
digest_files <- list.files(digest_path, ".pdf") %>% 
  str_remove(".pdf")

for(i in 1:length(digest_files)){
pages <- pdf_info(str_c(digest_path, digest_files[i], ".pdf"))$pages
pdf_convert(str_c(digest_path, digest_files[i], ".pdf"), 
            filenames = str_c(digest_path, "png/", digest_files[i], "_", 1:pages, ".png"),
            dpi = 300)


for(j in 1:pages){
png_name <- str_c(digest_path, "png/", digest_files[i], "_", j, ".png")
ocr_name <- str_c(digest_path, "ocr/", digest_files[i], "_", j, "_OCR")
args <- str_c(png_name, " ", ocr_name, " -l eng PDF")
system2(command = path_Tesseract, args = args)
}

ocr_names <- str_c(digest_path, "ocr/", digest_files[i], "_", 1:pages, "_OCR.pdf")
pdftools::pdf_combine(input = ocr_names,
                      output = str_c(digest_path, "/ocr_final/", digest_files[i], ".pdf"))
}

#' We do the same for reporter docs

reporter_path <- str_c(data_path, "/reporter_pdf/")
reporter_files <- list.files(reporter_path, ".pdf") %>% 
  str_remove(".pdf")

for(i in 93:length(reporter_files)){
  pages <- pdf_info(str_c(reporter_path, reporter_files[i], ".pdf"))$pages
  pdf_convert(str_c(reporter_path, reporter_files[i], ".pdf"), 
              filenames = str_c(reporter_path, "png/", reporter_files[i], "_", 1:pages, ".png"),
              dpi = 300)
  
  
  for(j in 1:pages){
    png_name <- str_c(reporter_path, "png/", reporter_files[i], "_", j, ".png")
    ocr_name <- str_c(reporter_path, "ocr/", reporter_files[i], "_", j, "_OCR")
    args <- str_c(png_name, " ", ocr_name, " -l eng PDF")
    system2(command = path_Tesseract, args = args)
  }
  
  ocr_names <- str_c(reporter_path, "ocr/", reporter_files[i], "_", 1:pages, "_OCR.pdf")
  pdftools::pdf_combine(input = ocr_names,
                        output = str_c(reporter_path, "/ocr_final/", reporter_files[i], ".pdf"))
}
           