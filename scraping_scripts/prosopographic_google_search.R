library(tidyverse)
library(rvest)
library(RSelenium)

prosopo_data <- read_csv2(path.expand("~/data/prosopo_db_test.csv"))

google_url <- "https://www.google.com/"

remDr <- rsDriver(browser = "firefox",
                  port = 4444L,
                  chromever = NULL)
browser <- remDr[["client"]]
browser$maxWindowSize()

browser$navigate(google_url)
browser$
  findElement("id", "W0wltc")$
  clickElement()

prosopo_url <- list()
names <- prosopo_data$Name

for(i in names){
  google_search <- browser$
    findElement("name", "q")
  
name <- i %>% 
  str_split("") %>% 
  .[[1]]

for(j in name){
google_search$sendKeysToElement(list(j))
Sys.sleep(runif(1))
}
google_search$sendKeysToElement(list(key = "enter"))
Sys.sleep(1.5)

search_html <- browser$getPageSource()[[1]] %>% 
  read_html()

urls <- search_html %>% 
  html_elements("div.yuRUbf") %>% 
  html_elements("a") %>% 
  html_attr("href") 

urls <- tibble(name = i, "url" = urls) %>% 
  filter(!str_detect(url, "translate.google.com"))

# go to the second page
browser$
  findElement("link text", "2")$
  clickElement()
Sys.sleep(1.3)

search_html <- browser$getPageSource()[[1]] %>% 
  read_html()

urls_2 <- search_html %>% 
  html_elements("div.yuRUbf") %>% 
  html_elements("a") %>% 
  html_attr("href")

urls_2 <- tibble(name = i,  "url" = urls_2) %>% 
  filter(! str_detect(url, "translate.google.com"))

prosopo_url[[i]] <- bind_rows(urls, urls_2)
browser$navigate(google_url)
}

prosopo_url <- bind_rows(prosopo_url)


######### Identifying website in URL #######

websites <- c("wikipedia",
              "wikidata",
              "linkedin",
              "littlesis",
              "scholar.google",
              "researchgate",
              "twitter",
              "cv",
              "econpapers.repec",
              "forbes")

prosopo_url_cleaned <- prosopo_url %>% 
  group_by(name) %>% 
  mutate(url_type = NA,
         id = cur_group_id())
for(i in websites){
  prosopo_url_cleaned <- prosopo_url_cleaned %>% 
    mutate(url_type = ifelse(str_detect(url, i), i, url_type))
}
prosopo_url_cleaned <- prosopo_url_cleaned %>% 
  mutate(url_type = ifelse(str_detect(url, str_remove_all(name, " | [A-Z](\\.)? ") %>% tolower() %>% str_c("(:\\/\\/|www\\.)", .)) &
                             is.na(url_type),
                           "personal_website",
                           url_type)) %>% 
  group_by(name, url_type) %>% 
  mutate(nb = 1:n()) %>% 
  mutate(url_type = ifelse(!is.na(url_type), str_c(url_type, nb, sep = "_"), url_type), .keep = "unused")

write_csv2(prosopo_url_cleaned,
           path.expand("~/data/prosopo_db_test_urls.csv"))
