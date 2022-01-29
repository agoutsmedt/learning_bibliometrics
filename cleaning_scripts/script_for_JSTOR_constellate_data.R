library(jsonlite)
library(tidyverse)
library(data.table)

# Warning: it seems there is a limit of 1500 in the number of 
# documents we can have in the JSONL file.
lines <- readLines("jstor_data.jsonl")
lines <- lapply(lines, fromJSON)
lines <- lapply(lines, unlist)

corpus <- data.table("title" = c(),
                        "wordCount" = c(),
                        "term" = c(),
                        "count" = c(),
                        "type" = c())

for(i in 1:length(lines)){
art_data <- bind_rows(lines[[i]])
art_data <- art_data %>% 
  select(title, wordCount, contains("gramCount")) %>% 
  pivot_longer(cols = contains("gramCount"),
               names_to = "term",
               values_to = "count") %>% 
  mutate(type = str_remove(term, "Count.*"),
         term = str_remove(term, ".*Count")) %>% 
  as.data.table

corpus <- rbind(corpus, art_data)
}
  
