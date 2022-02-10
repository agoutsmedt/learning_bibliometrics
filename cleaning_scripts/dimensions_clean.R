#' ---
#' title: "Script for extracting & cleaning Dimensions data"
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # What is this script for?
#' 
#' In this script, I show how to clean Dimensions data extracted from 
#' Dimensions website.
#'  
#' I will also show how to extract data using the Dimensions API and the 
#' [`dimensionsR`](https://github.com/massimoaria/dimensionsR) package.
#' 
#' > WARNING: This script is not complete, and the code for using the API is lacking.
#' 
#' # Loading packages and paths
#' 
#' ## Packages

package_list <- c(
  "here", # use for paths creation
  "data.table",
  "tidyverse",
  "bib2df", # for cleaning .bib data
  "janitor", # useful functions for cleaning imported data
  "dimensionsR"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c(
  "agoutsmedt/biblionetwork"
)
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}

#' ## Paths
data_path <- here("projets", "data", "tuto_biblio_dsge")
dimensions_path <- here(data_path, "dimensions")

#' # Cleaning Web extracted data
#' 
#' ## Loading the data
dimensions_data_1 <- read_csv(here(dimensions_path,
                                   "dimensions_dsge_data_2015-2022.csv"),
                              skip = 1)
dimensions_data_2 <- read_csv(here(dimensions_path,
                                   "dimensions_dsge_data_1996-2014.csv"),
                              skip = 1)

dimensions_data <- rbind(dimensions_data_1,
                         dimensions_data_2) %>% 
  clean_names() # cleaning column names with janitor to transform them

dimensions_data

#' ## Cleaning data
#' 
#' ### Cleaning duplicates 
#' 
#' One issue with Dimensions is that it integrates preprints, 
#' which can be interesting for certain types of analysis. 
#' However, we cannot know if an item is a preprint or not. 
#' The consequence of that is that *i)* we can have duplicates 
#' (the same article with the preprint and then the published version) but 
#' *ii)* we cannot discriminate between these duplicates as we don't know which one is the published version. 
#' What we can do is to spot the duplicates thanks to the title and keep one 
#' of the two duplicates depending on the presence of references or not. 
  

duplicated_articles <- dimensions_data %>%
  add_count(title) %>% 
  filter(n > 1) %>% 
  arrange(title, cited_references)

to_keep <- duplicated_articles %>% 
  group_by(title) %>% 
  slice_head(n = 1)

to_remove <- duplicated_articles %>% 
  filter(! publication_id %in% to_keep$publication_id)

dimensions_data <- dimensions_data %>% 
  filter(! publication_id %in% to_remove$publication_id)

#' ### Cleaning affiliations

#' The goal now is to create a table which associates each document to authors' affiliation 
#' (column `authors_affiliations_name_of_research_organization`) and the country of the affiliation 
#' (column `authors_affiliations_country_of_research_organization`). We cannot associate affiliations 
#' directly to authors as documents can have more authors than affiliations, and conversely.

dimensions_affiliations <- dimensions_data %>% 
  select(publication_id,
         authors_affiliations_name_of_research_organization,
         authors_affiliations_country_of_research_organization) %>% 
  separate_rows(starts_with("authors"), sep = "; ")

#' ### Cleaning References
#' 
#' Cleaning of references is quite easy for Dimensions as you have a easy to 
#' identify separator between references. And then you have the same information
#' for each references (see `column_names`) even if the information is lacking, and
#' you have an easy to identify separator between information.  

references_extract <- dimensions_data %>% 
  filter(! is.na(cited_references)) %>% 
  rename("citing_id" = publication_id) %>% # because the "publication_id" column is also the identifier of the reference
  select(citing_id, cited_references) %>% 
  separate_rows(cited_references, sep = ";(?=\\[)") %>%
  as_tibble

column_names <- c("authors",
                  "author_id",
                  "source",
                  "year",
                  "volume",
                  "issue",
                  "pagination",
                  "doi",
                  "publication_id",
                  "times_cited")

dimensions_direct_citation <- references_extract %>% 
  separate(col = cited_references, into = column_names, sep = "\\|")

#' We can now save the different tables.
saveRDS(dimensions_data, here(dimensions_path,
                    "data_cleaned",
                    "dimensions_manual_nodes.rds"))

saveRDS(dimensions_direct_citation, here(dimensions_path,
                              "data_cleaned",
                              "dimensions_manual_direct_citation.rds"))

dimensions_references <- dimensions_direct_citation %>% 
  distinct(publication_id, .keep_all = TRUE) %>% 
  select(-citing_id)

saveRDS(dimensions_references, here(dimensions_path,
                         "data_cleaned",
                         "references_manual_references.rds"))

edges <- biblionetwork::coupling_strength(direct_citation, 
                                          "citing_id", 
                                          "publication_id",
                                          weight_threshold = 3)
saveRDS(edges, here(dimensions_path,
                    "data_cleaned",
                    "dimensions_manual_edges.rds"))
write_excel_csv2(select(edges, Source, Target, weight), here(dimensions_path,
                                                             "data_cleaned",
                                                             "dimensions_manual_edges.csv"))

#' # Extracting data from the API
#' 
#' > To be completed.

token <- dsAuth(username = "aurelien.goutsmedt@uclouvain.be",
                password = "21091989_Aur",
                verbose = TRUE)

query <- dsQueryBuild(item = "publications",
                      words = c("DSGE", "Dynamic Stochastic General Equilibrium"),
                      words_boolean_op = "OR",
                      full.search = FALSE,
                      type = "article",
                      start_year = 1995,
                      end_year = 2021)

res <- dsApiRequest(token = token,
                    query = query,
                    limit = 0,
                    verbose = TRUE)