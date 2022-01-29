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
dimensions_data_reference_1 <- read_csv(here(dimensions_path,
                                "dimensions_dsge_references_data_2015-2022.csv"),
                           skip = 1)
dimensions_data_reference_2 <- read_csv(here(dimensions_path,
                                            "dimensions_dsge_references_data_1996-2014.csv"),
                                       skip = 1)

dimensions_data_reference <- rbind(dimension_data_reference_1,
                                  dimension_data_reference_2) %>% 
  mutate(citing_id = paste0("A", 1:n())) %>%  # We create a unique ID for each doc of our corpus
  clean_names()

#' ## Cleaning data
#' 
#' Cleaning of references is quite easy for Dimensions as you have a easy to 
#' identify separator between references. And then you have the same information
#' for each references (see `column_names`) even if the information is lacking, and
#' you have an easy to identify separator between information.  

references_extract <- dimensions_data_reference %>% 
  filter(! is.na(cited_references)) %>% 
  select(citing_id, cited_references) %>% 
  separate_rows(cited_references, sep = ";(?=\\[)") %>% 
  mutate(id_ref = 1:n()) %>% 
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
direct_citation <- references_extract %>% 
  separate(col = cited_references, into = column_names, sep = "\\|")
 
#' We can now save the different tables.
saveRDS(dimensions_data_reference, here(dimensions_path,
                    "data_cleaned",
                    "dimensions_manual_nodes.rds"))
write_excel_csv2(dimensions_data_reference, here(dimensions_path,
                             "data_cleaned",
                             "dimensions_manual_nodes.rds"))

saveRDS(direct_citation, here(dimensions_path,
                              "data_cleaned",
                              "dimensions_manual_direct_citation.rds"))

references <- direct_citation %>% 
  distinct(publication_id, .keep_all = TRUE) %>% 
  select(-citing_id, - id_ref)
saveRDS(references, here(dimensions_path,
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