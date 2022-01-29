#' # Loading packages, paths & data
#' 
#' ## packages

########## Loading packages, paths & data#############-------
package_list <- c(
  "here",
  "data.table",
  "tidyverse",
  "bib2df",
  "janitor",
  "dimensionsR"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c(
  "agoutsmedt/biblionetwork",
  "agoutsmedt/networkflow",
  "muschellij2/rscopus",
  "ParkerICI/vite"
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

#' ## Web extracted data
#dimensions_data <- read_csv(here(dimensions_path,
 #                                "dimensions_dsge_data.csv"),
  #                         skip = 1)
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

#' ## Extracting data from the API

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

#' # Cleaning data
#' 
#################### Cleaning data ################-----

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
 
# saving manually cleaned data
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
