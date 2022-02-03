#' ---
#' title: "Script for extracting & cleaning scopus data"
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
#' In this script, I show how to clean scopus data extracted from scopus website.
#' For cleaning references, I test two types of cleaning. I clean all the data 
#' myself, but I also try the `anystyle.io` references parser.
#'  
#' I also show how to extract data using the scopus API and the 
#' [`rscopus`](https://johnmuschelli.com/rscopus/index.html) package.
#' 
#' > WARNING: This script is not complete, and some work remains to do, notably for
#' cleaning data.
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
  "rscopus", # using Scopus API
  "biblionetwork" # creating edges
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

#' ## Paths

data_path <- here(path.expand("~"),
                  "data",
                  "tuto_biblio_dsge")
scopus_path <- here(data_path, "scopus")

#' # Cleaning scopus data from website search
#' 

scopus_search_1 <- read_csv(here(scopus_path, "scopus_search_1998-2013.csv"))
scopus_search_2 <- read_csv(here(scopus_path, "scopus_search_2014-2021.csv"))
scopus_search <- rbind(scopus_search_1, scopus_search_2) %>% 
  mutate(citing_id = paste0("A", 1:n())) %>% # We create a unique ID for each doc of our corpus
  clean_names()
#' There are several things to clean:
#' 
#' - several `Authors` per document. For some operation, better to have an association between 
#' a unique paper and a unique author (so at the end you should have as many lines as authors 
#' for one article);
#' - several `Affiliations` per article as well as several `Authors with affiliations`. It allows
#' you to connect authors with their affiliations, but here again you need to separate in 
#' as many lines as authors (and as `Affiliations`, considering that one author can have
#' many affiliations);
#' - `references`;
#' - Possibility to separate `Author Keywords` and `Index Keywords` if you want to use it.
#' 
#' Depending on what we want to study, there is several separate data.frame to create:
#' - we can keep the citing articles/nodes data.frame (and remove some useless columns);
#' - we can have an author table, using the column `Authors`, but also the
#' `Authors with affiliations` column to associate affiliations to author;
#' - we can have an affiliation table with the `Affiliations` column. The table should be 
#' separated from the author table, as one author can have many affiliations, and as 
#' the `Authors with affiliations` column can be missing while the `Affiliations` column
#' is not
#' - a reference table, with all the `references` cited. It involves to clean the references
#' such as giving them a unique ID for similar references;
#' - the direct citation table, with all the references cited by citing articles. This is the 
#' bridge between the citing articles table and the references table. And this is the table
#' we will use to build the edges of our network.
#' - Tables with `Author Keywords` and `Index Keywords`.
#' 
#' ## Extracting affilitions and corresponding authors
#' 
#' We have two column for affiliations:
#' - one column `Affiliations` with affiliations alone
#' - one column with both authors and affiliations.
#' 
#' For a more secure cleaning, we opt for the second column, as it allow us to associate
#' the author with his/her own affiliation as described in the column `authors_with_affiliation`.

scopus_affiliations <- scopus_search %>% 
  select(citing_id, authors, affiliations, authors_with_affiliations) %>% 
  separate_rows(authors, sep = ", ") %>% 
  separate_rows(contains("with"), sep = "; ") %>% 
  mutate(authors_from_affiliation = str_extract(authors_with_affiliations, 
                                                "^(.+?)\\.(?=,)"),
         authors_from_affiliation = str_remove(authors_from_affiliation, ","),
         affiliations = str_remove(authors_with_affiliations, "^(.+?)\\., "),
         country = str_remove(affiliations, ".*, ")) %>% # Country is after the last comma
  filter(authors == authors_from_affiliation) %>% 
  select(citing_id, authors, affiliations, country)

#' ## Extracting and cleaning references
references_extract <- scopus_search %>% 
  filter(! is.na(references)) %>% 
  select(citing_id, references) %>% 
  separate_rows(references, sep = "; ") %>% 
  mutate(id_ref = 1:n()) %>% 
  as_tibble

#' Here is a list of regex to extract standard information that we will
#' use later.
extract_authors_regex <- ".*[:upper:][:alpha:]+( Jr(.)?)?, ([A-Z]\\.[ -]?)?([A-Z]\\.[ -]?)?([A-Z]\\.)?[A-Z]\\."
extract_year_brackets <- "(?<=\\()\\d{4}(?=\\))"
extract_pages <- "(?<= (p)?p\\. )([A-Z])?\\d+(-([A-Z])?\\d+)?"
extract_volume_and_number <- "(?<=( |^)?)\\d+ \\(\\d+(-\\d+)?\\)"

cleaning_references <- references_extract %>% 
  mutate(authors = str_extract(references, paste0(extract_authors_regex, "(?=, )")),
         remaining_ref = str_remove(references, paste0(extract_authors_regex, ", ")), # cleaning from authors
         is_article = ! str_detect(remaining_ref, "^\\([:digit:]{4}"), 
         year = str_extract(references, extract_year_brackets) %>% as.integer)

#' To easy the cleaning we separate documents depending on the position of the year of 
#' publication. We use the variable `is_article` to determinate where the year is
#' and thus if the title is before the year or not.
cleaning_articles <- cleaning_references %>% 
  filter(is_article == TRUE) %>% 
  mutate(title = str_extract(remaining_ref, ".*(?=\\(\\d{4})"), # pre date extraction
         journal_to_clean = str_extract(remaining_ref, "(?<=\\d{4}\\)).*"), # post date extraction
         journal_to_clean = str_remove(journal_to_clean, "^,") %>% str_trim("both"), # cleaning a bit the journal info column
         pages = str_extract(journal_to_clean, extract_pages), # extracting pages
         volume_and_number = str_extract(journal_to_clean, extract_volume_and_number), # extracting standard volument and number: X (X)
         journal_to_clean = str_remove(journal_to_clean, " (p)?p\\. ([A-Z])?\\d+(-([A-Z])?\\d+)?"), # clean from extracted pages
         journal_to_clean = str_remove(journal_to_clean, "( |^)?\\d+ \\(\\d+(-\\d+)?\\)"), # clean from extracted volume and number
         volume_and_number = ifelse(is.na(volume_and_number), str_extract(journal_to_clean, "(?<= )([A-Z])?\\d+(-\\d+)?"), volume_and_number), # extract remaining numbers
         journal_to_clean = str_remove(journal_to_clean, " ([A-Z])?\\d+(-\\d+)?"), # clean from remaining numbers
         journal = str_remove_all(journal_to_clean, "^[:punct:]+( )?[:punct:]+( )?|(?<=,( )?)[:punct:]+( )?([:punct:])?|[:punct:]( )?[:punct:]+( )?$"), # extract journal info by removing inappropriate punctuations
         first_page = str_extract(pages, "\\d+"),
         volume = str_extract(volume_and_number, "\\d+"),
         issue = str_extract(volume_and_number, "(?<=\\()\\d+(?=\\))"),
         publisher = ifelse(is.na(first_page) & is.na(volume) & is.na(issue) & ! str_detect(journal, "(W|w)orking (P|p)?aper"), journal, NA),
         book_title = ifelse(str_detect(journal, " (E|e)d(s)?\\.| (E|e)dite(d|urs)? "), journal, NA), # Incollection article: Title of the book here
         book_title = str_extract(book_title, "[A-z ]+(?=,)"), # keeping only the title of the book
         publisher = ifelse(!is.na(book_title), NA, publisher), # if we have an incollection article, that's not a book, so no publisher
         journal = ifelse(!is.na(book_title) | ! is.na(publisher), NA, journal), # removing journal as what we have is a book
         publisher = ifelse(is.na(publisher) & str_detect(journal, "(W|w)orking (P|p)?aper"), journal, publisher), # adding working paper publisher information in publisher column
         journal = ifelse(str_detect(journal, "(W|w)orking (P|p)?aper"), "Working Paper", journal))

cleaned_articles <- cleaning_articles %>% 
  select(citing_id, id_ref, authors, year, title, journal, volume, issue, pages, first_page, book_title, publisher, references)

#' We do the same now with the remaining references (there are less numerous) which
#' are less easy to clean, due to the fact that the title is not separate from the
#' other publication information (journal or publisher).

cleaning_non_articles <- cleaning_references %>% 
  filter(is_article == FALSE) %>% 
  mutate(remaining_ref = str_remove(remaining_ref, "\\(\\d{4}\\)(,)? "),
         title = str_extract(remaining_ref, ".*(?=, ,)"),
         pages = str_extract(remaining_ref, "(?<= (p)?p\\. )([A-Z])?\\d+(-([A-Z])?\\d+)?"), # extracting pages
         volume_and_number = str_extract(remaining_ref, "(?<=( |^)?)\\d+ \\(\\d+(-\\d+)?\\)"), # extracting standard volument and number: X (X)
         remaining_ref = str_remove(remaining_ref, " (p)?p\\. ([A-Z])?\\d+(-([A-Z])?\\d+)?"), # clean from extracted pages
         remaining_ref = str_remove_all(remaining_ref, ".*, ,"), # clean dates and already extracted titles
         remaining_ref = str_remove(remaining_ref, "( |^)?\\d+ \\(\\d+(-\\d+)?\\)"), # clean from extracted volume and number
         volume_and_number = ifelse(is.na(volume_and_number), str_extract(remaining_ref, "(?<= )([A-Z])?\\d+(-\\d+)?"), volume_and_number), # extract remaining numbers
         remaining_ref = str_remove(remaining_ref, " ([A-Z])?\\d+(-\\d+)?"), # clean from remaining numbers
         journal = ifelse(str_detect(remaining_ref, "(W|w)orking (P|p)aper"), "Working Paper", NA),
         journal = ifelse(str_detect(remaining_ref, "(M|m)anuscript"), "Manuscript", journal),
         journal = ifelse(str_detect(remaining_ref, "(M|m)imeo"), "Mimeo", journal),
         publisher = ifelse(is.na(journal), remaining_ref, NA) %>% str_trim("both"),
         first_page = str_extract(pages, "\\d+"),
         volume = str_extract(volume_and_number, "\\d+"),
         issue = str_extract(volume_and_number, "(?<=\\()\\d+(?=\\))"),
         book_title = NA) # to be symetric with "cleaned_articles"

cleaned_non_articles <- cleaning_non_articles %>% 
  select(citing_id, id_ref, authors, year, title, journal, volume, issue, pages, first_page, book_title, publisher, references)

# merging the two files.
cleaned_ref <- rbind(cleaned_articles, cleaned_non_articles)

#' Now we have all the references, we can do a bit of cleaning on the authors name,
#' and extract useful information, like DOI, for matching later.

cleaned_ref <- cleaned_ref %>% 
  mutate(authors = str_remove(authors, " Jr\\."), # standardising authors name to favour matching later
         authors = str_remove(authors, "^\\(\\d{4}\\)(\\.)?( )?"),
         authors = str_remove(authors, "^, "),
         authors = ifelse(is.na(authors), str_extract(references, ".*[:upper:]\\.(?= \\d{4})"), authors), # specific case
         journal = str_remove(journal, "[:punct:]$"), # remove unnecessary punctuations at the end
         doi = str_extract(references, "(?<=DOI(:)? ).*|(?<=\\/doi\\.org\\/).*"),
         pii = str_extract(doi, "(?<=PII ).*"),
         doi = str_remove(doi, ",.*"), # cleaning doi
         pii = str_remove(pii, ",.*"), # cleaning pii
  ) 

#' ## Matching references together.
#'
#' What we need to do now is to find which references are the same, to give them 
#' a unique ID. The trade-off is to match as many true positive as possible (references that are the same)
#' while avoiding to match false positive, that is references that have some information
#' in common, but that are not the same references. For instance, matching only by the authors
#' name and the year is not sufficient, as these authors can have published several 
#' articles the same year. Here are several ways to identify a common reference that bear
#' very few risks of matching together different references:
#' - same first author or authors, year, volume and page (this is the most secure one): `fayvp` & `ayvp`
#' - same journal, volume, issue and first page: `jvip`
#' - same author, year and title: ayt
#' - same tite, year and first page: `typ`
#' - same Doi or PII
#' 
#' We extract first author surname to favour matching as there are more possibilities of 
#' differences for several authors that would prevent us to match same references.
#' 
#' For each type of matching, we are giving a new id to the matched references, by giving
#' the `id_ref` of the first references match. At the end, we compare all the new id created
#' with all the matching methods, and we take the smaller id.

cleaned_ref <- cleaned_ref %>%
  mutate(first_author = str_extract(authors, "^[[:alpha:]+[']?[ -]?]+, ([A-Z]\\.[ -]?)?([A-Z]\\.[ -]?)?([A-Z]\\.)?[A-Z]\\.(?=(,|$))"),
         first_author_surname = str_extract(first_author, ".*(?=,)"),
         across(.cols = c("authors", "first_author", "journal", "title"), ~toupper(.))) 

matching_ref <- function(data, id_ref, ..., col_name){
  match <- data %>% 
    group_by(...) %>% 
    mutate(new_id = min({{id_ref}})) %>% 
    drop_na(...) %>% 
    ungroup() %>% 
    select({{id_ref}}, new_id) %>% 
    rename_with(~ paste0(col_name, "_new_id"), .cols = new_id)
  
  data <- data %>% 
    left_join(match)
}

identifying_ref <- cleaned_ref %>%
  matching_ref(id_ref, first_author_surname, year, title, col_name = "fayt") %>% 
  matching_ref(id_ref, journal, volume, issue, first_page, col_name = "jvip") %>% 
  matching_ref(id_ref, authors, year, volume, first_page, col_name = "ayvp") %>% 
  matching_ref(id_ref, first_author_surname, year, volume, first_page, col_name = "fayvp") %>%
  matching_ref(id_ref, title, year, first_page, col_name = "typ") %>% 
  matching_ref(id_ref, pii, col_name = "pii") %>% 
  matching_ref(id_ref, doi, col_name = "doi") 

identifying_ref <- identifying_ref %>%  
  mutate(new_id_ref = select(., ends_with("new_id")) %>%  reduce(pmin, na.rm = TRUE),
         new_id_ref = ifelse(is.na(new_id_ref), id_ref, new_id_ref))  %>% 
  relocate(new_id_ref, .after = citing_id) %>% 
  select(-id_ref & ! ends_with("new_id")) 

#' ## Creating the different tables of data
#' 
#' Now we have matched the references, we can reorganise the table of direct citation
#' connectiong the citing articles to the references. We have as many lines as the number
#' of citations by citing articles.
direct_citation <- identifying_ref %>% 
  relocate(new_id_ref, .after = citing_id) %>% 
  select(-id_ref & ! ends_with("new_id")) 

#' We can extract the list of all the references cited. We have as many lines as references
#' cited by citing articles (i.e. a reference cited multiple times is only once in the table).
#' As for matched references, we have different information, we take a line where information
#' seem to be the most complete. 
important_info <- c("authors",
                    "year",
                    "title",
                    "journal",
                    "volume",
                    "issue",
                    "pages",
                    "book_title",
                    "publisher")
references <- direct_citation %>% 
  mutate(nb_na = rowSums(!is.na(select(., all_of(important_info))))) %>% 
  group_by(new_id_ref) %>% 
  slice_max(order_by = nb_na, n = 1, with_ties = FALSE)

#' We can save all the cleaned data
nodes <- scopus_search %>% 
select(-references)
saveRDS(nodes, here(scopus_path,
                    "data_cleaned",
                    "scopus_manual_nodes.rds"))
write_excel_csv2(nodes, here(scopus_path,
                             "data_cleaned",
                             "scopus_manual_nodes.csv"))

saveRDS(direct_citation, here(scopus_path,
                              "data_cleaned",
                              "scopus_manual_direct_citation.rds"))
write_excel_csv2(direct_citation, here(scopus_path,
                                       "data_cleaned",
                                       "scopus_manual_direct_citation.csv"))

saveRDS(references, here(scopus_path,
                         "data_cleaned",
                         "scopus_manual_references.rds"))
write_excel_csv2(references, here(scopus_path,
                                  "data_cleaned",
                                  "scopus_manual_references.csv"))

#' We create here the edges of the coupling network, using the `biblionetwork`
#' package.
direct_citation <- direct_citation %>% 
  mutate(new_id_ref = as.character(new_id_ref))
edges <- biblionetwork::coupling_strength(direct_citation, 
                                          "citing_id", 
                                          "new_id_ref",
                                          weight_threshold = 3)
saveRDS(edges, here(scopus_path,
                    "data_cleaned",
                    "scopus_manual_edges.rds"))
write_excel_csv2(select(edges, Source, Target, weight), here(scopus_path,
                                                             "data_cleaned",
                                                             "scopus_manual_edges.csv"))
#' # Clean the references with [Anystyle](https://anystyle.io/)
#'
#' Anystyle has a online version where you can put the text for which you want
#' to identify references. However, we will use the command line in order
#' to identify more references than what the online website allow (10000 while
#' we have more than 100000 raw references). 
#' 
#' Anystyle could be installed as a RubyGem. You thus need to install Ruby
#' ([here](https://rubyinstaller.org/downloads/) for Windows) and then install
#' anystyle: `gem install anystyle` (see more information [here](https://anystyle.io/)).
#' As you need to use the command line interface, you also need to install anystyle-cli:
#' `gem install anystyle-cli`. 
#' 
#' One that done, you need to save all the references (with one references
#' per line) in a  `.txt`. 

ref_text <- paste0(references_extract$references, collapse = "\\\n")
name_file <- "ref_in_text.txt"
write_file(ref_text,
           here(scopus_path,
                name_file))

#' To create the anystyle command, you need to name the repository where you
#' will send the `.bib` created by anystyle from your `.txt`
destination_anystyle <- "anystyle_cleaned"

anystyle_command <- paste0("anystyle -f bib parse ",
                           name_file,
                           " ",
                           destination_anystyle)

#' To use anystyle, you have to use the command line of the terminal. You first
#' have to set the path where the `.txt` is (which is here the `scopus_path`):
#' `cd the_path_where_is_the_.txt`.
#' 
#' The you can copy paste the anystyle command in the terminal, which here is:
#' `anystyle -f bib parse ref_in_text.txt anystyle_cleaned`
#' 
#' Once done, you juste have to wait for anystyle to produce the `.bib` file
#' that you transform in a data frame thanks to the `bib2df` package.

bib_ref <- bib2df(here(scopus_path,
                       destination_anystyle,
                       "ref_in_text.bib"))
bib_ref <- bib_ref %>% 
  janitor::clean_names() %>% 
  select_if(~!all(is.na(.))) # removing all empty columns

info_bib_ref <- bib_ref %>% 
  select(category, journal, title, publisher) %>% 
  rename_with(~paste0("bib_", .x)) %>% 
  mutate(id_ref = 1:n())

#' # Extracting Scopus data from API
#' 
#' ## Setting tokens
#' I call here the `API_key` of Scopus website and the `insttoken` I got from
#' them.

path_key <- here(path.expand("~"),
                 "tools", 
                 "scopus",
                 "scopus_api_key.txt")
file <- read_lines(path_key)
api_key <- str_extract(file[3], "(?<=Key: ).*")
insttoken <- str_extract(file[4], "(?<=insttoken: ).*")

set_api_key(api_key)
insttoken <- inst_token_header(insttoken)
is_elsevier_authorized(api_key = api_key, headers = insttoken)

#' ## Running the query and extracting the data
#'
#' We first run the query using "[Scopus Search API](https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl)" 
#' via `rscopus`. We get raw data with a lot of information in
#' it (the data but also information on the query, etc.). We use `rscopus` to 
#' put the data in different data.frames. We can finally extract the different 
#' tables:
#' - A table with all the articles and their metadata;
#' - A table with the list of all authors of the articles;
#' - A table with the list of affiliations.

dsge_query <- rscopus::scopus_search("TITLE-ABS-KEY(DSGE) OR TITLE-ABS-KEY(\"Dynamic Stochastic General Equilibrium\")", 
                                     view = "COMPLETE",
                                     headers = insttoken)
dsge_data_raw <- gen_entries_to_df(dsge_query$entries)

#' Saving intermediate object after the query: 
#' `saveRDS(dsge_data_raw, here(scopus_path, "scopus_dsge_data_raw.rds"))`

dsge_papers <- dsge_data_raw$df %>% 
  as_tibble()

dsge_affiliations <- dsge_data_raw$affiliation %>% 
  as_tibble()

dsge_authors <- dsge_data_raw$author %>% 
  as_tibble()

#' Now that we have the articles, we have to extract the references using scopus
#' "[Abstract Retrieval API](https://dev.elsevier.com/sc_abstract_retrieval_views.html)".
#' We use article internal identifier to find references. But we cannot query references
#' with several identifiers, so we need to make loop to extract references one by one.

citing_articles <- dsge_papers$`dc:identifier`
citation_list <- list()

for(i in 1:length(citing_articles)){
  citations_query <- abstract_retrieval(citing_articles[i],
                                        identifier = "scopus_id",
                                        view = "REF",
                                        headers = insttoken)
  citations <- gen_entries_to_df(citations_query$content$`abstracts-retrieval-response`$references$reference)
  
  message(i)
  if(length(citations$df) > 0){
    message(paste0(citing_articles[i], " n'est pas nul."))
    citations <- citations$df %>% 
      as_tibble(.name_repair = "unique") %>%
      select_if(~!all(is.na(.)))
    
    citation_list[[citing_articles[i]]] <- citations
  }
}

dsge_references <- bind_rows(citation_list, .id = "citing_art")
saveRDS(dsge_references, here(scopus_path,
                              "scopus_dsge_references.rds"))


#' # Draft bit of codes

# Extracting and cleaning journal names
journals <- identifying_ref %>% 
  select(new_id_ref, journal) %>% 
  add_count(new_id_ref, journal, name = "count_per_ref") %>% 
  unique %>% 
  add_count(new_id_ref, name = "number_form")%>% 
  filter(!is.na(journal) & number_form > 5 & count_per_ref > 5) %>%
  group_by(new_id_ref) %>% 
  slice_max(count_per_ref, n = 1, with_ties = FALSE) %>% 
  select(new_id_ref, journal) %>% 
  rename("journal_cleaned" = journal) 


