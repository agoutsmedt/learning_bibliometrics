Script for extracting & cleaning scopus data
================
Aurélien Goutsmedt
/ Last compiled on 2022-01-29

-   [1 What is this script for?](#what-is-this-script-for)
-   [2 Loading packages, paths and
    data](#loading-packages-paths-and-data)
    -   [2.1 Packages](#packages)
    -   [2.2 Paths](#paths)
-   [3 Cleaning scopus data from csv
    files](#cleaning-scopus-data-from-csv-files)
    -   [3.1 Matching references together to give them
        ID](#matching-references-together-to-give-them-id)
        -   [3.1.1 author-data-volume-page](#author-data-volume-page)
-   [4 Extracting Scopus data from
    API](#extracting-scopus-data-from-api)
    -   [4.1 Setting tokens](#setting-tokens)
    -   [4.2 Running the query and extracting the
        data](#running-the-query-and-extracting-the-data)
-   [5 Draft bit of codes](#draft-bit-of-codes)

# 1 What is this script for?

In this script, I show how to clean scopus data extracted from scopus
website. For cleaning references, I test two types of cleaning. I clean
all the data myself, but I also try the `anystyle.io` references parser.

I also show how to extract data using the scopus API and the
[`rscopus`](https://johnmuschelli.com/rscopus/index.html) package.

> WARNING: This script is not complete, and some work remains to do,
> notably for cleaning data.

# 2 Loading packages, paths and data

## 2.1 Packages

``` r
package_list <- c(
  "here", # use for paths creation
  "data.table",
  "tidyverse",
  "bib2df", # for cleaning .bib data
  "janitor", # useful functions for cleaning imported data
  "rscopus"
)
for (p in package_list) {
  if (p %in% installed.packages() == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

github_list <- c(
  "agoutsmedt/biblionetwork" # creating edges
)
for (p in github_list) {
  if (gsub(".*/", "", p) %in% installed.packages() == FALSE) {
    devtools::install_github(p)
  }
  library(gsub(".*/", "", p), character.only = TRUE)
}
```

## 2.2 Paths

``` r
data_path <- here(path.expand("~"),
                  "data",
                  "tuto_biblio_dsge")
scopus_path <- here(data_path, "scopus")
```

# 3 Cleaning scopus data from csv files

``` r
scopus_search_1 <- read_csv(here(scopus_path, "scopus_search_1998-2013.csv"))
scopus_search_2 <- read_csv(here(scopus_path, "scopus_search_2014-2021.csv"))
scopus_search <- rbind(scopus_search_1, scopus_search_2) %>% 
  mutate(Citing_ID = paste0("A", 1:n())) # We create a unique ID for each doc of our corpus
```

List of things to clean:

-   several `Authors` per document. For some operation, better to have
    an association between a unique paper and a unique author (so at the
    end you should have as many lines as authors for one article);
-   several `Affiliations` per article as well as several
    `Authors with affiliations`. It allows you to connect authors with
    their affiliations, but here again you need to separate in as many
    lines as authors (and as `Affiliations`, considering that one author
    can have many affiliations);
-   `References`;
-   Possibility to separate `Author Keywords` and `Index Keywords` if
    you want to use it.

Depending on what we want to study, there is several separate data.frame
to create: - we can keep the citing articles/nodes data.frame (and
remove some useless columns); - we can have an author table, using the
column `Authors`, but also the `Authors with affiliations` column to
associate affiliations to author; - we can have an affiliation table
with the `Affiliations` column. The table should be separated from the
author table, as one author can have many affiliations, and as the
`Authors with affiliations` column can be missing while the
`Affiliations` column is not - a reference table, with all the
`references` cited. It involves to clean the references such as giving
them a unique ID for similar references; - the direct citation table,
with all the references cited by citing articles. This is the bridge
between the citing articles table and the references table. And this is
the table we will use to build the edges of our network. - Tables with
`Author Keywords` and `Index Keywords`.

``` r
references_extract <- scopus_search %>% 
  filter(! is.na(References)) %>% 
  select(Citing_ID, References) %>% 
  separate_rows(References, sep = "; ") %>% 
  mutate(id_ref = 1:n()) %>% 
  as_tibble

# standard regex
extract_authors_regex <- ".*[:upper:][:alpha:]+( Jr(.)?)?, ([A-Z]\\.[ -]?)?([A-Z]\\.[ -]?)?([A-Z]\\.)?[A-Z]\\."
extract_year_brackets <- "(?<=\\()\\d{4}(?=\\))"
extract_pages <- "(?<= (p)?p\\. )([A-Z])?\\d+(-([A-Z])?\\d+)?"
extract_volume_and_number <- "(?<=( |^)?)\\d+ \\(\\d+(-\\d+)?\\)"

# cleaning references
cleaning_references <- references_extract %>% 
  mutate(authors = str_extract(References, paste0(extract_authors_regex, "(?=, )")),
         remaining_ref = str_remove(References, paste0(extract_authors_regex, ", ")), # cleaning from authors
         is_article = ! str_detect(remaining_ref, "^\\([:digit:]{4}"), 
         year = str_extract(References, extract_year_brackets) %>% as.integer)

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
  select(Citing_ID, id_ref, authors, year, title, journal, volume, issue, pages, first_page, book_title, publisher, References)

# cleaning books

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
  select(Citing_ID, id_ref, authors, year, title, journal, volume, issue, pages, first_page, book_title, publisher, References)


cleaned_ref <- rbind(cleaned_articles, cleaned_non_articles)

# some cleaning and normalising steps

cleaned_ref <- cleaned_ref %>% 
  mutate(authors = str_remove(authors, " Jr\\."), # standardising authors name to favour matching later
         authors = str_remove(authors, "^\\(\\d{4}\\)(\\.)?( )?"),
         authors = str_remove(authors, "^, "),
         authors = ifelse(is.na(authors), str_extract(References, ".*[:upper:]\\.(?= \\d{4})"), authors), # specific case
         doi = str_extract(References, "(?<=DOI(:)? ).*|(?<=\\/doi\\.org\\/).*"),
         pii = str_extract(doi, "(?<=PII ).*"),
         doi = str_remove(doi, ",.*"), # cleaning doi
         pii = str_remove(pii, ",.*"), # cleaning pii
  ) 
```

## 3.1 Matching references together to give them ID

There are several ways we want to identify a common reference: - same
first author or authors, year, volume and page (this is the most secure
one): fayvp & ayvp - same journal, volume, issue and first page: jvip -
same author, year and title: ayt - same Doi: doi, pii

### 3.1.1 author-data-volume-page

``` r
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
  relocate(new_id_ref, .after = Citing_ID) %>% 
  select(-id_ref & ! ends_with("new_id")) 

direct_citation <- identifying_ref %>% 
  relocate(new_id_ref, .after = Citing_ID) %>% 
  select(-id_ref & ! ends_with("new_id")) 

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

# Clean nodes
nodes <- scopus_search %>% 
  select(-References)

# creating network


# saving manually cleaned data
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

direct_citation <- direct_citation %>% 
  mutate(new_id_ref = as.character(new_id_ref))
edges <- biblionetwork::coupling_strength(direct_citation, 
                                          "Citing_ID", 
                                          "new_id_ref",
                                          weight_threshold = 3)
saveRDS(edges, here(scopus_path,
                    "data_cleaned",
                    "scopus_manual_edges.rds"))
write_excel_csv2(select(edges, Source, Target, weight), here(scopus_path,
                                                             "data_cleaned",
                                                             "scopus_manual_edges.csv"))

# Save all reference and put them in anystyle

ref_text <- paste0(references_extract$References, collapse = "\\\n")
name_file <- "ref_in_text.txt"
write_file(ref_text,
           here(scopus_path,
                name_file))
destination_anystyle <- "anystyle_cleaned"

anystyle_command <- paste0("anystyle -f bib parse ",
                           name_file,
                           " ",
                           destination_anystyle)
# extracting bib data from anystyle
bib_ref <- bib2df(here(scopus_path,
                       destination_anystyle,
                       "ref_in_text.bib"))
bib_ref <- bib_ref %>% 
  janitor::clean_names() %>% 
  select_if(~!all(is.na(.)))

info_bib_ref <- bib_ref %>% 
  select(category, journal, title, publisher) %>% 
  rename_with(~paste0("bib_", .x)) %>% 
  mutate(id_ref = 1:n())

# merging with cleaned data

test <- cleaned_articles %>% 
  left_join(info_bib_ref)
```

# 4 Extracting Scopus data from API

## 4.1 Setting tokens

I call here the `API_key` of Scopus website and the `insttoken` I got
from them.

``` r
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
```

## 4.2 Running the query and extracting the data

We first run the query using “[Scopus Search
API](https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl)” via
`rscopus`. We get raw data with a lot of information in it (the data but
also information on the query, etc.). We use `rscopus` to put the data
in different data.frames. We can finally extract the different tables: -
A table with all the articles and their metadata; - A table with the
list of all authors of the articles; - A table with the list of
affiliations.

``` r
dsge_query <- rscopus::scopus_search("TITLE-ABS-KEY(DSGE) OR TITLE-ABS-KEY(\"Dynamic Stochastic General Equilibrium\")", 
                                     view = "COMPLETE",
                                     headers = insttoken)
dsge_data_raw <- gen_entries_to_df(dsge_query$entries)
```

Saving intermediate object after the query:
`saveRDS(dsge_data_raw, here(scopus_path, "scopus_dsge_data_raw.rds"))`

``` r
dsge_papers <- dsge_data_raw$df %>% 
  as_tibble()

dsge_affiliations <- data$affiliation %>% 
  as_tibble()

dsge_authors <- data$author %>% 
  as_tibble()
```

Now that we have the articles, we have to extract the references using
scopus “[Abstract Retrieval
API](https://dev.elsevier.com/sc_abstract_retrieval_views.html)”. We use
article internal identifier to find references. But we cannot query
references with several identifiers, so we need to make loop to extract
references one by one.

``` r
citing_articles <- dsge_papers$`dc:identifier`
citations_query <- abstract_retrieval(citing_articles[1],
                                      identifier = "scopus_id",
                                      view = "REF",
                                      headers = insttoken)
citations <- gen_entries_to_df(citations_query$content$`abstracts-retrieval-response`$references$reference)
citation_list <- citations$df %>% 
  as_tibble() %>% 
  mutate(Citing_art = citing_articles[1])

for(i in 2:length(resting_id)){
  citations_query <- abstract_retrieval(resting_id[i],
                                        identifier = "scopus_id",
                                        view = "REF",
                                        headers = insttoken)
  citations <- gen_entries_to_df(citations_query$content$`abstracts-retrieval-response`$references$reference)
  
  message(i)
  if(length(citations$df) > 0){
    message(paste0(resting_id[i], " n'est pas nul."))
    citations <- citations$df %>% 
      as_tibble(.name_repair = "unique") %>%
      mutate(Citing_art = resting_id[i]) %>% 
      select_if(~!all(is.na(.)))
    
    citation_list <- bind_rows(citation_list, citations)
  }
}

done_id <- citation_list$Citing_art %>% unique
resting_id <- setdiff(citing_articles, done_id)
dsge_references <- citation_list
saveRDS(dsge_references, here(scopus_path,
                              "scopus_dsge_references.rds"))
```

# 5 Draft bit of codes

``` r
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
```
