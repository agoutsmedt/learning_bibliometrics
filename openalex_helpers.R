#' Find Concept Relationships in Openalex Concepts Data
#'
#' This function explores the hierarchical relationships of a given concept in a dataset, identifying both its parents (up to the highest level) and children (down to the lowest level).
#' @param data A data frame containing the concepts and their relationships.
#' @param concept The name of the concept for which to find relationships (as a string).
#' @return A data.table listing the related concepts, including their display names, normalized names, and levels.
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
#' find_relationships_dt(your_data, "geometry")
find_concept_relationships <- function(data, concept, exact_display_name = FALSE) {
  data.table::setDT(data) # Ensure the input is treated as a data.table
  
  # Split 'parent_display_names' into multiple rows and remove temporary index column
  data <- data[, .(openalex_id, display_name, normalized_name, level,
                   parent_display_names = unlist(tstrsplit(parent_display_names, ", "))), 
               by = .I][, I := NULL]
  
  # Normalize the concept for case-insensitive matching
  if(! exact_display_name) concept <- stringr::str_to_sentence(concept) 
  
  # Retrieve the unique level of the input concept
  concept_level <- data[display_name == concept, level] %>% 
    unique()
  
  # Initialize a list to store related concepts at each level
  related <- vector(mode = "list", length = 6)
  names(related) <- as.character(0:5)
  
  # Directly assign the input concept to its level
  related[[as.character(concept_level)]] <- data[display_name %in% concept,]
  
  # Finding parents recursively
  if(concept_level > 0){
    for(i in (concept_level-1):0){
      related[[paste(i)]] <- data[display_name %in% related[[paste(i+1)]]$parent_display_names,]#[, .(display_name, normalized_name, parent_display_names_search)] 
    }
  }
  
  # Finding children recursively
  if(concept_level < 5){
    for(j in (concept_level+1):5){
      related[[paste(j)]] <- data[parent_display_names %in% related[[paste(j-1)]]$display_name,]#[, .(display_name, normalized_name, parent_display_names_search)] 
    }
  }
  
  # Combine all related concepts into one data.table and order by level
  extracted_concepts <- data.table::rbindlist(related) %>% 
    .[order(level)]
  return(extracted_concepts)
}
