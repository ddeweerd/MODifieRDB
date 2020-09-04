#'@export
enrichment_object_to_db <- function(enrichment_object,
                                    module_name,
                                    enrichment_method,
                                    con){
   dbWriteTable(conn = con, "enrichment_register", prepare_enrichment_register(module_name,
                                                                              enrichment_method),
               append = TRUE)

  dbWriteTable(conn = con, "enrichment_objects", serialize_to_df(enrichment_object,
                                                                 "enrichment_object"),
               append = TRUE)
}
#'@export
enrichment_object_from_db <- function(rowid, con){
  query <- sprintf("SELECT * FROM enrichment_objects WHERE rowid IS '%s' ", rowid)
  raw_enrichment <- dbGetQuery(con, query)
  deserialize_object(raw_enrichment$enrichment_object)
}
#'@export
get_available_enrichment_objects <- function(con){
  if(!RSQLite::dbExistsTable(con, "enrichment_register")){
    stop("There are no enrichment results available", call. = F)
  }
  dbGetQuery(con, "SELECT rowid, *  FROM enrichment_register")
}
prepare_enrichment_register <- function(module_name, enrichment_method){
  as.data.frame(t(c(module_name, enrichment_method))) %>%
    set_colnames(c("module_name", "enrichment_method"))
}


#'@export
get_input_name_by_enrichment_row <- function(row_id, con){
  query <- sprintf("SELECT DISTINCT input_name FROM module_register
  WHERE module_name IS (SELECT module_name FROM enrichment_register WHERE rowid IS '%s') ", row_id)

  dbGetQuery(con, query)$input_name
}
#' @export
delete_enrichment_object <- function(row_id, con){
  delete_row("enrichment_register", "row_id", row_id, con)
  delete_row("enrichment_objects", "row_id", row_id, con)

}
