#'@export
enrichment_object_to_db <- function(enrichment_object,
                                    module_name,
                                    enrichment_method,
                                    con){
  if(!RSQLite::dbExistsTable(con, "enrichment_register")){
     create_enrichment_tables(con)
  }
  dbWriteTable(conn = con, "enrichment_register", prepare_enrichment_register(module_name,
                                                                              enrichment_method),
               append = TRUE)

  dbWriteTable(conn = con, "enrichment_objects", serialize_to_df(enrichment_object),
               append = TRUE)
}
#'@export
enrichment_object_from_db <- function(rowid, con){
  query <- sprintf("SELECT * FROM enrichment_objects WHERE rowid IS '%s' ", rowid)
  raw_enrichment <- dbGetQuery(con, query)
  unserialize(raw_enrichment[1,1][[1]])
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

serialize_to_df <- function(enrichment_object){
  tibble::tibble(data = blob::as.blob(serialize(enrichment_object, NULL))) %>%
    set_colnames("enrichment_object")
}
