#'@export
enrichment_object_to_db <- function(enrichment_object,
                                    enrichment_name,
                                    module_name,
                                    enrichment_method,
                                    con){
   dbWriteTable(conn = con, "enrichment_register", prepare_enrichment_register(enrichment_name,
                                                                               module_name,
                                                                               enrichment_method),
               append = TRUE)

  object_blob <- serialize_to_df(enrichment_object, "enrichment_object") %>%
    cbind(enrichment_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "enrichment_objects", object_blob,
               append = TRUE)
}
#'@export
enrichment_object_from_db <- function(enrichment_name, con){
  query <- sprintf("SELECT * FROM enrichment_objects WHERE enrichment_name IS '%s' ",
                   enrichment_name)
  raw_enrichment <- dbGetQuery(con, query)
  deserialize_object(raw_enrichment$enrichment_object)
}
#'@export
get_available_enrichment_objects <- function(con){
  if(!RSQLite::dbExistsTable(con, "enrichment_register")){
    stop("There are no enrichment results available", call. = F)
  }
  dbGetQuery(con, "SELECT *  FROM enrichment_register")
}
prepare_enrichment_register <- function(enrichment_name, module_name, enrichment_method){
  as.data.frame(t(c(enrichment_name, module_name, enrichment_method))) %>%
    set_colnames(c("enrichment_name", "module_name", "enrichment_method"))
}


#'@export
get_input_name_by_enrichment_row <- function(enrichment_name, con){
  query <- sprintf("SELECT DISTINCT input_name FROM module_register
  WHERE module_name IS (SELECT module_name FROM enrichment_register WHERE enrichment_name IS '%s') ",
                   enrichment_name)

  dbGetQuery(con, query)$input_name
}
#' @export
delete_enrichment_object <- function(enrichment_name, con){
  delete_row("enrichment_register", "enrichment_name", enrichment_name, con)
  delete_row("enrichment_objects", "enrichment_name", enrichment_name, con)

}
