#'@export
comhub_object_to_db <- function(comhub_object,
                                comhub_name,
                                input_name,
                                con){
  dbWriteTable(conn = con, "comhub_register", prepare_comhub_register(comhub_name,
                                                                      input_name),
               append = TRUE)

  object_blob <- serialize_to_df(comhub_object, "comhub_object") %>%
    cbind(comhub_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "comhub_objects", object_blob,
               append = TRUE)
}
#'@export
comhub_object_from_db <- function(comhub_name, con){
  query <- sprintf("SELECT * FROM comhub_objects WHERE comhub_name IS '%s' ",
                   comhub_name)
  raw_comhub <- dbGetQuery(con, query)
  deserialize_object(raw_comhub$comhub_object)
}
#'@export
get_available_comhub_objects <- function(con){

  dbGetQuery(con, "SELECT *  FROM comhub_register")
}
prepare_comhub_register <- function(comhub_name, input_name){
  as.data.frame(t(c(comhub_name, input_name))) %>%
    set_colnames(c("comhub_name", "input_name"))
}

#' @export
delete_comhub_object <- function(comhub_name, con){
  delete_row("comhub_register", "comhub_name", comhub_name, con)
  delete_row("comhub_objects", "comhub_name", comhub_name, con)

}
#' @export
get_comhub_by_module_name <- function(module_name, con){
  query <- sprintf('SELECT * FROM comhub_register WHERE input_name IS (SELECT input_name FROM module_register WHERE module_name IS "%s")',
                   module_name)

  input_and_comhub <- RSQLite::dbGetQuery(con, query)
  check_comhub_presence(input_and_comhub$input_name, con)
  comhub_object_from_db(comhub_name = input_and_comhub$comhub_name, con)
}
