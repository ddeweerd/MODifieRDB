#' @import MODifieR
#' @import dbplyr
#' @import dplyr
#' @import magrittr
#' @import RSQLite
#' @import tibble



#' @title  Connect to database
#'
#' @param db Location of the database
#'
#' @return Returns a database connection object to be used in all database related functions.
#'
#' @details
#' Connects to an SQLite database. If the database does not exist, an empty database will be created.
#'@export
connect_to_db <- function(db){
  if (file.exists(db)){
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  }else{
    con <- read_sql(db)
  }
  return(con)
}
#' Disconnect from database
#'
#' @inheritParams mcode_db
#'
#'@export
disconnect_db <- function(con){
  RSQLite::dbDisconnect(con)
}
get_table_hash <- function(){
  paste(sample(x = c(LETTERS, letters), size = 25), collapse="")
}
# Writes a data.frame to a table under the name 'hash'
write_table_to_hash <- function(table, hash, con){
  if (is.matrix(table)){
    table <- as.data.frame(table)
  }else if (is.table(table)){
    table <- as.data.frame(table)
  }
  dbWriteTable(con, hash, table, row.names = T)
}
write_hash_tables_to_db <- function(register_row, MODifieR_object, con){
  for (i in 2:ncol(register_row)){
    write_table_to_hash(table = MODifieR_object[[colnames(register_row[i])]],
                        hash = register_row[1,i],
                        con = con)
  }
}
get_register_row <- function(MODifieR_object){

  register_rownames <- names(MODifieR_object)[names(MODifieR_object) %in% two_dimensional]

  if (length(register_rownames) == 0){
    return(NULL)
  }

  hash_names <- replicate(n = length(register_rownames), get_table_hash())

  matrix(data = hash_names, nrow = 1) %>%
    as.data.frame(., stringsAsFactors = F) %>%
    set_colnames(register_rownames)
}

#Generic callers
#' Stores a MODifieR object in the database
#'
#' @param MODifieR_object Any MODifieR input or module object
#' @param object_name Unique name for the object in the database
#' @inheritParams mcode_db
#'@export
MODifieR_object_to_db <- function(MODifieR_object, object_name, con){
  UseMethod("MODifieR_object_to_db", MODifieR_object)
}
prepare_input_settings_for_db <- function(MODifieR_object, input_name){
  UseMethod("prepare_input_settings_for_db", MODifieR_object)
}

write_to_db <- function(MODifieR_object, main_register, register_row, settings, con){
  UseMethod("write_to_db", MODifieR_object)
}

#Checkers
check_ppi_network_presence <- function(MODifieR_module, con){
  if (!as.character(MODifieR_module$settings$ppi_network) %in%
      get_available_networks(con)){
    stop("PPI network not in database", call. = F)
  }
}
check_input_object_presence <- function(MODifieR_module, con){
  if (!as.character(MODifieR_module$settings$MODifieR_input) %in%
      get_available_input_objects(con)$input_name){
    stop("Input object not in database", call. = F)
  }
}
check_ppi_db_presence <- function(MODifieR_module, con){
  if (!as.character(basename(MODifieR_module$settings$db)) %in%
      basename(get_available_db_networks(con)$ppi_db_location)){
    stop("PPI DB not in database", call. = F)
  }
}
#collapse genes
collapse_genes <- function(genes){
  paste(genes, collapse = " ")
}

uncollapse_genes <- function(genes){
  unlist(strsplit(genes, split = " "))
}

add_unique_genes <- function(genes, con){

  existing_genes <- dbGetQuery(con, "SELECT * FROM unique_genes", stringsAsFactors = F)

  if (nrow(existing_genes) != 0){
    genes <- genes[!genes %in% unname(unlist(existing_genes))]
  }

  add_genes <- as.data.frame(genes) %>%
    set_names("gene_name")

  dbWriteTable(con, "unique_genes", add_genes, append = TRUE)
}
#' Get all unique genes present in the database
#'
#' @inheritParams mcode_db
#'
#' @return A vector containing all unique genes present in the database, both in input and module objects
#'
#' @export
get_unique_genes <- function(con){
  dbGetQuery(con, "SELECT * FROM unique_genes",
             stringsAsFactors = F) %>%
    unlist(.) %>%
    unique(.) %>%
    unname(.)
}

delete_tables <- function(info_table, con){

  tables_to_delete <- unname(unlist(info_table[names(info_table) %in% two_dimensional]))
  for (db_table in tables_to_delete){
    dbRemoveTable(con, db_table)
  }
}

delete_row <- function(table_name, field_identifier, row_identifier, con){
  dbExecute(con, sprintf("DELETE FROM %s WHERE %s IS '%s'",
                         table_name, field_identifier, row_identifier))
}

check_input_existence <- function(input_name, con){
  input_name %in% get_available_input_objects(con)$input_name
}

check_module_existence <- function(module_name, con){
  module_name %in% get_available_module_objects(con)$module_name
}

check_ppi_existence <- function(ppi_name, con){
  ppi_name %in% get_available_networks(con)
}

print_exists <- function(type){
  paste0(type, " already exists in database, choose another name")
}

print_if_null <- function(){
  paste0("Module Name is NULL, probably input field is empty")
}

print_if_empty <- function(){
  paste0("Module Name is empty")
}

check_unique_input <- function(input_name, con){
  if(check_input_existence(input_name, con)){
    stop(print_exists(input_name), call. = F)
  }
}

check_unique_module <- function(module_name, con){
  if(check_module_existence(module_name, con)){
    stop(print_exists(module_name), call. = F)
  }
}

check_unique_ppi <- function(ppi_name, con){
  if(check_ppi_existence(ppi_name, con)){
    stop(print_exists(ppi_name), call. = F)
  }
}

check_if_null <- function(cur_var){
  if (is.null(cur_var)){
    stop(print_if_null(cur_var), call. = F)
  }
}
check_if_empty <- function(cur_var){
  if (cur_var == ""){
    stop(print_if_empty(), call. = F)
  }
}

validate_inference_db <- function(module_name, con){
  check_if_empty(module_name)
  check_if_null(module_name)
  check_unique_module(module_name, con)
}

serialize_to_df <- function(enrichment_object, column_name){
  tibble::tibble(data = blob::as.blob(serialize(enrichment_object, NULL))) %>%
    set_colnames(column_name)
}

deserialize_object <- function(serialized_object){
  unserialize(serialized_object[[1]])
}

