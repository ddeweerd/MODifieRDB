#A Try catch should be added or a try that will drop the hashed table
#If register row insertion goes wrong
#' Store a PPI network in the database
#'
#' @inheritParams mcode_db
#' @param ppi_network A network as a dataframe where the first 2 columns are the interactions
#'@export
ppi_network_to_db <- function(ppi_network, ppi_name, con){

  check_unique_ppi(ppi_name, con)

  ppi_network <- MODifieR::validate_ppi(ppi_network)
  ppi_table <- get_table_hash()

  register_row <- as.data.frame(cbind(ppi_name, ppi_table))

  add_unique_genes(genes = unique(unlist(ppi_network[,1:2])), con = con)

  dbWriteTable(conn = con, "ppi_network_register", register_row, append = TRUE)
  dbWriteTable(con, ppi_table, ppi_network)

}

ppi_db_to_db <- function(ppi_db_location, ppi_name, con){

  register_row <- as.data.frame(cbind(ppi_name, ppi_db_location))
  dbWriteTable(conn = con, "ppi_db_register", register_row, append = TRUE)

}
#' Get the names of all PPI networks available in database
#'
#' @inheritParams mcode_db
#'
#'@export
#Show available PPI networks
get_available_networks <- function(con){
  unname(unlist(dbGetQuery(con, "SELECT ppi_name FROM ppi_network_register")))
}
#' Retrieve a PPI network from database by ppi_name
#' @inheritParams mcode_db
#'@export
ppi_network_from_db <- function(ppi_name, con){
  query <- sprintf("SELECT ppi_table FROM ppi_network_register WHERE ppi_name IS '%s' ", ppi_name)
  ppi_hash <- as.character(dbGetQuery(con, query))
  ppi_network <- dbGetQuery(con, sprintf("SELECT * FROM %s", ppi_hash))

  validate_ppi(ppi_network)
}
#' Get all names from PPI networks present in the database
#' @inheritParams mcode_db
#' @export
get_available_db_networks <- function(con){
  dbGetQuery(con, "SELECT * FROM ppi_db_register")
}

match_db_loc_to_ppi <- function(ppi_db_location, con){
  query <- sprintf("SELECT ppi_name FROM ppi_db_register WHERE ppi_db_location IS '%s' ", ppi_db_location)
  unname(unlist(dbGetQuery(con, query)))
}

match_ppi_to_db_loc <- function(ppi_name, con){
  query <- sprintf("SELECT ppi_db_location FROM ppi_db_register WHERE ppi_name IS '%s' ", ppi_name)
  unname(unlist(dbGetQuery(con, query)))
}

get_info_table_ppi_network <- function(ppi_name, con){
  query <- sprintf("SELECT * FROM ppi_network_register WHERE ppi_name IS '%s' ", ppi_name)
  dbGetQuery(con, query)
}


delete_ppi_from_db <- function(ppi_name, delete_module_objects = F, con){
  info_table <- get_info_table_ppi_network(ppi_name, con)

  delete_tables(info_table, con)

  delete_row("ppi_network_register", "ppi_name", ppi_name, con)
  delete_row("ppi_db_register", "ppi_name", ppi_name, con)

  if (delete_module_objects){
    query <- sprintf("SELECT module_name FROM module_register WHERE ppi_name IS '%s' ", ppi_name)
    modules_to_delete <- dbGetQuery(con, query) %>%
      unlist(.) %>%
      unname(.)

    sapply(modules_to_delete, delete_module_object, con = con)
  }
}

