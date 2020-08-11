#' @export
MODifieR_object_to_db.MODifieR_module <- function(MODifieR_module, object_name, con){

  check_unique_module(object_name, con)

  module_name <- object_name

  if (class(MODifieR_module)[2] %in% ppi_methods){
    check_ppi_network_presence(MODifieR_module, con = con)
  }else if (class(MODifieR_module)[2] %in% ppi_db_methods){
    check_ppi_db_presence(MODifieR_module, con = con)
  }

  check_input_object_presence(MODifieR_module, con = con)

  settings <- prepare_input_settings_for_db(MODifieR_module, module_name)

  main_register <- get_main_module_register_row(MODifieR_module, module_name, con)

  register_row <- get_register_row(MODifieR_module)

  if (!is.null(register_row)){
    register_row <- cbind(module_name, register_row)
  }

  write_to_db(MODifieR_module,
              main_register = main_register,
              register_row = register_row,
              settings = settings,
              con = con)
}

#Prepares for the main module register
get_main_module_register_row <- function(MODifieR_module, module_name, con){
  ppi_name <- as.character(MODifieR_module$settings$ppi_network)
  if (length(ppi_name) == 0){
    if (!length(MODifieR_module$settings$db) == 0){
      ppi_name <- match_db_loc_to_ppi(MODifieR_module$settings$db, con = con)
    }else{
    ppi_name <- NA
    }
  }
  cbind("module_name" = module_name,
        "module_length" = length(MODifieR_module$module_genes),
        "input_name" = as.character(MODifieR_module$settings$MODifieR_input),
        "module_type" = class(MODifieR_module)[2],
        "ppi_name" = ppi_name) %>%
    as.data.frame(., stringsAsFactors = F, drop = F)
}

prepare_module_genes_row <- function(MODifieR_module, main_register){
  cbind("module_name" = main_register$module_name,
        "module_type" = main_register$module_type,
        "module_genes" = collapse_genes(MODifieR_module$module_genes)) %>%
    as.data.frame(., stringsAsFactors = F)
}
#' Get the names of all module objects in the database
#'
#' @inheritParams mcode_db
#'
#'@export
get_available_module_objects <- function(con){
  dbGetQuery(con, "SELECT * FROM module_register")
}

get_module_type <- function(module_name, con){
  query <- sprintf("SELECT module_type FROM module_register WHERE module_name IS '%s' ", module_name)
  as.character(dbGetQuery(con, query))

}
#' Retrieve an module object from the database by module_name
#' @inheritParams mcode_db
#'@export
MODifieR_module_from_db <- function(module_name, con){
  module_type <- get_module_type(module_name = module_name, con)
  if(module_type == "Mcode"){
    fetch_mcode_module(module_name, con)
  }else if (module_type == "Clique_Sum_permutation"){
    fetch_clique_sum_permutation_module(module_name, con)
  }else if (module_type == "DIAMOnD"){
    fetch_diamond_module(module_name, con)
  }else if (module_type == "Correlation_clique"){
    fetch_correlation_clique_module(module_name, con)
  }else if (module_type == "module_discoverer"){
    fetch_module_discoverer_module(module_name, con)
  }else if (module_type == "MODA"){
    fetch_moda_module(module_name, con)
  }else if (module_type == "WGCNA"){
    fetch_wgcna_module(module_name, con)
  }else if (module_type == "DiffCoEx"){
    fetch_diffcoex_module(module_name, con)
  }
}

get_module_genes <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_genes WHERE module_name IS '%s' ", module_name)
  module_genes <- dbGetQuery(con, query)
  uncollapse_genes(module_genes$module_genes)
}

#' Delete a module object from database by module_name
#' @inheritParams mcode_db
#'@export
delete_module_object <- function(module_name, con){
  module_type <- get_module_type(module_name = module_name, con)
  dbBegin(con)
  if(module_type == "Mcode"){
    delete_mcode_module(module_name, con)
  }else if (module_type == "Clique_Sum_permutation"){
    delete_clique_sum_permutation_module(module_name, con)
  }else if (module_type == "DIAMOnD"){
    delete_diamond_module(module_name, con)
  }else if (module_type == "Correlation_clique"){
    delete_correlation_clique_module(module_name, con)
  }else if (module_type == "module_discoverer"){
    delete_module_discoverer_module(module_name, con)
  }else if (module_type == "MODA"){
    delete_moda_module(module_name, con)
  }else if (module_type == "WGCNA"){
    delete_wgcna_module(module_name, con)
  }else if (module_type == "DiffCoEx"){
    delete_diffcoex_module(module_name, con)
  }
  dbCommit(con)
}
