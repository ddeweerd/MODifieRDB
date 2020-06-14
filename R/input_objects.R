#' @export
MODifieR_object_to_db.MODifieR_input <- function(MODifieR_object, object_name, con){

  input_name <- object_name

  settings <- prepare_input_settings_for_db(MODifieR_object, input_name)

  register_row <- get_register_row(MODifieR_object = MODifieR_object)
  register_row <- cbind(input_name, register_row)

  main_register <- get_main_input_register_row(MODifieR_object, input_name)

  write_to_db(register_row = register_row,
              MODifieR_object = MODifieR_object,
              main_register = main_register,
              settings = settings,
              con = con)

}
#Prepares for the main input register
get_main_input_register_row <- function(MODifieR_object, input_name){
  cbind("input_name" = input_name, "input_type" = class(MODifieR_object)[3]) %>%
    as.data.frame(., stringsAsFactors = F)
}

##### Fetching functions

#' Get all names from input objects present in the database
#'
#' @inheritParams mcode_db
#'
#'
#'@export
get_available_input_objects <- function(con){
  dbGetQuery(con, "SELECT * FROM input_register")
}

get_input_type <- function(input_name, con){
  query <- sprintf("SELECT input_type FROM input_register WHERE input_name IS '%s' ", input_name)
  as.character(dbGetQuery(con, query))

}
#' Retrieve an input object from the database by input_name
#' @inheritParams mcode_db
#'@export
MODifieR_input_from_db <- function(input_name, con){
  input_type <- get_input_type(input_name = input_name, con)
  if(input_type == "MicroArray"){
    fetch_microarray_object(input_name, con)
  }else if (input_type == "RNA_seq"){
    fetch_rna_object(input_name, con)
  }

}

#' Delete an input object from the database
#'
#' @inheritParams mcode_db
#' @param delete_module_objects Also delete all module objects inferred with this input object? D
#' efault is FALSE
#'@export
delete_input_object <- function(input_name, delete_module_objects = FALSE, con){
  input_type <- get_input_type(input_name, con)
  dbBegin(con)
  if (input_type == "MicroArray"){
    delete_microarray_object(input_name, con)
  }else if (input_type == "RNA_seq"){
    delete_rna_object(input_name, con)
  }
  dbCommit(con)
  if (delete_module_objects){
    query <- sprintf("SELECT module_name FROM module_register WHERE input_name IS '%s' ", input_name)
    modules_to_delete <- dbGetQuery(con, query) %>%
      unlist(.) %>%
      unname(.)

    sapply(modules_to_delete, delete_module_object, con = con)
  }
}

