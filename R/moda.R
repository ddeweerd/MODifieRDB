
prepare_input_settings_for_db.MODA <- function(moda_module, module_name){
  settings <- moda_module$settings
  cbind(module_name, as.data.frame(settings[2:6]))
}


write_to_db.MODA <- function(moda_module,
                             main_register,
                             register_row,
                             settings,
                             con){

  module_genes_row <- prepare_module_genes_row(moda_module, main_register)

  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(moda_module,
                                 "moda_object") %>%
    cbind(module_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "moda_objects", object_blob,
               append = TRUE)



  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "moda_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

moda_get_module_list <- function(group_modules, module_name){

  module_list <- NULL
  for (i in 1:length(group_modules)){

    module_list_row <- c(module_name,
                         collapse_genes(group_modules[[i]]))
    module_list <- rbind(module_list, module_list_row)
  }

  colnames(module_list) <- c("module_name", "module_genes")

  return(as.data.frame(module_list, stringsAsFactors = F))
}

fetch_moda_module <- function(module_name, con){

  query <- sprintf("SELECT * FROM moda_objects WHERE module_name IS '%s' ", module_name)
  moda_object <- dbGetQuery(con, query)
  deserialize_object(moda_object$moda_object)

}

get_info_table_moda <- function(module_name, con){
  query <- sprintf("SELECT * FROM moda_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_moda_module <- function(module_name, con){


  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("moda_settings", "module_name", module_name, con)
}
