prepare_input_settings_for_db.DiffCoEx <- function(diffcoex_module, module_name){
  settings <- diffcoex_module$settings
  cbind(module_name, as.data.frame(settings[2:12]))
}


write_to_db.DiffCoEx <- function(diffcoex_module,
                              main_register,
                              register_row,
                              settings,
                              con){

  module_genes_row <- prepare_module_genes_row(diffcoex_module, main_register)


  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(diffcoex_module,
                                 "diffcoex_object") %>%
    cbind(module_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "diffcoex_objects", object_blob,
               append = TRUE)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "diffcoex_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)

}

fetch_diffcoex_module <- function(module_name, con){

  query <- sprintf("SELECT * FROM diffcoex_objects WHERE module_name IS '%s' ", module_name)
  diffcoex_object <- dbGetQuery(con, query)
  deserialize_object(diffcoex_object$diffcoex_object)
}

get_info_table_diffcoex <- function(module_name, con){
  query <- sprintf("SELECT * FROM diffcoex_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_diffcoex_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("diffcoex_settings", "module_name", module_name, con)
}
