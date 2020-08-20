
prepare_input_settings_for_db.module_discoverer <- function(module_discoverer_module,
                                                             module_name){
  settings <- module_discoverer_module$settings
  cbind(module_name, as.data.frame(settings[3:8]))
}

write_to_db.module_discoverer <- function(module_discoverer_module,
                                          main_register,
                                          register_row,
                                          settings,
                                          con){

  module_genes_row <- prepare_module_genes_row(module_discoverer_module, main_register)

  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(module_discoverer_module,
                                 "module_discoverer_object") %>%
    cbind(module_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "module_discoverer_objects", object_blob,
               append = TRUE)


  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "module_discoverer_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_module_discoverer_module <- function(module_name, con){

  query <- sprintf("SELECT * FROM module_discoverer_objects WHERE module_name IS '%s' ", module_name)
  module_discoverer_object <- dbGetQuery(con, query)
  deserialize_object(module_discoverer_object$module_discoverer_object)

}

delete_module_discoverer_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("module_discoverer_settings", "module_name", module_name, con)
}
