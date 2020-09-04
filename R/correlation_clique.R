
prepare_input_settings_for_db.Correlation_clique <- function(correlation_clique_module,
                                                                 module_name){
  settings <- correlation_clique_module$settings
  cbind(module_name, as.data.frame(settings[3:10]))
}

write_to_db.Correlation_clique <- function(correlation_clique_module,
                                           main_register,
                                           register_row,
                                           settings,
                                           con){

  module_genes_row <- prepare_module_genes_row(correlation_clique_module, main_register)
  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(correlation_clique_module,
                  "correlation_clique_object") %>%
  cbind(module_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "correlation_clique_objects", object_blob,
               append = TRUE)


  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "correlation_clique_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_correlation_clique_module <- function(module_name, con){

  query <- sprintf("SELECT * FROM correlation_clique_objects WHERE module_name IS '%s' ", module_name)
  correlation_clique_object <- dbGetQuery(con, query)
  deserialize_object(correlation_clique_object$correlation_clique_object)
}

get_info_table_correlation_clique <- function(module_name, con){
  query <- sprintf("SELECT * FROM correlation_clique_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_correlation_clique_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("correlation_clique_settings", "module_name", module_name, con)
  delete_row("correlation_clique_objects", "module_name", module_name, con)
}
