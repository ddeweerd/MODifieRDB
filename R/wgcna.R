
prepare_input_settings_for_db.WGCNA <- function(wgcna_module, module_name){
  settings <- wgcna_module$settings
  cbind(module_name, as.data.frame(settings[2:14]))
}


write_to_db.WGCNA <- function(wgcna_module,
                             main_register,
                             register_row,
                             settings,
                             con){

  module_genes_row <- prepare_module_genes_row(wgcna_module, main_register)

  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(wgcna_module,
                                 "wgcna_object") %>%
    cbind(module_name, ., stringsAsFactors = F)

  dbWriteTable(conn = con, "wgcna_objects", object_blob,
               append = TRUE)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "wgcna_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "wgcna_module_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_wgcna_module <- function(module_name, con){

  query <- sprintf("SELECT * FROM wgcna_objects WHERE module_name IS '%s' ", module_name)
  wgcna_object <- dbGetQuery(con, query)
  deserialize_object(wgcna_object$wgcna_object)


}

get_info_table_wgcna <- function(module_name, con){
  query <- sprintf("SELECT * FROM wgcna_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_wgcna_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("wgcna_settings", "module_name", module_name, con)
  delete_row("wgcna_objects", "module_name", module_name, con)
}
