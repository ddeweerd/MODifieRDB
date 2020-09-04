
prepare_input_settings_for_db.DIAMOnD <- function(diamond_module, module_name){
  settings <- diamond_module$settings
  cbind(module_name, as.data.frame(settings[3:7]))
}

write_to_db.DIAMOnD <- function(diamond_module,
                                main_register,
                                register_row,
                                settings,
                                con){

  module_genes_row <- prepare_module_genes_row(diamond_module, main_register)
  seed_genes_row <- prepare_seed_genes(main_register, diamond_module)
  ignored_genes_row <- prepare_ignored_genes(main_register, diamond_module)

  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(diamond_module,
                                 "diamond_object") %>%
    cbind(module_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "diamond_objects", object_blob,
               append = TRUE)



  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "diamond_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_diamond_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM diamond_objects WHERE module_name IS '%s' ", module_name)
  diamond_object <- dbGetQuery(con, query)
  deserialize_object(diamond_object$diamond_object)

}

get_diamond_seed_genes <- function(module_name, con){
  query <- sprintf("SELECT * FROM diamond_seed_genes WHERE module_name IS '%s' ", module_name)
  seed_genes <- dbGetQuery(con, query)
  uncollapse_genes(seed_genes$seed_genes)
}

get_diamond_ignored_genes <- function(module_name, con){
  query <- sprintf("SELECT * FROM diamond_ignored_genes WHERE module_name IS '%s' ", module_name)
  ignored_genes <- dbGetQuery(con, query)
  uncollapse_genes(ignored_genes$ignored_genes)
}

prepare_seed_genes <- function(main_register, diamond_module){
  cbind("module_name" = main_register$module_name,
        "seed_genes" = collapse_genes(diamond_module$seed_genes)) %>%
    as.data.frame(., stringsAsFactors = F)
}

prepare_ignored_genes <- function(main_register, diamond_module){
  cbind("module_name" = main_register$module_name,
        "ignored_genes" = collapse_genes(diamond_module$ignored_genes)) %>%
    as.data.frame(., stringsAsFactors = F)
}

get_info_table_diamond <- function(module_name, con){
  query <- sprintf("SELECT * FROM diamond_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_diamond_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("diamond_settings", "module_name", module_name, con)
  delete_row("diamond_objects", "module_name", module_name, con)

}
