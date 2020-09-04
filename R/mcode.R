
prepare_input_settings_for_db.Mcode <- function(mcode_module, module_name){

  settings <- mcode_module$settings
  cbind(module_name, as.data.frame(settings[3:10]))
}

write_to_db.Mcode <- function(mcode_module, main_register, register_row, settings, con){
  module_genes_row <- prepare_module_genes_row(mcode_module, main_register)

  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(mcode_module,
                                 "mcode_object") %>%
    cbind(module_name, ., stringsAsFactors = F)

  dbWriteTable(conn = con, "mcode_objects", object_blob,
               append = TRUE)
  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "mcode_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

mcode_get_module_list <- function(mcode_module, module_name){

  module_list <- NULL
  for (i in 1:length(mcode_module$module_scores)){

    module_list_row <- c(module_name,
                         mcode_module$module_scores[i],
                         collapse_genes(mcode_module$modules[[i]]))
    module_list <- rbind(module_list, module_list_row)
  }

  colnames(module_list) <- c("module_name", "module_score", "module_genes")

  return(as.data.frame(module_list, stringsAsFactors = F))
}

fetch_mcode_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM mcode_objects WHERE module_name IS '%s' ", module_name)
  mcode_object <- dbGetQuery(con, query)
  deserialize_object(mcode_object$mcode_object)


}

delete_mcode_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("mcode_settings", "module_name", module_name, con)
  delete_row("mcode_objects", "module_name", module_name, con)
}
