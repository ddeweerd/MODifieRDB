prepare_input_settings_for_db.Clique_Sum_permutation <- function(csp_module,
                                                                 module_name){
  settings <- csp_module$settings
  cbind(module_name, as.data.frame(settings[3:8]))
}

write_to_db.Clique_Sum_permutation <- function(csp_module,
                                               main_register,
                                               register_row,
                                               settings,
                                               con){

  module_genes_row <- prepare_module_genes_row(csp_module, main_register)

  module_name <- module_genes_row$module_name
  object_blob <- serialize_to_df(csp_module,
                                 "clique_sum_permutation_object") %>%
    cbind(module_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "clique_sum_permutation_objects", object_blob,
               append = TRUE)
  clique_sum_permutation_register_row <- as.data.frame(main_register$module_name)
  colnames(clique_sum_permutation_register_row) <- "module_name"



  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "clique_sum_permutation_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_clique_sum_permutation_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM clique_sum_permutation_objects WHERE module_name IS '%s' ", module_name)
  clique_sum_permutation_object <- dbGetQuery(con, query)
  deserialize_object(clique_sum_permutation_object$clique_sum_permutation_object)
}

delete_clique_sum_permutation_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("clique_sum_permutation_settings", "module_name", module_name, con)
  delete_row("clique_sum_permutation_objects", "module_name", module_name, con)
}
