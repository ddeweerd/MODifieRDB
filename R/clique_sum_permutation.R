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

  clique_sum_permutation_register_row <- as.data.frame(main_register$module_name)
  colnames(clique_sum_permutation_register_row) <- "module_name"

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "clique_sum_permutation_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "clique_sum_permutation_module_register", clique_sum_permutation_register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_clique_sum_permutation_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN clique_sum_permutation_settings USING(module_name) WHERE module_name IS '%s' ", module_name)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:4)]
  db <- match_ppi_to_db_loc(info_table$ppi_name, con)
  inputs <- list("MODifieR_input" = as.name(info_table$input_name),
                 "db" = db)

  settings <- c(inputs, settings)

  class(settings[6]) <- "logical"


  module_genes <- get_module_genes(module_name, con)

  MODifieR:::construct_clique_permutation_module(module_genes = module_genes,
                                                 input_class = input_class,
                                                 settings = settings)
}

delete_clique_sum_permutation_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("clique_sum_permutation_module_register", "module_name", module_name, con)
  delete_row("clique_sum_permutation_settings", "module_name", module_name, con)
}
