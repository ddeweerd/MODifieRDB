
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


  write_hash_tables_to_db(register_row, correlation_clique_module, con)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "correlation_clique_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "correlation_clique_module_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_correlation_clique_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN correlation_clique_module_register USING(module_name)
                 INNER JOIN correlation_clique_settings USING(module_name) WHERE module_name IS '%s' ", module_name)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:5)]

  inputs <- list("MODifieR_input" = as.name(info_table$input_name),
                 "ppi_network" = as.name(info_table$ppi_name))

  settings <- c(inputs, settings)

  class(settings[8]) <- "logical"

  module_genes <- get_module_genes(module_name, con)

  freqs <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["frequency_table"])), row.names = T)

  frequency_table <- xtabs(Freq ~ Var1, data = freqs)

  MODifieR:::construct_correlation_module(module_genes = module_genes,
                                          frequency_table = frequency_table,
                                          input_class = input_class,
                                          settings = settings)

}

get_info_table_correlation_clique <- function(module_name, con){
  query <- sprintf("SELECT * FROM correlation_clique_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_correlation_clique_module <- function(module_name, con){

  info_table <- get_info_table_correlation_clique(module_name, con)

  delete_tables(info_table, con)

  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("correlation_clique_module_register", "module_name", module_name, con)
  delete_row("correlation_clique_settings", "module_name", module_name, con)
}
