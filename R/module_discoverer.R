
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

  module_discoverer_register_row <- as.data.frame(main_register$module_name)
  colnames(module_discoverer_register_row) <- "module_name"

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "module_discoverer_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_discoverer_module_register", module_discoverer_register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_module_discoverer_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN module_discoverer_settings USING(module_name) WHERE module_name IS '%s' ", module_name)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:4)]

  inputs <- list("MODifieR_input" = as.name(info_table$input_name),
                 "ppi_network" = as.name(info_table$ppi_name))

  settings <- c(inputs, settings)

  module_genes <- get_module_genes(module_name, con)

  MODifieR:::construct_module_discoverer_module(module_genes = module_genes,
                                                graph = NULL,
                                                input_class = input_class,
                                                settings = settings)

}

delete_module_discoverer_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("module_discoverer_module_register", "module_name", module_name, con)
  delete_row("module_discoverer_settings", "module_name", module_name, con)
}
