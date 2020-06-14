
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

  write_hash_tables_to_db(register_row, diamond_module, con)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "diamond_seed_genes", seed_genes_row, append = TRUE)
  dbWriteTable(conn = con, "diamond_ignored_genes", ignored_genes_row, append = TRUE)
  dbWriteTable(conn = con, "diamond_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "diamond_module_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
}

fetch_diamond_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN diamond_module_register USING(module_name)
                 INNER JOIN diamond_settings USING(module_name) WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:5)]

  inputs <- list("MODifieR_input" = as.name(info_table$input_name),
                 "ppi_network" = as.name(info_table$ppi_name))

  settings <- c(inputs, settings)

  class(settings[6]) <- "logical"

  module_genes <- get_module_genes(module_name, con)
  seed_genes <- get_diamond_seed_genes(module_name, con)
  ignored_genes <- get_diamond_ignored_genes(module_name, con)
  added_genes <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["added_genes"])), row.names = T)

  MODifieR:::construct_diamond_module(module_genes = module_genes,
                                      seed_genes = seed_genes,
                                      ignored_genes = ignored_genes,
                                      added_genes = added_genes,
                                      input_class = input_class,
                                      settings = settings)

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

  info_table <- get_info_table_diamond(module_name, con)

  delete_tables(info_table, con)

  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("diamond_module_register", "module_name", module_name, con)
  delete_row("diamond_settings", "module_name", module_name, con)
  delete_row("diamond_seed_genes", "module_name", module_name, con)
  delete_row("diamond_ignored_genes", "module_name", module_name, con)
}
