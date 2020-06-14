
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

  softthreshold_value <- cbind(main_register$module_name, wgcna_module$softthreshold_value) %>%
    as.data.frame(.) %>%
    set_colnames(c("module_name", "softthreshold_value"))

  module_colors <- cbind(main_register$module_name, collapse_genes(wgcna_module$module_colors)) %>%
    as.data.frame(.) %>%
    set_colnames(c("module_name", "module_colors"))

  write_hash_tables_to_db(register_row, wgcna_module, con)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "wgcna_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "wgcna_module_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
  dbWriteTable(conn = con, "wgcna_soft_threshold", softthreshold_value, append = TRUE)
  dbWriteTable(conn = con, "wgcna_module_colors", module_colors, append = TRUE)
}

fetch_wgcna_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN wgcna_module_register USING(module_name)
                   INNER JOIN wgcna_settings USING(module_name) WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:6)]

  inputs <- list("MODifieR_input" = as.name(info_table$input_name))

  settings <- c(inputs, settings)

  module_genes <- get_module_genes(module_name, con)

  info_table_wgcna <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["info_table"])), row.names = T)
  correlation_to_trait_table <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["correlation_to_trait_table"])), row.names = T)


  query <- sprintf("SELECT module_colors FROM wgcna_module_colors WHERE module_name IS '%s' ", module_name)
  module_list_df <- dbGetQuery(con, query)

  module_colors <- unname(sapply(module_list_df$module_genes, uncollapse_genes))


  query <- sprintf("SELECT softthreshold_value FROM wgcna_soft_threshold WHERE module_name IS '%s' ", module_name)
  module_list_df <- dbGetQuery(con, query)

  softthreshold_value <- unname(unlist(module_list_df))

  class(settings[c(5, 7, 12 )]) <- "logical"

  MODifieR:::construct_wgcna_module(module_genes = module_genes,
                                    info_table = info_table_wgcna,
                                    module_cor_and_p_value = correlation_to_trait_table,
                                    powerEstimate = softthreshold_value,
                                    module_colors = module_colors,
                                    input_class = input_class,
                                    settings = settings)

}

get_info_table_wgcna <- function(module_name, con){
  query <- sprintf("SELECT * FROM wgcna_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_wgcna_module <- function(module_name, con){

  info_table <- get_info_table_wgcna(module_name, con)

  delete_tables(info_table, con)

  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("wgcna_module_register", "module_name", module_name, con)
  delete_row("wgcna_settings", "module_name", module_name, con)
  delete_row("wgcna_soft_threshold", "module_name", module_name, con)
  delete_row("wgcna_module_colors", "module_name", module_name, con)
}
