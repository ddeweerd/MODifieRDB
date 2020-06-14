
prepare_input_settings_for_db.MODA <- function(moda_module, module_name){
  settings <- moda_module$settings
  cbind(module_name, as.data.frame(settings[2:6]))
}


write_to_db.MODA <- function(moda_module,
                             main_register,
                             register_row,
                             settings,
                             con){

  module_genes_row <- prepare_module_genes_row(moda_module, main_register)
  module_list_group1 <- moda_get_module_list(group_modules = moda_module$group1_modules,
                                             module_name = main_register$module_name)
  module_list_group2 <- moda_get_module_list(group_modules = moda_module$group2_modules,
                                             module_name = main_register$module_name)

  write_hash_tables_to_db(register_row, moda_module, con)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "moda_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "moda_module_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
  dbWriteTable(conn = con, "moda_module_list_group1", module_list_group1, append = TRUE)
  dbWriteTable(conn = con, "moda_module_list_group2", module_list_group2, append = TRUE)
}

moda_get_module_list <- function(group_modules, module_name){

  module_list <- NULL
  for (i in 1:length(group_modules)){

    module_list_row <- c(module_name,
                         collapse_genes(group_modules[[i]]))
    module_list <- rbind(module_list, module_list_row)
  }

  colnames(module_list) <- c("module_name", "module_genes")

  return(as.data.frame(module_list, stringsAsFactors = F))
}

fetch_moda_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN moda_module_register USING(module_name)
                   INNER JOIN moda_settings USING(module_name) WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:5)]

  inputs <- list("MODifieR_input" = as.name(info_table$input_name))

  settings <- c(inputs, settings)

  module_genes <- get_module_genes(module_name, con)

  jaccard_table <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["jaccard_table"])), row.names = T)


  query <- sprintf("SELECT module_genes FROM moda_module_list_group1 WHERE module_name IS '%s' ", module_name)
  module_list_df <- dbGetQuery(con, query)

  group1_modules <- unname(sapply(module_list_df$module_genes, uncollapse_genes))


  query <- sprintf("SELECT module_genes FROM moda_module_list_group2 WHERE module_name IS '%s' ", module_name)
  module_list_df <- dbGetQuery(con, query)

  group2_modules <- unname(sapply(module_list_df$module_genes, uncollapse_genes))

  MODifieR:::construct_moda_module(module_genes = module_genes,
                                   modules1 = group1_modules,
                                   modules2 = group2_modules,
                                   jaccard_table = jaccard_table,
                                   input_class = input_class,
                                   settings = settings)

}

get_info_table_moda <- function(module_name, con){
  query <- sprintf("SELECT * FROM moda_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_moda_module <- function(module_name, con){

  info_table <- get_info_table_moda(module_name, con)

  delete_tables(info_table, con)

  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("moda_module_register", "module_name", module_name, con)
  delete_row("moda_settings", "module_name", module_name, con)
  delete_row("moda_module_list_group1", "module_name", module_name, con)
  delete_row("moda_module_list_group2", "module_name", module_name, con)
}
