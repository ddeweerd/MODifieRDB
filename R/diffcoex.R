
prepare_input_settings_for_db.DiffCoEx <- function(diffcoex_module, module_name){
  settings <- diffcoex_module$settings
  cbind(module_name, as.data.frame(settings[2:12]))
}


write_to_db.DiffCoEx <- function(diffcoex_module,
                              main_register,
                              register_row,
                              settings,
                              con){

  module_genes_row <- prepare_module_genes_row(diffcoex_module, main_register)

  diffcoex_module$module_p_values <- data.frame(names = row.names(diffcoex_module$module_p_values),
                                                diffcoex_module$module_p_values,
                                                stringsAsFactors = F)


  module_colors <- cbind(main_register$module_name, collapse_genes(diffcoex_module$module_colors)) %>%
    as.data.frame(., stringsAsFactors = F) %>%
    set_colnames(c("module_name", "module_colors"))

  diffcoex_module$color_vector <- as.data.frame(cbind(names(diffcoex_module$color_vector),
                                                       diffcoex_module$color_vector),
                                                 stringsAsFactors = F)

  #return (diffcoex_module)
  write_hash_tables_to_db(register_row, diffcoex_module, con)

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "diffcoex_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "diffcoex_module_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
  dbWriteTable(conn = con, "diffcoex_module_colors", module_colors, append = TRUE)
}

fetch_diffcoex_module <- function(module_name, con){
  query <- sprintf("SELECT * FROM module_register INNER JOIN diffcoex_module_register USING(module_name)
                   INNER JOIN diffcoex_settings USING(module_name) WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:6)]

  inputs <- list("MODifieR_input" = as.name(info_table$input_name))

  settings <- c(inputs, settings)

  module_genes <- get_module_genes(module_name, con)

  module_p_values_f <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["module_p_values"])))

  module_p_values <- data.frame("pvalue" = module_p_values_f[,3], stringsAsFactors = F)
  rownames(module_p_values) <- module_p_values_f[,1]

  color_table <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["color_vector"])), row.names = T)

  color_vector <- color_table[,2]

  names(color_vector) <- color_table[,1]

  query <- sprintf("SELECT module_colors FROM diffcoex_module_colors WHERE module_name IS '%s' ", module_name)
  module_list_df <- dbGetQuery(con, query)

  module_colors <- as.character(unname(sapply(module_list_df$module_colors, uncollapse_genes)))

  class(settings[6:7]) <- "logical"


  MODifieR:::construct_diffcoex_module(module_genes = module_genes,
                                       color_vector = color_vector,
                                       module_p_values = module_p_values,
                                       module_colors = module_colors,
                                       input_class = input_class,
                                       settings = settings)

}

get_info_table_diffcoex <- function(module_name, con){
  query <- sprintf("SELECT * FROM diffcoex_module_register WHERE module_name IS '%s' ", module_name)
  dbGetQuery(con, query)
}

delete_diffcoex_module <- function(module_name, con){

  info_table <- get_info_table_diffcoex(module_name, con)

  delete_tables(info_table, con)

  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("diffcoex_module_register", "module_name", module_name, con)
  delete_row("diffcoex_settings", "module_name", module_name, con)
  delete_row("diffcoex_module_colors", "module_name", module_name, con)
}
