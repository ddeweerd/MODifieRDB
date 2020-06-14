
prepare_input_settings_for_db.Mcode <- function(mcode_module, module_name){

  settings <- mcode_module$settings
  cbind(module_name, as.data.frame(settings[3:10]))
}

write_to_db.Mcode <- function(mcode_module, main_register, register_row, settings, con){
  module_genes_row <- prepare_module_genes_row(mcode_module, main_register)

  module_list_rows <- mcode_get_module_list(mcode_module = mcode_module,
                                            module_name = main_register$module_name)
  mcode_register_row <- as.data.frame(main_register$module_name)

  colnames(mcode_register_row) <- "module_name"

  dbWriteTable(conn = con, "module_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "mcode_module_register", mcode_register_row, append = TRUE)
  dbWriteTable(conn = con, "mcode_settings", settings, append = TRUE)
  dbWriteTable(conn = con, "module_genes", module_genes_row, append = TRUE)
  dbWriteTable(conn = con, "mcode_module_list", module_list_rows, append = TRUE)
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
  query <- sprintf("SELECT * FROM module_register INNER JOIN mcode_settings USING(module_name) WHERE module_name IS '%s' ", module_name)

  info_table <- as.list(dbGetQuery(con, query))
  input_class <-  get_input_type(input_name = info_table$input_name, con = con)

  settings <- info_table[-(1:4)]
  inputs <- list("MODifieR_input" = as.name(info_table$input_name),
                 "ppi_network" = as.name(info_table$ppi_name))

  settings <- c(inputs, settings)
  class(settings[c(5:6 , 8)]) <- "logical"

  query <- sprintf("SELECT * FROM mcode_module_list WHERE module_name IS '%s' ", module_name)
  module_list_df <- dbGetQuery(con, query)

  module_scores <- as.numeric(module_list_df$module_score)
  modules <- unname(sapply(module_list_df$module_genes, uncollapse_genes))

  module_genes <- get_module_genes(module_name, con)

  MODifieR:::construct_mcode_module(module_genes = module_genes,
                                                    modules = modules,
                                                    module_scores = module_scores,
                                                    input_class = input_class,
                                                    settings = settings)


}

delete_mcode_module <- function(module_name, con){
  delete_row("module_genes", "module_name", module_name, con)
  delete_row("module_register", "module_name", module_name, con)
  delete_row("mcode_module_register", "module_name", module_name, con)
  delete_row("mcode_settings", "module_name", module_name, con)
  delete_row("mcode_module_list", "module_name", module_name, con)
}
