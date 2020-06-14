
prepare_input_settings_for_db.MicroArray <- function(MODifieR_input, input_name){

  settings <- MODifieR_input$settings

  settings$expression_matrix <- as.character(settings$expression_matrix)
  settings$annotation_table <- as.character(settings$annotation_table)

  settings$group1_indici <- paste(settings$group1_indici, collapse = " ")
  settings$group2_indici <- paste(settings$group2_indici, collapse = " ")

  cbind(input_name, as.data.frame(settings))
}


write_to_db.MicroArray <- function(MODifieR_object, main_register, register_row, settings, con){
  write_hash_tables_to_db(register_row, MODifieR_object, con)

  #tags_df <- link_input_tag(tags, input_name = main_register$input_name, con = con)

  add_unique_genes(genes = unique(MODifieR_object$diff_genes$gene,
                                  rownames(MODifieR_object$annotated_exprs_matrix)),
                   con = con)

  dbWriteTable(conn = con, "microarray_input_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "input_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "microarray_input_settings", settings, append = TRUE)
  #dbWriteTable(conn = con, "tag_input", tags_df, append = TRUE)
}

get_info_table_microarray <- function(input_name, con){
  query <- sprintf("SELECT * FROM microarray_input_register INNER JOIN microarray_input_settings USING(input_name) WHERE input_name IS '%s' ", input_name)
  dbGetQuery(con, query)
}

fetch_microarray_object <- function(input_name, con){
  info_table <- get_info_table_microarray(input_name, con)

  diff_genes <- NULL
  limma_probe_table <- NULL
  annotated_exprs_matrix <- NULL
  expression_matrix <- NULL
  annotation_table <- NULL

  if (!is.na(as.character(info_table["diff_genes"]))){
    diff_genes <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["diff_genes"])), row.names = T)
  }
  if (!is.na(as.character(info_table["limma_probe_table"]))){
    limma_probe_table <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["limma_probe_table"])), row.names = T)
  }
  if (!is.na(as.character(info_table["annotated_exprs_matrix"]))){
    annotated_exprs_matrix <- as.matrix(dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["annotated_exprs_matrix"])), row.names = T))
  }
  if (!is.na(as.character(info_table["expression_matrix"]))){
    expression_matrix <- as.matrix(dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["expression_matrix"])), row.names = T))
  }
  if (!is.na(as.character(info_table["annotation_table"]))){
    annotation_table <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["annotation_table"])), row.names = T)
  }
  group1_indici <- as.integer(unlist(strsplit(info_table$group1_indici, " ")))
  group2_indici <- as.integer(unlist(strsplit(info_table$group2_indici, " ")))

  group1_label <- info_table$group1_label
  group2_label <- info_table$group2_label

  settings <- reconstruct_microarray_setting(info_table)

  settings$group1_indici <- group1_indici
  settings$group2_indici <- group2_indici

  input_object <- MODifieR::create_custom_microarray_input_object(diff_genes = diff_genes,
                                                                  limma_probe_table = limma_probe_table,
                                                                  annotated_exprs_matrix = annotated_exprs_matrix,
                                                                  expression_matrix = expression_matrix,
                                                                  annotation_table = annotation_table,
                                                                  group1_indici = group1_indici,
                                                                  group2_indici = group2_indici,
                                                                  group1_label = group1_label,
                                                                  group2_label = group2_label,
                                                                  settings = settings)

  return(input_object)
}

reconstruct_microarray_setting <- function(info_table){
  settings <- as.list(info_table[, 7:14]) %>%
    set_names(sub(pattern = '\\..[0-9]', replacement = "", x = names(.)))

  settings$use_adjusted <- as.logical(settings$use_adjusted)
  settings$expression_matrix <- as.name(settings$expression_matrix)
  settings$annotation_table <- as.name(settings$annotation_table)

  return(settings)
}

delete_microarray_object <- function(input_name, con){
  info_table <- get_info_table_microarray(input_name, con)
  delete_tables(info_table, con)

  delete_row("microarray_input_register", "input_name", input_name, con)
  delete_row("input_register", "input_name", input_name, con)
}
