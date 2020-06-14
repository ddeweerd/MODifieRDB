
prepare_input_settings_for_db.RNA_seq <- function(MODifieR_input, input_name){

  settings <- MODifieR_input$settings

  settings$count_matrix <- as.character(settings$count_matrix)

  settings$group1_indici <- paste(settings$group1_indici, collapse = " ")
  settings$group2_indici <- paste(settings$group2_indici, collapse = " ")

  cbind(input_name, as.data.frame(settings))
}

write_to_db.RNA_seq <- function(MODifieR_object, main_register, register_row, settings, con){
  write_hash_tables_to_db(register_row, MODifieR_object, con)

  #tags_df <- link_input_tag(tags, input_name = main_register$input_name, con = con)

  add_unique_genes(genes = unique(MODifieR_object$diff_genes$gene,
                                  rownames(MODifieR_object$annotated_exprs_matrix)),
                   con = con)

  #dbWriteTable(conn = con, "tag_input", tags_df, append = TRUE)

  dbWriteTable(conn = con, "rnaseq_input_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "input_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "rnaseq_input_settings", settings, append = TRUE)
}

get_info_table_rna <- function(input_name, con){
  query <- sprintf("SELECT * FROM rnaseq_input_register INNER JOIN rnaseq_input_settings USING(input_name) WHERE input_name IS '%s' ", input_name)
  dbGetQuery(con, query)
}

fetch_rna_object <- function(input_name, con){
  info_table <- get_info_table_rna(input_name, con)

  diff_genes <- NULL
  limma_probe_table <- NULL
  annotated_exprs_matrix <- NULL
  count_matrix <- NULL


  if (!is.na(as.character(info_table["diff_genes"]))){
    diff_genes <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["diff_genes"])), row.names = T)
  }
  if (!is.na(as.character(info_table["edgeR_deg_table"]))){
    edgeR_deg_table <- dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["edgeR_deg_table"])), row.names = T)
  }
  if (!is.na(as.character(info_table["annotated_exprs_matrix"]))){
    annotated_exprs_matrix <- as.matrix(dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["annotated_exprs_matrix"])), row.names = T))
  }
  if (!is.na(as.character(info_table["count_matrix"]))){
    count_matrix <- as.matrix(dbGetQuery(con, sprintf("SELECT * FROM %s", as.character(info_table["count_matrix"])), row.names = T))
  }

  group1_indici <- as.integer(unlist(strsplit(info_table$group1_indici, " ")))
  group2_indici <- as.integer(unlist(strsplit(info_table$group2_indici, " ")))

  group1_label <- info_table$group1_label
  group2_label <- info_table$group2_label

  settings <- reconstruct_rna_setting(info_table)

  settings$group1_indici <- group1_indici
  settings$group2_indici <- group2_indici

  input_object <- MODifieR::create_custom_rna_input_object(diff_genes = diff_genes,
                                                           edgeR_deg_table = edgeR_deg_table,
                                                           annotated_exprs_matrix = annotated_exprs_matrix,
                                                           count_matrix = count_matrix,
                                                           group1_indici = group1_indici,
                                                           group2_indici = group2_indici,
                                                           group1_label = group1_label,
                                                           group2_label = group2_label,
                                                           settings = settings)

  return(input_object)
}

reconstruct_rna_setting <- function(info_table){
  settings <- as.list(info_table[, 6:12]) %>%
    set_names(sub(pattern = '\\..[0-9]', replacement = "", x = names(.)))

  settings$use_adjusted <- as.logical(settings$use_adjusted)
  settings$normalize_quantiles <- as.logical(settings$normalize_quantiles)
  settings$count_matrix <- as.name(settings$count_matrix)

  return(settings)
}

delete_rna_object <- function(input_name, con){
  info_table <- get_info_table_rna(input_name, con)
  delete_tables(info_table, con)

  delete_row("rnaseq_input_register", "input_name", input_name, con)
  delete_row("input_register", "input_name", input_name, con)
}


