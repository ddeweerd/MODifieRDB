
prepare_input_settings_for_db.RNA_seq <- function(MODifieR_input, input_name){

  settings <- MODifieR_input$settings

  settings$count_matrix <- as.character(settings$count_matrix)

  settings$group1_indici <- paste(settings$group1_indici, collapse = " ")
  settings$group2_indici <- paste(settings$group2_indici, collapse = " ")

  cbind(input_name, as.data.frame(settings))
}

write_to_db.RNA_seq <- function(MODifieR_object, main_register, register_row, settings, con){

  input_name <- register_row$input_name
  object_blob <- serialize_to_df(MODifieR_object,
                                 "rnaseq_object") %>%
    cbind(input_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "rnaseq_input_objects", object_blob,
               append = TRUE)

  add_unique_genes(genes = unique(MODifieR_object$diff_genes$gene,
                                  rownames(MODifieR_object$annotated_exprs_matrix)),
                   con = con)

  dbWriteTable(conn = con, "rnaseq_input_register", register_row, append = TRUE)
  dbWriteTable(conn = con, "input_register", main_register, append = TRUE)
  dbWriteTable(conn = con, "rnaseq_input_settings", settings, append = TRUE)
}

get_info_table_rna <- function(input_name, con){
  query <- sprintf("SELECT * FROM rnaseq_input_register INNER JOIN rnaseq_input_settings USING(input_name) WHERE input_name IS '%s' ", input_name)
  dbGetQuery(con, query)
}

fetch_rna_object <- function(input_name, con){
  query <- sprintf("SELECT * FROM rnaseq_input_objects WHERE input_name IS '%s' ", input_name)
  rnaseq_input_object <- dbGetQuery(con, query)
  deserialize_object(rnaseq_input_object$rnaseq_object)
}
#' @export
delete_rna_object <- function(input_name, con){
  delete_row("rnaseq_input_settings", "input_name", input_name, con)
  delete_row("input_register", "input_name", input_name, con)
  delete_row("rnaseq_input_objects", "input_name", input_name, con)
}


