
prepare_input_settings_for_db.MicroArray <- function(MODifieR_input, input_name){

  settings <- MODifieR_input$settings

  settings$expression_matrix <- as.character(settings$expression_matrix)
  settings$annotation_table <- as.character(settings$annotation_table)

  settings$group1_indici <- paste(settings$group1_indici, collapse = " ")
  settings$group2_indici <- paste(settings$group2_indici, collapse = " ")

  cbind(input_name, as.data.frame(settings))
}


write_to_db.MicroArray <- function(MODifieR_object, main_register, register_row, settings, con){
  input_name <- register_row$input_name
  object_blob <- serialize_to_df(MODifieR_object,
                                 "microarray_object") %>%
    cbind(input_name, ., stringsAsFactors = F)
  dbWriteTable(conn = con, "microarray_input_objects", object_blob,
               append = TRUE)

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
  query <- sprintf("SELECT * FROM microarray_input_objects WHERE input_name IS '%s' ", input_name)
  microarray_input_object <- dbGetQuery(con, query)
  deserialize_object(microarray_input_object$microarray_object)
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
  delete_row("microarray_input_settings", "input_name", input_name, con)
  delete_row("input_register", "input_name", input_name, con)
}
