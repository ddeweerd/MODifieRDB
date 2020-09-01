create_table <- function(table_description, con){

  query <- paste(table_description, collapse = " ")
  dbExecute(con, query)

}

read_sql <- function(db){

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)

  sql_filepath <- system.file("sql", "database.sql", package = "MODifieRDB")

  sqlfile <- readLines(sql_filepath)

  table_descriptions <- vector()

  start_pos <- grep(pattern = "CREATE TABLE", x = sqlfile)
  end_pos <- grep(pattern = ");", x = sqlfile)

  for (i in 1:length(start_pos)){
    create_table(paste0(sqlfile[start_pos[i]:end_pos[i]], collapse = " "), con)
  }
  return(con)

}

# create_enrichment_tables <- function(con){
#   sql_filepath <- system.file("sql", "temporary_tables.sql", package = "MODifieRDB")
#
#   sqlfile <- readLines(sql_filepath)
#
#   table_descriptions <- vector()
#
#   start_pos <- grep(pattern = "CREATE TEMP TABLE", x = sqlfile)
#   end_pos <- grep(pattern = ");", x = sqlfile)
#
#   for (i in 1:length(start_pos)){
#     create_table(paste0(sqlfile[start_pos[i]:end_pos[i]], collapse = " "), con)
#   }
# }
