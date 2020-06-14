
get_available_tags <- function(con){
  dbGetQuery(con, "SELECT * FROM tags")
}

add_tags <- function(tags, con){
  tags <- data.frame("tag" = tags)

  dbWriteTable(conn = con, "tags", tags, append = TRUE)
}

get_tag <- function(tag, con){
  query <- sprintf("SELECT * FROM tags WHERE tag IS '%s' ", tag)
  dbGetQuery(con, query)
}

get_tags_for_input <- function(input_name, con){
  query <- sprintf("SELECT tag FROM tag_input WHERE input_name IS '%s' ", input_name)
  dbGetQuery(con, query)
}

link_input_tag <- function (tags, input_name, con){

  if (!is.null(tags)){
    for (tag in tags){
      if (nrow(MODifieRDB:::get_tag(tag, con)) == 0){
        stop("Tag ", tag, " not in database", call. = F)
      }
    }
  }

  as.data.frame(Reduce(f = "rbind", lapply(X = tags, function(x) cbind(input_name, x)))) %>%
    set_colnames(c("input_name", "tag"))
}


delete_tag <- function(tag, con){
  delete_row("tags", "tag", tag, con)
  delete_row("tag_input", "tag", tag, con)
}
