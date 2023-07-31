combine_xy <- function(r_antencent, r_consequence){

  r_ante <- as.character(r_antencent)
  r_conse <- as.character(r_consequence)

  split_string_ante <- strsplit(r_ante, ",")[[1]]
  clean_items_ante <- trimws(gsub("[^[:alnum:][:blank:]+?&/\\-]", "", split_string_ante), which = "both")
  clean_items_consequence <- trimws(gsub("[^[:alnum:][:blank:]+?&/\\-]", "", r_conse), which = "both")

  xy <- sort( c(clean_items_ante, clean_items_consequence) )

  output <- paste(xy, collapse = ",")
  return(output)
}
