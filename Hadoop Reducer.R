#Reducer

input <- file("stdin", open = "r")
lines <- readLines(input, warn = FALSE)

current_document <- NULL
positions <- character(0)

#Function to output the results for one document
emit_result <- function(document, locations){
  if (length(locations) > 0) {
    cat(
      document,
      "\t",
      paste(locations, collapse = ", "),
      "\n",
      sep = "")
  }
}

#Process sorted Mapper output
for (line in lines) {
  #Find the separator between key and value
  tab_position <- regexpr("\t", line, fixed = TRUE)[1]
  if (tab_position < 1) next
  
  document <- substring(line, 1, tab_position - 1)
  location <- substring(line, tab_position + 1)
  
  #If document changes, output the previous document
  if (!is.null(current_document) &&
      document != current_document) {
    emit_result(current_document, positions)
    positions <- character(0)
  }
  #Set the current document
  current_document <- document
  
  #store the row and column position
  positions <- c(positions, location)
}
#Output the final document
if (!is.null(current_document)){
  emit_result(current_document, positions)
}