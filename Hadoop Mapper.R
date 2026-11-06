#Mapper

#Obtain the search keyword
keyword <- Sys.getenv("SEARCH_KEYWORD")

if (keyword == ""){
  stop("Please provide a search keyword using SEARCH_KEYWORD.")}

#Read input supplied
input <- file("stdin", open = "r")
lines <- readLines(input, warn = FALSE)

#Process each input record
for (line in lines) {
  first_tab <- regexpr("\t", line, fixed = TRUE)[i]
  if (first_tab < 1) next
  
  remainder <- substring(line, first_tab + 1)
  second_relative <- regexpr("\t", remainder, fixed = TRUE)[i]
  
  if (second_relative < 1) next
  
  second_tab <- first_tab + second_relative
  
  #Extract document name, row number and text
  document <- substring(line, 1, first_tab - 1)
  
  row_number <- substring(
    line,
    first_tab + 1,
    second_tab - 1)
  
  text <- substring(line, second_tab + 1)
  
  #Escape special regex characters in the keyword
  escape_keyword <- gsub(
    "([][{}()+*^$|\\\\?.])",
    "\\\\\\1",
    keyword,
    perl = TRUE)
  
  #Search for the complete keyword, ignoring case
  pattern <- paste0("\\b", escaped_keyword, "\\b")
  
  positions <- gregexpr(
    pattern,
    text,
    ignore.case = TRUE,
    perl = TRUE)[[1]]
  
  #Skip lines without match
  if (length(positions) == 1 && positions[1] == -1){
    next}
  
  #Emit document name and row:column position
  for (column in positions){
    result <- paste0(row_number, ":", column)
    cat(
      document,
      "\t",
      result,
      "\n",
      sep = "")
  }
}