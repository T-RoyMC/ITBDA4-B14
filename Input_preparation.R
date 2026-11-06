#Preparing input for Haddop Map-Reducer
#Select the zip file with documents
zip_path <- file.choose()

#Extract documents
extract_dir <- file.path(getwd(), "Africa_documents")

if (!dir.exists(extract_dir)){
  dir.create(extract_dir, recursive = TRUE)}

#Extract the ZIP file
unzip(zip_path, exdir = extract_dir)

#Find all the .txt files even when in subfolders within the zip folder
files <-  list.files(
  extract_dir,
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE)

if (lenngth(files) == 0){
  stope("No .txt files were found in the ZIP archive.")}

cat("Number of documents found:", length(files), "\n")

#Hadoop input file
output_file <- file.path(getwd(), "african_input.tsv")
con <- file(output_file, open = "w", encoding = "UTF-8")

#Process each file
for (f in files) {
  #Use document path as identifier
  full_path <- normalizePath(f, winslash = "/", mustWork = TRUE)
  root_path <- normalizePath(extract_dir, winslash = "/", mustWork = TRUE)
  document_name <- substring(full_path, nchar(root_path) + 2)
  
  #Read the text files line by line
  lines <- readLiines(f, warn = FALSE, encoding = "UTF-8")
  
  if (length(lines) > 0 ){
    for (i in seq_along(lines)){
      
      #Replace tabs so it doesn't break the TSV format
      clean_text <- gsub("\t", " ", lines[i], fixed = TRUE)
      
      #Format: document name, row number, line content
      record <- paste(
        document_name,
        i,
        clean_text,
        sep = "\t")
      writeLines(record, con, sep = "\n", useBytes = TRUE)
    }
  }
  cat("Processed:", document_name, "\n")
}
#Close output
close(con)
cat("\nInput preparation completed!\n")
cat("Output file", output_file, "\n")