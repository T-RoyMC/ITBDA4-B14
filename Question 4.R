library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)

set.seed(123)
try(Sys.setlocale("LC_TIME", "C"), silent =TRUE)

#4.1
page_url <- "https://www.eduvos.com/qualifications/"
user_agent <- "ITBDA4-Student"

#Extract
resp <- GET(page_url, user_agent(user_agent), timeout(30))
stop_for_status(resp)
page <- read_html(content(resp, as = "text", encoding = "UTF-8"))

view_xpath <- ".//a[contains(normalize-space(.), 'View Programme') or contains(normalize-space(.), 'View Course')]"
view_links <- html_elements(page, xpath = "//a[contains(normalize-space(.), 'View Programme') or contains(normalize-space(.), 'View Course')]")
cat("Programme Links found on the page:", length(view_links), "\n")
if (length(view_links) < 20) stop("Fewer than 20 records found - the page layout may have changed.")

card_of <- function(link) {
  xml2::xml_find_first(link, paste0("ancestor::*[count(", view_xpath, ")=1][last()]"))
}

#Pull fields out of the unstructured card text
re_type <- "(?i)(Higher Certificate|Advanced Certificate|Postgraduate Diploma|Advanced Diploma|Diploma|Degree|Honours|Masters?|Access|Pre-?degree|Corporate Training|Short Course|Certification)"
re_faculty <- "(?i)(Humanities and Arts|Commerce|Technology|Information Technology|Law|Education|Engineering|Science)"
re_start <- "(?i)start\\s*date:?\\s*([0-9]{1,2}\\s+[A-Za-z]+\\s+[0-9]{4})"
re_view <- "(?i)view\\s+(programme|course)"

parse_card <- function(link){
  card <- card_of(link)
  txt <- str_squish(html_text2(card))
  href <- xml2::url_absolute(html_attr(link, "href"), page_url)
  head_node <- html_element(card, "h1, h2, h3, h4, h5")
  title <- if (!is.na(head_node)) str_squish(html_text2(head_node)) else NA_character_
  if (is.na(title) || !nzchar(title)) {
    ls <- html_elements(card, "a"); ls <- ls[!str_detect(html_text2(ls), re_view)]
    title <- if (length(ls)) str_squish(html_text2(ls[[1]])) else NA_character_
  }
  
  #labels Type/Faculty
  pos <- if (!is.na(title) && nzchar(title)) str_locate(txt, fixed(title))[1, ] else c(NA, NA)
  prefix <- if (!is.na(pos[1])) str_sub(txt, 1, pos[1] - 1) else ""
  suffix <- if (!is.na(pos[2])) str_sub(txt, pos[2] +1) else txt
  start <- str_match(txt, re_start)[, 2]
  type <- str_match(prefix, re_type)[, 2]
  faculty <- str_match(prefix, re_faculty)[, 2]
  desc <- suffix %>% str_remove(re_start) %>% str_remove(re_view)
  data.frame(raw_title = title, raw_type = type, raw_faculty = faculty,
             raw_description = str_squish(desc), raw_start_date = start,
             raw_url = href, raw_text = txt, stringsAsFactors = FALSE)
}

raw_data <- bind_rows(lapply(view_links, parse_card))
raw_data$scraped_on <- as.character(Sys.Date())
cat("Scraped", nrow(raw_data), "unstructured records\n")
str(raw_data[, 1:6])
print(head(raw_data[, c("raw_title", "raw_type", "raw_faculty", "raw_start_date")], 8))
write.csv(raw_data, "eduvos_qualifications_raw.csv", row.names = FALSE)

cat("\nMissing values in the raw data:\n")
print(colSums(is.na(raw_data) | raw_data == ""))

#4.2
clean <- raw_data %>%
  transmute(
    title = str_squish(raw_title),
    type = raw_type,
    faculty = raw_faculty,
    description = raw_description,
    start_raw = raw_start_date,
    url = raw_url
  )

#Remove Empty rows and duplicates
clean <- clean %>% filter(!is.na(title), nzchar(title)) %>% distinct(title, .keep_all = TRUE)

#Denoise the text
denoise <- function(x) {
  x <- str_replace_all(x, "&amp;", "&")
  x <- str_replace_all(x, "[..]", " ")
  x <- str_replace_all(x, "(\\.{2,}|…)\\s*$", "")
  x <- str_replace_all(x, "[^[:alnum:][:space:][:punct:]]", " ")
  str_squish(x)
}
clean$title <- denoise(clean$title)
clean$description <- denoise(clean$description)
clean$description[clean$description == ""] <- NA

#Make labels consistent
std_type <- function(x) {
  x <- str_to_title(str_squish(x))
  x <- str_replace(x, "^Masters?$", "Masters")
  x <- str_replace(x, "^Pre-?Degree$", "Pre-Degree")
  x
}
clean$type <- std_type(clean$type)
clean$faculty <- str_to_title(str_squish(clean$faculty))
clean$faculty <- str_replace(clean$faculty, "^Information Technology$", "Technology")
clean$faculty <- str_replace(clean$faculty, "^Humanities And Arts$", "Humanities and Arts")

#Regex base of missing categorical values from qualification name
infer_type <- function(t) case_when(
  str_detect(t, "(?i)^higher certificate") ~ "Higher Certificate",
  str_detect(t, "(?i)postgraduate diploma") ~ "Postgraduate Diploma",
  str_detect(t, "(?i)advanced diploma") ~ "Advanced Diploma",
  str_detect(t, "(?i)honours") ~ "Honours",
  str_detect(t, "(?i)^master") ~ "Masters",
  str_detect(t, "(?i)^bachelor|^bcom|^ba\\b|^bsc") ~ "Degree",
  str_detect(t, "(?i)^diploma") ~ "Diploma",
  str_detect(t, "(?i)^certificate|short course|course") ~ "Certificate",
  TRUE ~ NA_character_)

infer_faculty <- function(t) case_when(
  str_detect(t, "(?i)account|commerce|business|market|financ|economic|management|logistic|supply") ~ "Commerce",
  str_detect(t, "(?i)information|comput|software|data|cyber|network|technolog|\\bit\\b|systems") ~ "Technology",
  str_detect(t, "(?i)\\barts?\\b|design|psycholog|communicat|english|literature|media|humanit") ~ "Humanities and Arts",
  str_detect(t, "(?i)\\blaw\\b|legal") ~ "Law",
  TRUE ~ NA_character_)

clean$type <- ifelse(is.na(clean$type) | clean$type == "", infer_type(clean$title), clean$type)
clean$faculty <- ifelse(is.na(clean$faculty) | clean$faculty == "", infer_faculty(clean$title), clean$faculty)
clean$type[is.na(clean$type)] <- "Unknown"
clean$faculty[is.na(clean$faculty)] <- "Unknown"

#Parse dates
clean$start_date <- as.Date(clean$start_raw, format = "%d %B %Y")
clean$nqf_level <- case_when(
  str_detect(clean$title, "(?i)^higher certificate")  ~ 5L,
  str_detect(clean$title, "(?i)^diploma")             ~ 6L,
  str_detect(clean$title, "(?i)advanced diploma")     ~ 7L,
  str_detect(clean$title, "(?i)^bachelor|^bcom|^ba\\b") ~ 7L,
  str_detect(clean$title, "(?i)honours|postgraduate diploma") ~ 8L,
  str_detect(clean$title, "(?i)^master")              ~ 9L,
  TRUE ~ NA_integer_)
clean$title_chars <- nchar(clean$title)
clean$desc_words  <- ifelse(is.na(clean$description), NA_integer_,
                            str_count(clean$description, "\\S+"))

#Missing-values
n_missing_before <- c(description = sum(is.na(clean$description)),
                      start_date  = sum(is.na(clean$start_date)),
                      desc_words  = sum(is.na(clean$desc_words)))
clean$description_missing <- is.na(clean$description)
clean$start_missing       <- is.na(clean$start_date)

clean$description[is.na(clean$description)] <- "No description available"
clean <- clean %>%
  group_by(type) %>%
  mutate(desc_words = ifelse(is.na(desc_words) | description_missing,
                             as.integer(round(median(desc_words[!description_missing], na.rm = TRUE))),
                             desc_words)) %>%
  ungroup()
clean$desc_words[is.na(clean$desc_words)] <- as.integer(round(median(clean$desc_words, na.rm = TRUE)))
mode_date <- function(d) { d <- d[!is.na(d)]; if (!length(d)) as.Date(NA) else as.Date(names(sort(table(d), decreasing = TRUE))[1]) }
overall_mode <- mode_date(clean$start_date)
clean <- clean %>%
  group_by(type) %>%
  mutate(start_date = { m <- mode_date(start_date); ifelse(is.na(start_date), ifelse(is.na(m), overall_mode, m), start_date) }) %>%
  ungroup()
clean$start_date <- as.Date(clean$start_date, origin = "1970-01-01")
clean$days_to_start <- as.integer(clean$start_date - Sys.Date())

cat("\nMissing values BEFORE imputation:\n"); print(n_missing_before)

iqr_outliers <- function(x) { q <- quantile(x, c(.25, .75), na.rm = TRUE); i <- diff(q)
which(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i) }
cat("Description-length outliers (rows):", length(iqr_outliers(clean$desc_words)), "\n")

final <- clean %>%
  select(title, type, faculty, description, start_date, nqf_level,
         desc_words, title_chars, days_to_start, description_missing, start_missing, url)
cat("\nMissing values AFTER cleaning (nqf_level stays NA for non-NQF courses):\n")
print(colSums(is.na(final)))
cat("\nCleaned data:", nrow(final), "rows\n")
print(head(final[, c("title", "type", "faculty", "start_date", "nqf_level", "desc_words")], 8))
write.csv(final, "eduvos_qualifications_clean.csv", row.names = FALSE, na = "")