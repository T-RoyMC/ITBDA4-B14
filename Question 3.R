library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(igraph)
library(Matrix)

set.seed(123)
dir.create("q3_output", showWarnings = FALSE)

#3.1
base_url <- "https://quotes.toscrape.com"
user_agent <- "ITBDA-Student"
max_pages <- 10
pause_secs <- 1

#Robot
robots_ok <- tryCatch({
  r <- GET(paste0(base_url, "/robots.txt"), user_agent(user_agent), timeout(10))
  if (status_code(r) == 200) {
    txt <- content(r, as = "text", encoding = "UTF-8")
    cat("robots.txt found:\n", txt, "\n")
    !grepl("(?im)^\\s*disallow:\\s*/\\s*$", txt, perl = TRUE)
  } else {
    cat("No robots.txt restrictions (HTTP status", status_code(r), ")\n")
    TRUE
  }
}, error = function(e) {
  message("Could not read robots.txt (", conditionMessage(e), "); continuing politely.")
  TRUE
})
if (!robots_ok) stop("robots.txt disallows scraping this site.")

#Scrapping
scrape_page <- function(url){
  page <- read_html(url)
  nodes <- html_elements(page, "div.quote")
  data.frame(
    quote = html_text2(html_element(nodes, "span.text")),
    author = html_text2(html_element(nodes, "small.author")),
    tags = vapply(nodes, function(n) paste(html_text2(html_elements(n, "a.tag")), collapse = ","),
                  character(1)),
    next_page = rep(html_attr(html_element(page, "li.next a"), "href"), length(nodes)),
    stringsAsFactors = FALSE
  )
}

#Follow next Page link
raw_list <- list()
url <- paste0(base_url, "/")
for (i in seq_len(max_pages)) {
  cat("Scrapping page", i, ":", url, "\n")
  pg <- tryCatch(scrape_page(url), error = function(e) {message("Failed: ", conditionMessage(e)); NULL})
  if (is.null(pg) || nrow(pg) == 0) break
  pg$page <- i
  raw_list[[i]] <- pg
  nxt <- pg$next_page[1]
  if (is.na(nxt)) break
  url <- paste0(base_url, nxt)
  Sys.sleep(pause_secs)
}

#Dataframe structure
quotes_raw <- bind_rows(raw_list) %>% select(page, quote, author, tags)
cat("\nScraped", nrow(quotes_raw), "quotes from", length(raw_list), "pages\n")
str(quotes_raw)
print(head(quotes_raw, 5))
write.csv(quotes_raw, "Q3_output/raw_quotes.csv", row.names = FALSE)

#3.2
#Textual preprocessing
quotes <- quotes_raw %>%
  mutate(quote = str_trim(str_remove_all(quote, "[\u201c\u201d\"]"))) %>%
  filter(!is.na(quote), nzchar(trimws(quote))) %>%
  distinct(quote, .keep_all = TRUE) %>%
  mutate(id = row_number())
cat("After removing empty/duplicate rows:", nrow(quotes), "quotes\n")

#Remove encodings, quotes and punctuation
clean_text <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  x <- gsub("http\\S+|www\\.\\S+", " ", x)
  x <- gsub("<[^>]+>", " ", x)
  x <- tolower(x)
  x <- gsub("can't", "cannot", x)
  x <- gsub("won't", "will not", x)
  x <- gsub("'s\\b", "", x)
  x <- gsub("n't\\b", " not", x)
  x <- gsub("[^a-z ]", " ", x)
  str_squish(x)
}
quotes$clean <- clean_text(quotes$quote)

#Remove tokenise
extra_stop <- c("will", "can", "just", "one", "us", "get", "like", "must", "may", "even")
stop_data <- bind_rows(stop_words, data.frame(word = extra_stop, lexicon = "custom"))

tokens <- quotes %>%
  select(id, clean) %>%
  unnest_tokens(word, clean) %>%
  filter(nchar(word) >= 3) %>%
  anti_join(stop_data, by = "word")
tokens$stem <- SnowballC::wordStem(tokens$word, language = "english")

cat("Tokens kept after cleaning:", nrow(tokens), "| unique words:", n_distinct(tokens$word), "\n")
print(head(quotes[, c("quote", "clean")], 3))

#clean tags
tags_long <- quotes %>%
  select(id, author, tags) %>%
  separate_rows(tags, sep = ",") %>%
  rename(tag = tags) %>%
  filter(nzchar(tag), tag != "misattributed-john-lennon")

#3.3
#Word frequency
word_freq <- tokens %>% count(word, sort = TRUE)

freq_scores <- tokens %>%
  left_join(word_freq, by = "word") %>%
  group_by(id) %>%
  summarise(freq_score = mean(n), .groups = "drop") %>%
  left_join(quotes %>% select(id, quote, author), by = "id") %>%
  arrange(desc(freq_score))

n_sum <- 5
summary_freq <- head(freq_scores, n_sum)
cat("\n Summary 1: Top", n_sum, "quotes by word-frequency score\n")
for (i in seq_len(nrow(summary_freq)))
  cat(sprintf("%d. \"%s\" - %s (score %.1f)\n", i, summary_freq$quote[i],
              summary_freq$author[i], summary_freq$freq_score[i]))

#TextRank
tfidf <- tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n)
m <- cast_sparse(tfidf, id, word, tf_idf)
m <- m / pmax(sqrt(Matrix::rowSums(m^2)), 1e-12)
sim <- as.matrix(m %*% Matrix::t(m))
diag(sim) <- 0

g <- graph_from_adjacency_matrix(sim, mode = "undirected", weighted = TRUE)
pr <- page_rank(g)$vector
rank_data <- data.frame(id = as.integer(rownames(sim)), textrank = pr) %>%
  left_join(quotes %>% select(id, quote, author), by = "id") %>%
  arrange(desc(textrank))

summary_tr <- head(rank_data, n_sum)
cat("\n Summary 2: Top", n_sum, "quotes by TextRank \n")
for (i in seq_len(nrow(summary_tr)))
  cat(sprintf("%d. \"%s\" - %s (rank %.4f)\n", i, summary_tr$quote[i],
              summary_tr$author[i], summary_tr$textrank[i]))

overlap <- intersect(summary_freq$id, summary_tr$id)
cat("\n Quotes chosen by BOTH Techniques:", length(overlap), "of", n_sum, "\n")

#number of words
top_n_words <- 15

#Visualization
#Most frequent words
plot1 <- word_freq %>% slice_head(n = top_n_words) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  expand_limits(y = max(word_freq$n) * 1.1) +
  labs(title = "Most frequent words in the scraped quotes",
       subtitle = paste("After preprocessing;", nrow(quotes), "quotes"),
       x = "Word", y = "Number of occurrences") +
  theme_minimal()
print(plot1)

#TextRank Scores
plot2 <- summary_tr %>%
  mutate(label = paste0("#", row_number(), " ", author, ": ", str_trunc(quote, 45))) %>%
  ggplot(aes(x = reorder(label, textrank), y = textrank)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(title = "summary quotes selected by TextRank",
       x = "Quote", y = "TextRank score (centrality)") +
  theme_minimal()
print(plot2)

#wordcloud
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 2, max.words = 80,
          random.order = FALSE, colors = RColorBrewer::brewer.pal(8, "Dark2"))
title("Word Cloud of the quotes")



