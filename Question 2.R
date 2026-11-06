library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(slam)
set.seed(123)

#settings
n_topics <- 3
top_k <- 10

#2.1
#Building a dataframe
if (file.exists("news.zip")){
  news_dir <- file.path(tempdir(), "news_unzipped")
  unzip("news.zip", exdir = news_dir, junkpaths = TRUE)
} else {
  news_dir <- "."}

news_files <- list.files(news_dir, pattern = "^news_[0-9]+\\.txt$", full.names = TRUE)
news_files <- news_files[order(as.numeric(gsub("\\D", "", basename(news_files))))]
if (length(news_files) == 0) stop("No news_*.txt files found.")

news_data <- data.frame(
  file_name = basename(news_files),
  content = vapply(news_files, function(f) {
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")},
    character(1)),
  stringsAsFactors = FALSE,
  row.names = NULL)
cat("Raw data frame:", nrow(news_data), "documents\n")
print(data.frame(file_name = news_data$file_name,
                 content = substr(news_data$content, 1, 70))[1:3, ])

custom_stop <- c("sanewsgovza", "sanews", "govza", "said", "says", "say", "will",
                 "also", "read", "monday", "tuesday", "wednesday", "thursday",
                 "friday", "saturday", "sunday", "july", "per", "cent", "added",
                 "noted", "one", "two", "new", "however", "including", "year", "last",
                 "south", "africa", "african", "made", "still", "since", "well", "must")
clean_text <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("(?i)sanews\\.gov\\.za", " ", x, perl = TRUE)
  x <- gsub("http\\S+|www\\.\\S+", " ", x)
  x <- tolower(x)
  x <- gsub("[^a-z ]", " ", x)
  x}

news_data$clean <- clean_text(news_data$content)

corpus <- VCorpus(VectorSource(news_data$clean))
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), custom_stop))
corpus <- tm_map(corpus, stripWhitespace)

#Document-term matrix
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))
dtm <- dtm[, col_sums(dtm > 0) >= 2]
dtm <- dtm[row_sums(dtm) > 0, ]
cat("Document-term matrix:", nrow(dtm), "documents x", ncol(dtm), "terms\n")

news_data$content <- trimws(gsub("\\s+", " ", news_data$content))
print(data.frame(file_name = news_data$file_name,
                 cleaned = substr(trimws(gsub("\\s+", " ", news_data$clean)), 1, 70))[1:3, ])

#2.2
#LDA
lda_model <- LDA(dtm, k = n_topics, method = "Gibbs",
                 control = list(seed = 123, burnin = 1000, iter = 2000, thin = 100))

#top k words
top_words <- terms(lda_model, top_k)
cat("n Top", top_k, "words per topic \n")

#Word distribution
topic_words <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = top_k, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, desc(beta))
cat("\n Word distribution per topic P(word|topic)\n")
print(as.data.frame(topic_words), digits = 3)

#topic distribution
doc_topics <- tidy(lda_model, matrix = "gamma")
doc_topics$file_name <- news_data$file_name[as.integer(gsub("\\D", "", doc_topics$document))]
doc_topic_wide <- doc_topics %>%
  select(file_name, topic, gamma) %>%
  pivot_wider(names_from = topic, values_from = gamma, names_prefix = "Topic_") %>%
  mutate(dominant_topic = max.col(across(starts_with("Topic_")))) %>%
  arrange(as.numeric(gsub("\\D", "", file_name)))
cat("\n Topic distribution per document(gamma) \n")
print(as.data.frame(doc_topic_wide), digits = 3)

cat("\n Average topic share across documents \n")
print(round(colMeans(doc_topic_wide[, grep("^Topic_", names(doc_topic_wide))]), 3))

#2.3
#Bar chart of the top words in each topic
topic_cols <- RColorBrewer::brewer.pal(8, "Dark2")[seq_len(n_topics)]
bar_plots <- list()
for (t in seq_len(n_topics)) {
  tw <- topic_words[topic_words$topic == t, ]
  bar_plots[[t]] <- ggplot(tw, aes(x = reorder(term, beta), y = beta)) +
    geom_col(fill = topic_cols[t]) +
    geom_text(aes(label = round(beta, 3)), hjust = -0.1, size = 3.5) +
    coord_flip() +
    expand_limits(y = max(tw$beta) * 1.15) +
    labs(title = paste("Topic", t, "- top", top_k, "words"),
         x = "Word", y = "Probability of Word given topic(beta)") +
    theme_minimal()
  print(bar_plots[[t]])
}

#Combined bar graphs of each Topic
plot_bars <- ggplot(topic_words,
                    aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top words per topic (LDA)",
       x = "Word", y = "Probability of word given topic") +
  theme_minimal()
print(plot_bars)

#Word cloud for each topic
par(mfrow = c(1, 1))
all_beta <- tidy(lda_model, matrix = "beta")
for (t in seq_len(n_topics)) {
  cloud_words <- all_beta %>% filter(topic == t) %>% slice_max(beta, n = 25, with_ties = FALSE)
  set.seed(123)
  wordcloud(words = cloud_words$term, freq = cloud_words$beta, min.freq = 0,
            scale = c(2.5, 0.5), random.order = FALSE, rot.per = 0,
            colors = topic_cols[t])
  title(paste("Topic", t, "-word cloud"))
}
  
#Heatmap
graphics.off()
par(mfrow = c(1, 1))
plot_heat <- ggplot(doc_topics,
                    aes(x = factor(topic), y = reorder(file_name, as.numeric(gsub("\\D", "", file_name))),
                        fill = gamma)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(gamma, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Topic distribution per document", x = "Topic", y = "Document", fill = "share") +
  theme_minimal()
print(plot_heat)
