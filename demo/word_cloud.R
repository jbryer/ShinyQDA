library(ShinyQDA)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(reshape2)
library(wordcloud)
library(tm)

# Create connection to database file
db_file <- file.choose() # This will ask for where the SQLite file is located
qda_data <- ShinyQDA::qda(db_file)

text_data <- qda_data$get_text()
text <- text_data$qda_text
docs <- tm::Corpus(VectorSource(text))
docs <- docs %>%
	tm::tm_map(tm::removeNumbers) %>%
	tm::tm_map(tm::removePunctuation) %>%
	tm::tm_map(tm::stripWhitespace)
docs <- tm::tm_map(docs, content_transformer(tolower))
docs <- tm::tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words), freq = words)

wordcloud::wordcloud(words = df$word,
					 freq = df$freq,
					 min.freq = 2,
					 max.words = 200,
					 random.order = FALSE,
					 rot.per = 0.35,
					 colors = brewer.pal(8, "Dark2"))
