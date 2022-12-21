library(devtools)
library(usethis)

usethis::use_tidy_description()

# unlink("NAMESPACE")
devtools::document()
devtools::install(upgrade = 'never')
devtools::build()
devtools::check()

shiny::runApp('inst/daacs/')
shiny::runApp('inst/shiny_template')
shiny::runApp('inst/daacs_writing/')

ShinyQDA::shinyQDA()
unlink('ShinyQDA.sqlite')

# Add package imports
usethis::use_package('textdata', type = 'Imports')


qda_data <- ShinyQDA::qda('inst/daacs/daacs.sqlite')

DBI::dbListTables(qda_data$db_conn)

##### Data Prep
app_dir <- 'inst/daacs/'
textdata::lexicon_afinn(dir = app_dir)
textdata::lexicon_bing(dir = app_dir)
textdata::lexicon_loughran(dir = app_dir)
textdata::lexicon_nrc(dir = app_dir)

reviews <- read.csv('data-raw/archive/Musical_instruments_reviews.csv')

asep8 <- readxl::read_excel('data-raw/asap-aes/training_set_rel3.xlsx') |>
	dplyr::filter(essay_set == 8)
usethis::use_data(asep8, overwrite = TRUE)

asep8_rubric <- readxl::read_excel('data-raw/asap8_rubric.xlsx')
usethis::use_data(asep8_rubric, overwrite = TRUE)

asep7 <- readxl::read_excel('data-raw/asap-aes/training_set_rel3.xlsx') |>
	dplyr::filter(essay_set == 7)
usethis::use_data(asep7, overwrite = TRUE)

asep7_rubric <- readxl::read_excel('data-raw/asap7_rubric.xlsx')
usethis::use_data(asep7_rubric, overwrite = TRUE)


################################################################################
qda_data <- ShinyQDA::qda(file = 'inst/shiny/daacs.sqlite')

qda_data$get_last_update()

qda_data$get_code_questions()


codings <- qda_data$get_codings()
code_question_responses <- qda_data$get_code_question_responses()

table(codings$qda_id) |> as.data.frame()

text_df <- qda_data$get_text()
extra_cols <- character()
if(ncol(text_df) > 3) {
	extra_cols <- names(text_df)[seq(3, ncol(text_df) - 1)]
}
extra_cols

# codings <- ShinyQDA::get_coding_table(qda_data)
df <- ShinyQDA::qda_merge(qda_data)
code_cols <- names(df)[seq(ncol(text_df) + 2, ncol(df))]
code_cols

df_sum <- apply(df[,code_cols], 2, FUN = function(x) { sum(x, na.rm = TRUE )}) |>
	as.data.frame()
names(df_sum)[1] <- 'Count'
df_sum$Code <- row.names(df_sum)

library(ggplot2)
ggplot(df_sum, aes(x = Code, y = Count)) +
	geom_bar(stat = 'identity', fill = 'grey50') +
	geom_text(aes(label = Count), hjust = -1) +
	coord_flip() +
	ggtitle('Number of codes across all text')

# Word cloud
library(wordcloud)
library(tm)
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

wordcloud(words = df$word,
		  freq = df$freq,
		  min.freq = 2,
		  max.words = 200,
		  random.order = FALSE,
		  rot.per = 0.35,
		  colors = brewer.pal(8, "Dark2"))









# Test dataset
essays <- readr::read_csv('../../data-raw/writingrtf_ec.csv')
categories <- list(
	'metacognition' = c('planning', 'monitoring', 'evaluation'),
	'motivation' = c('mindset', 'test anxiety', 'mastery orientation'),
	'self efficacy' = c('self efficacy for online learning',
						'self efficacy for writing',
						'self efficacy for mathematics',
						'self efficacy for reading'),
	'strategies' = c('managing environment', 'understanding', 'managing time', 'help seeking')
)
codes <- c(names(categories), unlist(categories)) |> unname()

# Create the QDA data object
qda_data <- ShinyQDA::qda(
	file = 'daacs_test.rqda',
	df = essays[,c('DAACS_ID', 'essay', 'srlTotal', 'writeTotal')],
	id_col = 'DAACS_ID',
	text_col = 'essay',
	codes = codes
)
qda_data$save()

# Reread the QDA data file
qda_data <- ShinyQDA::qda(file = 'daacs_test.rqda', auto_save = FALSE)


ShinyQDA::shinyQDA(qda_data)

##### Hex Logo #################################################################
library(hexSticker)
library(showtext)
font_add_google("Gochi Hand", 'gochi')
p <- "man/figures/highlight.png"
hexSticker::sticker(p,
					filename = 'man/figures/ShinyQDA.png',
					p_size = 10,
					package = 'ShinyQDA',
					url = "github.com/jbryer/ShinyQDA",
					p_family = 'gochi',
					u_size = 5.5,
					s_width = .75, s_height = .75,
					s_x = 1, s_y = 1,
					p_x = 0.9, p_y = 1.6,
					p_color = "#3A66FF",
					h_fill = '#fff7dc',
					h_color = '#FFD33A',
					u_color = '#3A66FF',
					white_around_sticker = FALSE)
