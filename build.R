library(devtools)
library(usethis)

usethis::use_tidy_description()

devtools::document()
devtools::install(upgrade = 'never')
devtools::install(upgrade = 'never', build_vignettes = TRUE)
devtools::build_readme()
devtools::build()
devtools::check()

# Build Slides
rmarkdown::render(input = 'inst/slides/Intro_ShinyQDA.Rmd')
old_wd <- setwd('inst/slides/'); pagedown::chrome_print('Intro_ShinyQDA.html'); setwd(old_wd)

# Setup Github actions (only need to run once per project)
# usethis::use_pkgdown_github_pages()
# usethis::use_github_action("README.Rmd")

demo('daacs', package = 'ShinyQDA')

library(ShinyQDA)
shiny::runApp(paste0(find.package('ShinyQDA'), '/daacs_demo/'))

shiny::runApp('inst/daacs_demo/')

shiny::runApp(paste0(find.package('ShinyQDA'), '/daacs_demo/'))

install.packages(c('wordcloud2', 'ldatuning', 'modeltools', 'topicmodels', 'RColorBrewer'))

################################################################################

library(ShinyQDA)
data(daacs_data)
thetext <- daacs_data[1,]$qda_text

token <- c('words', 'characters', 'character_shingles', 'ngrams', 'skip_ngrams', 'sentences', 'lines', 'paragraphs', 'ptb')

tokens <- daacs_data[1,] |>
	dplyr::select(qda_text) |>
	tidytext::unnest_tokens(token, qda_text,
							token = 'words',
							to_lower = TRUE,
							strip_punct = TRUE
							# strip_numeric = FALSE
							# n = 3
							) |>
	table() |>
	as.data.frame() |>
	dplyr::filter(Freq > 2)

ggplot(tokens, aes(x = stats::reorder(token, Freq), y = Freq)) +
	ggplot2::geom_bar(stat = 'identity', fill = 'grey50') +
	ggplot2::geom_text(ggplot2::aes(label = Freq), hjust = -0.1) +
	ggplot2::coord_flip() +
	ggplot2::expand_limits(y = max(tokens$Freq) + max(tokens$Freq) * .05) +
	xlab('') +
	ggplot2::theme_minimal()

################################################################################

# Run shiny app examples
shiny::runApp('inst/daacs_demo/')
shiny::runApp('inst/shiny_template')
shiny::runApp('inst/daacs_writing/')

ShinyQDA::shinyQDA()
unlink('ShinyQDA.sqlite')

# Add package imports
# usethis::use_package('topicmodels', type = 'Imports')
# usethis::use_package('writexl', type = 'Suggests')

daacs_qda <- ShinyQDA::qda('inst/daacs_demo/qda.sqlite')
qda_data <- ShinyQDA::qda('inst/daacs_demo/qda.sqlite')
qda_data <- ShinyQDA::qda('inst/daacs_writing/daacs_writing.sqlite')

DBI::dbListTables(qda_data$db_conn)

##### Get merged data
qdadf <- ShinyQDA::qda_merge(qda_data, include_sentiments = TRUE)
names(qdadf)

##### Sentiment analysis

qdadf$srlTotal <- apply(qdadf, 1, FUN = function(x) {
	mean(as.numeric(x[c('motivation', 'strategies', 'metacognition')]))
})
qdadf$writeTotal <- apply(qdadf, 1, FUN = function(x) {
	mean(as.numeric(x[c('content', 'organization', 'paragraphs', 'sentences', 'conventions')]))
})
ggplot2::ggplot(qdadf[!duplicated(qdadf$qda_id),],
				ggplot2::aes(x = bing_total, y = srlTotal)) +
	ggplot2::geom_point() +
	ggplot2::geom_smooth(method = 'loess', se = FALSE, formula = y ~ x) +
	ggplot2::theme_minimal()


##### Data Prep ################################################################
app_dir <- 'inst/daacs/'

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

tools::resaveRdaFiles('data/asep7.rda')
tools::resaveRdaFiles('data/asep8.rda')

################################################################################
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
					p_size = 12,
					package = 'ShinyQDA',
					url = "jbryer.github.io/ShinyQDA/",
					p_family = 'gochi',
					u_size = 5.5,
					s_width = 0.95, s_height = 0.95,
					s_x = 1, s_y = 0.8,
					p_x = 0.9, p_y = 1.55,
					p_color = "#3A66FF",
					h_fill = '#fff7dc',
					h_color = '#FFD33A',
					u_color = '#3A66FF',
					white_around_sticker = TRUE)
