library(devtools)
library(usethis)

usethis::use_tidy_description()

devtools::document()
devtools::install(upgrade = 'never')
devtools::build_readme()
devtools::build()
devtools::check()

# Setup Github actions (only need to run once per project)
# usethis::use_pkgdown_github_pages()
# usethis::use_github_action("README.Rmd")

################################################################################
# Test creating a new app
library(ShinyQDA)
data("daacs_data", package = 'ShinyQDA')
username <- 'admin'

ShinyQDA::new_app(name = 'daacs_demo',
				  dir = getwd(),
				  qda_data = daacs_data,
				  run_app = FALSE)

daacs_qda <- qda('daacs_demo/qda.sqlite')

data("daacs_codings", package = 'ShinyQDA')
data("daacs_text_responses", package = 'ShinyQDA')
data("daacs_rubric", package = 'ShinyQDA')

# Add text questions
daacs_qda$add_text_question(stem = 'Non-responsive to prompt', type = 'checkbox')
daacs_qda$add_text_question(stem = 'Additional comments about the text', type = 'text')
daacs_qda$get_text_questions()

# Add code questions
daacs_qda$add_code_question(
	stem = 'Content of Essay',
	type = 'checkbox',
	options = c('Definition of concepts',
				'Interpretation_strength',
				'Interpretation_weakness',
				'Interpretation_mixed',
				'Interpretation_medium',
				'Interpretation_strategies awareness',
				'Interpretation_strategies commitment',
				'Interpretation_judgments')
)
daacs_qda$add_code_question(
	stem = 'Does this text represent',
	type = 'checkbox',
	options = c('Strength',
				'Weakness',
				'Mixed',
				'Medium',
				'Strategies Awareness',
				'Strategies Commitment')
)
daacs_qda$add_code_question(
	stem = 'Judgment about survey or feedback',
	type = 'checkbox',
	options = c('Agree',
				'Disagree',
				'Valuable/helpful/useful',
				'Not valuable/helpful/useful',
				'Other')
)
daacs_qda$get_code_questions()

# Add codes
categories <- list(
	'metacognition' = c('planning', 'monitoring', 'evaluation'),
	'motivation' = c('mindset', 'test anxiety', 'mastery orientation'),
	'self efficacy' = c('self efficacy for online learning',
						'self efficacy for writing',
						'self efficacy for mathematics',
						'self efficacy for reading'),
	'strategies' = c('managing environment', 'understanding', 'managing time', 'help seeking'),
	'procrastination' = c()
)
codes <- c(names(categories), unlist(categories)) |> unname()
daacs_qda$add_codes(codes)
for(i in seq_len(length(categories))) {
	category <- names(categories)[i]
	codes <- categories[[i]]
	for(i in codes) {
		daacs_qda$update_code(i, parent = category)
	}
}
daacs_qda$get_codes()

# Add rubric
daacs_qda$add_rubric(
	rubric_name = 'daacs',
	description = 'DAACS Scoring Rubric',
	rubric = daacs_rubric
)

shiny::runApp('daacs_demo')

# Get the merged data
daacs_merged <- qda_merge(daacs_qda, include_sentiments = TRUE)


unlink('daacs_demo', recursive = TRUE)
################################################################################

# Run shiny app examples
shiny::runApp('inst/daacs/')
shiny::runApp('inst/shiny_template')
shiny::runApp('inst/daacs_writing/')

ShinyQDA::shinyQDA()
unlink('ShinyQDA.sqlite')

# Add package imports
usethis::use_package('tibble', type = 'Imports')
usethis::use_package('writexl', type = 'Suggests')

qda_data <- ShinyQDA::qda('inst/daacs/daacs.sqlite')
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
