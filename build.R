library(devtools)
library(usethis)

usethis::use_tidy_description()

unlink("NAMESPACE")
devtools::document()
devtools::install(upgrade = 'never')
devtools::build()
devtools::check()

usethis::use_package('lubridate', type = 'Imports')

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


