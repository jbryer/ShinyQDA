library(ShinyQDA)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(reshape2)

# Create connection to database file
db_file <- file.choose() # This will ask for where the SQLite file is located
qda_data <- ShinyQDA::qda(db_file)

df <- qda_data$get_text() |>
	dplyr::select(qda_id, qda_text)

# Tokenize words
tokens <- df |>
	tidytext::unnest_tokens(
		output = 'token',
		input = 'qda_text',
		token = 'words',
		to_lower = TRUE)

# Remove stopwords
tokens <- tokens |>
	dplyr::filter(!(token %in% stopwords::stopwords(source = 'snowball')))

# ngrams
tokens <- df |>
	tidytext::unnest_tokens(
		output = 'token',
		input = 'qda_text',
		token = 'ngrams',
		n = 2
	)

# Word frequency plot
tokens |>
	dplyr::count(token, sort = TRUE) |>
	dplyr::top_n(n = 20, n) |>
	dplyr::mutate(token = reorder(token, n)) |>
	ggplot2::ggplot(ggplot2::aes(x = token, y = n)) +
	ggplot2::geom_bar(stat = 'identity') +
	ggplot2::coord_flip() +
	ggplot2::xlab('')


# Convert to a wide data.frame
tokens_wide <- tokens |>
	reshape2::dcast(qda_id ~ token, fun.aggregate = length, value.var = 'token')

