#' Plot sentiment for an individual text.
#'
#' @param text the text to calculate sentiment bar plot for.
#' @param lexicon the sentiment dictionary to use.
#' @param lexicon_dir directory containing the sentiment dictionayr.
#' @export
sentiment_plot <- function(text,
						   lexicon = c('nrc', 'bing', 'loughran', 'afinn'),
						   lexicon_dir = '.'
						   ) {
	if(lexicon[1] == 'nrc') {
		# sentiments <- tidytext::get_sentiments("nrc")
		sentiments <- textdata::lexicon_nrc(dir = lexicon_dir)
	} else if(lexicon[1] == 'bing') {
		sentiments <- tidytext::get_sentiments("bing")
	} else if(lexicon[1] == 'afinn') {
		# sentiments <- tidytext::get_sentiments("afinn")
		sentiments <- textdata::lexicon_afinn(dir = lexicon_dir)
		sentiments$sentiment <- as.character(sentiments$value)
	} else if(lexicon[1] == 'loughran') {
		# sentiments <- tidytext::get_sentiments("loughran")
		sentiments <- textdata::lexicon_loughran(dir = lexicon_dir)
	} else {
		stop(paste0('Unknown lexicon: ', lexicon[1]))
	}
	colors <- lexicon_colors[[lexicon[1]]]
	colors_df <- data.frame(sentiment = names(colors),
							color = colors,
							row.names = NULL)

	paragraphs <- strsplit(text, '\n')[[1]]

	tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
						 text = paragraphs) |>
		dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
		tidytext::unnest_tokens(token, text,
								token = 'words',
								to_lower = FALSE,
								strip_punct = FALSE) |>
		dplyr::mutate(token_lower = tolower(token)) |>
		dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
		dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

	tokens_tab <- table(tokens$sentiment) |> as.data.frame()

	ggplot2::ggplot(tokens_tab, ggplot2::aes(x = Var1, y = Freq, fill = Var1)) +
		ggplot2::geom_bar(stat = 'identity') +
		ggplot2::geom_text(ggplot2::aes(label = Freq), hjust = -0.1) +
		ggplot2::expand_limits(y = max(tokens_tab$Freq) + max(tokens_tab$Freq) * .05) +
		ggplot2::scale_fill_manual(values = colors) +
		ggplot2::scale_x_discrete(limits = names(colors)) +
		ggplot2::coord_flip() +
		ggplot2::xlab('') +
		ggplot2::ylab('Frequency') +
		ggplot2::theme(legend.position = 'none') +
		ggplot2::theme_minimal()
}
