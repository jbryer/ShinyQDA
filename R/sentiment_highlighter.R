colors_bing <- c(positive = '#67a9cf',
				 negative = '#ef8a62')
colors_nrc <- c(anger = '#fb8072',
				anticipation = '#bebada',
				disgust = '#d9d9d9',
				fear = '#fdb462',
				joy = '#ffffb3',
				negative = '#b3de69',
				positive = '#80b1d3',
				sadness = '#fccde5',
				surprise = '#8dd3c7',
				trust = '#bc80bd')

#' Returns an HTML character highlighting the sentiments.
#'
#' @param thetext the text to highlight.
#' @export
sentiment_highlighter <- function(text, lexicon = c('nrc', 'bing')) {
	colors <- NULL
	sentiments <- NULL
	if(lexicon[1] == 'nrc') {
		sentiments <- tidytext::get_sentiments("nrc")
		colors <- colors_nrc
	} else if(lexicon[1] == 'bing') {
		sentiments <- tidytext::get_sentiments("bing")
		colors <- colors_bing
	} else {
		stop(paste0('Unknown lexicon: ', lexicon[1]))
	}
	colors_df <- data.frame(sentiment = names(colors), color = colors, row.names = NULL)

	tokens <- data.frame(text = text) |>
		dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
		tidytext::unnest_tokens(token, text, to_lower = FALSE, strip_punct = FALSE) |>
		dplyr::mutate(token_lower = tolower(token)) |>
		dplyr::left_join(sentiments, by = c('token_lower' = 'word')) |>
		dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

	tokens$html <- tokens$token
	sentiment_rows <- !is.na(tokens$color)
	tokens[sentiment_rows,]$html <- paste0("<span style='background-color: ",
										   tokens[sentiment_rows,]$color,
										   "; word-wrap: break-word; display: inline; white-space: pre-wrap;' class='tooltip2'>",
										   "<span class='tooltiptext2'>",
										   tokens[sentiment_rows,]$sentiment,
										   "</span>",
										   tokens[sentiment_rows,]$token,
										   "</span>")
	html <- paste0(tokens$html, collapse = ' ')
	return(html)
}
