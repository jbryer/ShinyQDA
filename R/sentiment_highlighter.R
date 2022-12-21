#' Color palettes for the various sentiment lexicons
#'
#' @export
lexicon_colors <- list(
	bing = c(positive = '#67a9cf',
			  negative = '#ef8a62'),
	nrc = c(anger = '#fb8072',
			 anticipation = '#bebada',
			 disgust = '#d9d9d9',
			 fear = '#b3de69',
			 joy = '#ffffb3',
			 negative = '#ef8a6',
			 positive = '#80b1d3',
			 sadness = '#fccde5',
			 surprise = '#8dd3c7',
			 trust = '#bc80bd'),
	loughran = c(constraining = '#fccde5',
				  litigious = '#fb8072',
				  negative = '#ef8a62',
				  positive = '#67a9cf',
				  superfluous = '#fccde5',
				  uncertainty = '#ffffb3'),
	afinn = c('-5' = '#67001f',
			  '-4' = '#b2182b',
			  '-3' = '#d6604d',
			  '-2' = '#f4a582',
			  '-1' = '#fddbc7',
			  '0' = '#f7f7f7',
			  '1' = '#d1e5f0',
			  '2' = '#92c5de',
			  '3' = '#4393c3',
			  '4' = '#2166ac',
			  '5' = '#053061')
)

#' Returns an HTML character highlighting the sentiments.
#'
#' @param thetext the text to highlight.
#' @importFrom tidytext unnest_tokens get_sentiments
#' @importFrom dplyr left_join mutate
#' @importFrom stringr str_remove_all
#' @export
sentiment_highlighter <- function(text, lexicon = c('nrc', 'bing', 'loughran', 'afinn')) {
	colors <- NULL
	sentiments <- NULL
	if(lexicon[1] == 'nrc') {
		sentiments <- tidytext::get_sentiments("nrc")
	} else if(lexicon[1] == 'bing') {
		sentiments <- tidytext::get_sentiments("bing")
	} else if(lexicon[1] == 'afinn') {
		sentiments <- tidytext::get_sentiments("afinn")
		sentiments$sentiment <- as.character(sentiments$value)
	} else if(lexicon[1] == 'loughran') {
		sentiments <- tidytext::get_sentiments("loughran")

	} else {
		stop(paste0('Unknown lexicon: ', lexicon[1]))
	}
	colors <- lexicon_colors[[lexicon[1]]]
	colors_df <- data.frame(sentiment = names(colors),
							color = colors,
							row.names = NULL)

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
