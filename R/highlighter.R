#' Highlights the codes within the text using HTML.
#'
#' @param text the text to highlight.
#' @param codings a data.frame with the codings.
#' @param codes a data.frame with the codes (and colors).
#' @param link if TRUE codes will link to a modal dialog to edit them.
#' @export
highlighter <- function(text, codings, codes, link = TRUE) {
	# The color will be determined by the first code
	first_code <- sapply(strsplit(codings$codes, ';'), FUN = function(x) { x[[1]]})
	codes <- codes[!duplicated(codes$code),]
	row.names(codes) <- codes$code
	colors <- codes[first_code,]$color
	if(any(is.na(colors))) {
		colors[is.na(colors)] <- '#FFFF00'
	}
	hover_text <- paste0(codings$coder, ': ', codings$codes)
	starts <- codings$start
	if(link) {
		names(starts) <- paste0(
			"<span ",
		   "onclick='Shiny.onInputChange(\"edit_coding\", \"", codings$coding_id, ';', as.integer(Sys.time()), "\");' ",
			"style='background-color: ", colors, "' ",
			"class='tooltip2' ",
			">"
			, "<span class='tooltiptext2'>", hover_text, "</span>"
		)
	} else {
		names(starts) <- paste0(
			"<span ",
			"style='background-color: ", colors, "' ",
			"class='tooltip2' ",
			">"
			, "<span class='tooltiptext2'>", hover_text, "</span>"
		)
	}
	ends <- codings$end
	names(ends) <- rep('</span>', nrow(codings))
	tags <- c(starts, ends)
	tags <- tags[order(tags, decreasing = TRUE)]
	for(i in seq_len(length(tags))) {
		left <- substr(text, 0, tags[i] - 1)
		right <- substr(text, tags[i], nchar(text))
		text <- paste0(left, names(tags)[i], right)
	}
	return(text)
}
