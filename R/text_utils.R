#' Wrap label text.
#'
#' Adapted from https://github.com/hadley/ggplot2/wiki/labeller
#'
#' @param value vector (converted using \code{\link{as.character}}) to be wrapped.
#' @param width the maximum width of each line in characters.
#' @export
text_wrap_mod <- function(value, width = 25) {
	sapply(strwrap(as.character(value), width = width, simplify = FALSE),
		   paste, collapse="\n")
}

#' Returns the first words that fit within the given number of characeters.
#'
#' @param value character to truncate
#' @param width maximum number of characters the resulting string should be.
#' @export
text_truncate <- function(value, width = 50) {
	sapply(
		value,
		FUN = function(x) {
			txt <- strwrap(as.character(x), width = width, simplify = FALSE)[[1]]
			txt <- txt[txt != '']
			txt[1]
		}
	)
}
