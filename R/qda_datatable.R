page_length <- 20 # TODO: make an package option

#' Wrapper to `datatable` function.
#'
#' This is a wrapper to the [DT::datatable()] function. The primary purpose is
#' to centralize the options to `datatable` so that all tables in the ShinyQDA
#' app are consistent.
#'
#' @param df a data.frame.
#' @export
qda_datatable <- function(df) {
	df |>
		DT::datatable(
			rownames = FALSE,
			filter = 'top',
			options = list(
				pageLength = page_length
			),
			selection = 'single'
		)
}
