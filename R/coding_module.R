#' UI for text coding.
#'
#' @export
coding_ui <- function(id) {
	ns <- NS(id)
	DT::dataTableOutput(ns('text_table'))
}



#' Server text coding.
#'
#' @export
coding_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
		}
	)
}
