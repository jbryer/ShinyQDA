#' Run the ShinyQDA application locally.
#'
#' @param df data.frame containing the essays
#' @param ... other parmaeters passed to [shiny::runApp].
#' @export
#' @importFrom colourpicker colourInput
#' @import shiny
shinyQDA <- function(qda_data, ...) {
	shiny_env <- new.env()

	if(!missing(qda_data)) {
		assign('qda_data', qda_data, shiny_env)
	}
	# if(!missing(id_col)) {
	# 	assign('id_col', id_col, shiny_env)
	# }
	# if(!missing(essay_col)) {
	# 	assign('essay_col', essay_col, shiny_env)
	# }

	environment(shiny_ui) <- shiny_env
	environment(shiny_server) <- shiny_env

	app <- shiny::shinyApp(
		ui = shiny_ui,
		server = shiny_server
	)

	shiny::runApp(app, ...)
}
