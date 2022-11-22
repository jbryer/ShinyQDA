#' Run the ShinyQDA application locally.
#'
#' @param df data.frame containing the essays
#' @param ... other parmaeters passed to [shiny::runApp].
#' @export
#' @importFrom colourpicker colourInput
#' @import shiny
#' @import shinyauthr
shinyQDA <- function(qda_data, ...) {
	shiny_env <- new.env()

	if(!missing(qda_data)) {
		assign('qda_data', qda_data, shiny_env)
	}

	environment(shiny_ui) <- shiny_env
	environment(shiny_server) <- shiny_env

	app <- shiny::shinyApp(
		ui = shiny_ui,
		server = shiny_server
	)

	shiny::runApp(app, ...)
}
