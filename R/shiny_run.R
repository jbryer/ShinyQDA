#' Run the ShinyQDA application locally.
#'
#' @param df data.frame containing the essays
#' @param authenticate use [shinymanager:;secure_app()].
#' @param enable_admin enable admin mode for `shinymanager`.
#' @param keep_token Logical, keep the token used to authenticate in the URL,
#'        it allow to refresh the application in the browser, but careful the
#'        token can be shared between users.
#' @param ... other parmaeters passed to [shiny::runApp].
#' @export
#' @importFrom shiny runApp
shinyQDA <- function(qda_data,
					 authenticate = !interactive(),
					 enable_admin = TRUE,
					 keep_token = TRUE,
					 ...) {
	shiny_env <- new.env()

	if(!missing(qda_data)) {
		assign('qda_data', qda_data, shiny_env)
	}

	environment(shiny_ui) <- shiny_env
	environment(shiny_server) <- shiny_env

	app <- NULL
	if(authenticate) {
		app <- shiny::shinyApp(
			ui = shinymanager::secure_app(shiny_ui,
										  enable_admin = enable_admin,
										  keep_token = keep_token,
										  db = qda_data$db_file),
			server = shiny_server)
	} else {
		app <- shiny::shinyApp(
			ui = shiny_ui,
			server = shiny_server
		)

	}
	shiny::runApp(app, ...)
}
