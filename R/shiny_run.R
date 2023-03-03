#' Run the ShinyQDA application locally.
#'
#' @param qda_data_file file name to save results. See [ShinyQDA::qda()] for more information.
#' @param authenticate use [shinymanager::secure_app()].
#' @param enable_admin enable admin mode for `shinymanager`.
#' @param keep_token Logical, keep the token used to authenticate in the URL,
#'        it allow to refresh the application in the browser, but careful the
#'        token can be shared between users.
#' @param users_passphrase passphrase to pass to `shinymanager`.
#' @param ... other parameters passed to [shiny::runApp()] and [shiny::shinyApp()].
#' @export
#' @importFrom shiny runApp shinyApp
#' @importFrom shinymanager secure_app
shinyQDA <- function(qda_data_file = 'ShinyQDA.sqlite',
					 authenticate = !interactive(),
					 enable_admin = TRUE,
					 keep_token = TRUE,
					 users_passphrase = 'ShinyQDA',
					 ...) {
	shiny_env <- new.env()
	assign('qda_data_file', qda_data_file, shiny_env)
	environment(shiny_ui) <- shiny_env
	environment(shiny_server) <- shiny_env

	qda_data <- qda(file = qda_data_file,
					users_passphrase = users_passphrase)

	app <- NULL
	if(authenticate) {
		app <- shiny::shinyApp(
			ui = shinymanager::secure_app(ui = shiny_ui,
										  enable_admin = enable_admin,
										  keep_token = keep_token,
										  db = qda_data_file),
			server = shiny_server,
			...)
	} else {
		app <- shiny::shinyApp(
			ui = shiny_ui,
			server = shiny_server,
			...
		)
	}
	shiny::runApp(app, ...)
}
