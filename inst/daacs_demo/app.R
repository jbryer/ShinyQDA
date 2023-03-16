library(ShinyQDA)
library(shiny)
library(shinymanager)
library(shinyTree)

# Options for ShinyQDA app
qda_data_file <- 'qda.sqlite'
use_authentication <- FALSE
enable_admin <- TRUE

shinymanager::set_labels(
	language = "en",
	"Please authenticate" = "Login to ShinyQDA:",
	"Username:" = "Username:",
	"Password:" = "Password:"
)

if(interactive() &
   file.exists('../../R/shiny_ui.R') &
   file.exists('../../R/shiny_server.R')
) {
	# This block allows running the ShinyQDA app through the source files.
	# This means we don't have to rebuild the package for each run.
	message('Running in interactive mode locally...')

	devtools::load_all('../../')

	if(!file.exists(qda_data_file)) {
		source('setup.R', local = TRUE)
	}
} else {
	shiny_ui <- ShinyQDA::shiny_ui
	shiny_server <- ShinyQDA::shiny_server
}

shiny_env <- new.env()
assign('qda_data_file', qda_data_file, envir = shiny_env)
assign('username', 'Rater1', envir = shiny_env)
environment(shiny_ui) <- shiny_env
environment(shiny_server) <- shiny_env

if(use_authentication) {
	shiny::shinyApp(ui = shinymanager::secure_app(shiny_ui,
												  enable_admin = TRUE,
												  keep_token = TRUE,
												  db = qda_data_file),
					server = shiny_server)
} else {
	shiny::shinyApp(ui = shiny_ui,
					server = shiny_server)
}
