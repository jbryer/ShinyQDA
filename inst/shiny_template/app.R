library(ShinyQDA)
library(shiny)
library(shinymanager)

# Options for ShinyQDA app
qda_data_file <- 'qda.sqlite'
use_authentication <- TRUE
enable_admin <- TRUE

# Change settings for shinymanager
shinymanager::set_labels(
	language = "en",
	"Please authenticate" = "Login to ShinyQDA:",
	"Username:" = "Username:",
	"Password:" = "Password:"
)

# Run the app
ShinyQDA::shinyQDA(qda_data_file,
				   authenticate = use_authentication,
				   enable_admin = enable_admin)
