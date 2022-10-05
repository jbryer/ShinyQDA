#' Shiny User Interface for QDA
#'
#' @export
shiny_ui <- function(title = 'ShinyQDA') {
	shiny::navbarPage(
		title = 'ShinyQDA',
		# tabPanel(
		# 	'Codebook'
		# ),
		shiny::tabPanel(
			'Text Coding',
			marker::useMarker(),
			shinyjs::useShinyjs(),
			shiny::tags$head(tags$style(textOutput('get_color_css'))),
			# javascript code to send data to shiny server
			shiny::tags$script('
                function getSelectionText() {
                    var text = "";
                    if (window.getSelection) {
                        text = window.getSelection().toString();
                    } else if (document.selection) {
                        text = document.selection.createRange().text;
                    }
                    return text;
                }
		        document.onmouseup = document.onkeyup = document.onselectionchange = function() {
        		    var selection = getSelectionText();
            		Shiny.onInputChange("text_selection", selection);
        		};
			'),
			shiny::fluidRow(
				shiny::column(12, shiny::uiOutput('essay_selection'))
			),
			shiny::tabsetPanel(
				shiny::tabPanel(
					'Code Editor',
					shiny::sidebarLayout(
						shiny::sidebarPanel(
							shiny::actionButton('add_tag_button', 'Add Code'),
							shiny::uiOutput('text_codes_ui')
						),
						shiny::mainPanel(
							shiny::div(id="text-to-mark", shiny::htmlOutput('text_output'))
						)
					)
				),
				shiny::tabPanel(
					'View All Codes',
					DT::dataTableOutput('coding_table')
				)
			)
		),
		shiny::tabPanel(
			'Table View',
			DT::dataTableOutput('text_table')
		)
	)
}
