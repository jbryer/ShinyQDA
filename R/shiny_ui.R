#' Shiny User Interface for QDA
#'
#' @importFrom shiny icon tags navbarPage tabPanel fluidRow column uiOutput plotOutput tabsetPanel sidebarLayout sidebarPanel mainPanel actionButton htmlOutput verbatimTextOutput
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
#' @export
shiny_ui <- function(title = 'ShinyQDA') {
	shiny::navbarPage(
		title = 'ShinyQDA',
		windowTitle = 'ShinyQDA: Qualitative Data Analysis',
		inverse = FALSE,
		collapsible = TRUE,
		shiny::tabPanel(
			title = 'Coding',
			icon = shiny::icon('pen-to-square'),
			shinyjs::useShinyjs(),
			shiny::tags$style(
				type = 'text/css',
				'.modal-dialog { width: 90% !important; }'
			),
			# javascript code to send data to shiny server
			# https://stackoverflow.com/questions/42274461/can-shiny-recognise-text-selection-with-mouse-highlighted-text
			shiny::tags$script('
                function getSelectionText() {
                    var text = "";
                    if (window.getSelection) {
                        text = window.getSelection().toString();
                        /* text = window.getSelection().getRangeAt().toString(); */
                    } else if (document.selection) {
                        text = document.selection.createRange().text;
                        /* text = document.selection.getRangeAt().toString(); */
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
							width = 4,
							# shiny::p('Selected id: ', shiny::textOutput('selected_id')),
							shiny::actionButton('add_tag_button', 'Add Code'),
							# TODO: enable selected highlighting
							# shiny::uiOutput('text_codes_ui'),
							shiny::hr(),
							shiny::uiOutput('questions_ui')
						),
						shiny::mainPanel(
							width = 8,
							shiny::htmlOutput('text_output')
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
			title = 'Data',
			icon = shiny::icon('table'),
			data_view_ui('ShinyQDA')
		),
		shiny::tabPanel(
			title = 'Codebook',
			icon = shiny::icon('book'),
			codebook_ui('ShinyQDA')
		),
		# shiny::tabPanel(
		# 	'Coders',
		# 	DT::dataTableOutput('coders_table')
		# ),
		shiny::tabPanel(
			title = 'Raw Data',
			icon = shiny::icon('database'),
			# uiOutput('codes_ui')
			shiny::p('These table represents the raw data stored in the ShinyQDA file (using SQLite).'),
			shiny::tabsetPanel(
				shiny::tabPanel('Codes', DT::dataTableOutput('codes_table')),
				shiny::tabPanel('Codings', DT::dataTableOutput('codings_table')),
				shiny::tabPanel('Code Questions', DT::dataTableOutput('code_questions_table')),
				shiny::tabPanel('Code Question Responses', DT::dataTableOutput('code_question_responses_table')),
				shiny::tabPanel('Text Questions', DT::dataTableOutput('text_questions_table')),
				shiny::tabPanel('Text Question Responses', DT::dataTableOutput('text_question_responses_table')),
				shiny::tabPanel('Assignments', DT::dataTableOutput('assignments_table'))
			)

		),
		shiny::navbarMenu(
			title = 'Analysis',
			icon = shiny::icon('chart-simple'),
			shiny::tabPanel(
				'Descriptives'
			),
			shiny::tabPanel(
				'Sentiment'
			),
			shiny::tabPanel(
				'Topic Modeling'
			)
		),
		shiny::tabPanel(
			title = 'My Info',
			icon = shiny::icon('user'),
			shiny::verbatimTextOutput('auth_output')
		)
	)
}
