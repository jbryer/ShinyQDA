#' Shiny User Interface for QDA
#'
#' @param title title displayed in the top left of the Shiny app.
#' @importFrom shiny icon tags navbarPage tabPanel fluidRow column uiOutput plotOutput tabsetPanel sidebarLayout sidebarPanel mainPanel actionButton htmlOutput verbatimTextOutput
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
#' @export
shiny_ui <- function(request) {
	shiny::navbarPage(
		# TODO: Would be nice if these were parameters. Not allowed on shiny_ui
		# function definition, would need to be passed through the environment
		title = 'ShinyQDA',
		windowTitle = 'ShinyQDA: Qualitative Data Analysis',
		# position = 'fixed-top',
		inverse = FALSE,
		collapsible = TRUE,
		fluid = TRUE,
		theme = NULL,
		shiny::tabPanel(
			title = 'Data',
			icon = shiny::icon('table'),
			data_view_ui('ShinyQDA')
		),
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
				shiny::column(9, shiny::uiOutput('essay_selection')),
				shiny::column(3, shiny::selectInput(
					inputId = 'essay_selection_subset',
					label = 'Filter Text',
					choices = c('All', 'Not coded', 'Not coded by me', 'Coded', 'Coded by me')))
			),
			shiny::tabsetPanel(
				shiny::tabPanel(
					'Code Editor',
					shiny::sidebarLayout(
						shiny::sidebarPanel(
							# style = "height: 90vh; overflow-y: auto;",
							width = 4,
							# shiny::p('Selected id: ', shiny::textOutput('selected_id')),
							shiny::actionButton('add_tag_button', 'Add Code'),
							shiny::hr(),
							shiny::uiOutput('text_coders_ui'),
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
					'Rubric',
					shiny::tags$style(shiny::HTML("div.highlight { background-color: #fff7bc; color: blue; width: 100%; height: 100% }")),
					shiny::htmlOutput('rubric')
				),
				shiny::tabPanel(
					'View All Codes',
					DT::dataTableOutput('coding_table')
				),
				shiny::tabPanel(
					'Sentiment',
					shiny::selectInput('sentiment_lexicon',
									   choices = c('Bing binary sentiment' = 'bing',
									   			   'NRC Word-Emotion Association' = 'nrc',
									   			   'Loughran-McDonald Sentiment' = 'loughran',
									   			   'AFINN-111 dataset' = 'afinn'),
									   label = 'Sentiment lexicon:'),
					shiny::htmlOutput('sentiment_text')
				)
			)
		),
		shiny::navbarMenu(
			title = 'Setup',
			icon = shiny::icon('gears'),
			shiny::tabPanel(
				title = 'Codebook',
				icon = shiny::icon('book'),
				codebook_ui('ShinyQDA')
			),
			shiny::tabPanel(
				title = 'Questions',
				icon = shiny::icon('clipboard-question'),
				questions_ui('ShinyQDA')
			),
			shiny::tabPanel(
				title = 'Raw Data',
				icon = shiny::icon('database'),
				# uiOutput('codes_ui')
				shiny::p('These tables represents the raw data stored in the ShinyQDA file (using SQLite).'),
				qda_view_ui('ShinyQDA')
			)
		),
		# shiny::tabPanel(
		# 	'Coders',
		# 	DT::dataTableOutput('coders_table')
		# ),
		shiny::navbarMenu(
			title = 'Analysis',
			icon = shiny::icon('chart-simple'),
			shiny::tabPanel(
				'Descriptives',
				icon = shiny::icon('chart-bar'),
				descriptives_ui('ShinyQDA')
			),
			shiny::tabPanel(
				'Sentiment',
				icon = shiny::icon('face-smile')
			),
			shiny::tabPanel(
				'Topic Modeling',
				icon = shiny::icon('comments')
			)
		),
		shiny::tabPanel(
			title = 'My Info',
			icon = shiny::icon('user'),
			shiny::verbatimTextOutput('auth_output')
		)
	)
}
