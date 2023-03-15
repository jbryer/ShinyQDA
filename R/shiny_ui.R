#' Shiny User Interface for QDA
#'
#' @param debug parater passed to [shinyjs::useShinyjs()]
#' @param request a Shiny request object.
#' @importFrom shiny icon tags navbarPage tabPanel fluidRow column uiOutput plotOutput tabsetPanel sidebarLayout sidebarPanel mainPanel actionButton htmlOutput verbatimTextOutput
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
#' @export
shiny_ui <- function(request, debug = TRUE) {
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
			shinyjs::useShinyjs(debug = debug),
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
							shiny::uiOutput('text_info'),
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
									   selected = 'bing',
									   label = 'Sentiment lexicon:'),
					shiny::fluidRow(
						shiny::column(
							8,
							shiny::htmlOutput('sentiment_text')
						),
						shiny::column(
							4,
							shiny::plotOutput('sentiment_text_plot', height = '400px')
						)
					)
				),
				shiny::tabPanel(
					'Tokenization',
					shiny::sidebarLayout(
						shiny::sidebarPanel(
							shiny::selectInput('tokenization_type',
											   label = 'Tokenization unit',
											   choices = c('words', 'characters', 'ngrams'),
											   selected = 'ngrams'),
							shiny::numericInput('tokenizer_min_tokens',
												label = 'Minimum tokens to include',
												value = 2, min = 1, step = 1),
							shiny::checkboxInput('tokenization_tolower',
												 label = 'Lowercase',
												 value = TRUE),
							shiny::conditionalPanel(
								condition = "input.tokenization_type == 'words'",
								shiny::checkboxInput('tokenizer_strip_punct',
													 label = 'Strip punctuation',
													 value = TRUE),
								shiny::checkboxInput('tokenizer_strip_numeric',
													 'Strip numeric',
													 value = TRUE)
							),
							shiny::conditionalPanel(
								condition = "input.tokenization_type == 'ngrams'",
								shiny::numericInput('tokenizer_ngrams_n',
													label = 'nGrams n = ',
													value = 2,
													min = 1, max = 5, step = 1)
							)
						),
						shiny::mainPanel(
							shiny::plotOutput('tokenization_plot', height = '600px')
						)
					)
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
				title = 'Rubric',
				icon = shiny::icon('table-list'),
				rubric_edit_ui('ShinyQDA')
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
				icon = shiny::icon('face-smile'),
				sentiment_ui('ShinyQDA')
			),
			shiny::tabPanel(
				'Co-Occurrence Plot',
				icon = shiny::icon('table-cells'),
				shiny::plotOutput('cooccurrence_plot', width = '100%', height = '700px')
			),
			shiny::tabPanel(
				'Topic Modeling',
				icon = shiny::icon('comments'),
				topic_modeling_ui('ShinyQDA')
			)
		),
		shiny::tabPanel(
			title = 'My Info',
			icon = shiny::icon('user'),
			shiny::verbatimTextOutput('auth_output')
		)
	)
}
