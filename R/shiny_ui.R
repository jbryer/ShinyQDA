#' Shiny User Interface for QDA
#'
#' @importFrom shiny tags navbarPage tabPanel fluidRow column uiOutput plotOutput tabsetPanel sidebarLayout sidebarPanel mainPanel actionButton htmlOutput verbatimTextOutput
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyTree shinyTree
#' @export
shiny_ui <- function(title = 'ShinyQDA') {
	shiny::navbarPage(
		title = 'ShinyQDA',
		shiny::tabPanel(
			'Text Coding',
			shinyjs::useShinyjs(),
			shiny::tags$head(tags$style("
				.tooltip2 {
				  position: relative;
				  display: inline-block;
				  border-bottom: 1px dotted black;
				}

				.tooltip2 .tooltiptext2 {
				  visibility: hidden;
				  width: 120px;
				  background-color: black;
				  color: #fff;
				  text-align: center;
				  border-radius: 6px;
				  padding: 5px 0;
				  position: absolute;
				  z-index: 1;
				  bottom: 150%;
				  left: 50%;
				  margin-left: -60px;
				}

				.tooltip2 .tooltiptext2::after {
				  content: '';
				  position: absolute;
				  top: 100%;
				  left: 50%;
				  margin-left: -5px;
				  border-width: 5px;
				  border-style: solid;
				  border-color: black transparent transparent transparent;
				}

				.tooltip2:hover .tooltiptext2 {
				  visibility: visible;
				}
			")),
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
							shiny::actionButton('add_tag_button', 'Add Code'),
							shiny::uiOutput('text_codes_ui'),
							hr(),
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
			'Codebook',
			shiny::sidebarLayout(
				shiny::sidebarPanel(
					shinyTree::shinyTree("tree",
										 theme = "proton",
										 multiple = FALSE,
										 animation = FALSE,
										 dragAndDrop = TRUE,
										 sort = FALSE,
										 wholerow = TRUE,
										 unique = TRUE,
										 contextmenu = TRUE)
				),
				shiny::mainPanel(
					shiny::verbatimTextOutput('codebook_output')
				)
			)
		),
		# shiny::tabPanel(
		# 	'Coders',
		# 	DT::dataTableOutput('coders_table')
		# ),
		shiny::tabPanel(
			'Text Data',
			DT::dataTableOutput('text_table')
		),
		shiny::tabPanel(
			'Data',
			# uiOutput('codes_ui')
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
		shiny::tabPanel(
			'My Info',
			shiny::verbatimTextOutput('auth_output')
		)
	)
}
