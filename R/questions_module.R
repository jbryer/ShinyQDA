#' UI for the raw data view.
#'
#' This will display at tab group for each table in the [qda()] data object.
#'
#' @export
questions_ui <- function(id) {
	ns <- NS(id)
	tagList(
		shiny::tabsetPanel(
			shiny::tabPanel(
				title = 'Text Questions',
				shiny::hr(),
				DT::dataTableOutput(ns('text_questions_table'))
			),
			shiny::tabPanel(
				title = 'Code Questions',
				shiny::br(),
				shiny::actionButton(ns('new_code_question'), 'New Code Question'),
				shiny::hr(),
				DT::dataTableOutput(ns('code_questions_table'))
			)
		)
	)
}

#' Server the raw data view.
#'
#' @importFrom shinyWidgets ask_confirmation
#' @export
questions_server <- function(id, qda_data, page_length = 20) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$code_questions_table <- DT::renderDataTable({
				input$update_code_question
				input$confirm_code_question_delete
				qda_data()$get_code_questions() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			selected_code_stem <- shiny::reactiveVal('')

			output$code_question_ui <- renderUI({
				ns <- session$ns
				code_questions <- qda_data()$get_code_questions()
				df <- data.frame(
					stem = '',
					type = 'text',
					order = max(code_questions$order, na.rm = TRUE) + 1,
					options = ''
				)
				if(!is.null(input$code_questions_table_rows_selected)) {
					df <- code_questions[input$code_questions_table_rows_selected, , drop = FALSE]
				}
				shiny::tagList(
					shiny::textInput(
						inputId = ns('code_question_stem'),
						label = 'Stem',
						value = df[1,]$stem,
						width = '100%'),
					shiny::selectInput(
						inputId = ns('code_question_type'),
						label = 'Type',
						choices = c('text', 'radio', 'checkbox'),
						selected = df[1,]$type
					),
					shiny::numericInput(
						inputId = ns('code_question_order'),
						label = 'Order',
						value = df[1,]$order
					),
					shiny::selectizeInput(
						inputId = ns('code_question_options'),
						label = 'Options',
						choices = strsplit(df[1,]$options, ';')[[1]],
						selected = strsplit(df[1,]$options, ';')[[1]],
						multiple = TRUE,
						options = list(create = TRUE),
						width = '100%'
					)
				)
			})

			shiny::observeEvent(input$new_code_question, {
				ns <- session$ns
				selected_code_stem('')
				shiny::showModal(
					shiny::modalDialog(
						shiny::uiOutput(ns('code_question_ui')),
						title = 'New Code Question',
						easyClose = FALSE,
						footer = shiny::tagList(
							shiny::actionButton(ns('cancel_modal'), 'Cancel'),
							shiny::actionButton(ns('update_code_question'), 'Save')
						)
					)
				)
			})

			shiny::observeEvent(input$delete_code_question, {
				ns <- session$ns
				shinyWidgets::ask_confirmation(
					inputId = ns('confirm_code_question_delete'),
					title = 'Confirm Deletion',
					text = 'Are you sure you wish to delete this question?',
					btn_labels = c('No', 'Yes'))
			})

			shiny::observeEvent(input$confirm_code_question_delete, {
				if(input$confirm_code_question_delete) {
					qda_data()$delete_code_question(selected_code_stem())
				}
				selected_code_stem('')
				shiny::removeModal()
			})

			shiny::observe({
				ns <- session$ns
				if(!is.null(input$code_questions_table_rows_selected)) {
					df <- qda_data()$get_code_questions()
					df <- df[input$code_questions_table_rows_selected, , drop = FALSE]
					selected_code_stem(df[1,]$stem)
					shiny::showModal(
						shiny::modalDialog(
							p("Note: Editing code questions will not change the stems
							  or responses already entered. Changes will be reflected in any new codings completed."),
							shiny::uiOutput(ns('code_question_ui')),
							title = 'Edit Code Question',
							easyClose = FALSE,
							footer = shiny::tagList(
								shiny::actionButton(ns('cancel_modal'), 'Cancel'),
								shiny::actionButton(ns('delete_code_question'), 'Delete'),
								shiny::actionButton(ns('update_code_question'), 'Save')
							)
						)
					)
				}
			})

			observeEvent(input$update_code_question, {
				qda_data()$delete_code_question(selected_code_stem())
				qda_data()$add_code_question(stem = input$code_question_stem,
										   type = input$code_question_type,
										   order = input$code_question_order,
										   options = input$code_question_options)
				selected_code_stem('')
				shiny::removeModal()
			})

			output$text_questions_table <- DT::renderDataTable({
				qda_data()$get_text_questions() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			shiny::observe({
				ns <- session$ns
				if(!is.null(input$text_questions_table_rows_selected)) {
					df <- qda_data()$get_text_questions()
					df <- df[input$text_questions_table_rows_selected, , drop = FALSE]
					shiny::showModal(
						shiny::modalDialog(
							p("Note: Editing code questions will not change the stems
							  or responses already entered. Changes will be reflected in any new codings completed."),
							shiny::textInput(
								inputId = ns('text_question_stem'),
								label = 'Stem',
								value = df[1,]$stem,
								width = '100%'),
							shiny::selectInput(
								inputId = ns('text_question_type'),
								label = 'Type',
								choices = c('text', 'radio', 'checkbox'),
								selected = df[1,]$type
							),
							shiny::numericInput(
								inputId = ns('text_question_order'),
								label = 'Order',
								value = df[1,]$order
							),
							title = 'Edit Text Question',
							easyClose = FALSE,
							footer = shiny::tagList(
								shiny::actionButton('cancel_modal', 'Cancel'),
								shiny::actionButton(ns('update_text_question'), 'Save')
							)
						)
					)
				}
			})

			observeEvent(input$update_text_question, {
				df <- qda_data()$get_text_questions()
				df <- df[input$text_questions_table_rows_selected, , drop = FALSE]
				old_stem <- df[1,]$stem

				shiny::removeModal()
			})


		}
	)
}
