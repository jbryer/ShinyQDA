#' UI for the raw data view.
#'
#' This will display at tab group for each table in the [qda()] data object.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
questions_ui <- function(id) {
	ns <- NS(id)
	tagList(
		shiny::tabsetPanel(
			shiny::tabPanel(
				title = 'Text Questions',
				shiny::br(),
				shiny::actionButton(ns('new_text_question'), 'New Text Question'),
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
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @importFrom shinyWidgets ask_confirmation
#' @export
questions_server <- function(id, qda_data, page_length = 20) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			selected_code_stem <- shiny::reactiveVal('')
			selected_text_stem <- shiny::reactiveVal('')

			##### Tables
			output$text_questions_table <- DT::renderDataTable({
				input$update_text_question
				input$confirm_text_question_delete
				qda_data()$get_text_questions() |> qda_datatable()
			})

			output$code_questions_table <- DT::renderDataTable({
				input$update_code_question
				input$confirm_code_question_delete
				qda_data()$get_code_questions() |> qda_datatable()
			})

			##### UI output
			# Text questions
			output$text_question_ui <- renderUI({
				# TODO: this can be refactored into a utlity function
				ns <- session$ns
				text_questions <- qda_data()$get_text_questions()
				df <- data.frame(
					stem = '',
					type = 'text',
					order = max(text_questions$order, na.rm = TRUE) + 1,
					options = ''
				)
				if(!is.null(input$text_questions_table_rows_selected)) {
					df <- text_questions[input$text_questions_table_rows_selected, , drop = FALSE]
				}
				shiny::tagList(
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
					shiny::selectizeInput(
						inputId = ns('text_question_options'),
						label = 'Options',
						choices = strsplit(df[1,]$options, ';')[[1]],
						selected = strsplit(df[1,]$options, ';')[[1]],
						multiple = TRUE,
						options = list(create = TRUE),
						width = '100%'
					)
				)
			})

			# Code questions
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

			##### Add new question (button observe)
			# Text questions
			shiny::observeEvent(input$new_text_question, {
				ns <- session$ns
				selected_text_stem('')
				shiny::showModal(
					shiny::modalDialog(
						shiny::uiOutput(ns('text_question_ui')),
						title = 'New Text Question',
						size = 'l',
						easyClose = FALSE,
						footer = shiny::tagList(
							shiny::actionButton(ns('cancel_modal'), 'Cancel'),
							shiny::actionButton(ns('update_text_question'), 'Save')
						)
					)
				)
			})

			# Code questions
			shiny::observeEvent(input$new_code_question, {
				ns <- session$ns
				selected_code_stem('')
				shiny::showModal(
					shiny::modalDialog(
						shiny::uiOutput(ns('code_question_ui')),
						title = 'New Code Question',
						size = 'l',
						easyClose = FALSE,
						footer = shiny::tagList(
							shiny::actionButton(ns('cancel_modal'), 'Cancel'),
							shiny::actionButton(ns('update_code_question'), 'Save')
						)
					)
				)
			})

			##### Delete question
			# Text questions
			shiny::observeEvent(input$delete_text_question, {
				ns <- session$ns
				shinyWidgets::ask_confirmation(
					inputId = ns('confirm_text_question_delete'),
					title = 'Confirm Deletion',
					text = 'Are you sure you wish to delete this question?',
					btn_labels = c('No', 'Yes'))
			})

			shiny::observeEvent(input$confirm_text_question_delete, {
				if(input$confirm_text_question_delete) {
					qda_data()$delete_text_question(selected_text_stem())
				}
				selected_text_stem('')
				shiny::removeModal()
			})

			# Code questions
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

			##### Row selections
			# Text Questions
			shiny::observe({
				ns <- session$ns
				if(!is.null(input$text_questions_table_rows_selected)) {
					df <- qda_data()$get_text_questions()
					df <- df[input$text_questions_table_rows_selected, , drop = FALSE]
					selected_text_stem(df[1,]$stem)
					shiny::showModal(
						shiny::modalDialog(
							shiny::p("Note: Editing text questions will not change the stems
							  or responses already entered. Changes will be reflected in any new completions."),
							shiny::uiOutput(ns('text_question_ui')),
							title = 'Edit Text Question',
							size = 'l',
							easyClose = FALSE,
							footer = shiny::tagList(
								shiny::actionButton(ns('cancel_modal'), 'Cancel'),
								shiny::actionButton(ns('delete_text_question'), 'Delete'),
								shiny::actionButton(ns('update_text_question'), 'Save')
							)
						)
					)
				}
			})

			# Code questions
			shiny::observe({
				ns <- session$ns
				if(!is.null(input$code_questions_table_rows_selected)) {
					df <- qda_data()$get_code_questions()
					df <- df[input$code_questions_table_rows_selected, , drop = FALSE]
					selected_code_stem(df[1,]$stem)
					shiny::showModal(
						shiny::modalDialog(
							shiny::p("Note: Editing code questions will not change the stems
							  or responses already entered. Changes will be reflected in any new codings completed."),
							shiny::uiOutput(ns('code_question_ui')),
							title = 'Edit Code Question',
							size = 'l',
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

			##### Update question
			# Text questions
			observeEvent(input$update_text_question, {
				qda_data()$delete_text_question(selected_text_stem())
				qda_data()$add_text_question(stem = input$text_question_stem,
											 type = input$text_question_type,
											 order = input$text_question_order,
											 options = input$text_question_options)
				selected_text_stem('')
				shiny::removeModal()
			})

			# Code questions
			observeEvent(input$update_code_question, {
				qda_data()$delete_code_question(selected_code_stem())
				qda_data()$add_code_question(stem = input$code_question_stem,
										   type = input$code_question_type,
										   order = input$code_question_order,
										   options = input$code_question_options)
				selected_code_stem('')
				shiny::removeModal()
			})
		}
	)
}
