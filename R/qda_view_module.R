#' UI for the raw data view.
#'
#' This will display at tab group for each table in the [qda()] data object.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
qda_view_ui <- function(id) {
	ns <- NS(id)
	tagList(
		shiny::tabsetPanel(
			shiny::tabPanel('Codes', DT::dataTableOutput(ns('qda_codes_table'))),
			shiny::tabPanel('Codings', DT::dataTableOutput(ns('qda_codings_table'))),
			shiny::tabPanel('Code Questions', DT::dataTableOutput(ns('qda_code_questions_table'))),
			shiny::tabPanel('Code Question Responses', DT::dataTableOutput(ns('qda_code_question_responses_table'))),
			shiny::tabPanel('Text Questions', DT::dataTableOutput(ns('qda_text_questions_table'))),
			shiny::tabPanel('Text Question Responses', DT::dataTableOutput(ns('qda_text_question_responses_table'))),
			shiny::tabPanel('Assignments', DT::dataTableOutput(ns('qda_assignments_table'))),
			shiny::tabPanel('Log', DT::dataTableOutput(ns('qda_log_table')))
		)
	)
}

#' Server the raw data view.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
qda_view_server <- function(id, qda_data, page_length = 20) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$qda_codes_table <- DT::renderDataTable({
				# refresh()
				input$codebook_tree
				qda_data()$get_codes() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			output$qda_code_questions_table <- DT::renderDataTable({
				# refresh()
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

			output$qda_code_question_responses_table <- DT::renderDataTable({
				# refresh()
				qda_data()$get_code_question_responses() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			output$qda_text_questions_table <- DT::renderDataTable({
				# refresh()
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

			output$qda_text_question_responses_table <- DT::renderDataTable({
				# refresh()
				questions <- qda_data()$get_text_questions()
				for(i in seq_len(nrow(questions))) {
					stem <- questions[i,]$stem
					input[[paste0('text_', textutils::HTMLencode(stem))]]
				}

				qda_data()$get_text_question_responses() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			output$qda_codings_table <- DT::renderDataTable({
				# refresh()
				qda_data()$get_codings() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			output$qda_assignments_table <- DT::renderDataTable({
				# refresh()
				qda_data()$get_assignments() |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})

			output$qda_log_table <- DT::renderDataTable({
				qda_data()$get_log() |>
					dplyr::arrange(desc(timestamp)) |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = page_length
						),
						selection = 'single'
					)
			})
		}
	)
}
