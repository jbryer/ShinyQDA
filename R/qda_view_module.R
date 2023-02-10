#' UI for the raw data view.
#'
#' This will display at tab group for each table in the [qda()] data object.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
qda_view_ui <- function(id, download_xlsx = TRUE, download_rda = TRUE) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::navlistPanel(
			widths = c(3, 9),
			header = shiny::tagList(
				shiny::downloadButton(ns('qda_download_xlsx'), label = 'Download Excel'),
				shiny::downloadButton(ns('qda_download_rda'), label = 'Download R Data')
			),
			shiny::tabPanel('Codes', DT::dataTableOutput(ns('qda_codes_table'))),
			shiny::tabPanel('Codings', DT::dataTableOutput(ns('qda_codings_table'))),
			shiny::tabPanel('Code Questions', DT::dataTableOutput(ns('qda_code_questions_table'))),
			shiny::tabPanel('Code Question Responses', DT::dataTableOutput(ns('qda_code_question_responses_table'))),
			shiny::tabPanel('Text Questions', DT::dataTableOutput(ns('qda_text_questions_table'))),
			shiny::tabPanel('Text Question Responses', DT::dataTableOutput(ns('qda_text_question_responses_table'))),
			shiny::tabPanel('Rubrics', DT::dataTableOutput(ns('qda_rubrics_table'))),
			shiny::tabPanel('Rubric Criteria', DT::dataTableOutput(ns('qda_rubric_criteria_table'))),
			shiny::tabPanel('Rubric Responses', DT::dataTableOutput(ns('qda_rubric_responses_table'))),
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
qda_view_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$qda_codes_table <- DT::renderDataTable({
				input$codebook_tree
				qda_data()$get_codes() |> qda_datatable()
			})

			output$qda_code_questions_table <- DT::renderDataTable({
				qda_data()$get_code_questions() |> qda_datatable()
			})

			output$qda_code_question_responses_table <- DT::renderDataTable({
				qda_data()$get_code_question_responses() |> qda_datatable()
			})

			output$qda_text_questions_table <- DT::renderDataTable({
				qda_data()$get_text_questions() |> qda_datatable()
			})

			output$qda_text_question_responses_table <- DT::renderDataTable({
				questions <- qda_data()$get_text_questions()
				for(i in seq_len(nrow(questions))) {
					stem <- questions[i,]$stem
					input[[paste0('text_', textutils::HTMLencode(stem))]]
				}
				qda_data()$get_text_question_responses() |> qda_datatable()
			})

			output$qda_rubrics_table <- DT::renderDataTable({
				qda_data()$get_rubrics() |> qda_datatable()
			})

			output$qda_rubric_criteria_table <- DT::renderDataTable({
				DBI::dbReadTable(qda_data()$db_conn, 'rubric_criteria') |> qda_datatable()
			})

			output$qda_rubric_responses_table <- DT::renderDataTable({
				DBI::dbReadTable(qda_data()$db_conn, 'rubric_responses') |> qda_datatable()
			})

			output$qda_codings_table <- DT::renderDataTable({
				qda_data()$get_codings() |> qda_datatable()
			})

			output$qda_assignments_table <- DT::renderDataTable({
				qda_data()$get_assignments() |> qda_datatable()
			})

			output$qda_log_table <- DT::renderDataTable({
				qda_data()$get_log() |> qda_datatable()
			})

			output$qda_download_xlsx <- shiny::downloadHandler(
				filename = function() {
					paste0(id, '-', Sys.Date(), '.xlsx')
				},
				content = function(file) {
					tables <- dbListTables(qda_data()$db_conn)
					tabs <- list()
					for(i in tables) {
						tabs[[i]] <- dbReadTable(qda_data()$db_conn, i)
					}
					writexl::write_xlsx(tabs, path = file)
				}
			)

			output$qda_download_rda <- shiny::downloadHandler(
				filename = function() {
					paste0(id, '-', Sys.Date(), '.rda')
				},
				content = function(file) {
					tables <- dbListTables(qda_data()$db_conn)
					tabs <- list()
					for(i in tables) {
						tabs[[i]] <- dbReadTable(qda_data()$db_conn, i)
					}
					save(list = names(tabs), file = file, envir = as.environment(tabs))
				}
			)
		}
	)
}
