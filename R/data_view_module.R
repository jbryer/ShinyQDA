#' UI for the data view.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
data_view_ui <- function(id) {
	ns <- NS(id)
	tagList(
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
		DT::dataTableOutput(ns('text_table'))
	)
}

#' Server for the data view.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [@ShinyQDA::qda()].
#' @export
data_view_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			get_text_data <- reactive({
				df <- qda_data()$get_text()
				if(nrow(df) == 0) {
					return(data.frame
				}
				codes_table <- get_coding_table(qda_data())
				codes_table <- cbind(codes_table[,1:2], n_codes = apply(codes_table[,3:ncol(codes_table)], 1, sum))

				codings <- qda_data()$get_codings()
				highlights_table <- table(codings$qda_id) |>
					as.data.frame() |>
					dplyr::rename(n_highlights = Freq,
								  qda_id = Var1)

				tab <- merge(df, codes_table, by = c('qda_id'), all.x = TRUE)
				tab <- merge(tab, highlights_table, by = 'qda_id', all.x = TRUE)
				return(tab)
			})

			# Table view of the data
			output$text_table <- DT::renderDataTable({
				df <- get_text_data()
				df$qda_text <- ShinyQDA::text_truncate(df$qda_text)
				DT::datatable(
					df,
					rownames = FALSE,
					filter = 'top',
					options = list(
						# https://stackoverflow.com/questions/50922686/freeze-header-and-footer-in-datatable-shiny
						# scrollX = TRUE,
						# scrollY = "400px"
						pageLength = 20
					),
					selection = 'single'
				)
			})

			output$text_details <- shiny::renderUI({
				ns <- session$ns
				df <- get_text_data()
				txt_data <- df[input$text_table_rows_selected, , drop = FALSE]
				txt <- txt_data$qda_text
				id <- txt_data$qda_id
				codings <- qda_data()$get_codings(id = id)
				if(nrow(codings) > 0) {
					txt <- highlighter(txt, codings, qda_data()$get_codes(), link = FALSE)
				}
				txt <- gsub('\\n', '<p/>', txt)

				tabsetPanel(
					tabPanel(
						title = 'Full Text',
						shiny::HTML(txt)
					),
					tabPanel(
						title = 'Text Questions',
						DT::dataTableOutput(ns('text_details_text_questions'))
					),
					tabPanel(
						title = 'Code Questions',
						DT::dataTableOutput(ns('text_details_code_questions'))
					)
				)
			})

			output$text_details_text_questions <- DT::renderDataTable({
				df <- get_text_data()
				txt_data <- df[input$text_table_rows_selected, , drop = FALSE]
				id <- txt_data$qda_id
				text_questions <- qda_data()$get_text_question_responses(id)
				if(nrow(text_questions) == 0) {
					return(NULL)
				}
				text_questions |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = 10
						),
						selection = 'single'
					)
			})

			output$text_details_code_questions <- DT::renderDataTable({
				df <- get_text_data()
				txt_data <- df[input$text_table_rows_selected, , drop = FALSE]
				id <- txt_data$qda_id
				code_questions <- qda_data()$get_code_question_responses(id = id)
				if(nrow(code_questions) == 0) {
					return(NULL)
				}
				code_questions |>
					DT::datatable(
						rownames = FALSE,
						filter = 'top',
						options = list(
							pageLength = 10
						),
						selection = 'single'
					)
			})

			# Reactive function to determine if a row is selected
			shiny::observe({
				ns <- session$ns
				if(!is.null(input$text_table_rows_selected)) {
					shiny::showModal(
						shiny::modalDialog(
							shiny::uiOutput(ns('text_details')),
							title = 'Details',
							easyClose = TRUE)
					)
				}
			})
		}
	)
}
