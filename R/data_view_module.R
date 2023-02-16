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
		shiny::fluidRow(
			shiny::column(
				width = 8,
				shiny::uiOutput(ns('column_selection'))
			),
			shiny::column(
				width = 2,
				shiny::br(),
				shiny::downloadButton(ns('download_merged_excel'),
									  'Download Excel')
			),
			shiny::column(
				width = 2,
				shiny::br(),
				shiny::downloadButton(ns('download_merged_csv'),
									  'Download CSV')
			)
		),
		DT::dataTableOutput(ns('text_table'))
	)
}

# Columns that should be added/removed all together. The value is the prefix from qda_merge
grouped_colunns <- c('Codings' = 'code_',
					 'NRC Sentiment' = 'nrc_',
					 'Bing Sentiment' = 'bing_',
					 'Loughran Sentiment' = 'loughran_',
					 'AFINN Sentiment' = 'afinn_')

#' Server for the data view.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
data_view_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			get_text_data <- reactive({
				return(qda_merge(qda_data(),
								 include_sentiments = TRUE,
								 sentiment_dir = '.'))
			})

			get_text_data_view <- reactive({
				df <- get_text_data()
				cols_to_view <- input$columns_to_view
				for(i in seq_len(length(grouped_colunns))) {
					if(names(grouped_colunns)[i] %in% cols_to_view) {
						cols_to_view <- cols_to_view[-grep(names(grouped_colunns)[i], cols_to_view)]
						cols_to_view <- c(cols_to_view, names(df)[grep(grouped_colunns[i], names(df))])
					}
				}
				return(df[,cols_to_view])
			})

			# Select columns to view
			output$column_selection <- shiny::renderUI({
				ns <- session$ns
				df <- get_text_data()
				selected_cols <- c('qda_id', 'qda_text', 'coder', 'n_codes', 'n_highlights')
				selected_cols <- selected_cols[selected_cols %in% names(df)]

				all_cols <- names(df)
				for(i in seq_len(length(grouped_colunns))) {
					cols <- grep(grouped_colunns[i], all_cols)
					if(length(i) > 0) {
						all_cols <- all_cols[-cols]
					}
				}

				all_cols <- c(all_cols, names(grouped_colunns))

				shiny::selectizeInput(
					inputId = ns('columns_to_view'),
					label = 'Columns to include:',
					multiple = TRUE,
					choices = all_cols,
					selected = selected_cols,
					width = '100%'
				)
			})

			# Table view of the data
			output$text_table <- DT::renderDataTable({
				df <- get_text_data_view()
				if('qda_text' %in% names(df)) {
					df$qda_text <- ShinyQDA::text_truncate(df$qda_text, width = 100)
				}

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
							size = 'l',
							easyClose = TRUE)
					)
				}
			})

			output$download_merged_excel <- shiny::downloadHandler(
				filename = function() {
					paste0(id, '-', Sys.Date(), '.xlsx')
				},
				content = function(file) {
					writexl::write_xlsx(get_text_data_view(), path = file)
				}
			)

			output$download_merged_csv <- shiny::downloadHandler(
				filename = function() {
					paste0(id, '-', Sys.Date(), '.csv')
				},
				content = function(file) {
					write.csv(get_text_data_view(), path = file)
				}
			)

		}
	)
}
