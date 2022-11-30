#' UI for the data view.
#'
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
#' @export
data_view_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			get_text_data <- reactive({
				# qda_merge(qda_data)
				df <- qda_data()$get_text()
				codes_table <- get_coding_table(qda_data())
				codes_table <- cbind(codes_table[,1:2], n_codes = apply(codes_table[,3:ncol(codes_table)], 1, sum))
				merge(df, codes_table, by = c('qda_id'), all.x = TRUE)
			})

			# Table view of the data
			output$text_table <- DT::renderDataTable({
				# refresh()
				df <- get_text_data()
				df$qda_text <- ShinyQDA::text_truncate(df$qda_text)
				DT::datatable(
					df,
					rownames = FALSE,
					filter = 'top',
					options = list(
						pageLength = 20
					),
					selection = 'single'
				)
			})

			# Reactive function to determine if a row is selected
			shiny::observe({
				if(!is.null(input$text_table_rows_selected)) {
					df <- get_text_data()
					txt_data <- df[input$text_table_rows_selected, , drop = FALSE]
					txt <- txt_data$qda_text
					id <- txt_data$qda_id
					codings <- qda_data()$get_codings(id = id)
					if(nrow(codings) > 0) {
						txt <- highlighter(txt, codings, qda_data()$get_codes(), link = FALSE)
					}
					txt <- gsub('\\n', '<p/>', txt)
					shiny::showModal(
						shiny::modalDialog(
							shiny::HTML(txt),
							title = 'Full Text',
							easyClose = TRUE)
					)
				}
			})
		}
	)
}
