#' UI for the raw data view.
#'
#' This will display at tab group for each table in the [qda()] data object.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param download_xlsx include a download Excel button.
#' @param download_rda include a download R data button.
#' @export
qda_view_ui <- function(id, download_xlsx = TRUE, download_rda = TRUE) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::uiOutput(ns('raw_data_view'))
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
			tables <- reactive({
				tables <- list()
				for(i in DBI::dbListTables(qda_data()$db_conn)) {
					tables[[i]] <- DBI::dbReadTable(qda_data()$db_conn, i)
				}
				return(tables)
			})

			observe({
				tables <- tables()
				for(i in seq_len(length(tables))) {
					local({
						this_table_name <- names(tables)[i]
						this_table <- tables[[i]]
						output[[paste0('raw_data_', this_table_name)]] <- DT::renderDataTable({
							this_table |> qda_datatable()
						})
					})
				}
			})

			output$raw_data_view <- renderUI({
				ns <- session$ns
				tables <- tables()
				tabs <- list()
				tabs$widths <- c(3, 9)
				tabs$header = shiny::tagList(
					shiny::downloadButton(ns('qda_download_xlsx'), label = 'Download Excel'),
					shiny::downloadButton(ns('qda_download_rda'), label = 'Download R Data'),
					shiny::downloadButton(ns('qda_download_sqlite'), label = 'Download SQLite Database')
				)
				for(i in seq_len(length(tables))) {
					if(!names(tables)[i] %in% c('credentials', 'logs', 'pwd_mngt')) {
						tabs[[length(tabs) + 1]] <- shiny::tabPanel(names(tables)[i],
								DT::dataTableOutput(ns(paste0('raw_data_', names(tables)[i]))))
					}
				}
				do.call(shiny::navlistPanel, tabs)
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

			output$qda_download_sqlite <- shiny::downloadHandler(
				filename = function() {
					paste0(id, '-', Sys.Date(), '.sqlite')
				},
				content = function(file) {
					file.copy(qda_data()$db_file, file)
				}
			)
		}
	)
}
