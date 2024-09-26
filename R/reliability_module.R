#' UI for the inter-rater reliability analysis.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
#' @importFrom shiny NS
reliability_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::tabsetPanel(
			shiny::tabPanel(
				title = 'Summary',
				shiny::fluidRow(
					shiny::column(
						width = 3,
						shiny::uiOutput(ns('reliability_timeframe'))
					),
					shiny::column(
						width = 9,
						shiny::uiOutput(ns('reliability_coders'))
					)
				),
				DT::dataTableOutput(ns('reliability_summary'))
			),
			shiny::tabPanel(
				title = 'By Text Document',
					shiny::uiOutput(ns('reliability_text_selection')),
					shiny::uiOutput(ns('reliability_text_codes_select_ui')),
					shiny::fluidRow(
						shiny::column(
							width = 6,
							shiny::uiOutput(ns('reliability_coder1_selection')),
							shiny::htmlOutput(ns('reliability_text1')),
							shiny::hr(),
							shiny::plotOutput(ns('reliability_plot1'), height = '500px')
						),
						shiny::column(
							width = 6,
							shiny::uiOutput(ns('reliability_coder2_selection')),
							shiny::htmlOutput(ns('reliability_text2')),
							shiny::hr(),
							shiny::plotOutput(ns('reliability_plot2'), height = '500px')
						)
					)
			)
		)
	)
}


#' Server for the inter-rater reliability analysis.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
reliability_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$reliability_coders <- shiny::renderUI({
				ns <- session$ns
				coders <- qda_data()$get_coders()
				coders <- coders$user
				shiny::selectInput(inputId = ns('reliability_coders'),
								   label = 'Coders',
								   choices = coders,
								   multiple = TRUE,
								   selected = coders,
								   width = '100%')
			})

			output$reliability_timeframe <- shiny::renderUI({
				ns <- session$ns
				codings <- qda_data()$get_codings()
				codings$date_added <- as.Date(codings$date_added)
				shiny::dateRangeInput(
					inputId = ns('reliability_timeframe'),
					label = 'Timeframe',
					min = min(codings$date_added),
					max = max(codings$date_added),
					start = min(codings$date_added),
					end = max(codings$date_added)
				)
			})

			output$reliability_summary <- DT::renderDT({
				req(input$reliability_coders)
				req(input$reliability_timeframe)
				codings <- qda_data()$get_codings()
				if(nrow(codings) > 0) {
					codings <- codings |>
						dplyr::mutate(date_added <- as.Date(date_added)) |>
						dplyr::filter(date_added >= input$reliability_timeframe[1] &
									  date_added <= input$reliability_timeframe[2])
				}
				df <- irr(codings = codings, coders = input$reliability_coders)
				df$code <- as.factor(df$code)
				DT::datatable(
					df,
					rownames = FALSE,
					filter = 'top',
					options = list(
						# https://stackoverflow.com/questions/50922686/freeze-header-and-footer-in-datatable-shiny
						# scrollX = TRUE,
						# scrollY = "400px"
						pageLength = 40
					),
					selection = 'single'
				) |>
					DT::formatPercentage(columns = c('pra')) |>
					DT::formatRound(digits = 3, columns = c('icc1', 'icc2', 'icc3', 'icc1k', 'icc2k', 'icc3k'))
			})

			output$reliability_text_selection <- shiny::renderUI({
				ns <- session$ns

				n_char_preview <- 80

				shiny::isolate({
					text <- qda_data()$get_text()
					codings <- qda_data()$get_codings()
				})

				tab <- table(codings$qda_id, codings$coder)
				double_codings <- row.names(tab)[apply(tab, 1, FUN = function(x) { sum(x > 0) >= 2 })]
				codings <- codings |>
					dplyr::filter(qda_id %in% double_codings)
				text <- text |>
					dplyr::filter(qda_id %in% double_codings)

				if(nrow(text) == 0) {
					return(shiny::strong('No text has been coded by more than one rater.'))
				}

				choices <- text$qda_id
				names(choices) <- text$qda_text
				names(choices)[nchar(names(choices)) > n_char_preview] <- paste0(
					substr(names(choices)[nchar(names(choices)) > n_char_preview], 1, n_char_preview),
					'...'
				)
				names(choices) <- paste0(text$qda_id, ': ', names(choices))

				shiny::selectInput(inputId = ns('reliability_selected_text'),
								   label = 'Select text:',
								   choices = choices,
								   multiple = FALSE,
								   width = '100%')
			})

			output$reliability_text_codes_select_ui <- shiny::renderUI({
				codes <- qda_data()$get_codes()$code
				ns <- session$ns
				shiny::selectizeInput(
					inputId = ns('reliability_text_codes_to_view'),
					label = 'Codes to view',
					choices = codes,
					selected = codes,
					multiple = TRUE,
					width = '100%'
				)
			})

			output$reliability_coder1_selection <- shiny::renderUI({
				req(input$reliability_selected_text)
				ns <- session$ns

				shiny::isolate({
					codings <- qda_data()$get_codings()
				})
				codings <- codings |>
					dplyr::filter(qda_id == input$reliability_selected_text)
				raters <- unique(codings$coder)

				shiny::selectInput(inputId = ns('reliability_coder1_selection'),
								   label = 'Coder 1',
								   choices = raters,
								   selected = raters[1],
								   multiple = FALSE)
			})

			output$reliability_coder2_selection <- shiny::renderUI({
				req(input$reliability_selected_text)
				ns <- session$ns

				shiny::isolate({
					codings <- qda_data()$get_codings()
				})
				codings <- codings |>
					dplyr::filter(qda_id == input$reliability_selected_text)
				raters <- unique(codings$coder)

				shiny::selectInput(inputId = ns('reliability_coder2_selection'),
								   label = 'Coder 2',
								   choices = raters,
								   selected = raters[2],
								   multiple = FALSE)
			})

			output$reliability_text1 <- shiny::renderText({
				req(input$reliability_selected_text)
				req(input$reliability_coder1_selection)

				shiny::isolate({
					thetext <- qda_data()$get_text(input$reliability_selected_text) |>
						dplyr::select(qda_text)
					thetext <- thetext[1,1,drop=TRUE]
					codings <- qda_data()$get_codings()
				})

				codings <- codings |>
					dplyr::filter(qda_id == input$reliability_selected_text &
								  coder == input$reliability_coder1_selection)

				if(nrow(codings) > 0) {
					rows <- grep(pattern = paste0(input$reliability_text_codes_to_view, collapse = '|'),
								 x = codings$codes)
					codings <- codings[rows,]
				}

				thetext <- highlighter(thetext, codings, qda_data()$get_codes(), link = FALSE)
				# Convert line breaks to HTML line breaks
				thetext <- gsub('\\n', '<p/>', thetext)
				return(thetext)
			})

			output$reliability_text2 <- shiny::renderText({
				req(input$reliability_selected_text)
				req(input$reliability_coder2_selection)

				shiny::isolate({
					thetext <- qda_data()$get_text(input$reliability_selected_text) |>
						dplyr::select(qda_text)
					thetext <- thetext[1,1,drop=TRUE]
					codings <- qda_data()$get_codings()
				})

				codings <- codings |>
					dplyr::filter(qda_id == input$reliability_selected_text &
								  coder == input$reliability_coder2_selection)

				if(nrow(codings) > 0) {
					rows <- grep(pattern = paste0(input$reliability_text_codes_to_view, collapse = '|'),
								 x = codings$codes)
					codings <- codings[rows,]
				}

				thetext <- highlighter(thetext, codings, qda_data()$get_codes(), link = FALSE)
				# Convert line breaks to HTML line breaks
				thetext <- gsub('\\n', '<p/>', thetext)
				return(thetext)
			})

			output$reliability_plot1 <- shiny::renderPlot({
				req(input$reliability_selected_text)
				req(input$reliability_coder1_selection)

				all_codes <- input$reliability_text_codes_to_view
				shiny::isolate({
					codings <- qda_data()$get_codings()
					# all_codes <- qda_data()$get_codes()
					if(nrow(codings) > 0) {
						rows <- grep(pattern = paste0(input$reliability_text_codes_to_view, collapse = '|'),
									 x = codings$codes)
						codings <- codings[rows,]
					}
				})

				codes <- codings |>
					dplyr::filter(qda_id == input$reliability_selected_text) |>
					dplyr::filter(coder == input$reliability_coder1_selection) |>
					dplyr::filter(codes != '') |>
					dplyr::select(codes)
				codes <- unlist(strsplit(codes[,1,drop=TRUE], ';')) |>
					table() |>
					as.data.frame()

				ggplot2::ggplot(codes, aes(x = Var1, y = Freq)) +
					ggplot2::geom_bar(stat = 'identity', color = 'grey50') +
					ggplot2::geom_text(aes(label = Freq), hjust = -0.5) +
					ggplot2::coord_flip() +
					ggplot2::theme_minimal() +
					ggplot2::xlab('') +
					ggplot2::scale_x_discrete(limits = all_codes) +
					ggplot2::ggtitle(paste0('Distribution of codes by ', input$reliability_coder1_selection))
			})

			output$reliability_plot2 <- shiny::renderPlot({
				req(input$reliability_selected_text)
				req(input$reliability_coder2_selection)

				all_codes <- input$reliability_text_codes_to_view
				shiny::isolate({
					codings <- qda_data()$get_codings()
					# all_codes <- qda_data()$get_codes()
					if(nrow(codings) > 0) {
						rows <- grep(pattern = paste0(input$reliability_text_codes_to_view, collapse = '|'),
									 x = codings$codes)
						codings <- codings[rows,]
					}
				})

				codes <- codings |>
					dplyr::filter(qda_id == input$reliability_selected_text) |>
					dplyr::filter(coder == input$reliability_coder2_selection) |>
					dplyr::filter(codes != '') |>
					dplyr::select(codes)
				codes <- unlist(strsplit(codes[,1,drop=TRUE], ';')) |>
					table() |>
					as.data.frame()

				ggplot2::ggplot(codes, aes(x = Var1, y = Freq)) +
					ggplot2::geom_bar(stat = 'identity', color = 'grey50') +
					ggplot2::geom_text(aes(label = Freq), hjust = -0.5) +
					ggplot2::coord_flip() +
					ggplot2::theme_minimal() +
					ggplot2::xlab('') +
					ggplot2::scale_x_discrete(limits = all_codes) +
					ggplot2::ggtitle(paste0('Distribution of codes by ', input$reliability_coder2_selection))
			})
		}
	)
}
