#' UI for the sentiment analysis
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
#' @importFrom shiny NS
sentiment_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				style = "height: 90vh; overflow-y: auto;",
				shiny::selectInput(
					inputId = 'lexicon_plot',
					label = 'Analysis Type',
					choices = c('Bing binary sentiment' = 'bing',
								'NRC Word-Emotion Association' = 'nrc',
								'Loughran-McDonald Sentiment' = 'loughran',
								'AFINN-111 dataset' = 'afinn'),
					selected = 'nrc',
					multiple = FALSE
				)
			),
			shiny::mainPanel(
				shiny::conditionalPanel(
					"input.lexicon_plot == 'bing'",
					shiny::plotOutput(ns('sentiment_bing_plot'), height = '600px'),
				),
				shiny::conditionalPanel(
					"input.lexicon_plot == 'nrc'",
					shiny::plotOutput(ns('sentiment_nrc_plot'), height = '600px')
				),
				shiny::conditionalPanel(
					"input.lexicon_plot == 'loughran'",
					shiny::plotOutput(ns('sentiment_loughran_plot'), height = '600px')
				),
				shiny::conditionalPanel(
					"input.lexicon_plot == 'afinn'",
					shiny::plotOutput(ns('sentiment_afinn_plot'), height = '600px')
				)
			)
		)
	)
}



#' Server for the sentiment analysis.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2 wordcloud2Output
#' @import tm
sentiment_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$sentiment_bing_plot <- shiny::renderPlot({
				qdadf <- ShinyQDA::qda_merge(qda_data(), include_sentiments = TRUE)
				ggplot2::ggplot(qdadf[!duplicated(qdadf$qda_id),],
								aes(x = bing_total, color = !! sym(group_var))) +
					geom_density()
			})

			output$sentiment_nrc_plot <- shiny::renderPlot({
				qdadf <- ShinyQDA::qda_merge(qda_data(), include_sentiments = TRUE)
				qdadf_nrc <- qdadf |>
					dplyr::distinct(qda_id, .keep_all = TRUE) |>
					dplyr::select(starts_with('nrc_'))
				names(qdadf_nrc) <- gsub('nrc_', '', names(qdadf_nrc))
				nrc_total <- apply(qdadf_nrc, 1, sum)
				qdadf_nrc <- apply(qdadf_nrc, 2, function(x) { x / nrc_total }) |>
					as.data.frame()

				qdadf_nrc <- qdadf_nrc |>
					reshape2::melt()
				nrc_tab <- psych::describeBy(qdadf_nrc$value,
											 group = qdadf_nrc$variable,
											 mat = TRUE)

				ggplot2::ggplot(qdadf_nrc, ggplot2::aes(x = variable, y = value)) +
					ggplot2::geom_boxplot() +
					ggplot2::geom_errorbar(
						data = nrc_tab,
						ggplot2::aes(x = group1, y = mean, ymin = mean - se, ymax = mean + se),
						color = 'green3', size = 1, width = 0.4) +
					ggplot2::geom_point(
						data = nrc_tab, ggplot2::aes(x = group1, y = mean),
						color = 'blue', size = 3) +
					ggplot2::ylab('Proportion of Encoded Words') + ggplot2::xlab('') +
					ggplot2::coord_flip() +
					ggplot2::theme_minimal()
			})

			output$sentiment_loughran_plot <- shiny::renderPlot({
				qdadf <- ShinyQDA::qda_merge(qda_data(), include_sentiments = TRUE)
				qdadf_loughran <- qdadf |>
					dplyr::distinct(qda_id, .keep_all = TRUE) |>
					dplyr::select(starts_with('loughran_'))
				names(qdadf_loughran) <- gsub('loughran_', '', names(qdadf_loughran))
				loughran_total <- apply(qdadf_loughran, 1, sum)
				qdadf_loughran <- apply(qdadf_loughran, 2, function(x) { x / loughran_total }) |>
					as.data.frame()

				qdadf_loughran <- qdadf_loughran |>
					reshape2::melt()
				loughran_tab <- psych::describeBy(qdadf_loughran$value,
												  group = qdadf_loughran$variable,
												  mat = TRUE)

				ggplot2::ggplot(qdadf_loughran, ggplot2::aes(x = variable, y = value)) +
					ggplot2::geom_boxplot() +
					ggplot2::geom_errorbar(
						data = loughran_tab,
						ggplot2::aes(x = group1, y = mean, ymin = mean - se, ymax = mean + se),
						color = 'green3', size = 1, width = 0.4) +
					ggplot2::geom_point(
						data = loughran_tab, ggplot2::aes(x = group1, y = mean),
						color = 'blue', size = 3) +
					ggplot2::ylab('Proportion of Encoded Words') + ggplot2::xlab('') +
					ggplot2::coord_flip() +
					ggplot2::theme_minimal()
			})

			output$sentiment_afinn_plot <- shiny::renderPlot({
				qdadf <- ShinyQDA::qda_merge(qda_data(), include_sentiments = TRUE)
				ggplot2::ggplot(qdadf[!duplicated(qdadf$qda_id),],
								ggplot2::aes(x = afinn_sentiment)) +
					ggplot2::geom_density()
			})

		}
	)
}
