#' UI for the descriptive statistics.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
#' @importFrom shiny NS
#' @importFrom shinyTree shinyTree renderTree
#' @importFrom RColorBrewer brewer.pal.info
descriptives_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				style = "height: 90vh; overflow-y: auto;",
				shiny::selectInput(
					inputId = 'plot_type',
					label = 'Analysis Type',
					choices = c('Word Frequencies', 'Code Frequencies', 'Wordcloud', 'Wordcloud 2'),
					selected = 'barplot',
					multiple = FALSE
				),
				shiny::conditionalPanel(
					"input.plot_type == 'Word Frequencies'",
					shiny::sliderInput(inputId = ns('freq_ngrams'),
									   label = 'nGrams n',
									   min = 1, max = 3, value = 1),
					shiny::numericInput(inputId = ns('freq_n_ngrams'),
										label = 'Number of words/phrases',
										value = 20,
										min = 2,
										max = 100),
					shiny::checkboxInput(inputId = ns('freq_remove_stopwords'),
										 label = 'Remove stopwords',
										 value = TRUE),
					shiny::checkboxInput(inputId = ns('freq_to_lower'),
										 label = 'To lowercase',
										 value = TRUE)
				),
				shiny::conditionalPanel(
					"input.plot_type == 'Wordcloud'",
					shiny::sliderInput(
						inputId = ns('wordcloud_min_freq'),
						label = 'Minimum word frequency to plot',
						min = 1,
						max = 50,
						value = 2
					),
					shiny::selectInput(
						inputId = ns('wordcloud_palette'),
						label = 'Color palette',
						choices = row.names(RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]),
						selected = 'Dark2'
					)
				),
				shiny::conditionalPanel(
					"input.plot_type == 'Wordcloud 2'",
					# TODO: add addional options here: https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
					shiny::selectInput(
						inputId = ns('wordcloud2_palette'),
						label = 'Color palette',
						choices = c('random-dark', 'random-light')
					)
				)
			),
			shiny::mainPanel(
				shiny::conditionalPanel(
					"input.plot_type == 'Code Frequencies'",
					shiny::plotOutput(ns('code_barplot'), height = '600px'),
				),
				shiny::conditionalPanel(
					"input.plot_type == 'Word Frequencies'",
					shiny::plotOutput(ns('word_frequency_plot'), height = '600px')
				),
				shiny::conditionalPanel(
					"input.plot_type == 'Wordcloud'",
					shiny::plotOutput(ns('wordcloud_plot'), height = '600px')
				),
				shiny::conditionalPanel(
					"input.plot_type == 'Wordcloud 2'",
					wordcloud2::wordcloud2Output(ns('wordcloud2_plot'), height = '600px')
				)
			)
		)
	)
}



#' Server for the descriptive statistics.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2 wordcloud2Output
#' @import tm
descriptives_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$word_frequency_plot <- shiny::renderPlot({
				df <- qda_data()$get_text() |>
					dplyr::select(qda_id, qda_text)

				# Tokenize words
				tokens <- df |>
					tidytext::unnest_tokens(
						output = 'token',
						input = 'qda_text',
						token = 'ngrams',
						n = input$freq_ngrams,
						to_lower = input$freq_to_lower)

				# Remove stopwords
				if(input$freq_remove_stopwords) {
					tokens <- tokens |>
						dplyr::filter(!(token %in% stopwords::stopwords(source = 'snowball')))
				}

				tokens <- tokens |>
					dplyr::count(token, sort = TRUE) |>
					dplyr::top_n(n = input$freq_n_ngrams, n) |>
					dplyr::mutate(token = reorder(token, n))

				ggplot2::ggplot(tokens, aes(x = token, y = n)) +
						ggplot2::geom_bar(stat = 'identity', fill = 'grey50') +
						ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.1) +
						ggplot2::coord_flip() +
						ggplot2::expand_limits(y = max(tokens$n) + max(tokens$n) * .05) +
						xlab('') +
						ggtitle('Word Frequencies')
			})

			output$code_barplot <- shiny::renderPlot({
				df <- qda_merge(qda_data(), include_codes = TRUE) |>
					dplyr::select(dplyr::starts_with('code_'))
				names(df) <- gsub('code_', '', names(df))
				text_df <- qda_data()$get_text()
				df_sum <- apply(df, 2, FUN = function(x) { sum(x, na.rm = TRUE )}) |>
					as.data.frame()
				names(df_sum)[1] <- 'Count'
				df_sum$Code <- row.names(df_sum)
				df_sum <- df_sum[order(df_sum$Count, decreasing = FALSE),]
				df_sum$Code <- factor(df_sum$Code,
									  levels = df_sum$Code,
									  ordered = TRUE)

				ggplot2::ggplot(df_sum, ggplot2::aes(x = Code, y = Count)) +
					ggplot2::geom_bar(stat = 'identity', fill = 'grey50') +
					ggplot2::geom_text(ggplot2::aes(label = Count), hjust = -0.1) +
					ggplot2::coord_flip() +
					ggplot2::expand_limits(y = max(df_sum$Count) + max(df_sum$Count) * .05) +
					ggplot2::ggtitle('Number of codes across all text')

			})

			output$wordcloud_plot <- shiny::renderPlot({
				text_data <- qda_data()$get_text()
				text <- text_data$qda_text
				suppressWarnings({
					docs <- tm::Corpus(tm::VectorSource(text))
					docs <- docs %>%
						tm::tm_map(tm::removeNumbers) %>%
						tm::tm_map(tm::removePunctuation) %>%
						tm::tm_map(tm::stripWhitespace)
					docs <- tm::tm_map(docs, tm::content_transformer(tolower))
					docs <- tm::tm_map(docs, tm::removeWords, tm::stopwords("english"))
					dtm <- tm::TermDocumentMatrix(docs)
					matrix <- as.matrix(dtm)
					words <- sort(rowSums(matrix),decreasing=TRUE)
					df <- data.frame(word = names(words), freq = words)

					# par(mar = rep(0, 4))
					wordcloud::wordcloud(
						words = df$word,
						freq = df$freq,
						min.freq = input$wordcloud_min_freq,
						max.words = 200,
						random.order = FALSE,
						rot.per = 0.35,
						colors = RColorBrewer::brewer.pal(8, input$wordcloud_palette))
				})
			})

			output$wordcloud2_plot <- wordcloud2::renderWordcloud2({
				text_data <- qda_data()$get_text()
				text <- text_data$qda_text
				suppressWarnings({
					docs <- tm::Corpus(tm::VectorSource(text))
					docs <- docs %>%
						tm::tm_map(tm::removeNumbers) %>%
						tm::tm_map(tm::removePunctuation) %>%
						tm::tm_map(tm::stripWhitespace)
					docs <- tm::tm_map(docs, tm::content_transformer(tolower))
					docs <- tm::tm_map(docs, tm::removeWords, tm::stopwords("english"))
					dtm <- tm::TermDocumentMatrix(docs)
					matrix <- as.matrix(dtm)
					words <- sort(rowSums(matrix),decreasing=TRUE)
					df <- data.frame(word = names(words), freq = words)

					wordcloud2::wordcloud2(
						df,
						color = input$wordcloud2_palette
					)
				})
			})
		}
	)
}
