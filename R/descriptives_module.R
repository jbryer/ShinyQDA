#' UI for the descriptive statistics.
#'
#' @export
#' @importFrom shiny NS
#' @importFrom shinyTree shinyTree renderTree
descriptives_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				shiny::selectInput(
					inputId = 'plot_type',
					label = 'Analysis Type',
					choices = c('Barplot', 'Wordcloud', 'Wordcloud 2'),
					selected = 'barplot',
					multiple = FALSE
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
						choices = row.names(brewer.pal.info[brewer.pal.info$category == 'qual',]),
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
					"input.plot_type == 'Barplot'",
					shiny::plotOutput(ns('code_barplot'), height = '600px'),
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
#' @export
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2 wordcloud2Output
#' @import tm
descriptives_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			output$code_barplot <- shiny::renderPlot({
				df <- ShinyQDA::qda_merge(qda_data())
				text_df <- qda_data()$get_text()
				code_cols <- names(df)[seq(ncol(text_df) + 2, ncol(df))]
				df_sum <- apply(df[,code_cols], 2, FUN = function(x) { sum(x, na.rm = TRUE )}) |>
					as.data.frame()
				names(df_sum)[1] <- 'Count'
				df_sum$Code <- row.names(df_sum)

				ggplot2::ggplot(df_sum, ggplot2::aes(x = Code, y = Count)) +
					ggplot2::geom_bar(stat = 'identity', fill = 'grey50') +
					ggplot2::geom_text(ggplot2::aes(label = Count), hjust = -1) +
					ggplot2::coord_flip() +
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
