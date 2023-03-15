#' UI for the topic modeling analysis
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
#' @importFrom shiny NS
topic_modeling_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				style = "height: 90vh; overflow-y: auto;",
				shiny::h4('Model Exploration'),
				shiny::numericInput(inputId = ns('topic_modeling_min_frequency'),
									label = 'Minimum Word Frequency',
									value = 2,
									min = 1, max = 100),
				shiny::sliderInput(inputId = ns('topic_modeling_try_topics'),
								   label = 'Number of topics to compare models',
								   min = 2, max = 100,
								   value = c(2, 20)),
				shiny::sliderInput(inputId = ns('topic_modeling_try_topics_step'),
								   label = 'Step between number of topics',
								   value = 2,
								   min = 1, max = 20),
				shiny::numericInput(inputId = ns('topic_modeling_n_terms'),
									label = 'Number of terms to preview',
									value = 40,
									min = 1, max = Inf),
				shiny::hr(),
				shiny::h4('Model Selection'),
				shiny::numericInput(inputId = ns('topic_modeling_k'),
									label = 'Number of topics',
									value = 2,
									min = 1, max = 100),
				shiny::p(shiny::strong('Number of terms: '),
						 shiny::textOutput(ns('number_of_terms')))
			),
			shiny::mainPanel(
				shiny::tabsetPanel(
					shiny::tabPanel(
						title = 'Find Number of Topics',
						shiny::plotOutput(ns('topics_number_plot'))
					),
					shiny::tabPanel(
						title = 'Terms',
						shiny::dataTableOutput(ns('terms_table'))
					),
					shiny::tabPanel(
						title = 'Wordcloud',
						shiny::uiOutput(ns('topic_selector')),
						shiny::plotOutput(ns('topic_word_cloud'), height = '600px', width = '100%')
					)
				)
			)
		)
	)
}



#' Server for the sentiment analysis.
#'
#' Much of the code and analysis was adapted from https://ladal.edu.au/topicmodels.html
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom wordcloud2 wordcloud2 renderWordcloud2 wordcloud2Output
#' @importFrom ldatuning FindTopicsNumber FindTopicsNumber_plot
#' @importFrom modeltools posterior
#' @importFrom topicmodels LDA terms
#' @import tm
topic_modeling_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			dtm <- shiny::reactive({
				textdata <- qda_data()$get_text()[,c('qda_id', 'qda_text')]
				names(textdata) <- c('doc_id', 'text')

				stopwords::stopwords_getsources()
				stop_words <- stopwords::stopwords(language = 'en',
												   source = 'snowball', # TODO: Make option
												   simplify = TRUE)
				corpus <- tm::Corpus(tm::DataframeSource(textdata))
				# Pre-process TODO: many of these stems should be exposed as checkboxes
				processedCorpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
				processedCorpus <- tm::tm_map(processedCorpus, tm::removeWords, stop_words)
				processedCorpus <- tm::tm_map(processedCorpus, tm::removePunctuation, preserve_intra_word_dashes = TRUE)
				processedCorpus <- tm::tm_map(processedCorpus, tm::removeNumbers)
				processedCorpus <- tm::tm_map(processedCorpus, tm::stemDocument, language = "en")
				processedCorpus <- tm::tm_map(processedCorpus, tm::stripWhitespace)

				DTM <- tm::DocumentTermMatrix(processedCorpus,
											  control = list(bounds = list(global = c(input$topic_modeling_min_frequency, Inf))))
				return(DTM)
			})

			topicModels <- shiny::reactive({
				topicmodels::LDA(x = dtm(),
								 k = input$topic_modeling_k,
								 method = "Gibbs",
								 control = list(seed = 2112, iter = 500, verbose = 0))
			})

			output$topics_number_plot <- shiny::renderPlot({
				# TODO: should have some kind of status for the user as this may take a while to render
				result <- ldatuning::FindTopicsNumber(
					dtm(),
					topics = seq(from = input$topic_modeling_try_topics[1],
								 to = input$topic_modeling_try_topics[2],
								 by = input$topic_modeling_try_topics_step),
					metrics = c("CaoJuan2009",  "Deveaud2014"), # TODO: Make parameters
					method = "Gibbs",
					control = list(seed = 2112, iter = 500, verbose = 0),
					verbose = FALSE
				)

				ldatuning::FindTopicsNumber_plot(result)
			})

			output$number_of_terms <- shiny::renderText({
				DTM <- dtm()
				topicModel <- topicModels()
				tm::nTerms(DTM)
			})

			output$terms_table <- shiny::renderDataTable({
				topicModel <- topicModels()
				topicmodels::terms(topicModel, input$topic_modeling_n_terms)
			})

			output$topic_selector <- shiny::renderUI({
				ns <- session$ns
				choices <- 1:input$topic_modeling_k
				topicModel <- topicModels()
				tmResult <- modeltools::posterior(topicModel)

				names(choices) <- paste0('Topic ', 1:input$topic_modeling_k, ': ', apply(tmResult$terms, 1, FUN = function(x) {
					paste0(names(sort(x, decreasing = TRUE)[1:4]), collapse = ';')
				}))

				shiny::selectInput(inputId = ns('topic_selector'),
								   label = 'Select topic',
								   choices = choices,
								   selected = 1)
			})

			output$topic_word_cloud <- shiny::renderPlot({
				req(input$topic_selector)
				topicModel <- topicModels()
				tmResult <- modeltools::posterior(topicModel)
				nTerms <- min(input$topic_modeling_n_terms, ncol(tmResult$terms))
				topterms <- sort(tmResult$terms[input$topic_selector,], decreasing=TRUE)[1:nTerms]
				words <- names(topterms)
				probabilities <- sort(tmResult$terms[input$topic_selector,], decreasing=TRUE)[1:nTerms]
				wordcloud::wordcloud(words, probabilities,
									 rot.per = 0.35,
									 random.order = FALSE,
									 color = RColorBrewer::brewer.pal(8, "Dark2"))
			})
		}
	)
}
