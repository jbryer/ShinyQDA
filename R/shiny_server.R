
#' This palette was created from https://colorbrewer2.org using qualitative
#' type with 12 classes. There are two schemes given these parameters, the first
#' 12 are from a lighter palette and the second 12 are from a darker palette.
color_palette <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
				   '#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f',
				   '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
				   '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

#' Shiny Server for QDA
#'
#' @export
shiny_server <- function(input, output, session) {
	evaluator <- Sys.info()['user'] # TODO: enable login with multiple users

	marker <- marker::marker$new("#text-to-mark")

	# CSS classes for the code highlighting
	output$get_color_css <- shiny::renderText({
		css <- ''
		for(i in seq_len(length(qda_data$codes))) {
			css <- paste0(css, '.', qda_data$codes[i], '{background-color:', qda_data$code_colors[i], ';}')
		}
		return(css)
	})

	# Can use a different data set if desired
	# TODO: have a default data.frame for demo purposes
	# if(!exists('qda_data')) {
	# 	message('No data specified, using mtcars...')
	# 	data(mtcars, envir = environment())
	# 	qda_data <- mtcars
	# 	default_y <- 'mpg' # Default variable selected for the dependent variable
	# 	default_x <- 'wt'  # Default variable selected for the independent variable
	# }

	# id_col <- mget('id_col', ifnotfound = names(qda_data)[1])
	# essay_col <- mget('essay_col', ifnotfound = names(qda_data)[2])

	# Select box for the text
	output$essay_selection <- shiny::renderUI({
		n_char_preview <- 60
		choices <- qda_data$df[,qda_data$id_col,drop=TRUE]
		names(choices) <- qda_data$df[,qda_data$text_col,drop=TRUE]
		names(choices)[nchar(names(choices)) > n_char_preview] <- paste0(
			substr(names(choices)[nchar(names(choices)) > n_char_preview], 1, n_char_preview),
			'...'
		)
		names(choices) <- paste0(qda_data$df[,qda_data$id_col,drop=TRUE], ': ', names(choices))
		shiny::selectizeInput(inputId = 'selected_text',
							  label = 'Select text:',
							  choices = choices,
							  multiple = FALSE,
							  width = '100%')
	})

	selected_text <- reactiveVal('')

	# Text output. Note that this will replace new lines (i.e. \n) with <p/>
	output$text_output <- shiny::renderText({
		shiny::req(input$selected_text)
		shinyjs::disable('add_tag_button')
		thetext <- qda_data$df |>
			dplyr::filter(.data[[qda_data$id_col]] == input$selected_text) |>
			dplyr::select(qda_data$text_col)
		thetext <- thetext[1,1,drop=TRUE]
		thetext <- gsub('\\n', '<p/>', thetext)
		return(thetext)
	})

	# Enable/disable the add tag button when text is selected/deselected
	observeEvent(input$text_selection, {
		if(nchar(input$text_selection) > 0) {
			shinyjs::enable('add_tag_button')
		} else {
			shinyjs::disable('add_tag_button')
		}
	})

	# Output the selected/highlighted text
	output$selected_text_results = shiny::renderText({
		input$text_selection
	})

	# Show the modal dialog to add a tag
	shiny::observeEvent(input$add_tag_button, {
		selected_text(input$text_selection)
		shiny::showModal(
			shiny::modalDialog(
				shiny::uiOutput('coding_ui'),
				title = 'Add new coding tag',
				footer = shiny::tagList(
					shiny::modalButton('Cancel'),
					shiny::actionButton('add_tag', 'Add')
				)
			)
		)
	})

	# Save the tag and close the modal
	shiny::observeEvent(input$add_tag, {
		qda_data$add_coding(
			id = input$selected_text,
			text = selected_text(),
			codes = input$new_code,
			memo = input$new_code_memo
		)
		qda_data$save()
		shiny::removeModal()
	})

	# Check box group of tags assigned to the current essay
	output$text_codes_ui <- shiny::renderUI({
		req(input$selected_text)
		input$add_tag
		ui <- NULL
		codes <- qda_data$codings |>
			dplyr::filter(id == input$selected_text)
		if(nrow(codes) > 0) {
			text_codes <- codes$codes
			ui <- shiny::checkboxGroupInput(
				inputId = 'text_codes',
				label = 'Codes assigned to this text',
				choices = text_codes,
				selected = text_codes)
		}
		return(ui)
	})

	observe({
		req(input$selected_text)
		codes <- qda_data$codings |>
			dplyr::filter(id == input$selected_text)
		marker$unmark()
		for(i in seq_len(length(input$text_codes))) {
			code <- codes |> dplyr::filter(input$text_codes[i] %in% codes)
			for(j in seq_len(nrow(code))) {
				marker$mark(code[j,'text',drop=TRUE],
							className = color_palette[qda_data$codes == input$text_codes[i]])
			}
		}
	})

	# Codings table for the selected text
	output$coding_table <- DT::renderDataTable({
		req(input$selected_text)
		codes <- qda_data$codings |>
			dplyr::filter(id == input$selected_text)
		DT::datatable(
			codes,
			rownames = FALSE,
			options = list(
				pageLength = 20
			),
			selection = 'single'
		)
	})

	# UI for the add tag modal
	output$coding_ui <- shiny::renderUI({
		txt <- selected_text()
		ui <- list(
			shiny::p(strong('Selected text: '), txt),
			shiny::selectizeInput(inputId = 'new_code',
								  label = 'Codes:',
								  choices = qda_data$codes,
								  multiple = TRUE,
								  options = list(create = TRUE)),
			shiny::textAreaInput(inputId = 'new_code_memo',
								 label = 'Memo',
								 value = '',
								 width = '100%',
								 height = '100px')
		)
		do.call(shiny::wellPanel, ui)
	})

	# Table view of the data
	output$text_table <- DT::renderDataTable({
		df <- qda_data$df
		df[,qda_data$text_col] <- ShinyQDA::text_truncate(df[,qda_data$text_col,drop=TRUE])
		DT::datatable(
			df,
			rownames = FALSE,
			options = list(
				# rowCallback = DT::JS(
				# 	"function(row, data) {",
				# 	"var full_text = data[1]",
				# 	"$('td', row).attr('title', full_text);",
				# 	"}")
					pageLength = 20
			),
			selection = 'single'
		) #|> DT::formatRound(columns = seq(1, ncol(df)), digits = 2)
	})

	# Reactive function to determine if a row is selected
	text_table_selection <- observe({
		if(!is.null(input$text_table_rows_selected)) {
			txt <- qda_data$df[input$text_table_rows_selected, qda_data$text_col, drop = TRUE]
			txt <- gsub('\\n', '<p/>', txt)
			showModal(
				modalDialog(HTML(txt),
							title = 'Full Text')
			)
		}
	})

	# Get all codes with options
	output$get_codes <- shiny::renderUI({
		codes <- qda_data$codes
		ui <- list()
		for(i in seq_len(length(codes))) {

		}
		do.call(div, ui)
	})
}
