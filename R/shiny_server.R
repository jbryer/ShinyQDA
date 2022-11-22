# TODO: add localization options
modal_size <- 'xl'
codes_label <- 'Codes (domains/subdomains):'
add_code_label <- 'Add new coding'
edit_code_label <- 'Edit coding'

#' Converts the text to something more reasonable to use as an input ID.
#'
#' @export
# input_id_encoder <- function(txt, prefix = '', postfix = '') {
# 	txt <-
# }

#' Highlights the codes within the text using HTML.
#'
#' @param text the text to highlight.
#' @param codings a data.frame with the codings.
#' @param codes a data.frame with the codes (and colors).
#' @export
highlighter <- function(text, codings, codes) {
	# The color will be determined by the first code
	first_code <- sapply(strsplit(codings$codes, ';'), FUN = function(x) { x[[1]]})
	codes <- codes[!duplicated(codes$code),]
	row.names(codes) <- codes$code
	colors <- codes[first_code,]$color
	if(any(is.na(colors))) {
		colors[is.na(colors)] <- '#FFFF00'
	}
	hover_text <- paste0(codings$coder, ': ', codings$codes)
	starts <- codings$start
	names(starts) <- paste0(
		"<span ",
		"onclick='Shiny.onInputChange(\"edit_coding\", \"", codings$coding_id, ';', as.integer(Sys.time()), "\");' ",
		"style='background-color: ", colors, "' ",
		"class='tooltip2' ",
		">"
		, "<span class='tooltiptext2'>", hover_text, "</span>"
	)
	ends <- codings$end
	names(ends) <- rep('</span>', nrow(codings))
	tags <- c(starts, ends)
	tags <- tags[order(tags, decreasing = TRUE)]
	for(i in seq_len(length(tags))) {
		left <- substr(text, 0, tags[i] - 1)
		right <- substr(text, tags[i], nchar(text))
		text <- paste0(left, names(tags)[i], right)
	}
	return(text)
}

#' Shiny Server for QDA
#'
#' @import shiny
#' @importFrom textutils HTMLencode
#' @export
shiny_server <- function(input, output, session) {
	############################################################################
	##### User Authentication
	# call the server part
	# check_credentials returns a function to authenticate users
	res_auth <- secure_server(
		check_credentials = check_credentials(
			db = qda_data$db_file,
			passphrase = qda_data$users_passphrase
		)
	)

	output$auth_output <- renderPrint({
		reactiveValuesToList(res_auth)
	})

	get_username <- reactive({
		username <- reactiveValuesToList(res_auth)$user
		if(is.null(username)) {
			username <- Sys.info()['user']
		}
		return(username)
	})

	get_users <- reactive({
		# dbReadTable(user_db, 'users')
	})

	output$coders_table <- DT::renderDataTable({
		get_users() |>
			dplyr::select(!password)
	})

	############################################################################
	##### Text display and coding

	# Select box for the current text to code
	output$essay_selection <- shiny::renderUI({
		n_char_preview <- 60

		text <- qda_data$get_text()
		choices <- text$qda_id
		names(choices) <- text$qda_text
		names(choices)[nchar(names(choices)) > n_char_preview] <- paste0(
			substr(names(choices)[nchar(names(choices)) > n_char_preview], 1, n_char_preview),
			'...'
		)
		names(choices) <- paste0(text$qda_id, ': ', names(choices))
		shiny::selectizeInput(inputId = 'selected_text',
							  label = 'Select text:',
							  choices = choices,
							  multiple = FALSE,
							  width = '100%')
	})

	selected_text <- reactiveVal('')

	# Text output. Note that this will replace new lines (i.e. \n) with <p/>
	output$text_output <- shiny::renderText({
		# Trigger re-rendering after canceling coding modal dialog so that the links get new values
		input$cancel_modal
		input$add_tag # Re-render after a new code is added
		input$edit_tag
		code_edit_id()

		shiny::req(input$selected_text)
		shinyjs::disable('add_tag_button')
		thetext <- qda_data$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]
		# Highlight codes
		codings <- qda_data$get_codings(input$selected_text)
		if(nrow(codings) > 0) {
			thetext <- highlighter(thetext, codings, qda_data$get_codes())
		}
		# Convert line breaks to HTML line breaks
		thetext <- gsub('\\n', '<p/>', thetext)
		return(thetext)
	})

	# Enable/disable the add tag button when text is selected/deselected
	observeEvent(input$text_selection, {
		if(nchar(input$text_selection) > 0) {
			shinyjs::enable('add_tag_button')
			code_edit_id(0)
		} else {
			shinyjs::disable('add_tag_button')
			code_edit_id(0)
		}
	})

	############################################################################
	##### Modal dialog to add/edit codes
	# Show the modal dialog to add a tag
	shiny::observeEvent(input$add_tag_button, {
		selected_text(input$text_selection)
		shiny::showModal(
			shiny::modalDialog(
				shiny::uiOutput('coding_ui'),
				title = add_code_label,
				footer = shiny::tagList(
					shiny::actionButton('cancel_modal', 'Cancel'),
					shiny::actionButton('add_tag', 'Add')
				),
				size = modal_size
			)
		)
	})

	# Save the tag and close the modal
	shiny::observeEvent(input$add_tag, {
		selected_text <- selected_text()
		thetext <- qda_data$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]
		pos <- gregexpr(selected_text, thetext, fixed = TRUE)[[1]]
		code_ids <- integer()
		for(i in pos[pos != -1]) {
			code_ids <- c(
				code_ids,
				qda_data$add_coding(id = input$selected_text,
									text = selected_text,
									start = i,
									end = i + nchar(selected_text),
									codes = input$new_code,
									coder = get_username())
			)
		}

		code_questions <- qda_data$get_code_questions()
		for(i in seq_len(nrow(code_questions))) {
			for(j in code_ids) {
				qda_data$add_code_question_response(
					coding_id = j,
					stem = code_questions[i,]$stem,
					answer = paste0(input[[paste0('coding_', textutils::HTMLencode(code_questions[i,]$stem))]], collapse = ';'),
					coder = get_username()
				)
			}
		}
		code_edit_id(0)
		shiny::removeModal()
	},
	priority = 1000)


	# ID for the code to edit. Should be 0 when not in edit mode
	#  (i.e. will trigger creating a new code when 0)
	code_edit_id <- reactiveVal(0)

	# Modal dialog to edit an existing tag
	shiny::observeEvent(input$edit_coding, {
		# HACK: The event value is formatted as coding_id;System_Time so that
		# if the user clicks the same link the event is actually triggered. This
		# relies on the text being rerendered when the user clicks cancel
		coding_id <- strsplit(input$edit_coding, ';')[[1]][1]
		coding <- qda_data$get_codings(coding_id = coding_id)
		code_edit_id(coding_id)
		selected_text(coding[1,]$text)
		shiny::showModal(
			shiny::modalDialog(
				shiny::uiOutput('coding_ui'),
				title = edit_code_label,
				footer = shiny::tagList(
					shiny::actionButton('cancel_modal', 'Cancel'),
					shiny::actionButton('edit_tag', 'Save')
				),
				size = modal_size
			)
		)
	})

	observeEvent(input$cancel_modal, {
		code_edit_id(0)
		removeModal()
	})

	# Edit the tag and close the modal
	shiny::observeEvent(input$edit_tag, {
		edit_id <- code_edit_id()
		coding <- NULL
		question_responses <- NULL
		coding <- qda_data$get_codings(coding_id = edit_id)
		question_responses <- qda_data$get_code_question_responses(edit_id)

		qda_data$delete_code_question_responses(edit_id)

		selected_codes <- NULL
		if(!is.null(coding)) {
			if(nrow(coding) > 0) {
				selected_codes <- strsplit(coding$codes, split = ';')[[1]]
			}
		}

		qda_data$update_coding(edit_id, input$new_code)

		code_questions <- qda_data$get_code_questions()
		for(i in seq_len(nrow(code_questions))) {
			qda_data$add_code_question_response(
				edit_id,
				stem = code_questions[i,]$stem,
				answer = paste0(input[[paste0('coding_', textutils::HTMLencode(code_questions[i,]$stem))]], collapse = ';'),
				coder = get_username()
			)
		}

		shiny::removeModal()
	})

	# UI for the add tag modal
	output$coding_ui <- shiny::renderUI({
		txt <- selected_text()

		edit_id <- code_edit_id()
		question_responses <- NULL
		coding <- NULL
		if(edit_id > 0) {
			coding <- qda_data$get_codings(coding_id = edit_id)
			question_responses <- qda_data$get_code_question_responses(coding_id = edit_id)
		}

		selected_codes <- NULL
		if(!is.null(coding)) {
			if(nrow(coding) > 0) {
				selected_codes <- strsplit(coding$codes, split = ';')[[1]]
			}
		}

		ui <- list(
			shiny::p(strong('Selected text: '), txt),
			shiny::selectizeInput(inputId = 'new_code',
								  label = codes_label,
								  choices = qda_data$get_codes()$code,
								  multiple = TRUE,
								  selected = selected_codes,
								  options = list(create = TRUE))
		)

		code_questions <- qda_data$get_code_questions()

		for(i in seq_len(nrow(code_questions))) {
			if(code_questions[i,]$type == 'text') {
				value = ''
				if(!is.null(question_responses)) {
					response <- question_responses[question_responses$stem == code_questions[i,]$stem,]
					if(nrow(response) > 0) {
						value <- response[1,]$answer
					}
				}
				ui[[length(ui) + 1]] <- shiny::textAreaInput(
					inputId = paste0('coding_', textutils::HTMLencode(code_questions[i,]$stem)),
					label = code_questions[i,]$stem,
					value = value
				)
			} else if(code_questions[i,]$type == 'radio') {
				selected <- character(0)
				if(!is.null(question_responses)) {
					response <- question_responses[question_responses$stem == code_questions[i,]$stem,]
					if(nrow(response) > 0) {
						selected <- strsplit(response[1,]$answer, split = ';')[[1]]
					}
				}

				options <- strsplit(code_questions[i,]$options, split = ';')[[1]]
				ui[[length(ui) + 1]] <- shiny::radioButtons(
					inputId = paste0('coding_', textutils::HTMLencode(code_questions[i,]$stem)),
					label = code_questions[i,]$stem,
					choices = options,
					selected = selected
				)
			} else if(code_questions[i,]$type == 'checkbox') {
				selected <- character(0)
				if(!is.null(question_responses)) {
					response <- question_responses[question_responses$stem == code_questions[i,]$stem,]
					if(nrow(response) > 0) {
						selected <- strsplit(response[1,]$answer, split = ';')[[1]]
					}
				}

				options <- strsplit(code_questions[i,]$options, split = ';')[[1]]
				ui[[length(ui) + 1]] <- shiny::checkboxGroupInput(
					inputId = paste0('coding_', textutils::HTMLencode(code_questions[i,]$stem)),
					label = code_questions[i,]$stem,
					choices = options,
					selected = selected
				)
			} else {
				warning(paste0('Unknown question type, ',
							   code_questions[i,]$type,
							   ' for the following question: ',
							   code_questions[i,]$stem))
			}
		}
		do.call(shiny::wellPanel, ui)
	})

	############################################################################
	# Check box group of tags assigned to the current essay
	output$text_codes_ui <- shiny::renderUI({
		req(input$selected_text)
		input$add_tag
		ui <- NULL
		codes <- qda_data$get_codings() |>
			dplyr::filter(qda_id == input$selected_text)
		if(nrow(codes) > 0) {
			text_codes <- codes$codes
			text_codes <- unique(text_codes)
			ui <- shiny::checkboxGroupInput(
				inputId = 'text_codes',
				label = 'Codes assigned to this text',
				choices = text_codes,
				selected = text_codes)
		}
		return(ui)
	})


	############################################################################
	# UI for text questions
	output$questions_ui <- renderUI({
		questions <- qda_data$get_text_questions()
		responses <- qda_data$get_text_question_responses(input$selected_text,
														  get_username())
		ui <- list()
		for(i in seq_len(nrow(questions))) {
			if(questions[i,]$type == 'text') {
				value <- ''
				if(!is.null(responses)) {
					response <- responses[responses$stem == questions[i,]$stem,]
					if(nrow(response) > 0) {
						value <- response[1,]$answer
					}
				}
				ui[[length(ui) + 1]] <- shiny::textAreaInput(
					inputId = paste0('text_', textutils::HTMLencode(questions[i,]$stem)),
					label = questions[i,]$stem,
					value = value
				)
			} else if(questions[i,]$type == 'radio') {
				selected <- character(0)
				if(!is.null(responses)) {
					response <- responses[responses$stem == questions[i,]$stem,]
					if(nrow(response) > 0) {
						selected <- strsplit(response[1,]$answer, split = ';')[[1]]
					}
				}
				options <- strsplit(questions$options, split = ';')[[1]]
				ui[[length(ui) + 1]] <- shiny::radioButtons(
					inputId = paste0('text_', textutils::HTMLencode(questions[i,]$stem)),
					label = questions[i,]$stem,
					choices = options,
					selected = selected
				)
			} else if(questions[i,]$type == 'checkbox') {
				selected <- character(0)
				if(!is.null(responses)) {
					response <- responses[responses$stem == questions[i,]$stem,]
					if(nrow(response) > 0) {
						selected <- strsplit(response[1,]$answer, split = ';')[[1]]
					}
				}
				options <- strsplit(questions$options, split = ';')[[1]]
				ui[[length(ui) + 1]] <- shiny::checkboxGroupInput(
					inputId = paste0('text_', textutils::HTMLencode(questions[i,]$stem)),
					label = questions[i,]$stem,
					choices = options,
					selected = selected
				)
			} else {
				warning(paste0('Unknown question type, ',
							   questions[i,]$type,
							   ' for the following question: ',
							   questions[i,]$stem))
			}
		}
		# if(length(ui) > 0) {
		# 	ui[[length(ui) + 1]] <- actionButton('save_text_question_responses', label = 'Save')
		# }
		do.call(shiny::div, ui)
	})

	observe({
		req(input$selected_text)
		questions <- qda_data$get_text_questions()
		for(i in seq_len(nrow(questions))) {
			stem <- questions[i,]$stem
			value <- input[[paste0('text_', textutils::HTMLencode(stem))]]
			qda_data$delete_text_question_response(
				id = input$selected_text,
				coder = get_username()
			)
			qda_data$add_text_question_response(
				id = input$selected_text,
				stem = stem,
				answer = value,
				coder = get_username()
			)
		}
	})
	# observeEvent(input$save_text_question_responses, {
	#
	# })

	############################################################################
	##### Table outputs ########################################################

	# Coding table for the selected text
	output$coding_table <- DT::renderDataTable({
		req(input$selected_text)
		input$add_tag
		codes <- qda_data$get_codings() |>
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

	##### Overall view across all texts ########################################

	# Table view of the data
	output$text_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		df <- qda_data$get_text()
		df$qda_text <- ShinyQDA::text_truncate(df$qda_text)
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
			# TODO: Show tags for this text
			txt <- qda_data$get_text()[input$text_table_rows_selected, 'qda_text', drop = TRUE]
			txt <- gsub('\\n', '<p/>', txt)
			showModal(
				modalDialog(shiny::HTML(txt),
							title = 'Full Text',
							size = modal_size)
			)
		}
	})

	##### qda Table Views ######################################################
	output$codes_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		input$save_text_question_responses
		qda_data$get_codes() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

	output$code_questions_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		input$save_text_question_responses
		qda_data$get_code_questions() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

	output$code_question_responses_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		qda_data$get_code_question_responses() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

	output$text_questions_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		input$save_text_question_responses
		qda_data$get_text_questions() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

	output$text_question_responses_table <- DT::renderDataTable({
		# make sure this table is updated when values change.
		input$edit_tag
		input$add_tag
		questions <- qda_data$get_text_questions()
		for(i in seq_len(nrow(questions))) {
			stem <- questions[i,]$stem
			input[[paste0('text_', textutils::HTMLencode(stem))]]
		}

		qda_data$get_text_question_responses() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

	output$codings_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		input$save_text_question_responses
		qda_data$get_codings() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

	output$assignments_table <- DT::renderDataTable({
		input$edit_tag
		input$add_tag
		input$save_text_question_responses
		qda_data$get_assignments() |>
			DT::datatable(
				rownames = FALSE,
				options = list(
					pageLength = 20
				),
				selection = 'single'
			)
	})

}
