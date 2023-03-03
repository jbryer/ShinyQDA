utils::globalVariables(c("password", "qda_id", "coder", "qda_text", "answer",
						 "criteria", "."))

#' Shiny Server for QDA
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @importFrom shiny renderPrint reactive reactiveVal reactivePoll reactiveValuesToList renderUI HTML selectizeInput renderText req observe observeEvent showModal modalDialog removeModal actionButton uiOutput tagList checkboxGroupInput textAreaInput radioButtons div req
#' @importFrom textutils HTMLencode
#' @importFrom shinyjs enable disable
#' @importFrom DT datatable renderDataTable JS formatRound
#' @importFrom shinymanager secure_server check_credentials
#' @importFrom shinyTree renderTree
#' @importFrom shinyjs runjs
#' @importFrom stringr str_squish str_remove_all
#' @export
shiny_server <- function(input, output, session) {
	# TODO: add localization options
	codes_label <- 'Codes (domains/subdomains):'
	add_code_label <- 'Add new coding'
	edit_code_label <- 'Edit coding'

	############################################################################
	##### Setup the connection to SQLite database
	if(!exists('qda_data_file')) {
		message('Creating qda.sqlite file...')
		qda_data_file <- 'qda.sqlite'
	}
	qda_data_db <- qda(qda_data_file)

	# Using reactivePoll to ensure the app is refreshed when the data changes.
	qda_data <- shiny::reactivePoll(
		intervalMillis = 1000,
		session,
		checkFunc = function() {
			# Why does this change even when the database doesn't
			# v <- tools::md5sum(qda_data_file)
			qda_data_db$get_last_update()
		},
		valueFunc = function() {
			# print('Refreshing data...')
			qda_data_db
		}
	)

	############################################################################
	##### User Authentication
	# call the server part
	# check_credentials returns a function to authenticate users
	res_auth <- shinymanager::secure_server(
		check_credentials = shinymanager::check_credentials(
			db = qda_data()$db_file,
			passphrase = qda_data()$users_passphrase
		)
	)

	output$auth_output <- shiny::renderPrint({
		shiny::reactiveValuesToList(res_auth)
	})

	get_username <- shiny::reactive({
		username <- shiny::reactiveValuesToList(res_auth)$user
		if(is.null(username)) {
			username <- Sys.info()['user']
		}
		return(username)
	})

	get_users <- shiny::reactive({
		qda_data()$get_coders()
	})

	output$coders_table <- DT::renderDataTable({
		get_users() |> dplyr::select(!password)
	})

	############################################################################
	# Codebook Tree
	codebook_server('ShinyQDA', qda_data)

	############################################################################
	##### Questions (text and code)
	questions_server('ShinyQDA', qda_data)

	#### Rubric ################################################################
	rubric_edit_server('ShinyQDA', qda_data)

	##### Overall view across all texts ########################################
	data_view_server('ShinyQDA', qda_data)

	##### qda Table Views ######################################################
	qda_view_server('ShinyQDA', qda_data)

	##### Descriptive Statistics ###############################################
	descriptives_server('ShinyQDA', qda_data)

	##### Sentiment Analysis ###################################################
	sentiment_server('ShinyQDA', qda_data)

	############################################################################
	##### Text display and coding

	# Select box for the current text to code
	output$essay_selection <- shiny::renderUI({
		n_char_preview <- 60

		shiny::isolate({
			text <- qda_data()$get_text()
			codings <- qda_data()$get_codings()
		})

		if(nrow(text) == 0) {
			return(NULL)
		}

		if(input$essay_selection_subset == 'Not coded') {
			text <- text |>
				dplyr::filter(!qda_id %in% codings$qda_id)
		} else if(input$essay_selection_subset == 'Not coded by me') {
			codings <- codings |> dplyr::filter(coder == get_username())
			text <- text |>
				dplyr::filter(!qda_id %in% codings$qda_id)
		} else if(input$essay_selection_subset == 'Coded') {
			text <- text |>
				dplyr::filter(qda_id %in% codings$qda_id)
		} else if(input$essay_selection_subset == 'Coded by me') {
			codings <- codings |> dplyr::filter(coder == get_username())
			text <- text |>
				dplyr::filter(qda_id %in% codings$qda_id)
		}

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

	output$selected_id <- shiny::renderText({
		input$selected_text
	})

	text_selection <- shiny::reactiveVal('')

	output$text_info <- shiny::renderUI({
		code_edit_id()
		shiny::req(input$selected_text)
		thetext <- qda_data()$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]
		div(
			shiny::strong('Number of paragraphs: '),
				prettyNum(stringr::str_count(thetext, '[^\r\n]+'), big.mark = ','), shiny::br(),
			shiny::strong('Number of sentences: '),
				prettyNum(length(gregexpr('[[:alnum:] ][.!?]', thetext)[[1]]), big.mark = ','), shiny::br(),
			shiny::strong('Number of words: '),
				prettyNum(stringr::str_count(thetext, '\\w+'), big.mark = ','), shiny::br(),
			shiny::strong('Number of characters: '),
				prettyNum(nchar(thetext), big.mark = ',')
		)
	})

	# Text output. Note that this will replace new lines (i.e. \n) with <p/>
	output$text_output <- shiny::renderText({
		# Force refresh if a code/tag has been added, deleted, or edited
		input$edit_tag
		input$add_tag
		input$confirm_delete_tag

		code_edit_id()
		shiny::isolate({
			qda_data <- qda_data()
		})

		shiny::req(input$selected_text)
		shinyjs::disable('add_tag_button')
		thetext <- qda_data$get_text(input$selected_text) |>
			dplyr::select(qda_text)

		thetext <- thetext[1,1,drop=TRUE]
		# Highlight codes
		codings <- qda_data$get_codings(input$selected_text)
		# if(!is.null(input$text_coder)) {
			codings <- codings |> dplyr::filter(coder %in% input$text_coder)
		# }
		if(nrow(codings) > 0) {
			thetext <- highlighter(thetext, codings, qda_data$get_codes())
		}
		# Convert line breaks to HTML line breaks
		thetext <- gsub('\\n', '<p/>', thetext)
		return(thetext)
	})

	##### Sentiment view of the text
	output$sentiment_text <- shiny::renderText({
		shiny::req(input$selected_text)
		thetext <- qda_data()$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]
		return(sentiment_highlighter(thetext,
									 lexicon = input$sentiment_lexicon,
									 lexicon_dir = '.'))
	})

	output$sentiment_text_plot <- shiny::renderPlot({
		shiny::req(input$selected_text)
		thetext <- qda_data()$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]

		sentiment_plot(thetext,
					   lexicon = input$sentiment_lexicon,
					   lexicon_dir = '.')
	})

	############################################################################
	##### Modal dialog to add/edit codes
	# Show the modal dialog to add a tag
	shiny::observeEvent(input$add_tag_button, {
		text_selection(input$text_selection)
		shiny::showModal(
			shiny::modalDialog(
				shiny::uiOutput('coding_ui'),
				title = add_code_label,
				size = 'l',
				footer = shiny::tagList(
					shiny::actionButton('cancel_modal', 'Cancel'),
					shiny::actionButton('add_tag', 'Add')
				)
			)
		)
	})

	# Enable/disable the add tag button when text is selected/deselected
	shiny::observeEvent(input$text_selection, {
		# Check to see if the selected text can be found in the document
		text_selection <- input$text_selection
		thetext <- qda_data()$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]
		# thetext <- stringr::str_squish(thetext)
		thetext <- stringr::str_remove_all(thetext, '  ')
		pos <- gregexpr(text_selection, thetext, fixed = TRUE)[[1]]

		if(nchar(input$text_selection) > 0) {# & length(pos[pos != -1]) != 0) {
			shinyjs::enable('add_tag_button')
			code_edit_id(0)
		} else {
			shinyjs::disable('add_tag_button')
			code_edit_id(0)
		}
	})

	# Save the tag and close the modal
	shiny::observeEvent(input$add_tag, {
		text_selection <- text_selection()
		thetext <- qda_data()$get_text(input$selected_text) |>
			dplyr::select(qda_text)
		thetext <- thetext[1,1,drop=TRUE]
		# thetext <- stringr::str_squish(thetext)
		thetext <- stringr::str_remove_all(thetext, '  ')
		pos <- gregexpr(text_selection, thetext, fixed = TRUE)[[1]]
		code_ids <- integer()
		# TODO: Make sure there is a match and report error to user
		if(length(pos[pos != -1]) == 0) {
			msg <- paste0('Could not match the selected text within the document using a regular expression: \n',
						  text_selection)
			shinyWidgets::alert(
				title = 'Problem finding match',
				text = msg,
				status = 'warning',
				dismissible = TRUE)
			warning(msg)
		}
		for(i in pos[pos != -1]) {
			code_ids <- c(
				code_ids,
				qda_data()$add_coding(
					id = input$selected_text,
					text = text_selection,
					start = i,
					end = i + nchar(text_selection),
					codes = input$new_code,
					coder = get_username())
			)
		}

		code_questions <- qda_data()$get_code_questions()
		for(i in seq_len(nrow(code_questions))) {
			for(j in code_ids) {
				qda_data()$add_code_question_response(
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
		# relies on the text being re-rendered when the user clicks cancel
		coding_id <- strsplit(input$edit_coding, ';')[[1]][1]
		coding <- qda_data()$get_codings(coding_id = coding_id)
		code_edit_id(coding_id)
		text_selection(coding[1,]$text)
		shiny::showModal(
			shiny::modalDialog(
			# modalDialog2(
				shiny::uiOutput('coding_ui'),
				title = edit_code_label,
				size = 'l',
				footer = shiny::tagList(
					shiny::actionButton('cancel_modal', 'Cancel'),
					shiny::actionButton('delete_tag', 'Delete'),
					shiny::actionButton('edit_tag', 'Save')
				)
			)
		)
	})

	shiny::observeEvent(input$delete_tag, {
		ns <- session$ns
		shinyWidgets::ask_confirmation(
			inputId = ns('confirm_delete_tag'),
			title = 'Confirm Deletion',
			text = 'Are you sure you wish to delete this code?',
			btn_labels = c('No', 'Yes'))
	})

	shiny::observeEvent(input$confirm_delete_tag, {
		if(input$confirm_delete_tag) {
			edit_id <- code_edit_id()
			qda_data()$delete_coding(edit_id)
			qda_data()$delete_code_question_responses(edit_id)
		}
		shiny::removeModal()
	})

	shiny::observeEvent(input$cancel_modal, {
		code_edit_id(0)
		shiny::removeModal()
	})

	# Edit the tag and close the modal
	shiny::observeEvent(input$edit_tag, {
		edit_id <- code_edit_id()
		coding <- NULL
		question_responses <- NULL
		coding <- qda_data()$get_codings(coding_id = edit_id)
		question_responses <- qda_data()$get_code_question_responses(edit_id)

		qda_data()$delete_code_question_responses(edit_id)

		selected_codes <- NULL
		if(!is.null(coding)) {
			if(nrow(coding) > 0) {
				selected_codes <- strsplit(coding$codes, split = ';')[[1]]
			}
		}

		qda_data()$update_coding(edit_id, input$new_code)

		code_questions <- qda_data()$get_code_questions()
		for(i in seq_len(nrow(code_questions))) {
			qda_data()$add_code_question_response(
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
		shiny::isolate({
			qda_data <- qda_data()
		})
		txt <- text_selection()

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
			shiny::p(shiny::strong('Selected text: '), txt),
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
	output$text_coders_ui <- shiny::renderUI({
		req(input$selected_text)
		ui <- NULL
		codes <- qda_data()$get_codings(input$selected_text)
		if(nrow(codes) > 0) {
			coders <- unique(c(get_username(), codes$coder))
			ui <- shiny::checkboxGroupInput(
				inputId = 'text_coder',
				label = 'Coders who coded this text:',
				choices = coders,
				selected = get_username())
		} else {
			ui <- shiny::checkboxGroupInput(
				inputId = 'text_coder',
				label = 'Coders who coded this text:',
				choices = get_username(),
				selected = get_username())
		}
		return(ui)
	})

	############################################################################
	# UI for text questions
	output$questions_ui <- shiny::renderUI({
		questions <- qda_data()$get_text_questions()
		responses <- qda_data()$get_text_question_responses(input$selected_text, get_username())
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
		if(nrow(questions) > 0) {
			ui[[length(ui) + 1]] <- actionButton(
				inputId = 'save_text_questions',
				label = 'Save Responses')
		}
		do.call(shiny::div, ui)
	})

	shiny::observeEvent(input$save_text_questions, {
		# TODO: Provide some indicationg that there are unsaved changes
		shiny::req(input$selected_text)
		questions <- qda_data()$get_text_questions()
		responses <- qda_data()$get_text_question_responses(id = input$selected_text,
															coder = get_username())
		for(i in seq_len(nrow(questions))) {
			stem <- questions[i,]$stem
			new_value <- input[[paste0('text_', textutils::HTMLencode(stem))]]
			if(!is.null(new_value)) {
				old_value <- ifelse(nrow(responses) == 0,
									'',
									responses |>
										dplyr::filter(stem == stem) |>
										dplyr::select(answer) |>
										as.character())
				if(old_value != new_value) {
					qda_data()$delete_text_question_response(
						id = input$selected_text,
						coder = get_username()
					)
					qda_data()$add_text_question_response(
						id = input$selected_text,
						stem = stem,
						answer = new_value,
						coder = get_username()
					)
				}
			}
		}
	})

	############################################################################
	##### Rubric
	css_cell <- 'style="text-align:left;vertical-align:top;background-color:#FFFFFF;height:inherit;border:1px solid #000000;"'

	current_rubric <- shiny::reactiveValues()

	output$rubric <- shiny::renderText({
		rubrics <- qda_data()$get_rubrics()
		if(nrow(rubrics) == 0) {
			return('No rubric defined.')
		}
		rubric_name <- rubrics[1,]$rubric_name
		rubric <- qda_data()$get_rubric(rubric_name)
		if(nrow(rubric) == 0) {
			return('No rubric criteria defined.')
		}
		rubric <- reshape2::dcast(rubric, criteria ~ scoring_level, value.var = 'description')

		current_rubric$rubric_name <- rubric_name
		current_rubric$rubric <- rubric

		scores <- qda_data()$get_rubric_responses(rubric_name = rubric_name,
												  qda_id = input$selected_text,
												  coder = get_username())

		txt <- '<center><table style="height:100px;border:1px solid #000000; width:100%;"><tr>'
		for(i in seq_len(ncol(rubric))) {
			txt <- paste0(txt, '<th style="text-align:center; border:1px solid #000000">', names(rubric)[i], '</th>')
		}
		txt <- paste0(txt, '</tr>')
		for(i in seq_len(nrow(rubric))) {
			txt <- paste0(txt, '<tr>')
			for(j in seq_len(ncol(rubric))) {
				if(j == 1) {
					txt <- paste0(txt, '<td ', css_cell, '><strong>', rubric[i, j, drop = TRUE], '</strong></td>')
				} else {
					score <- scores |> dplyr::filter(
						qda_id == input$selected_text,
						rubric_name == rubric_name,
						coder == get_username(),
						criteria == rubric[i,1,drop=TRUE])
					cssclass <- ifelse(nrow(score) > 0 & as.character(score[1,]$score) == names(rubric)[j],
						   'class="highlight" ', '')
					txt <- paste0(txt, '<td ', css_cell, ' ',
								  'onclick="Shiny.onInputChange(\'cell_selection\', \'row', i, 'column', j, '\'); ">',
								  '<div id="row', i, 'column', j, '" ',
								  cssclass,
								  'style="width:100%;height:100%"> ',
								  rubric[i, j, drop = TRUE], '</div></td>')
				}
			}
			txt <- paste0(txt, '</tr>')
		}
		txt <- paste0(txt, "</table></center>")

		return(txt)
	})

	observeEvent(input$cell_selection, {
		cell <- input$cell_selection
		cell_pos <- strsplit(cell, 'column')[[1]] %>% gsub('row', '', .) |> as.integer()
		rubric <- shiny::isolate(current_rubric$rubric)
		rubric_name <- shiny::isolate(current_rubric$rubric_name)
		for(i in seq_len(ncol(rubric))) {
			shinyjs::removeCssClass(id = paste0('row', cell_pos[1], 'column', i), class = 'highlight')
		}
		# Save score to database
		qda_data()$delete_rubric_response(rubric_name = rubric_name,
										  qda_id = input$selected_text,
										  coder = get_username(),
										  criteria = rubric[cell_pos[1],1])
		qda_data()$add_rubric_response(
			rubric_name = rubric_name,
			qda_id = input$selected_text,
			coder = get_username(),
			criteria = rubric[cell_pos[1],1],
			score = names(rubric)[cell_pos[2]]
		)
		shinyjs::addCssClass(id = input$cell_selection, class = "highlight")
	})


	############################################################################
	##### Table outputs ########################################################

	# Coding table for the selected text
	output$coding_table <- DT::renderDataTable({
		shiny::req(input$selected_text)
		codes <- qda_data()$get_codings()
		if(nrow(codes) == 0) {
			return(NULL)
		}
		codes <- codes |> dplyr::filter(qda_id == input$selected_text)
		# codes <- codes |> dplyr::filter(qda_id == 'UMGC565')

		code_questions <- qda_data()$get_code_question_responses()
		code_questions <- code_questions |>
			dplyr::filter(coding_id %in% codes$coding_id)
		code_questions <- code_questions[!duplicated(code_questions[,c('coding_id', 'stem', 'coder')], fromLast = TRUE),]

		if(nrow(code_questions) > 0) {
			code_questions <- reshape2::dcast(code_questions, coding_id + coder ~ stem, value.var = 'answer')
			codes2 <- merge(codes, code_questions, by = c('coding_id', 'coder'), all.x = TRUE)
		} else {
			codes2 <- codes
		}
		codes2 |> qda_datatable()
	})

	############################################################################
	output$cooccurrence_plot <- shiny::renderPlot({
		qda_merge(qda_data()) |>
			dplyr::select(dplyr::starts_with('code_')) |>
			ShinyQDA::cooccurance_plot(qda_data) + ggplot2::theme(legend.position = 'none')
	})
}
