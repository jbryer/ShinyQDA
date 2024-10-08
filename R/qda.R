utils::globalVariables(c('qda_id', 'qda_text', 'password'))

#' Data object for qualitative data analysis.
#'
#' This function creates an object to manipulate data for qualitative analysis.
#' The data is stored in a SQLite database by default, but can be configures to
#' work with any SQL database through the `DBI` R package interface. Although
#' you can manipulate the SQL database directly, we recommend using the methods
#' (functions) defined in the returned object. This will ensure data integrity.
#'
#' @section Methods:
#'
#' ```{r, child = "man/rmd/qda.Rmd"}
#' ```
#'
#' @param file the filename and path to the file this function will save data to.
#' @param users_passphrase passpharse used to encrypt the user authentication table.
#'        See [shinymanager::create_db()] for more info.
#' @param users a vector of usernames who can login to the ShinyQDA app.
#' @param users_names a vector of names for the users.
#' @param users_passwords a vector of passwords for the users.
#' @param users_is_admin a logicial vector indicating if the user has administrator privileges.
#' @return a list with functions to access and edit qualitiative data.
#' @import RSQLite
#' @import DBI
#' @import dplyr
#' @importFrom shinymanager create_db read_db_decrypt
#' @importFrom utils timestamp
#' @export
qda <- function(
		file,
		users_passphrase = 'ShinyQDA',
		users = c('admin'),
		users_names = users,
		users_passwords = rep('password', length(users)),
		users_is_admin = rep(TRUE, length(users))
) {
	qda_db <- DBI::dbConnect(RSQLite::SQLite(), file)

	# on.exit(DBI::dbDisconnect(qda_db))

	# TODO: The name should be the function name, the value should be the documentation.
	methods_docs <- character()

	tables <- DBI::dbListTables(qda_db)

	qda_data <- list(
		db_conn = qda_db,
		db_file = file,
		users_passphrase = users_passphrase
	)

	##### change_log ###########################################################
	# create table
	if(!'change_log' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'change_log',
						   data.frame(
								coder = character(),
								table = character(),
								description = character(),
								timestamp = character()
						   ))
	}

	methods_docs['log'] <- 'Adds an entry to the log file when data has been changed. For internal use only.'
	qda_data$log <- function(coder = Sys.info()['user'], table = NA, description = NA, timestamp = as.character(Sys.time())) {
		new_row <- data.frame(
			coder = coder,
			table = table,
			description = description,
			timestamp = timestamp
		)
		DBI::dbWriteTable(qda_db, 'change_log', new_row, append = TRUE)
	}

	qda_data$get_log <- function() {
		DBI::dbReadTable(qda_db, 'change_log')
	}

	qda_data$get_last_update <- function() {
		DBI::dbGetQuery(qda_db, 'SELECT MAX(timestamp) FROM change_log') |> unname()
	}

	##### text_data ############################################################
	# create table
	if(!'text_data' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'text_data',
						   data.frame(
						   		qda_id = character(),
						   		qda_text = character(),
						   		date_added = character()
						   ))
		qda_data$log(Sys.info()['user'], 'text_data', 'created table')
	}

	# add_text
	#
	# @param df data frame containing the text
	# @param id_col name of the primary key column in the data frame.
	# @param text_col name of the column containing the text to be coded.
	# @param overwrite if TRUE existing data will be overwritten.
	#        See [DBI::dbWriteTable()] for more info.
	qda_data$add_text <- function(df, id_col, text_col, overwrite = FALSE) {
		if(missing(id_col)) {
			stop('id_col parameter is required. This should be a primary key.')
		}
		if(missing(text_col)) {
			stop('text_col parameter is required.')
		}
		if(length(unique(df[,id_col,drop=TRUE])) != nrow(df)) {
			stop('id_col must be a primary key. That is, no duplicate values.')
		}

		df <- df |>
			dplyr::rename(qda_id = id_col,
						  qda_text = text_col) |>
			dplyr::mutate(qda_id = as.character(qda_id),
						  # qda_text = stringr::str_squish(qda_text),
						  qda_text = stringr::str_remove_all(qda_text, '  '),
						  date_added = as.character(Sys.time()))
		DBI::dbWriteTable(qda_db,
						  'text_data',
						  df,
						  overwrite = overwrite,
						  append = !overwrite)
		qda_data$log(Sys.info()['user'], 'text_data', paste0('added ', nrow(df), ' rows to text_data'))
	}

	# get_text
	qda_data$get_text <- function(id) {
		if(missing(id)) {
			DBI::dbReadTable(qda_db, 'text_data')
		} else {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM text_data WHERE qda_id = "', id, '"')
			)
		}
	}

	##### codings ##############################################################
	# create table
	if(!'codings' %in% tables) {
		DBI::dbCreateTable(qda_db,
					   'codings',
					   data.frame(
					   		coding_id = integer(),
					   		qda_id = character(),
					   		text = character(),
					   		start = integer(),
					   		end = integer(),
					   		codes = character(),
					   		coder = character(),
					   		date_added = character()) )
		qda_data$log(Sys.info()['user'], 'codings', 'created table')
	}

	# add_codings
	# @return the id for the newly inserted coding.
	qda_data$add_coding <- function(id, text = NA, start = NA, end = NA, codes = NA, coder = Sys.info()['user']) {
		codings <- DBI::dbGetQuery(
			qda_db,
			'SELECT coding_id FROM codings')
		coding_id <- ifelse(nrow(codings > 0),
							max(codings$coding_id) + 1,
							1)
		codes_table <- qda_data$get_codes()
		missing_codes <- codes[!codes %in% codes_table$code]
		if(length(missing_codes) > 0) {
			qda_data$add_codes(missing_codes)
		}
		new_row <- data.frame(
			coding_id = coding_id,
			qda_id = id,
			text = text,
			start = start,
			end = end,
			codes = do.call(paste0, list(codes, collapse = ';')),
			coder = coder,
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(qda_db,
						  'codings',
						  new_row,
						  append = TRUE)
		qda_data$log(Sys.info()['user'], 'codings', paste0(new_row[1,], collapse = ', '))
		return(coding_id)
	}

	# update_coding
	# @param coding_id coding id
	# @param codes codes to update
	qda_data$update_coding <- function(coding_id, codes) {
		DBI::dbExecute(
			qda_db,
			paste0('UPDATE codings SET codes = "',
				   do.call(paste0, list(codes, collapse = ';')),
				   '" WHERE coding_id = "', coding_id, '"')
		)
		qda_data$log(Sys.info()['user'], 'codings', paste0('updated coding_id = ', coding_id))
	}

	# delete_coding
	# @param id coding id
	qda_data$delete_coding <- function(coding_id) {
		if(missing(coding_id)) {
			stop('Must specify code coding_id')
		}
		query <- paste0('DELETE FROM codings WHERE coding_id = "', coding_id, '" ')
		DBI::dbExecute(qda_db, query)
		qda_data$log(Sys.info()['user'], 'codings', query)
	}

	# get_codings
	#
	# @param id the text
	# @param code_id the id for the coding
	qda_data$get_codings <- function(id, coding_id) {
		codings <- data.frame()
		if(missing(id) & missing(coding_id)) {
			codings <- DBI::dbReadTable(qda_db, 'codings')
		} else if(missing(coding_id)) {
			codings <- DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM codings WHERE qda_id = "', id, '"')
			)
		} else if(missing(id)) {
			codings <- DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM codings WHERE coding_id = "', coding_id, '"')
			)
		} else {
			codings <- DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM codings WHERE coding_id = "', coding_id, '" AND qda_id = "', id, '"')
			)
		}
		if(nrow(codings) > 0) {
			# There was a bug where adding a code could have been called more than
			# once probably because the user double clicked the "Add" button.
			# The button is now disabled on the first click within the "shiny_server.R"
			# script. In case duplicats still get through, or for legacy apps, this
			# will remove the duplicate codings.
			dups <- duplicated(codings[,c('qda_id', 'text', 'codes', 'coder')])
			if(sum(dups) > 0) {
				message('There were duplicate codings found. They will be removed before returning.')
				codings <- codings[!dups,]
			}
		}
		return(codings)
	}

	##### codes ################################################################
	# create table
	if(!'codes' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'codes',
						   data.frame(
						   		code = character(),
						   		color = character(),
						   		description = character(),
						   		parent = character(),
						   		date_added = character()) )
		qda_data$log(Sys.info()['user'], 'codes', 'created table')
	}

	# get_codes
	qda_data$get_codes <- function() {
		DBI::dbReadTable(qda_db, 'codes')
	}

	# add_codes
	qda_data$add_codes <- function(codes, colors, descriptions) {
		codes_table <- DBI::dbReadTable(qda_db, 'codes')
		if(missing(colors)) {
			new_colors <- get_colors()
			colors <- new_colors[seq_len(length(codes)) + nrow(codes_table) %% length(new_colors)]
		}
		if(missing(descriptions)) {
			descriptions <- rep(NA_character_, length(codes))
		}
		new_rows <- data.frame(
			code = codes,
			color = colors,
			description = descriptions,
			parent = NA,
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(
			qda_db,
			'codes',
			new_rows,
			append = TRUE
		)
		qda_data$log(Sys.info()['user'], 'codes', paste0('added ', paste0(new_rows, collapse = ', ')))
		invisible(new_rows)
	}

	# update_code
	qda_data$update_code <- function(code, color, description, parent) {
		new_vals <- c()
		if(!missing(color)) {
			new_vals <- c(new_vals, paste0('color = "', color, '"'))
		}
		if(!missing(description)) {
			new_vals <- c(new_vals, paste0('description = "', description, '"'))
		}
		if(!missing(parent)) {
			new_vals <- c(new_vals, paste0('parent = "', parent, '"'))
		}
		query <- paste0( 'UPDATE codes SET ',
						 # do.call(paste0, list(new_vals, collapse = ',')),
						 paste0(new_vals, collapse = ','),
						 ' WHERE code = "', code, '"')
		DBI::dbExecute(qda_db, query)
		qda_data$log(Sys.info()['user'], 'codes', query)
	}

	###### code_questions ######################################################
	# create table
	if(!'code_questions' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'code_questions',
						   data.frame(
						   		stem = character(),
						   		type = character(),
						   		order = integer(),
						   		options = character(),
						   		date_added = character()) )
		qda_data$log(Sys.info()['user'], 'code_questions', 'created table')
	}

	# add_code_question
	qda_data$add_code_question <- function(stem,
										   type = c('text', 'radio', 'checkbox'),
										   order,
										   options) {
		if(missing(order)) {
			code_questions <- DBI::dbReadTable(qda_db, 'code_questions')
			new_order <- ifelse(nrow(code_questions) > 0,
								max(code_questions$order + 1),
								1)
		} else {
			new_order <- order
		}
		new_row <- data.frame(
			stem = stem,
			type = type[1],
			order = new_order,
			options = ifelse(missing(options), '', paste0(options, collapse = ';')),
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(qda_db, 'code_questions', new_row, append = TRUE)
		qda_data$log(Sys.info()['user'], 'code_questions', paste0(new_row[1,], collapse = ', '))
	}

	qda_data$delete_code_question <- function(stem) {
		query <- paste0('DELETE FROM code_questions WHERE ',
						'stem = "', stem, '" ')
		DBI::dbExecute(qda_db, query)
		qda_data$log(Sys.info()['user'], 'code_questions', query)
	}

	# get_code_questions
	qda_data$get_code_questions <- function() {
		df <- DBI::dbReadTable(qda_db, 'code_questions')
		df[order(df$order),]
	}

	##### code_question_responses ##############################################
	# create table
	if(!'code_question_responses' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'code_question_responses',
						   data.frame(
						   		coding_id = numeric(),
						   		stem = character(),
						   		answer = character(),
						   		coder = character(),
						   		date_added = character()) )
		qda_data$log(Sys.info()['user'], 'code_questions_responses', 'created table')
	}

	# get_code_question_responses
	qda_data$get_code_question_responses <- function(coding_id, id) {
		if(!missing(id)) {
			codings <- qda_data$get_codings(id = id)
			DBI::dbReadTable(qda_db, 'code_question_responses') |>
				dplyr::filter(coding_id %in% codings$coding_id)
		} else if(!missing(coding_id)) {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM code_question_responses WHERE coding_id = "', coding_id, '"')
			)
		} else {
			DBI::dbReadTable(qda_db, 'code_question_responses')
		}
	}

	# delete_code_question_responses
	# @param id the coding id
	qda_data$delete_code_question_responses <- function(coding_id) {
		if(missing(coding_id)) {
			stop('Must specify code coding_id')
		}
		query <- paste0('DELETE FROM code_question_responses WHERE ',
						'coding_id = "', coding_id, '" ')
		DBI::dbExecute(qda_db, query )
		qda_data$log(Sys.info()['user'], 'code_questions_responses', query)
	}

	# add_code_question_response
	# @param code_id the id from the codings table.
	qda_data$add_code_question_response <- function(coding_id, stem, answer, coder = Sys.info()['user']) {
		new_row <- data.frame(
			coding_id = coding_id,
			stem = stem,
			answer = answer,
			coder = coder,
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(qda_db, 'code_question_responses', new_row, append = TRUE)
		qda_data$log(coder, 'code_questions_responses', paste0(new_row[1,], collapse = ', '))
	}

	qda_data$update_code_question_response <- function(coding_id, stem, answer, coder) {
		if(missing(coding_id)) {
			stop('coding_id parameter is required')
		}

		new_vals <- c()
		if(!missing(stem)) {
			new_vals <- c(new_vals, paste0('stem = "', stem, '"'))
		}
		if(!missing(answer)) {
			new_vals <- c(new_vals, paste0('answer = "', answer, '"'))
		}
		if(!missing(coder)) {
			new_vals <- c(new_vals, paste0('coder = "', coder, '"'))
		}
		query <- paste0( 'UPDATE code_question_responses SET ',
						 paste0(new_vals, collapse = ', '),
						 ' WHERE coding_id = "', coding_id, '"')
		DBI::dbExecute(qda_db, query)
		qda_data$log(Sys.info()['user'], 'code_questions_responses', query)
	}

	##### text_questions #######################################################
	# create table
	if(!'text_questions' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'text_questions',
						   data.frame(
						   		stem = character(),
						   		type = character(),
						   		order = integer(),
						   		options = character(),
						   		date_added = character()) )
		qda_data$log(Sys.info()['user'], 'text_questions', 'created table')
	}

	# add_text_question
	qda_data$add_text_question <- function(stem, type, order, options) {
		if(missing(order)) {
			text_questions <- DBI::dbReadTable(qda_db, 'text_questions')
			new_order <- ifelse(nrow(text_questions) > 0,
								max(text_questions$order + 1),
								1)
		} else {
			new_order <- order
		}
		new_row <- data.frame(
			stem = stem,
			type = type[1],
			order = new_order,
			options = ifelse(missing(options), '', paste0(options, collapse = ';')),
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(qda_db, 'text_questions', new_row, append = TRUE)
		qda_data$log(Sys.info()['user'], 'text_questions', paste0(new_row[1,], collapse = ', '))
	}

	# get_text_questions
	qda_data$get_text_questions <- function() {
		df <- DBI::dbReadTable(qda_db, 'text_questions')
		df[order(df$order),]
	}

	qda_data$delete_text_question <- function(stem) {
		query <- paste0('DELETE FROM text_questions WHERE ',
						'stem = "', stem, '" ')
		DBI::dbExecute(qda_db, query)
		qda_data$log(Sys.info()['user'], 'text_questions', query)
	}

	##### text_question_responses ##############################################
	# create table
	if(!'text_question_responses' %in% tables) {
		DBI::dbCreateTable(qda_db,
					   'text_question_responses',
					   data.frame(
					   		qda_id = character(),
					   		stem = character(),
					   		answer = character(),
					   		coder = character(),
					   		date_added = character()) )
		qda_data$log(Sys.info()['user'], 'text_questions_responses', 'created table')
	}

	# get_text_question_responses
	# @param id text id
	# @param coder the coder who entered the answers
	qda_data$get_text_question_responses <- function(id, coder) {
		if(missing(id) & missing(coder)) {
			DBI::dbReadTable(qda_db, 'text_question_responses')
		} else if(missing(coder)) {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM text_question_responses WHERE qda_id = "', id, '"')
			)
		} else if(missing(id)) {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM text_question_responses WHERE coder = "', coder, '"')
			)
		} else {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM text_question_responses WHERE qda_id = "', id, '" AND coder = "', coder, '"')
			)
		}
	}

	# delete_text_question_responses
	# @param id the coding id
	qda_data$delete_text_question_responses <- function(id, coder = Sys.info()['user']) {
		if(missing(id)) {
			stop('Must specify id')
		}
		query <- paste0('DELETE FROM text_question_responses WHERE ',
						'qda_id = "', id, '" AND coder = "', coder, '"')
		DBI::dbExecute(qda_db, query)
		qda_data$log(coder, 'text_questions_responses', query)
	}

	# update_text_question_response
	qda_data$update_text_question_response <- function(id, stem, new_answer, coder = Sys.info()['user']) {
		query <- paste0('UPDATE text_question_responses SET answer = "', new_answer,
						'" WHERE qda_id ="', id, '" AND stem = "', stem, '"')
		DBI::dbExecute(qda_db, query)
		qda_data$log(coder, 'text_question_responses', query)
	}

	# add_text_question_response
	qda_data$add_text_question_response <- function(id, stem, answer, coder = Sys.info()['user']) {
		new_row <- data.frame(
			qda_id = id,
			stem = stem,
			answer = answer,
			coder = coder,
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(qda_db, 'text_question_responses', new_row, append = TRUE)
		qda_data$log(coder, 'text_questions_responses', paste0(new_row, collapse = ', '))
	}

	##### rubrics ##############################################################
	# create table
	if(!'rubrics' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'rubrics',
						   data.frame(
						   		rubric_name = character(),
						   		n_scoring_levels = integer(),
						   		scoring_levels = character(),
						   		description = character(),
						   		enabled = logical(),
						   		date_added = character()
						   ))
	}

	if(!'rubric_criteria' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'rubric_criteria',
						   data.frame(
						   		rubric_name = character(),
						   		criteria = character(),
						   		scoring_level = integer(),
						   		description = character(),
						   		date_added = character()
						   	))
	}

	qda_data$get_rubrics <- function() {
		DBI::dbReadTable(qda_db, 'rubrics')
	}

	qda_data$get_rubric <- function(rubric_name) {
		query <- paste0(
			'SELECT * FROM rubric_criteria WHERE rubric_name = "', rubric_name, '"'
		)
		DBI::dbGetQuery(qda_db, query)
	}

	# add_rubric
	qda_data$add_rubric <- function(
		rubric_name,
		description = NA,
		rubric,
		scoring_levels = names(rubric)[2:ncol(rubric)],
		enabled = TRUE
	) {
		# rubric_tab <- get_rubrics()

		rubrics_new_row <- data.frame(
			rubric_name = rubric_name,
			n_scoring_levels = length(scoring_levels),
			scoring_levels = paste0(scoring_levels, collapse = ';'),
			description = description,
			enabled = enabled,
			date_added = as.character(Sys.time())
		)

		rubric_criteria_new_row <- reshape2::melt(rubric, id.var = names(rubric)[1])
		names(rubric_criteria_new_row)[1:3] <- c('criteria', 'scoring_level', 'description')
		rubric_criteria_new_row$rubric_name <- rubric_name
		rubric_criteria_new_row$date_added <- as.character(Sys.time())
		rubric_criteria_new_row <- rubric_criteria_new_row[,c('rubric_name', 'criteria', 'scoring_level', 'description', 'date_added')]
		rubric_criteria_new_row$scoring_level <- factor(as.character(rubric_criteria_new_row$scoring_level),
														levels = scoring_levels,
														ordered = TRUE) |>
			as.integer() - 1

		DBI::dbWriteTable(qda_db, 'rubrics', rubrics_new_row, append = TRUE)
		qda_data$log(Sys.info()['user'], 'rubrics', paste0('Added new rubric ', rubric_name))
		DBI::dbWriteTable(qda_db, 'rubric_criteria', rubric_criteria_new_row, append = TRUE)
		qda_data$log(Sys.info()['user'], 'rubric_criteria', paste0('Added new rubric ', rubric_name))
	}

	qda_data$update_rubric_criteria <- function(rubric_name,
												criteria,
												score_level,
												description) {
		query <- paste0('UPDATE rubric_criteria SET ',
						'description = "', description, '" WHERE ',
						'rubric_name = "', rubric_name, '" AND ',
						'criteria = "', criteria, '" AND ',
						'scoring_level = "', as.character(score_level), '"')
		DBI::dbExecute(qda_db, query)
		qda_data$log(Sys.info()['user'], 'update_rubric_criteria', query)
	}

	if(!'rubric_responses' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'rubric_responses',
						   data.frame(
						   		rubric_name = character(),
						   		qda_id = character(),
						   		coder = character(),
						   		criteria = character(),
						   		score = integer(),
						   		date_added = character()
						   ))
	}

	qda_data$delete_rubric_response <- function(rubric_name,
												qda_id,
												coder,
												criteria) {
		query <- paste0('DELETE FROM rubric_responses WHERE ',
						'qda_id = "', qda_id, '" AND ',
						'coder = "', coder, '" AND ',
						'rubric_name = "', rubric_name, '" AND ',
						'criteria = "', criteria, '"')
		DBI::dbExecute(qda_db, query)
		qda_data$log(coder, 'delete_rubric_response', query)
	}

	qda_data$add_rubric_response <- function(rubric_name,
											 qda_id,
											 coder,
											 criteria,
											 score) {
		rubric_response_new_row <- data.frame(
			rubric_name = rubric_name,
			qda_id = qda_id,
			coder = coder,
			criteria = criteria,
			score = score,
			date_added = as.character(Sys.time())
		)
		DBI::dbWriteTable(qda_db, 'rubric_responses', rubric_response_new_row, append = TRUE)
		qda_data$log(coder, 'rubric_responses', paste0('Added new rubric response ',
													   rubric_name, '; ', qda_id, '; ',
													   criteria, ' = ', score))
	}

	qda_data$get_rubric_responses <- function(rubric_name, qda_id, coder) {
		DBI::dbGetQuery(
			qda_db,
			paste0('SELECT * FROM rubric_responses WHERE ',
				   'rubric_name = "', rubric_name, '" AND ',
				   'qda_id = "', qda_id, '" AND ',
				   'coder = "', coder, '"')
		)
	}

	##### assignments ##########################################################
	# create table
	if(!'assignments' %in% tables) {
		DBI::dbCreateTable(qda_db,
						   'assignments',
						   data.frame(
						   		qda_id = character(),
						   		coder = character(),
						   		date_added = character()
						   ))
		qda_data$log(Sys.info()['user'], 'assignments', 'created table')
	}

	# get_assignments
	# @param coder the coder.
	# @param id id of the text
	qda_data$get_assignments <- function(coder, id) {
		if(!missing(id) & !missing(coder)) {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM assignments WHERE qda_id = "', id, '" AND coder = "', coder, '"')
			)
		} else if(!missing(coder)) {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM assignments WHERE coder = "', coder, '"')
			)
		} else if(!missing(id)) {
			DBI::dbGetQuery(
				qda_db,
				paste0('SELECT * FROM assignments WHERE qda_id = "', id, '"')
			)
		} else {
			DBI::dbReadTable(qda_db, 'assignments')
		}
	}

	##### Shiny Manager ########################################################
	# credentials_db <- DBI::dbConnect(RSQLite::SQLite(), 'users.sqlite')
	# DBI::dbListTables(credentials_db)


	if(!'credentials' %in% tables) {
		if(users[1] == 'admin') {
			warning(paste0('Creating default user admin with password "',
						   users_passwords[1],
						   '". Recommend changing the password upon first login.'))
		}
		credentials <- data.frame(
			user = users,
			name = users_names,
			password = users_passwords,
			start = as.character(Sys.Date()),
			expire = NA,
			admin = users_is_admin,
			email = "",
			comment = 'ShinyQDA coders.',
			stringsAsFactors = TRUE)
		credentials <- shinymanager::create_db(
			credentials_data = credentials,
			sqlite_path = file,
			passphrase = users_passphrase
		)

	}

	qda_data$get_coders <- function() {
		shinymanager::read_db_decrypt(
			qda_data$db_conn,
			name = 'credentials',
			passphrase = users_passphrase) |> dplyr::select(!password)
	}

	qda_data$methods_docs <- methods_docs

	class(qda_data) <- 'qda'
	return(qda_data)
}
