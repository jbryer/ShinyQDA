#' Create a new ShinyQDA application
#'
#' @param name name of the application directory.
#' @param dir directory where the application will be created.
#' @param qda_data a data.frame with the text data.
#' @param id_column the name of the column in `qda_data` for the primary key.
#' @param text_column the name of the column in `qda_data` containing the text data.
#' @param initialize_sentiment_dictionaries whether to copy the sentiment
#'        dictionaries into the app directory.
#' @param run_app if TRUE the Shiny application will be started once it has been created.
#' @importFrom rstudioapi selectDirectory
#' @importFrom utils file.edit
#' @returns Returns a [ShinyQDA::qda()] object.
#' @export
new_app <- function(
		name = 'My_ShinyQDA',
		dir = ifelse(interactive(),
					 rstudioapi::selectDirectory(caption = 'Select location for ShinyQDA application'),
					 getwd()),
		qda_data,
		id_column = find_primary_key_column(qda_data),
		text_column = find_document_column(qda_data),
		initialize_sentiment_dictionaries = TRUE,
		run_app = interactive()
) {
	if(missing(qda_data) | is.null(id_column) | is.null(text_column)) {
		stop('Must provide a data.frame with a primary key and document column.')
	}

	app_dir <- paste0(dir, '/', name)
	dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

	if(file.exists(paste0(app_dir, '/app.R'))) {
		stop(paste0('Shiny application already exists in ', app_dir))
	}

	pkg_path <- find.package('ShinyQDA')
	app_template <- paste0(pkg_path, '/shiny_template/app.R')
	if(!file.exists(app_template)) {
		stop('Could not find the ShinyQDA template.')
	}

	if(!file.copy(app_template, paste0(app_dir, '/app.R'))) {
		stop("Could not copy the ShinyQDA template.")
	}

	if(initialize_sentiment_dictionaries) {
		tmp <- textdata::lexicon_afinn(dir = app_dir)
		tmp <- textdata::lexicon_bing(dir = app_dir)
		tmp <- textdata::lexicon_loughran(dir = app_dir)
		tmp <- textdata::lexicon_nrc(dir = app_dir)
	}

	qda <- qda(paste0(app_dir, '/qda.sqlite'))
	qda$add_text(df = qda_data,
				 id_col = id_column,
				 text_col = text_column,
				 overwrite = TRUE
	)

	# utils::file.edit(paste0(app_dir, '/app.R'))
	# invisible(qda)

	message(paste0('You can run your application with the following command:\n',
				   'shiny::runApp("', app_dir, '"\n',
				   'Note the default login is admin/pass. You should change that after logging in.'))

	if(run_app) {
		shiny::runApp(app_dir)
	}
}

#' Finds the first column that can be used as the primary key.
#'
#' This function loops through the given data.frame and returns the name of the
#' first column it finds that could be used as a primary key. That is, the first
#' column where the length of unique values equals the number of rows.
#'
#' @param df a data.frame to search for a primary key.
#' @return the name of the first column that could be a primary key or NULL if one could not be found.
find_primary_key_column <- function(df) {
	if(!is.data.frame(df)) {
		stop('df must be a data.frame.')
	}
	for(i in seq_len(ncol(df))) {
		if(length(unique(df[,i,drop=TRUE])) == nrow(df)) {
			return(names(df)[i])
		}
	}
	return(NULL)
}

#' Find the column with the longest character string.
#'
#' @param df a data.frame to search for the document column.
#' @return the name of the column that is likely to contain the documents (i.e. text).
#' @export
find_document_column <- function(df) {
	if(!is.data.frame(df)) {
		stop('df must be a data.frame.')
	}
	string_length <- integer(length = ncol(df))
	for(i in seq_len(ncol(df))) {
		if(is.character(df[,i,drop=TRUE])) {
			string_length[i] <- max(nchar(df[,i,drop=TRUE]))
		} else {
			string_length[i] <- -1
		}
	}
	longest_string <- max(string_length)
	if(longest_string > 0) {
		return(names(df)[ which(longest_string == string_length)[1] ])
	} else {
		return(NULL)
	}
}
