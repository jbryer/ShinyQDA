#' Data object for qualitative data analysis
#'
#' @export
qda <- function(
		file,
		df,
		id_col = names(df)[1],
		text_col = names(df)[2],
		codes = character(),
		auto_save = TRUE
) {

	if(!missing(file) & file.exists(file)) {
		message(paste0('Reading QDA from ', file, '...'))
		qda_data <- readRDS(file)
		return(qda_data)
	}

	message('Creating a new QDA object...')
	qda_data <- list(
		df = df,
		file = file,
		id_col = id_col,
		text_col = text_col,
		codes = codes,
		code_colors = color_palette[seq_len(length(codes)) %% length(color_palette)],
		categories = list(),
		codings = data.frame(
			id = character(),
			text = character(),
			codes = character(),
			memos = character()
		),
		auto_save = auto_save
	)

	if(length(qda_data$codes) > length(color_palette)) {
		warning("There are more codes than colors in the default palette. Some codes will have the same color.")
	}

	qda_data$add_coding <- function(id, text = NA, codes = NA, memo = NA) {
		new_codings <- rbind(
			qda_data$codings,
			data.frame(id = id,
					   text = text,
					   codes = paste0(codes, collapse=';'),
					   memo = memo)
		)
		assign('codings', new_codings, envir = qda_data)
		if(qda_data$auto_save) {
			qda_data$save()
		}
		invisible(TRUE)
	}

	qda_data$save <- function(file = qda_data$file) {
		message(paste0('Saving ', file, '...'))
		saveRDS(qda_data, file)
	}

	qda_data <- list2env(qda_data)

	class(qda_data) <- 'qda'

	if(qda_data$auto_save) {
		qda_data$save()
	}

	return(qda_data)
}
