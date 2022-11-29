#' Returns a table where each row represents a text coded by a coder and each
#' column represents the number of codes present by that coder.
#'
#' @param qda_data a [ShinyQDA::qda()] object.
#' @param aggregate_fun function used to aggregate multiple tags per text per
#'        per coder. See [reshape2::dcast()] for more information.
#' @export
#' @importFrom dplyr bind_rows rename
#' @importFrom reshape2 dcast
get_coding_table <- function(qda_data, aggregate_fun = sum) {
	# Merge in codings
	codings <- qda_data$get_codings()
	codings$code <- strsplit(codings$codes, split = ';')
	tmp <- apply(codings, 1, FUN = function(x) {
		x[c('qda_id', 'coder', 'code')]
	})
	codes <- dplyr::bind_rows(lapply(tmp, as.data.frame.list))
	codes <- table(codes$coder, codes$qda_id, codes$code) |>
		as.data.frame() |>
		dplyr::rename(qda_id = Var2, coder = Var1, code = Var3) |>
		reshape2::dcast(qda_id + coder ~ code,
						fun.aggregate = aggregate_fun,
						value.var = 'Freq')
	return(codes)
}

#' Merges codings with the text data.
#'
#' @param qda_data a [ShinyQDA::qda()] object.
#' @param aggregate_fun function used to aggregate multiple tags per text per
#'        per coder. See [reshape2::dcast()] for more information.
#' @export
qda_merge <- function(qda_data, ..) {
	df <- qda_data$get_text()
	df <- merge(df, codes, by = 'qda_id', all = TRUE)
	# TODO: Merge in code questions
	code_questions <- qda_data$get_code_question_responses()
	codes <- get_code_table(qda_data, ...)
	# TODO: Merge in text questions
	text_questions <- qda_data$get_code_questions()
	return(df)
}
