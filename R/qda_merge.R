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
	if(nrow(codings) > 0) {
		codings <- codings[nchar(codings$codes) > 0,]
	}
	if(nrow(codings) == 0) {
		return(data.frame())
	}
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
#' @param sentiment_dir if specified, the sentiment lexicon will be loaded
#'        from the given directory.
#' @param include_code_counts include code counts in returned data.frame.
#' @param include_codes include columns for each code. Each column will start with `code_`.
#' @param include_text_counts include number of characters, words, sentences, and paragraphs.
#' @param include_text_questions include responses to text questions in returned data.frame.
#' @param include_code_questions include code question responses in returned data.frame.
#' @param include_sentiments include sentiment analysis in returend data.frame.
#' @param include_bing_sentiment include Bing sentiment analysis in returned
#'        data.frame. Columns will begin with `bing_`.
#' @param include_nrc_sentiment include NRC sentiment analysis in returned
#'        data.frame. Columns will begin with `nrc_`.
#' @param include_loughran_sentiment include Loughran sentiment analysis in
#'        returned data.frame. Columns will begin with `loughran_`.
#' @param include_afinn_sentiment include AFINN sentiment analysis in returned
#'        data.frame. Columns will begin with `afinn_`.
#' @param ... parameters passed to other functions.
#' @export
qda_merge <- function(qda_data,
					  include_code_counts = TRUE,
					  include_codes = TRUE,
					  include_text_counts = TRUE,
					  include_text_questions = TRUE,
					  include_code_questions = TRUE,
					  sentiment_dir,
					  include_sentiments = FALSE,
					  include_bing_sentiment = include_sentiments,
					  include_nrc_sentiment = include_sentiments,
					  include_loughran_sentiment = include_sentiments,
					  include_afinn_sentiment = include_sentiments,
					  ...) {
	df <- qda_data$get_text()

	if(nrow(df) == 0) {
		warning('No text data found.')
		return(data.frame())
	}

	tab <- df

	codes_table <- get_coding_table(qda_data)

	# n_codes by coder
	if(nrow(codes_table) > 0 & include_code_counts) {
		codes_table_aggr <- cbind(codes_table[,1:2],
							 n_codes = apply(codes_table[,3:ncol(codes_table)], 1, sum))
		tab <- merge(tab, codes_table_aggr, by = c('qda_id'), all.x = TRUE)
	}

	# Add a column for each code
	if(nrow(codes_table) > 0 & include_codes) {
		names(codes_table) <- gsub(' ', '_', names(codes_table))
		names(codes_table)[3:ncol(codes_table)] <- paste0('code_', names(codes_table)[3:ncol(codes_table)])
		tab <- merge(tab, codes_table, by = c('qda_id', 'coder'), all.x = TRUE)
	}

	# Number of highlights (i.e. codes per essay by coder)
	codings <- qda_data$get_codings()
	if(nrow(codings) > 0) {
		highlights_table <- table(codings$qda_id) |>
			as.data.frame() |>
			dplyr::rename(n_highlights = Freq,
						  qda_id = Var1)
		tab <- merge(tab, highlights_table, by = 'qda_id', all.x = TRUE)
	}

	# Include character, word, sentence, and paragraph counts
	if(include_text_counts) {
		tab$n_paragraphs <- stringr::str_count(tab$qda_text, '[^\r\n]+')
		tab$n_sentences <- sapply(tab$qda_text, function(x) {
			length(gregexpr('[[:alnum:] ][.!?]', x)[[1]])
		})
		tab$n_words <- stringr::str_count(tab$qda_text, '\\w+')
		tab$n_characters <- nchar(tab$qda_text)
	}

	##### Sentiment Analysis ###################################################
	if(include_nrc_sentiment) {
		if(missing(sentiment_dir)) {
			nrc <- tidytext::get_sentiments("nrc")
		} else {
			nrc <- textdata::lexicon_nrc(dir = sentiment_dir)
		}

		tokens <- df |>
			dplyr::select(qda_id, qda_text) |>
			dplyr::mutate(qda_text = stringr::str_remove_all(qda_text, '  ')) |>
			tidytext::unnest_tokens(token, qda_text,
									token = 'words',
									to_lower = FALSE,
									strip_punct = FALSE) |>
			dplyr::mutate(token_lower = tolower(token)) |>
			dplyr::left_join(nrc, by = c('token_lower' = 'word'), multiple = 'all') |>
			dplyr::select(qda_id, sentiment) |>
			reshape2::dcast(qda_id ~ sentiment, fun.aggregate = length, value.var = 'sentiment') |>
			dplyr::select(!`NA`) |>
			dplyr::rename_with(~ paste0('nrc_', .x), -c(1))
		tab <- merge(tab, tokens, by = 'qda_id', all.x = TRUE)
	}

	if(include_bing_sentiment) {
		if(missing(sentiment_dir)) {
			bing <- tidytext::get_sentiments("bing")
		} else {
			bing <- textdata::lexicon_bing(dir = sentiment_dir)
		}

		tokens <- df |>
			dplyr::select(qda_id, qda_text) |>
			dplyr::mutate(qda_text = stringr::str_remove_all(qda_text, '  ')) |>
			tidytext::unnest_tokens(token, qda_text,
									token = 'words',
									to_lower = FALSE,
									strip_punct = FALSE) |>
			dplyr::mutate(token_lower = tolower(token)) |>
			dplyr::left_join(bing, by = c('token_lower' = 'word'), multiple = 'all') |>
			dplyr::select(qda_id, sentiment) |>
			reshape2::dcast(qda_id ~ sentiment, fun.aggregate = length, value.var = 'sentiment') |>
			dplyr::select(!`NA`) |>
			dplyr::rename_with(~ paste0('bing_', .x), -c(1)) |>
			dplyr::mutate(bing_total = (-1 * bing_negative + bing_positive) / (bing_negative + bing_positive))
		tab <- merge(tab, tokens, by = 'qda_id', all.x = TRUE)
	}

	if(include_afinn_sentiment) {
		if(missing(sentiment_dir)) {
			afinn <- tidytext::get_sentiments("afinn")
		} else {
			afinn <- textdata::lexicon_afinn(dir = sentiment_dir)
		}
		tokens <- df |>
			dplyr::select(qda_id, qda_text) |>
			dplyr::mutate(qda_text = stringr::str_remove_all(qda_text, '  ')) |>
			tidytext::unnest_tokens(token, qda_text,
									token = 'words',
									to_lower = FALSE,
									strip_punct = FALSE) |>
			dplyr::mutate(token_lower = tolower(token)) |>
			dplyr::left_join(afinn, by = c('token_lower' = 'word'), multiple = 'all') |>
			dplyr::select(qda_id, value) |>
			dplyr::group_by(qda_id) |>
			dplyr::summarise(afinn_sentiment = mean(value, na.rm = TRUE))
		tab <- merge(tab, tokens, by = 'qda_id', all.x = TRUE)
	}

	if(include_loughran_sentiment) {
		if(missing(sentiment_dir)) {
			loughran <- tidytext::get_sentiments("loughran")
		} else {
			loughran <- textdata::lexicon_loughran(dir = sentiment_dir)
		}
		tokens <- df |>
			dplyr::select(qda_id, qda_text) |>
			dplyr::mutate(qda_text = stringr::str_remove_all(qda_text, '  ')) |>
			tidytext::unnest_tokens(token, qda_text,
									token = 'words',
									to_lower = FALSE,
									strip_punct = FALSE) |>
			dplyr::mutate(token_lower = tolower(token)) |>
			dplyr::left_join(loughran, by = c('token_lower' = 'word'), multiple = 'all') |>
			dplyr::select(qda_id, sentiment) |>
			reshape2::dcast(qda_id ~ sentiment, fun.aggregate = length, value.var = 'sentiment') |>
			dplyr::select(!`NA`) |>
			dplyr::rename_with(~ paste0('loughran_', .x), -c(1))
		tab <- merge(tab, tokens, by = 'qda_id', all.x = TRUE)
	}
	##### End Sentiment analysis ###############################################

	# Add text questions
	if(include_text_questions) {
		df_text <- question_responses_to_data_frame(
			questions = qda_data$get_text_questions(),
			responses = qda_data$get_text_question_responses(),
			id_col = 'qda_id'
		)
		if(nrow(df_text) > 0) {
			names(df_text) <- make.names(names(df_text))
			tab <- merge(tab, df_text, by = c('qda_id', 'coder'), all.x = TRUE)
		}
	}

	# TODO: Add code questions
	if(include_code_questions) {
		df_code <- question_responses_to_data_frame(
			questions = qda_data$get_code_questions(),
			responses = qda_data$get_code_question_responses(),
			id_col = 'coding_id'
		)
		codings <- qda_data$get_codings()
		df_code <- merge(codings[,c('qda_id', 'coding_id')],
						 df_code,
						 by = 'coding_id')
		aggr_cols <- sapply(df_code, class) == 'logical'
		aggr_cols <- names(aggr_cols)[aggr_cols]
		df_code2 <- df_code[,c('qda_id', 'coder', aggr_cols)]
		tmp <- df_code2 |>
			dplyr::group_by(qda_id, coder) |>
			dplyr::summarise(dplyr::across(dplyr::everything(), function(x) { sum(x, na.rm = TRUE) }), .groups = 'keep')
		names(tmp) <- make.names(names(tmp))
		tab <- merge(tab, tmp, by = c('qda_id', 'coder'), all.x = TRUE)
	}

	return(tab)
}

#' Utility function to convert code and text questions to a wide data.frame.
#'
question_responses_to_data_frame <- function(questions, responses, id_col) {
	unique_responses <- responses[!duplicated(responses[,c(id_col,'coder')]),c(id_col,'coder')]
	df_code <- data.frame(id = unique_responses[,id_col],
						  coder = unique_responses$coder)
	row.names(df_code) <- paste0(df_code$id, '_', df_code$coder)
	for(i in seq_len(nrow(questions))) {
		stem <- questions[i,]$stem
		type <- questions[i,]$type
		if(type == 'checkbox' | type == 'radio') {
			options <- strsplit(questions[i,]$options, ';', fixed = TRUE)[[1]]
			for(j in seq_len(length(options))) {
				tmp <- responses |> dplyr::filter(stem == !!stem)
				col <- paste0(stem, '_', options[j])
				# df_code[paste0(tmp[,id_col], '_', tmp$coder), col] <- as.logical(NA)
				df_code[,col] <- as.logical(NA)
				ans <- grep(options[j], tmp$answer)
				if(length(ans) > 0) {
					df_code[ans,col] <- TRUE
				}
			}
		} else {
			tmp <- responses |> dplyr::filter(stem == !!stem)
			df_code[paste0(tmp[,id_col], '_', tmp$coder),stem] <- tmp$answer
		}
	}
	row.names(df_code) <- NULL
	names(df_code)[1] <- id_col
	return(df_code)
}
