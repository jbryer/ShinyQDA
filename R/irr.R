#' Calculate inter-rater reliability statistics
#'
#' This function will calculate inter-rater reliability statistics for each code.
#' The resulting data.frame will have the following columns:
#'
#' * `code` - the code.
#' * `n_text` - total number of text documents coded.
#' * `n_double_coded` - the number of text documents with at least two coders.
#' * `n_codes` - the total number of times this code has been used.
#' * `n_text_with_code` - the number of text documents with this code.
#' * `pra` - Exact percent rater agreement. This compares the number of times
#'           the code was used for each text document.
#' * `icc1`, `icc2`, `icc3`, `icc1k`, `icc2k`, `icc3k` - Intraclass correlation
#'           coefficient. See [psych::ICC] for more details.
#'
#'
#'
#' @param qda_data a `qda` object.
#' @param min_ratings the minimal number of text documents that are double coded
#'        for the IRR statistics to be calculated.
#' @param coders a character vector of coders to include in the IRR calculations.
#'        If omitted then all coders will be used.
#' @param include_zero_codes include text docuements where none of the coders
#'        applied a code to that text.
#' @return a data.frame with inter-rater reliability (IRR) statistics for each code
#'         (see `qda_data$get_codes()`) and pair of coders.
#' @importFrom psych ICC
#' @export
irr <- function(qda_data,
				codings = qda_data$get_codings(),
				min_ratings = 5,
				coders,
				include_zero_codes = TRUE) {
	# codings <- qda_data$get_codings()

	codings2 <- apply(codings[,c('qda_id', 'coder', 'codes')], 1, FUN = function(x) {
		codes <- strsplit(x['codes'], ';')[[1]]
		if(length(codes) == 0) {
			codes <- NA
		}
		data.frame(qda_id = rep(x['qda_id'], length(codes)),
				   coder = rep(x['coder'], length(codes)),
				   code = codes)
	}) |> dplyr::bind_rows() |> dplyr::filter(!is.na(code))
	row.names(codings2) <- NULL

	coding_summary <- codings2 |>
		dplyr::group_by(qda_id, coder, code) |>
		dplyr::count() |>
		reshape2::dcast(qda_id + coder ~ code, value.var = 'n') %>%
		replace(is.na(.), 0)

	if(missing(coders)) {
		coders <- unique(coding_summary$coder)
	} else {
		coding_summary <- coding_summary |> filter(coder %in% coders)
	}
	# coding_pairs <- combn(coders, 2) |> t()

	# irr_summary_pairwise <- data.frame(coder1 = character(),
	# 						  coder2 = character(),
	# 						  code = character(),
	# 						  n_texts = integer(),
	# 						  pra = numeric(),
	# 						  avg_diff = numeric())

	irr_summary <- data.frame()

	for(irr_code in unique(codings2$code)) {
		# irr_code <- 'Commitment-generic'

		code_ratings <- coding_summary |>
			dplyr::select(qda_id, coder, dplyr::all_of(irr_code)) |>
			reshape2::dcast(qda_id ~ coder, value.var = irr_code)

		if(!include_zero_codes) {
			zero_rows <- apply(code_ratings[,-1], 1, FUN = function(x) {
				sum(x, na.rm = TRUE) == 0
			})
			code_ratings <- code_ratings[!zero_rows,]
		}

		n_text_with_code <- apply(code_ratings[,-1], 1, FUN = function(x) {
			sum(x, na.rm = TRUE) > 0
		}) |> sum()

		n_codes <- sum(code_ratings[,-1], na.rm = TRUE)

		# n_raters <- apply(code_ratings[,-1], 1, FUN = function(x) {
		# 	sum(!is.na(x))
		# })
		agree <- apply(code_ratings[,-1], 1, FUN = function(x) {
			x <- x[!is.na(x)]
			if(length(x) == 1) {
				return(NA) # Only one rater
			} else {
				return(length(unique(x)) == 1)
			}
		})
		n_double_coded = sum(!is.na(agree))

		if(n_double_coded > min_ratings & n_text_with_code > min_ratings) {
			icc <- data.frame(type = c('ICC1', 'ICC2', 'ICC3', 'ICC1k', 'ICC2k', 'ICC3k'),
							  ICC = rep(NA, 6))
			suppressWarnings({suppressMessages({ # Maybe this isn't a good thing.
				try({ icc <- psych::ICC(code_ratings[,-1])$results }, silent = TRUE)
			})})

			irr_summary <- rbind(irr_summary, data.frame(
				code = irr_code,
				n_text = nrow(code_ratings),
				n_double_coded = n_double_coded,
				n_codes = n_codes,
				n_text_with_code = n_text_with_code,
				pra = sum(agree, na.rm = TRUE) / n_double_coded,
				icc1 = icc[1,]$ICC,
				icc2 = icc[2,]$ICC,
				icc3 = icc[3,]$ICC,
				icc1k = icc[4,]$ICC,
				icc2k = icc[5,]$ICC,
				icc3k = icc[6,]$ICC
			))
		} else {
			irr_summary <- rbind(irr_summary, data.frame(
				code = irr_code,
				n_text = nrow(code_ratings),
				n_double_coded = n_double_coded,
				n_codes = n_codes,
				n_text_with_code = n_text_with_code,
				pra = NA,
				icc1 = NA,
				icc2 = NA,
				icc3 = NA,
				icc1k = NA,
				icc2k = NA,
				icc3k = NA
			))
		}

		# for(i in seq_len(nrow(coding_pairs))) {
		# 	coder1 <- coding_pairs[i,1]
		# 	coder2 <- coding_pairs[i,2]
		# 	ratings1 <- coding_summary |> filter(coder == coder1) |> select(qda_id, all_of(irr_code))
		# 	ratings2 <- coding_summary |> filter(coder == coder2) |> select(qda_id, all_of(irr_code))
		# 	ratings <- merge(ratings1, ratings2, by = 'qda_id')
		#
		# 	# NOTE: Skip if there are note enough double ratings
		# 	if(nrow(ratings) > min_ratings) {
		# 		pra <- sum(ratings[,2,drop=TRUE] == ratings[,3,drop=TRUE]) / nrow(ratings)
		# 		avg_diff <- mean(abs(ratings[,2,drop=TRUE] - ratings[,3,drop=TRUE]))
		# 		icc <- data.frame(type = c('ICC1', 'ICC2', 'ICC3', 'ICC1k', 'ICC2k', 'ICC3k'),
		# 						  ICC = rep(NA, 6))
		# 		suppressMessages({
		# 			try({ icc <- psych::ICC(ratings[,2:3])$results }, silent = TRUE)
		# 		})
		#
		# 		irr_summary_pairwise <- rbind(irr_summary_pairwise,
		# 							 data.frame(coder1 = coder1,
		# 							 		   coder2 = coder2,
		# 							 		   code = irr_code,
		# 							 		   n_texts = nrow(ratings),
		# 							 		   pra = pra,
		# 							 		   icc1 = icc[1,]$ICC,
		# 							 		   icc2 = icc[2,]$ICC,
		# 							 		   icc3 = icc[3,]$ICC,
		# 							 		   icc1k = icc[4,]$ICC,
		# 							 		   icc2k = icc[5,]$ICC,
		# 							 		   icc3k = icc[6,]$ICC
		# 							 ))
		# 	}
		# }
	}

	return(irr_summary)
	# return(irr_summary_pairwise)
}
