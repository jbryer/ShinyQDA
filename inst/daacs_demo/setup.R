library(ShinyQDA)
data("daacs_data", package = 'ShinyQDA')
username <- 'Rater1'

daacs_qda <- qda('qda.sqlite')
daacs_qda$add_text(df = daacs_data,
				   id_col = 'id',
				   text_col = 'qda_text',
				   overwrite = TRUE
)

daacs_qda <- qda('qda.sqlite')

data("daacs_codings", package = 'ShinyQDA')
data("daacs_text_responses", package = 'ShinyQDA')
data("daacs_rubric", package = 'ShinyQDA')

# Add text questions
daacs_qda$add_text_question(stem = 'Non-responsive to prompt',
							type = 'checkbox')
daacs_qda$add_text_question(stem = 'Additional comments about the text',
							type = 'text')
daacs_qda$get_text_questions()

# Add code questions
daacs_qda$add_code_question(
	stem = 'Content of Essay',
	type = 'checkbox',
	options = c('Definition of concepts',
				'Interpretation_strength',
				'Interpretation_weakness',
				'Interpretation_mixed',
				'Interpretation_medium',
				'Interpretation_strategies awareness',
				'Interpretation_strategies commitment',
				'Interpretation_judgments')
)
daacs_qda$add_code_question(
	stem = 'Does this text represent',
	type = 'checkbox',
	options = c('Strength',
				'Weakness',
				'Mixed',
				'Medium',
				'Strategies Awareness',
				'Strategies Commitment')
)
daacs_qda$add_code_question(
	stem = 'Judgment about survey or feedback',
	type = 'checkbox',
	options = c('Agree',
				'Disagree',
				'Valuable/helpful/useful',
				'Not valuable/helpful/useful',
				'Other')
)

# Add codes
categories <- list(
	'metacognition' = c('planning', 'monitoring', 'evaluation'),
	'motivation' = c('mindset', 'test anxiety', 'mastery orientation'),
	'self efficacy' = c('self efficacy for online learning',
						'self efficacy for writing',
						'self efficacy for mathematics',
						'self efficacy for reading'),
	'strategies' = c('managing environment', 'understanding', 'managing time', 'help seeking'),
	'procrastination' = c()
)
codes <- c(names(categories), unlist(categories)) |> unname()
daacs_qda$add_codes(codes)
for(i in seq_len(length(categories))) {
	category <- names(categories)[i]
	codes <- categories[[i]]
	for(i in codes) {
		daacs_qda$update_code(i, parent = category)
	}
}

# Add rubric
daacs_qda$add_rubric(
	rubric_name = 'daacs',
	description = 'DAACS Scoring Rubric',
	rubric = daacs_rubric
)

# Add existing codings
daacs_code_questions <- daacs_qda$get_code_questions()
for(i in seq_len(nrow(daacs_codings))) {
	coder <- daacs_codings[i,]$coder
	thetext <- daacs_data[daacs_data$id == daacs_codings[i,]$id,]$qda_text
	pos <- gregexpr(daacs_codings[i,]$text, thetext, fixed = TRUE)[[1]]
	coding_id <- daacs_qda$add_coding(
		id = daacs_codings[i,]$id,
		text = daacs_codings[i,]$text,
		codes = strsplit(daacs_codings[i,]$codes, ';')[[1]],
		start = pos,
		end = pos + nchar(daacs_codings[i,]$text),
		coder = coder
	)
	for(j in seq_len(nrow(daacs_code_questions))) {
		stem <- daacs_code_questions[j,]$stem
		if(stem %in% names(daacs_codings)) {
			val <- daacs_codings[i,stem]
			daacs_qda$add_code_question_response(coding_id = coding_id,
												 stem = stem,
												 answer = val,
												 coder = coder)
		}
	}
}

# Add existing text questions
for(i in seq_len(nrow(daacs_text_responses))) {
	daacs_qda$add_text_question_response(
		id = daacs_text_responses[i,]$id,
		stem = daacs_text_responses[i,]$stem,
		answer = daacs_text_responses[i,]$answer,
		coder = daacs_text_responses[i,]$coder
	)
}

# Add rubric scores
for(i in seq_len(nrow(daacs_data))) {
	for(j in unique(daacs_rubric$SubCriteria)) {
		daacs_qda$add_rubric_response(
			rubric_name = 'daacs',
			qda_id = daacs_data[i,]$id,
			coder = 'Rater1',
			criteria = j,
			score = daacs_data[i,j,drop=TRUE]
		)
	}
}

afinn <- textdata::lexicon_afinn(dir = '.')
bing <- textdata::lexicon_bing(dir = '.')
loughran <- textdata::lexicon_loughran(dir = '.')
nrc <- textdata::lexicon_nrc(dir = '.')

