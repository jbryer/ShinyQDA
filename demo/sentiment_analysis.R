library(ShinyQDA)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(reshape2)

# Create connection to database file
db_file <- file.choose() # This will ask for where the SQLite file is located
qda_data <- ShinyQDA::qda(db_file)

qdadf <- ShinyQDA::qda_merge(qda_data, include_sentiments = TRUE)


# Bing
ggplot2::ggplot(qdadf[!duplicated(qdadf$qda_id),],
				aes(x = bing_total, color = !! sym(group_var))) +
	geom_density()


# Afinn
ggplot2::ggplot(qdadf[!duplicated(qdadf$qda_id),],
				aes(x = afinn_sentiment, color = !! sym(group_var))) +
	geom_density()

# Loughran
qdadf_loughran <- qdadf |>
	dplyr::distinct(qda_id, .keep_all = TRUE) |>
	dplyr::select(starts_with('loughran_'))
names(qdadf_loughran) <- gsub('loughran_', '', names(qdadf_loughran))
loughran_total <- apply(qdadf_loughran, 1, sum)
qdadf_loughran <- apply(qdadf_loughran, 2, function(x) { x / loughran_total }) |>
	as.data.frame() |>
	reshape2::melt()
loughran_tab <- psych::describeBy(qdadf_loughran$value,
								  group = qdadf_loughran$variable,
								  mat = TRUE)

ggplot2::ggplot(qdadf_loughran, ggplot2::aes(x = variable, y = value)) +
	ggplot2::geom_boxplot() +
	ggplot2::geom_errorbar(
		data = loughran_tab,
		ggplot2::aes(x = group1, y = mean, ymin = mean - se, ymax = mean + se),
		color = 'green3', size = 1, width = 0.4) +
	ggplot2::geom_point(
		data = loughran_tab, ggplot2::aes(x = group1, y = mean),
		color = 'blue', size = 3) +
	ggplot2::ylab('Proportion of Encoded Words') + ggplot2::xlab('') +
	ggplot2::coord_flip() +
	ggplot2::theme_minimal()

# NRC
qdadf_nrc <- qdadf |>
	dplyr::distinct(qda_id, .keep_all = TRUE) |>
	dplyr::select(starts_with('nrc_'))
names(qdadf_nrc) <- gsub('nrc_', '', names(qdadf_nrc))
nrc_total <- apply(qdadf_nrc, 1, sum)
qdadf_nrc <- apply(qdadf_nrc, 2, function(x) { x / nrc_total }) |>
	as.data.frame() |>
	qdadf_nrc |>
	reshape2::melt()
nrc_tab <- psych::describeBy(qdadf_nrc$value,
							 group = qdadf_nrc$variable,
							 mat = TRUE)

ggplot2::ggplot(qdadf_nrc, ggplot2::aes(x = variable, y = value)) +
	ggplot2::geom_boxplot() +
	ggplot2::geom_errorbar(
		data = nrc_tab,
		ggplot2::aes(x = group1, y = mean, ymin = mean - se, ymax = mean + se),
		color = 'green3', size = 1, width = 0.4) +
	ggplot2::geom_point(
		data = nrc_tab, ggplot2::aes(x = group1, y = mean),
		color = 'blue', size = 3) +
	ggplot2::ylab('Proportion of Encoded Words') + ggplot2::xlab('') +
	ggplot2::coord_flip() +
	ggplot2::theme_minimal()


