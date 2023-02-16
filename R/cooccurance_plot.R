#' Co-Occurance Plot
#'
#'
#' @references Pokorny, Jen & Norman, Alex & Zanesco, Anthony & Bauer-Wu,
#'        Susan & Sahdra, Baljinder & Saron, Clifford. (2016). Network Analysis
#'        for the Visualization and Analysis of Qualitative Data. Psychological
#'        Methods. 23. 10.1037/met0000129.
#' @param qda_data a [ShinyQDA::qda()] object.
#' @param ... parameters pass to other functions.
#' @import ggplot2
#' @importFrom dplyr rename_with everything mutate
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_replace_all
#' @importFrom reshape2 melt
#' @importFrom tidyr replace_na
#' @export
cooccurance_plot <- function(qda_data, ...) {
	tab2 <- qda_merge(qda_data) |>
		dplyr::select(dplyr::starts_with('code_')) |>
		dplyr::mutate_all(~ tidyr::replace_na(., 0))

	if(sum(tab2) == 0) {
		stop('No codings or co-occurances found.')
	}

	mat <- matrix(nrow = ncol(tab2),
				  ncol = ncol(tab2),
				  dimnames = list(names(tab2), names(tab2)))

	for(i in 1:(nrow(mat)-1)) {
		for(j in (i+1):(ncol(mat))) {
			tmp <- tab2[,c(rownames(mat)[i], colnames(mat)[j])]
			mat[rownames(mat)[i], colnames(mat)[j]] <- sum(tmp[,1,drop=TRUE] > 0 & tmp[,2,drop=TRUE] > 0)
		}
	}
	# Fill in the diagonal
	for(i in 1:nrow(mat)) {
		mat[rownames(mat)[i], rownames(mat)[i]] <- sum(tab2[,rownames(mat)[i],drop=TRUE] > 0)
	}

	mat <- mat |>
		as.data.frame() |>
		dplyr::rename_with(~ gsub('code_', '', .x), .cols = dplyr::everything()) |>
		tibble::rownames_to_column('x') |>
		dplyr::mutate(x = stringr::str_replace_all(x, 'code_', '')) |>
		reshape2::melt(id.var = 'x', variable.name = 'y', value.name = 'value')


	ggplot2::ggplot(mat, ggplot2::aes(x = x, y = y, fill = value)) +
		ggplot2::geom_tile(na.rm = TRUE) +
		ggplot2::geom_text(ggplot2::aes(label = value), na.rm = TRUE) +
		ggplot2::scale_fill_gradient2(na.value = 'white') +
		ggplot2::xlab('') + ggplot2::ylab('') +
		ggplot2::coord_flip() +
		ggplot2::theme(aspect = 1, axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
