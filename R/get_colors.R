#' This palette was created from https://colorbrewer2.org using qualitative
#' type with 12 classes. There are two schemes given these parameters, the first
#' 12 are from a lighter palette and the second 12 are from a darker palette.
#'
#' @docType data
#' @export
color_palette <- toupper(c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
						   '#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f',
						   '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
						   '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))

#' Returns a color palette to use for code highlighting.
#'
#' @export
get_colors <- function() {
	colors <- c(color_palette,
		RColorBrewer::brewer.pal(n = 12, name = 'Paired'),
		RColorBrewer::brewer.pal(n = 9, name = 'Pastel1'),
		RColorBrewer::brewer.pal(n = 12, name = 'Set3'),
		RColorBrewer::brewer.pal(n = 8, name = 'Accent'),
		RColorBrewer::brewer.pal(n = 8, name = 'Dark2')
	)
	colors <- unique(colors)
	return(colors)
}
