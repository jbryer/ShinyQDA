#' UI for text coding.
#'
#' @export
coding_ui <- function(id) {
	ns <- NS(id)
	tagList(
		shiny::tags$head(tags$style("
					.tooltip2 {
					  position: relative;
					  display: inline-block;
					  border-bottom: 1px dotted black;
					}

					.tooltip2 .tooltiptext2 {
					  visibility: hidden;
					  width: 120px;
					  background-color: black;
					  color: #fff;
					  text-align: center;
					  border-radius: 6px;
					  padding: 5px 0;
					  position: absolute;
					  z-index: 1;
					  bottom: 150%;
					  left: 50%;
					  margin-left: -60px;
					}

					.tooltip2 .tooltiptext2::after {
					  content: '';
					  position: absolute;
					  top: 100%;
					  left: 50%;
					  margin-left: -5px;
					  border-width: 5px;
					  border-style: solid;
					  border-color: black transparent transparent transparent;
					}

					.tooltip2:hover .tooltiptext2 {
					  visibility: visible;
					}
				")),
		DT::dataTableOutput(ns('text_table'))
	)
}



#' Server text coding.
#'
#' @export
coding_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
		}
	)
}
