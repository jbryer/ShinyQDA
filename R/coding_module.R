#' UI for text coding.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
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
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [@ShinyQDA::qda()].
#' @export
coding_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
		}
	)
}
