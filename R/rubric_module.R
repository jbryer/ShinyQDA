#' UI for the rubric.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
rubric_edit_ui <- function(id) {
	ns <- NS(id)
	shiny::uiOutput(ns('rubric_edit_ui'))
}

#' Server logic for the rubric.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param qda_data QDA data object, see [ShinyQDA::qda()].
#' @export
rubric_edit_server <- function(id, qda_data, page_length = 20) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			# Build the UI
			output$rubric_edit_ui <- renderUI({
				ns <- session$ns
				ui <- list()
				rubrics <- shiny::isolate({ qda_data()$get_rubrics() }) |>
					dplyr::filter(enabled == 1)
				if(nrow(rubrics) == 1) { # Edit rubric
					criteria <- shiny::isolate({ qda_data()$get_rubric(rubrics[1,]$rubric_name) })
					criteria <-criteria |>
						dplyr::select(criteria, scoring_level, description) |>
						reshape2::dcast(criteria ~ scoring_level, value.var = 'description')
					for(i in seq_len(nrow(criteria))) {
						row <- list()
						for(j in seq(2, ncol(criteria))) {
							row[[length(row) + 1]] <- shiny::column(
								width = floor(12 / (ncol(criteria)-1)),
								shiny::textAreaInput(
									inputId = ns(paste0(criteria[i,1,drop=TRUE], '_', names(criteria)[j])),
									label = paste0(criteria[i,1,drop=TRUE], ': ', names(criteria)[j]),
									height = '150px',
									width = '100%',
									value = criteria[i,j,drop=TRUE]
								)
								# shiny::p(
								# 	criteria[i,j,drop=TRUE]
								# )
							)
						}
						ui[[length(ui) + 1]] <- do.call(shiny::fluidRow, row)
					}
				} else if(nrow(rubrics) > 1) {
					# TODO: Send error to user
					stop('There should only be one active rubric!')
				} else { # Create rubric

				}

				do.call(shiny::tagList, ui)
			})

			# Edit rubric criteria
			observe({
				rubrics <- shiny::isolate({ qda_data()$get_rubrics() }) |>
					dplyr::filter(enabled == 1)
				ns <- session$ns
				if(nrow(rubrics) == 1) { # Edit rubric
					rubric_name <- rubrics[1,]$rubric_name
					criteria <- shiny::isolate({ qda_data()$get_rubric(rubric_name) })
					criteria <- criteria |>
						dplyr::select(criteria, scoring_level, description) |>
						reshape2::dcast(criteria ~ scoring_level, value.var = 'description')
					for(i in seq_len(nrow(criteria))) {
						for(j in seq(2, ncol(criteria))) {
							val <- input[[paste0(criteria[i,1,drop=TRUE], '_', names(criteria)[j])]]
							if(!is.null(val)) {
								if(val != criteria[i,j,drop=TRUE]) {
									shiny::isolate({
										qda_data()$update_rubric_criteria(
											rubric_name = rubrics[1,]$rubric_name,
											criteria = criteria[i,1,drop=TRUE],
											score_level = names(criteria)[j],
											description = val
										)
									})
								}
							}
						}
					}
				}
			})

			# Create a new rubric

		}
	)
}
