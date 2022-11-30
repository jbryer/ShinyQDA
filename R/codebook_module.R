#' UI for the codebook.
#'
#' @export
#' @importFrom shiny NS
#' @importFrom shinyTree shinyTree renderTree
codebook_ui <- function(id) {
	ns <- shiny::NS(id)
	tagList(
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				width = 6,
				shiny::actionButton(ns("add_code_dialog"), "Add Code"),
				shiny::actionButton(ns("closeAll"), "Collapse All"),
				shiny::actionButton(ns("openAll"), "Expand All"),
				shiny::hr(),
				shinyTree::shinyTree(ns("codebook_tree"),
									 theme = "proton",
									 multiple = FALSE,
									 animation = FALSE,
									 dragAndDrop = TRUE,
									 sort = FALSE,
									 wholerow = TRUE,
									 unique = TRUE,
									 contextmenu = FALSE)
			),
			shiny::mainPanel(
				width = 6,
				shiny::uiOutput(ns('codebook_output'))
			)
		)
	)
}



#' Server for the codebook.
#'
#' @export
codebook_server <- function(id, qda_data) {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			shiny::observeEvent(input$cancel_modal, {
				add_code_message('')
				shiny::removeModal()
			})

			output$codebook_tree <- shinyTree::renderTree({
				# ns <- session$ns
				# refresh()
				codes <- qda_data()$get_codes()
				roots <- codes[is.na(codes$parent) | codes$parent == '',]

				if(nrow(roots) == 0) {
					# TODO: Good for now, but should display error to users
					stop('No roots found')
				}

				build_tree <- function(roots, all_codes) {
					tree <- as.list(roots$code)
					names(tree) <- roots$code
					for(i in seq_len(nrow(roots))) {
						code <- roots[i,]$code
						children <- codes[which(codes$parent == code),]
						if(nrow(children) > 0) {
							tree[[code]] <- build_tree(children, all_codes)
						} else {
							tree[[code]] <- code
						}
					}
					return(tree)
				}

				build_tree(roots, children)
			})

			output$codebook_output <- shiny::renderUI({
				ns <- session$ns
				req(input$codebook_tree)
				# TODO: Would really like to have the tree expanded by default
				# shinyjs::runjs(shiny::HTML('$("#codebook_tree").jstree("open_all");'))

				selected_code <- NULL
				node <- shinyTree::get_selected(input$codebook_tree)
				ui <- list()
				if(length(node) > 0) {
					code <- node[[1]][1]
					codes <- qda_data()$get_codes()
					selected_code <- codes[codes$code == code,]
					# TODO: Maybe allow changing the code name. This would require changing
					#       all references to this code as well.
					# ui[[length(ui) + 1]] <- shiny::textInput(
					# 	inputId = ns('code_name'),
					# 	label = 'Code:',
					# 	value = code
					# )
					ui[[length(ui) + 1]] <- shiny::p(strong('Code: '), code)
					ui[[length(ui) + 1]] <- shiny::textAreaInput(
						inputId = ns('code_description'),
						label = 'Description:',
						value = selected_code[1,]$description,
						height = '150px'
					)
					colors <- get_colors()

					ui[[length(ui) + 1]] <- colourpicker::colourInput(
						inputId = ns('code_color'),
						label = 'Color:',
						value = selected_code[1,]$color,
						palette = 'limited',
						allowedCols = colors
					)
				}

				do.call(shiny::wellPanel, ui)
			})

			observeEvent(input$codebook_tree, {
				tree <- input$codebook_tree
				traverse_tree <- function(node, parent = '') {
					for(i in seq_len(length(node))) {
						code <- names(node)[i]
						qda_data()$update_code(code = code,
											 parent = parent)
						if(!is.null(node[[i]]) & !is.null(names(node[[i]]))) {
							traverse_tree(node[[i]], parent = names(node)[i])
						}
					}
				}
				traverse_tree(tree)
			})

			observeEvent(input$code_description, {
				node <- shinyTree::get_selected(input$codebook_tree)
				code <- node[[1]][1]
				val <- input$code_description
				if(!is.na(val) & val != 'NA') {
					qda_data()$update_code(code, description = val)
				}
			})

			observeEvent(input$code_color, {
				node <- shinyTree::get_selected(input$codebook_tree)
				code <- node[[1]][1]
				# selected_code <- codes[codes$code == code,]
				qda_data()$update_code(code, color = input$code_color)
			})

			shiny::observeEvent(input$closeAll, {
				ns <- session$ns
				shinyjs::runjs(shiny::HTML(paste0('$("#', ns('codebook_tree'), '").jstree("close_all");')))
			})

			shiny::observeEvent(input$openAll, {
				ns <- session$ns
				shinyjs::runjs(shiny::HTML(paste0('$("#', ns('codebook_tree'), '").jstree("open_all");')))
			})

			add_code_message <- reactiveVal('')

			shiny::observeEvent(input$add_code_dialog, {
				ns <- session$ns
				shiny::showModal(
					shiny::modalDialog(
						uiOutput(ns('new_code_ui')),
						title = 'Add New Code',
						footer = shiny::tagList(
							shiny::actionButton(ns('cancel_modal'), 'Cancel'),
							shiny::actionButton(ns('add_code'), 'Add')
						)
					)
				)
			})

			output$new_code_ui <- renderUI({
				ns <- session$ns
				colors <- get_colors()
				codes_table <- qda_data()$get_codes()
				color <- colors[(nrow(codes_table) + 1) %% length(colors)]

				ui <- list(
					shiny::strong(add_code_message()),
					shiny::textInput(
						inputId = ns('new_code_name'),
						label = 'Code:',
						width = '100%'),
					shiny::textAreaInput(
						inputId = ns('new_code_description'),
						label = 'Description:',
						height = '150px',
						width = '100%'),
					colourpicker::colourInput(
						inputId = ns('new_code_color'),
						label = 'Color:',
						value = color,
						palette = 'limited',
						allowedCols = colors
					)
				)
				do.call(shiny::wellPanel, ui)
			})

			observeEvent(input$add_code, {
				codes_table <- qda_data()$get_codes()
				if(input$new_code_name == '') {
					add_code_message('Please enter a code name')
					return()
				} else if(input$new_code_name %in% codes_table$code) {
					add_code_message(paste0(input$new_code_name, ' has already been used Use a unique code name.'))
					return()
				}

				qda_data()$add_codes(
					codes = input$new_code_name,
					colors = input$new_code_color,
					descriptions = input$new_code_description
				)

				add_code_message('')
				shiny::removeModal()
			})
		}
	)
}
