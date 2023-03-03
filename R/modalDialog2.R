#' Alternative modal dialog.
#'
#' Adapted from https://stackoverflow.com/questions/63882483/how-to-adjust-shiny-modaldialog-width-to-a-dt-object-to-fully-show-the-table-i
#'
#' @param ... currently unused.
#' @param title modal dialog title.
#' @param footer footer of the modal dialog.
#' @param easyClose If TRUE, the modal dialog can be dismissed by clicking
#'        outside the dialog box, or be pressing the Escape key.
#' @param fade If FALSE, the modal dialog will have no fade-in animation
#'        (it will simply appear rather than fade in to view).
#' @export
modalDialog2 <- function(...,
						 title = NULL,
						 footer = shiny::modalButton("Dismiss"),
						 easyClose = FALSE,
						 fade = TRUE)
{
	cls <- if(fade) { "modal fade" } else { "modal" }
	div(id = "shiny-modal",
		class = cls,
		# style = 'left: 10px !important;',
		tabindex = "-1",
		`data-backdrop` = if(!easyClose) "static",
		`data-keyboard` = if (!easyClose) "false",
		div(class = "modal-dialog",
			div(class = "modal-content",
				style = "width: 80vw !important; left: -10vw !important; min-width: 80vw !important; max-width: 80vw !important;",
				if(!is.null(title))
					div(class = "modal-header", tags$h4(class = "modal-title", title)),
				div(class = "modal-body", ...),
				if (!is.null(footer))
					div(class = "modal-footer", footer))
			),
		tags$script("$('#shiny-modal').modal().focus();"))
}
