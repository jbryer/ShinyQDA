#' Utility function to determine whether the package dependencies are available.
#'
#' @param dependencies character vector with the package names to check.
#' @return character vector of packages not available.
#' @export
missing_dependencies <- function(dependencies) {
	missing_packages <- c()
	for(i in dependencies) {
		if(!requireNamespace(i, quietly = TRUE)) {
			missing_packages <- c(missing_packages, i)
		}
	}
	return(missing_packages)
}

#' Utility function to determine whether the package dependencies are available.
#'
#' @inheritParams missing_dependencies
#' @return TRUE if all the required packages are available and have been loaded
#'         using the [base::requireNamespace()] function.
#' @export
check_dependencies <- function(dependencies) {
	return(length(missing_dependencies(dependencies)) == 0)
}
