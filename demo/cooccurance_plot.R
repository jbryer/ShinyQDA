library(ShinyQDA)
library(RSQLite)
library(ggplot2)
library(dplyr)

# Create connection to database file
db_file <- file.choose() # This will ask for where the SQLite file is located
qda_data <- ShinyQDA::qda(db_file)

qda_merge(qda_data) |>
	dplyr::select(dplyr::starts_with('code_')) |>
	dplyr::select(!code_test) |>
	ShinyQDA::cooccurance_plot(qda_data) + ggplot2::theme(legend.position = 'none')
