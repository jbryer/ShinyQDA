library(RSQLite)
library(writexl)

# Create connection to database file
db_file <- file.choose() # This will ask for where the SQLite file is located
db <- dbConnect(SQLite(), db_file)

# List all tables in database
dbListTables(db)

# Get tables as data.frames
text_data <- dbReadTable(db, 'text_data')
codings <- dbReadTable(db, 'codings')

# Covert to an Excel file
tables <- dbListTables(db)
tabs <- list()
for(i in tables) {
	tabs[[i]] <- dbReadTable(db, i)
}
writexl::write_xlsx(tabs, path = file.choose(new = TRUE))
