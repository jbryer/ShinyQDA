library(ShinyQDA)

# Create the project
data("daacs_data", package = 'ShinyQDA')
ShinyQDA::new_app(name = 'daacs_demo',
				  dir = getwd(),
				  qda_data = daacs_data,
				  id_column = 'id',
				  text_column = 'qda_text',
				  initialize_sentiment_dictionaries = TRUE,
				  run_app = FALSE)

# Get the qda object
daacs_qda <- ShinyQDA::qda('daacs_demo/qda.sqlite')
ls(daacs_qda)
daacs_qda$get_text() |> View()

# Add a rubric
data("daacs_rubric", package = 'ShinyQDA')
daacs_qda$add_rubric(
	rubric_name = 'daacs',
	description = 'DAACS Scoring Rubric',
	rubric = daacs_rubric
)

# Run the app
shiny::runApp('daacs_demo', port = 2112)

# Next steps:
# 1. Login: admin / password (show user management, change password)
# 2. Setup -> Codebook: Create codes: metacognition, motivation, strategies
# 3. Setup -> Questions
#    A. Add text questions: Is this essay on topic? (options: yes, no); Comments
#    B. Add code questions: Type (options: commitment, awareness)

