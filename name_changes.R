# Add the names of the variables to the dataframe
library(readxl)
#read datasets
df <- read_excel("C:/Users/lisa.reiners/OneDrive - ACTED/Documents/MMR2301/review_template/inputs/REACH_MMR2301_DATASET_RMS_InPerson.xlsx", guess = 50000, sheet="HH_Data_Clean")

questions <- read_excel("C:/Users/lisa.reiners/OneDrive - ACTED/Documents/MMR2301/review_template/inputs/REACH_MMR2301_DATASET_RMS_InPerson.xlsx", sheet="KOBO survey")

# Install and load the required package
#install.packages("shiny")
#library(shiny)

# ui <- fluidPage(
#   titlePanel("User Input"),
#   sidebarLayout(
#     sidebarPanel(
#       textInput("user_input", "Is your data with the correct names? (yes or no):", value = "yes")
#     ),
#     mainPanel(
#       actionButton("submit", "Submit")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   observeEvent(input$submit, {
#     user_input <- input$user_input
#     if (tolower(user_input) == "yes") {
#       # Proceed with the code using the provided variable names
#       
#     
#       
#     } else {
#       # Adjust the variable names accordingly and rerun the code
#     }
#   })
# }

#shinyApp(ui = ui, server = server)


# Create an empty row with column names matching the dataframe
empty_row <- setNames(data.frame(matrix(NA, nrow = 1, ncol = ncol(df))), colnames(df))

# Combine the empty row and the original dataframe
df <- rbind(empty_row, df)

# Reset row names
row.names(df) <- NULL

#list with names columns
name_data_columns <- as.list(colnames(df)) 


# Iterate over the elements of the name_data_columns list
for (i in 1:length(name_data_columns)) {
  column_name <- name_data_columns[[i]]
  
  # Iterate over the rows of the selected column using a loop
  for (j in 1:nrow(questions)) {
    if (column_name == questions[j, "name"]) {
      df[1, column_name] <- tolower(questions[j, "label::English (en)"])
    }
  }
}

# Print the updated dataframe
print(df)

# Optionally, save the dataframe to an Excel file
# write.xlsx(df, file = "data_names.xlsx")