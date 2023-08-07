library(readxl)

#The only section that should be changed.
file <- XXX
raww <- read_excel(file, guess = 10000, sheet = "Raw Data")
cleann <- read_excel(file, guess = 10000, sheet = "Clean Data")
logg <- read_excel(file, guess = 50000,sheet = "Cleaning Log")
colnames(logg) <- logg[1,]
logg <- logg[-1, ]
dell <- read_excel(file, guess = 50000, sheet = "Deletions")

#Kobo
questions <- read_excel(file, sheet="Kobo")
choices <- read_excel(file, sheet = "Choice")

## practice

#colnames(raww)[colnames(raww) == "_uuid"] <- "uuid"
#colnames(raww)[colnames(raww) == "__version__"] <- "uuid"
#colnames(raww)[colnames(raww) == "_status"] <- "_submitted_by"

# Create a list to store datasets
datasetList <- list(raww = raww, cleann = cleann,logg = logg, dell = dell)

# Iterate through the datasets in the list
for (name_datasets in names(datasetList)) {
  dataset <- datasetList[[name_datasets]]  # Get the current dataset
  
  have_uuid <- FALSE  # Flag to track if there are duplicate uuid columns
  column_names <- colnames(dataset)  # Get all column names
  duplicate_columns <- duplicated(column_names)  # Find duplicate column names
  
  # Check if there are any duplicate columns
  if (any(duplicate_columns)) {
    duplicate_column_names <- column_names[duplicate_columns]  # Save the names of the duplicated columns
    
    # Iterate through the duplicated column names
    for (value in duplicate_column_names) {
      if (value == "uuid" || value == "_uuid" || value == "__uuid") {
        have_uuid <- TRUE
        print(paste("There are duplicated uuid columns. Please choose the correct one and delete the incorrect one. It is in the:", name_datasets))
      }
    }
  }
  
  # If there are no duplicate uuid columns
  if (!have_uuid) {
    column_found <- FALSE  # Flag to track if the column is found
    uuid_name_datasets <- NULL  # Variable to store the name of the UUID column
    
    # Iterate through the column names
    for (column in column_names) {
      if (column == "uuid" || column == "_uuid" || column == "__uuid") {
        uuid_name_datasets <- column
        column_found <- TRUE
        break  # Stop the loop if the column is found
      }
    }
    
    if (column_found) {
      print(paste("The column 'uuid' is found in:", name_datasets, "dataset"))
      print(uuid_name_datasets)
      
      # Create a dynamic variable name to store the uuid_name_datasets value
      assign(paste("uuid_", name_datasets, sep = ""), uuid_name_datasets)
      
    } else {
      print(paste("Column 'uuid' not found!", name_datasets, "dataset"))
    }
  }
}

#Prueba
#uuid_cleann
#uuid_logg

#names from the cleaning log
colname_logg <- colnames(logg)
var_logg <- NULL
old_value_logg <- NULL

for (col_name in colname_logg) {
  if (grepl("question", tolower(col_name))) {
    var_logg <- col_name
    print(var_logg)
  }
  if (grepl("old", tolower(col_name))) {
    old_value_logg <- col_name
    print(old_value_logg)
  }
  if (grepl("new", tolower(col_name))) {
    new_value_logg <- col_name
    print(new_value_logg)
  }
}

#prueba
#new_value_logg
