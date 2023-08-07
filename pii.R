library(readxl)

#The only section that should be changed.
file <- "C:/Users/lisa.reiners/OneDrive - ACTED/Documents/IRQ2301/review_template/inputs/IRQ2301_Winter_cash_assistance_dataset.xlsx"
data <- read_excel(file, guess = 10000, sheet = "Clean Data")

function (data, i.know.this.check.is.insufficient = F) 
 {
  # Check for sensitive columns
  names_col <- colnames(data)
  sensitive.cols <- character(0)
  
  for (name in tolower(names_col)) {
    contains_substring <- grepl("name|gps|phone|location|latitude|longitude", name)
    if (contains_substring) {
      sensitive.cols <- append(sensitive.cols, name)
    }
  }
  
  if (length(sensitive.cols) == 0) {
    # You need to define what empty_issues_table() does or remove this line if not needed.
    return(empty_issues_table())
  }
  
  # Show only the columns identified as PII
  pii_df <- data[, sensitive.cols]
  
  # Drop columns that are empty
  pii_df <- pii_df[, colSums(is.na(pii_df)) < nrow(pii_df)]
  
  # Get the unique values for each column
  unique_matrix <- apply(pii_df, 2, unique)
  
  # Create a dataframe of the unique_matrix
  unique_df <- data.frame(
    variable = names(unique_matrix),
    unique_values = I(unique_matrix),
    stringsAsFactors = FALSE
  )
  
  
  # Create the unique_sensitive.cols dataframe
  sensitive.cols <- data.frame(
    index = NA,
    value = sapply(unique_matrix, function(values) {
      if (length(values) >= 3) {
        return(paste(head(values, 3), collapse = ", ") %>% paste0("..."))
      } else {
        return(paste(values, collapse = ", "))
      }
    }),
    variable = unique_df$variable,
    has_issue = TRUE,
    issue_type = "Potentially sensitive information. Please ensure all PII is removed",
    stringsAsFactors = FALSE
  )
  
  # Reset row names
  rownames(sensitive.cols) <- NULL
  
  if (!i.know.this.check.is.insufficient) {
    warning("sensitive_columns() is rudimentary and does not provide ANY data protection.")
  }
  return(sensitive.cols)
}






