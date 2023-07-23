library(readxl)

#The only section that should be changed.
file <- "C:/Users/lisa.reiners/OneDrive - ACTED/Documents/IRQ2301/review_template/inputs/IRQ2301_Winter_cash_assistance_dataset.xlsx"
data <- read_excel(file, guess = 10000, sheet = "Clean Data")

#Check PII
names_col <- colnames(data)
pii_col <- character(0)

for (name in tolower(names_col)){
  contains_substring <- grepl("name|gps|phone|location|lat|long", name)
  if(contains_substring == TRUE){
    pii_col <- append(pii_col, name)
  }
}

# Show only the columns identified as PII
pii_df <- cleann[, pii_col]

# # Drop columns that are empty
# pii_df <- pii_df[, colSums(is.na(pii_df)) < nrow(pii_df)]
# 
# # Get the unique values for each column
# unique_matrix <- apply(pii_df, 2, unique)


#function (data, i.know.this.check.is.insufficient = F) 
#{
  #Check PII
  names_col <- colnames(data)
  pii_col <- character(0)
  
  for (name in tolower(names_col)){
      contains_substring <- grepl("name|gps|phone|location|lat|long", name)
      if(contains_substring == TRUE){
        pii_col <- append(pii_col, name)
      }
  }
  
  for (name in tolower(names_col)){
    sensitive.cols <- grepl("name|gps|phone|location|lat|long", 
                           x = name)
    if(sensitive.cols == TRUE){
    pii_col <- append(sensitive.cols, name)
    }
  }
  
  
    if (length(sensitive.cols) == 0) {
      return(empty_issues_table())
    }
  
  
  sensitive.cols <- data.frame(index = NA, value = NA, variable = sensitive.cols, 
                               has_issue = TRUE, issue_type = "Potentially sensitive information. Please ensure all PII is removed", 
                               stringsAsFactors = F)
  if (!i.know.this.check.is.insufficient) {
    warning("sensitive_columns() is rudimentary and does not provide ANY data protection.")
  }
  return(sensitive.cols)


