library(readxl)
library(tidyverse)

file <- "C:/Users/lisa.reiners/OneDrive - ACTED/Documents/IRQ2301/V4/IRQ2301_Winter_cash_assistance_dataset_V4.xlsx"

raww <- read_excel(file, guess = 10000, sheet = "Raw Data")

data <- read_excel(file, guess = 50000, sheet="Clean Data")

logg <- read_excel(file, guess = 50000,sheet = "Cleaning Log")
colnames(logg) <- logg[1,]
logg <- logg[-1, ]


dell <- read_excel(file, guess = 50000, sheet = "Deletions")

questions <- read_excel(file, sheet="Kobo")

choices <- read_excel(file, sheet = "Choice")

uuid_raww <- "_uuid"
uuid_cleann <- "_uuid"
uuid_logg <- "uuid"
uuid_dell <- "_uuid"

#names from the cleaning log
var_logg <- "Question"
old_value_logg <- "Old Value"
new_value_logg <- "New Value"


duration_threshold_lower = 15
duration_threshold_upper = 100 

my_function <- function(data, duration_threshold_lower, duration_threshold_upper) {
  if (!is.data.frame(data))
    stop("data needs to be a dataframe!")
  if (!("start" %in% names(data)))
    stop("data needs to have a column called 'start' for this function to work")
  if (!("end" %in% names(data)))
    stop("data needs to have a column called 'end' for this function to work")
  
  # Regular expression pattern to match the format "%Y-%m-%dT%H:%M:%S.000%z"
  date_format_pattern <- "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}.000\\+\\d{4}"
  
  # Check if the start date column matches the desired format
  is_start_in_desired_format <- grepl(date_format_pattern, data$start)
  
  # Check if the end date column matches the desired format
  is_end_in_desired_format <- grepl(date_format_pattern, data$end)
  
  # Print the result for start date column
  if (all(is_start_in_desired_format))
    print("The start date column is in the desired format.")
  else
    data$start<-strftime(data$start,"%Y-%m-%dT%H:%M:%S.000%z")
  
  # Print the result for end date column
  if (all(is_end_in_desired_format))
    print("The end date column is in the desired format.")
  else
    data$end<-strftime(data$end,"%Y-%m-%dT%H:%M:%S.000%z")
  
  #confirm the change is made 
  
  # Check if the start date column matches the desired format
  is_start_in_desired_format <- grepl(date_format_pattern, data$start)
  
  # Check if the end date column matches the desired format
  is_end_in_desired_format <- grepl(date_format_pattern, data$end)
  
  # Check if both start and end date columns have the desired format
  if (all(is_start_in_desired_format) && all(is_end_in_desired_format)) 
    print("The date columns are in the desired format.")
  else 
    stop("Data does not have the correct format for the date columns.")
  
  df_col_separated <- data %>% separate(start, c("start_date", "start_time"), "T") %>% separate(end, c("end_date", "end_time"), "T")
  
  #calcula la diferencia en dias entre start and end
  df_col_separated$days_diff <- difftime(as.POSIXct(df_col_separated$end_date), as.POSIXct(df_col_separated$start_date), units = "days")
  
  same_date <- df_col_separated[((as.numeric(df_col_separated$days_diff)) == 0), ]
  
  different_date <- df_col_separated[((as.numeric(df_col_separated$days_diff)) > 0), ]
  same_date$start_time <- round(as.difftime(same_date$start_time, units = "mins"), 2)
  same_date$end_time <- round(as.difftime(same_date$end_time, units = "mins"), 2)
  same_date$duration_min <- as.numeric(same_date$end_time - same_date$start_time)
  same_date$time_short_flag <- ifelse(same_date$duration_min < duration_threshold_lower, 1, 0)  # Modified comparison here
  same_date$time_long_flag <- ifelse(same_date$duration_min > duration_threshold_upper, 1, 0)  # Modified comparison here
  
  too_short <- same_date %>% dplyr::filter(time_short_flag == 1)
  too_short$issue_type <- "form duration too short"
  
  too_long <- same_date %>% dplyr::filter(time_long_flag == 1)
  too_long$issue_type <- "form duration too long"
  
  time_problems <- rbind(too_short, too_long)
  colnames(time_problems)[grep("uuid", colnames(time_problems))] <- "uuid"
  
  time_problems$variable <- "Completion Duration (min)"
  time_problems$has_issue <- "TRUE"
  
  time_problems$issue_type <- ifelse(time_problems$time_short_flag == 1, "form duration too short", "form duration too long")  # Updated issue_type assignment
  time_problems$issue_type <- as.factor(time_problems$issue_type)
  
  time_grab <- time_problems %>% dplyr::select(uuid, duration_min, variable, has_issue, issue_type)
  names(time_grab) <- c("index", "value", "variable", "has_issue", "issue_type")
  
  return(time_grab)
}

# Applying the function to data
result <- my_function(data,duration_threshold_lower,duration_threshold_upper)

# Printing the result
print(result)

