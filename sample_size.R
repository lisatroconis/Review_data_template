library(dplyr)

#dataset to compare 
sample <- read_xlsx("REACH_MMR2301_DATASET_RMS_InPerson.xlsx",sheet="Sheet2")

## Size Control

#sampling size (Tor) == dataset size
#add the sampling size 
# How to impute this on R 
sampling_size = 945






######################################################################
data_size = length(cleann$`__uuid`) #modify

#comparasion if the gol is completed 
if_else(sampling_size<=data_size,"sampling size completed", "sampling size NO completed")
num_sample_missing = sampling_size - data_size

#per region
# Set the first row as column names
colnames(sample) <- sample[1, ]
sample <- sample[-1, ]

#clean names
sample$`Sub-Region` <- tolower(sample$`Sub-Region`)           # Convert values to lowercase
sample$`Sub-Region` <- gsub("\\(|\\)", "", sample$`Sub-Region`)  # Remove parentheses
sample$`Sub-Region` <- gsub(" ", "_", sample$`Sub-Region`)     # Remove spaces


# Iterate over elements in the list to compare for region
list_regions_sample = unique(sample$`Sub-Region`)
list_regions_sample <- list_regions_sample[list_regions_sample != "Total"]

#list_regions_data = unique(cleann$state_region)
cleann_regions_counts <- cleann %>% 
  group_by(state_region) %>% 
  count()

# Merge dataframes based on "Name" column
merged_cleann <- merge(cleann_regions_counts, sample, by.x = "state_region", by.y = "Sub-Region", all = TRUE)

write_xlsx(merged_df, path = "size_sample_data.xlsx")
