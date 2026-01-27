library(readxl)
library(dplyr)
library(purrr)

setwd("YOUR PATH/00-final-tables/")
# Load tall-authors (see code 01)
main_file <- "all-authors.xlsx"
main_df <- read_excel(main_file)

# Load original Wikidata query results to get additional data (gender, date of birth etc)
# Load all-unique-authors containing original uniqe authors
folder_path <- "YOUR PATH/origi"
all_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
other_files <- all_files[!grepl("all-unique-authors.xlsx", all_files)]

# Combina the two files
all_other_data <- map_dfr(other_files, function(file) {
  sheet_names <- excel_sheets(file)
  map_dfr(sheet_names, ~ read_excel(file, sheet = .x))
})

all_other_data <- all_other_data %>% 
distinct(article, .keep_all = TRUE)

# Select the relevant columns (ensure 'cid' exists)
columns_to_keep <- c("cid", "nem", "születési_idő", 
                     "halálozási_idő", "születési_név")

all_other_data <- all_other_data %>% select(any_of(columns_to_keep))

all_other_data_unique <- all_other_data %>%
  group_by(cid) %>%
  slice(1) %>%  # keep only the first occurrence per author
  ungroup()

merged_df <- main_df %>%
  left_join(all_other_data_unique, by = c("author_id" = "cid"))

write_xlsx(merged_df, "merged_authors.xlsx") # you may need to add info manually for some of the ids

