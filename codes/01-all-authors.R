library(purrr)
library(dplyr)
library(readxl)
library(writexl)
all_files <- list.files(path = "YOUR PATH/authors/",
                        pattern = "-author-all.xlsx", full.names = TRUE)

# read all authors from all tradtitions
all_authors <- map_df(all_files, function(file_path) {
  language <- gsub("-author-all.xlsx", "", basename(file_path))
  sheets <- excel_sheets(file_path)
  
  map_df(sheets, function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      mutate(Wikipedia = gsub("_valid-writer", "-wiki", sheet),
             language = language) %>%
      select(id, language, Wikipedia)
  }) %>%
    distinct()
})

# Final dataframe: unique authors per language per wiki
all_authors_df <- all_authors %>%
  distinct(id, language, Wikipedia) %>%
  rename(author_id = id, wiki = Wikipedia)

all_authors_df
length(unique(all_authors_df$author_id))

write_xlsx(all_authors_df, "all-authors.xlsx")

