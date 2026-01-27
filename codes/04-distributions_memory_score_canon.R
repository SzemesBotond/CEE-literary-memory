library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(ggplot2)
library(waffle)

## Among Wikipedias

colorcode <- c("cz-wiki" = "#11457E",
"hr-wiki" = "#FF0000",
"hu-wiki" = "#436F4D",
"pl-wiki" = "#DC143C",
"ro-wiki" = "#FFD700",
"sk-wiki" = "#0B4EA2",
"sl-wiki" = "#1D854C",
"sr-wiki" = "#C6363C",
"uk-wiki" = "#0057B7")


#from final-tables/authors always select one: cz, hr etc.
file_path <- "uk-author-all.xlsx"  
language_author <- gsub("-author-all.xlsx", "",file_path)
varname <- paste(language_author, "_author", sep="")
language_author <- str_to_title(language_author)
# Get all sheet names
sheets <- excel_sheets(file_path)

# Read all sheets at once
all_authors_lang <- map_df(sheets, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
   # select(1:8) %>%
    distinct(id, .keep_all = TRUE) %>%  
    mutate(Wikipedia = gsub("_valid-writer", "-wiki", sheet))  # remove "-wp" and use as label
})

# Unique authors (cid assumed to be the ID column)
unique_ids <- unique(all_authors_lang$id)

# Count per sheet
counts <- all_authors_lang %>%
  group_by(Wikipedia) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Percentage = round(n / length(unique_ids) * 100))

assign(varname, counts)


fig <- ggplot(uk_author, aes(x= Wikipedia, y= Percentage, 
                            fill = Wikipedia ))+
  geom_bar(stat = "identity", color="black") +
  scale_fill_manual(values = colorcode) +  
  labs(title = paste(language_author,"authors in foreign Wikipedias"),
       x = "", y = "Percetnage")+
  ylim(0,100)+
  theme_bw()
fig

ggsave("06-uk-authors.png", fig, 
       width = 40, height = 20, units = "cm",
       bg= "white")


## 2. Within a Wikipedia

# List of existing dataframes
dfs <- list(
  cz_author = cz_author,
  hr_author = hr_author,
  hu_author = hu_author,
  pl_author = pl_author,
  ro_author = ro_author,
  sk_author = sk_author,
  sl_author = sl_author,
  sr_author = sr_author,
  uk_author = uk_author
)

# Restructure: for each df, add "author" column from df name
dfs_named <- imap(dfs, function(df, nm) {
  df %>%
    mutate(author = str_remove(nm, "_author")) %>%  # get "cz", "hu", etc.
    select(Wikipedia, author, n)
})

# Combine all into one big dataframe
combined <- bind_rows(dfs_named)


# Split into new dfs by Wikipedia value
new_dfs <- combined %>%
  group_by(Wikipedia) %>%
  group_split() %>%
  set_names(sort(unique(combined$Wikipedia))) %>%
  map(function(df) {
    # Compute raw percentages
    pct <- df$n / sum(df$n) * 100
    
    # Round using a trick to make the sum exactly 100
    pct_rounded <- floor(pct)
    remainder <- 100 - sum(pct_rounded)
    
    if (remainder > 0) {
      # Add 1 to the largest 'remainder' values to sum to 100
      idx <- order(pct - pct_rounded, decreasing = TRUE)[1:remainder]
      pct_rounded[idx] <- pct_rounded[idx] + 1
    }
    
    df %>%
      mutate(Percentage = pct_rounded) %>%
      select(author, n, Percentage)
  })


# Assign each as a separate variable (safe names)
walk2(new_dfs, names(new_dfs), function(df, nm) {
  safe_name <- str_replace_all(nm, "-", "_")  # replace "-" with "_"
  assign(safe_name, df, envir = .GlobalEnv)
})


colorcode <- c("cz" = "#11457E",
               "hr" = "#FF0000",
               "hu" = "#436F4D",
               "pl" = "#DC143C",
               "ro" = "#FFD700",
               "sk" = "#0B4EA2",
               "sl" = "#1D854C",
               "sr" = "#C6363C",
               "uk" = "#0057B7")

# Make a named vector for waffle()
# use always one wiki name: cz, hr etc.
vector <- setNames(uk_wiki$Percentage, uk_wiki$author)

fig2 <- waffle(vector, rows = 10, colors = colorcode) +
  labs(title = "Uk Wikipedia",
       subtitle = "Distribution of foreign authors")


ggsave("05-uk-wiki.png", fig2, 
       width = 20, height = 15, units = "cm",
       dpi = 300,
       bg= "white")

## 3.  Memory Score

author_dfs <- dfs
wiki_dfs <- new_dfs

# Names for reference
author_names <- c("cz", "hr", "hu", "pl", "ro", "sk", "sl", "sr", "uk")
wiki_names   <- c("cz", "hr", "hu", "pl", "ro", "sk", "sl", "sr", "uk")

# Compute memory scores for all pairs
memory_scores <- map2_dfr(author_dfs, author_names, function(auth_df, auth_name) {
  map2_dfr(wiki_dfs, wiki_names, function(wiki_df, wiki_name) {
    
    # Get author's percentage in wiki relative to all wikis
    auth_pct <- auth_df %>%
      filter(Wikipedia == paste0(wiki_name, "-wiki")) %>%
      pull(Percentage)
    
    # Get author's percentage in wiki relative to all authors
    wiki_pct <- wiki_df %>%
      filter(author == auth_name) %>%
      pull(Percentage)
    
    # Multiply to get memory score
    tibble(
      author = paste0(auth_name, "\n(", wiki_name, " wiki)"),
      wiki = wiki_name,
      memory_score = auth_pct * wiki_pct
    )
    
  })
})

memory_scores

# Make a unique factor ordering based on memory_score
memory_scores <- memory_scores %>%
  arrange(memory_score) %>% # ascending
mutate(author = factor(author, levels = author))

# acess top or bottom of the list
memory_scores_top <- memory_scores[1:15,]

fig3 <- ggplot(memory_scores_top, aes(x = author, y = memory_score, fill = wiki)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorcode) +
  labs(title = "Memory Scores",
       x = "Author's Language",
       y = "Memory score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig3

ggsave("05-memroy_score_bot15.png", fig3, 
       width = 60, height = 20, units = "cm",
       dpi = 300,
       bg= "white")

write_xlsx(memory_scores, "memory-scores.xlsx")

## 4. Canon

# Path to the folder with Excel files
folder_path <- "YOUR PATH/00-final-tables/authors"

# List all xlsx files in the folder
excel_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize empty list to store results
canon <- list()

# Loop through each Excel file
for(file in excel_files) {
  
  # Get all sheet names
  sheets <- excel_sheets(file)
  
  # Read all sheets into a list of dataframes
  sheet_data <- lapply(sheets, function(sheet) {
    read_excel(file, sheet = sheet) %>% select(id)
  })
  
  # Find IDs common across all sheets
  common_ids <- Reduce(intersect, sheet_data)
  
  # Store in the canon list with file name (without extension) as name
  canon[[tools::file_path_sans_ext(basename(file))]] <- common_ids
}

canon
combined_canon <- bind_rows(canon, .id = "source")
write_xlsx(combined_canon, path = "canon.xlsx")
