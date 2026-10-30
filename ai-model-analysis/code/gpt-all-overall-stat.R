library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)
library(writexl)

root <- "ai-model-analysis"
gpt_all_dir <- file.path(root, "gpt-all-extended")

languages <- c("cz", "hu", "hr", "ro", "pl", "sl", "sk", "sr", "uk")

# Author-language code -> language code expected in the model output.
# Czech is "cs" in model output, while the other codes match the filenames.
output_lang_code <- c(
  cz = "cs",
  hu = "hu",
  hr = "hr",
  ro = "ro",
  pl = "pl",
  sl = "sl",
  sk = "sk",
  sr = "sr",
  uk = "uk"
)

files <- list.files(
  gpt_all_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
) |>
  sort()

if (length(files) == 0) {
  stop("No Excel files found in: ", gpt_all_dir)
}

# Filename format: author-language + Wiki-language + "wiki.xlsx"
# Example: cz-huwiki.xlsx
file_info <- tibble(
  path = files,
  filename = basename(files)
) |>
  mutate(
    filename_parts = str_match(
      tolower(filename),
      "^([a-z]{2})-([a-z]{2})wiki\\.xlsx$"
    ),
    author_code = filename_parts[, 2],
    wiki_code = filename_parts[, 3]
  ) |>
  select(-filename_parts)

if (anyNA(file_info$author_code) || anyNA(file_info$wiki_code)) {
  stop("Could not parse author and Wiki languages from one or more filenames.")
}

if (any(!file_info$author_code %in% languages)) {
  stop("At least one filename has an unexpected author-language code.")
}

if (any(!file_info$wiki_code %in% languages)) {
  stop("At least one filename has an unexpected Wiki-language code.")
}

missing_languages <- setdiff(languages, unique(file_info$author_code))
if (length(missing_languages) > 0) {
  stop(
    "No files found for these author languages: ",
    paste(missing_languages, collapse = ", ")
  )
}

if (anyDuplicated(paste(file_info$author_code, file_info$wiki_code))) {
  stop("There is more than one file for an author-language/Wiki-language pair.")
}

read_classified_file <- function(path, author_code, wiki_code) {
  dat <- read_excel(path)
  names(dat) <- tolower(names(dat))
  
  if (!all(c("id", "writer", "language") %in% names(dat))) {
    stop("Expected id, writer, and language columns in: ", basename(path))
  }
  
  # Deduplicate by ID, retaining the first row and its other fields.
  dat <- dat |>
    mutate(id = as.character(id)) |>
    distinct(id, .keep_all = TRUE) |>
    mutate(
      writer_text = toupper(as.character(writer)),
      language_text = as.character(language),
      writer_is_true = !is.na(writer) &
        writer_text %in% c("TRUE", "1"),
      status = case_when(
        writer_is_true & is.na(language_text) ~ "NOLANG",
        is.na(writer) ~ "ERROR",
        writer_is_true &
          str_detect(
            coalesce(language_text, ""),
            fixed(output_lang_code[[author_code]])
          ) ~ "YES",
        !is.na(writer) ~ "NO",
        TRUE ~ NA_character_
      )
    ) |>
    select(id, status)
  
  dat$wiki_code <- wiki_code
  dat
}

summarise_author_language <- function(code) {
  author_files <- file_info |>
    filter(author_code == code)
  
  long_data <- pmap_dfr(
    list(author_files$path, author_files$author_code, author_files$wiki_code),
    function(path, author_code, wiki_code) {
      read_classified_file(path, author_code, wiki_code)
    }
  )
  
  # One row per author ID, with a column for each available Wiki edition.
  wide_data <- long_data |>
    pivot_wider(
      names_from = wiki_code,
      values_from = status
    )
  
  wiki_cols <- unique(author_files$wiki_code)
  wiki_count <- rowSums(!is.na(wide_data[, wiki_cols, drop = FALSE]))
  
  more_than_one_wiki <- sum(wiki_count > 1)
  one_wiki <- sum(wiki_count == 1)
  
  # Debated: at least two different nonmissing statuses across Wiki editions.
  debated_ids <- wide_data |>
    rowwise() |>
    mutate(
      n_different_statuses = n_distinct(
        c_across(all_of(wiki_cols)),
        na.rm = TRUE
      )
    ) |>
    ungroup() |>
    filter(n_different_statuses > 1) |>
    pull(id)
  
  # Not analyzed: unique IDs with ERROR or NOLANG, excluding debated IDs.
  not_analyzed_ids <- long_data |>
    filter(status %in% c("ERROR", "NOLANG")) |>
    distinct(id) |>
    filter(!id %in% debated_ids)
  
  tibble(
    Language = code,
    Analyzed = nrow(wide_data),
    Debated = length(debated_ids),
    `More than one wiki` = more_than_one_wiki,
    `One wiki` = one_wiki,
    `Not analyzed` = nrow(not_analyzed_ids)
  )
}

table4 <- map_dfr(languages, summarise_author_language)

print(table4)

write_xlsx(
  table4,
  file.path(root, "gpt-all-extended-table4-overall-statistics.xlsx")
)
