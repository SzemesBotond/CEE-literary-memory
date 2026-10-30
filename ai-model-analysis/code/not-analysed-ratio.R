library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(writexl)

root <- "ai-model-compare"

folders <- c(
  "gpt-all" = file.path(root, "gpt-all"),
  "gpt-all-extended" = file.path(root, "gpt-all-extended"),
  "gemma3:27b" = file.path(root, "gemma27"),
  "gpt-oss:120b" = file.path(root, "gpt-os-120"),
  "llama3.1:8b" = file.path(root, "llama8b"),
  "llama3.3:70b" = file.path(root, "llama70b")
)

wiki_name_to_code <- c(
  czech = "cz",
  croatian = "hr",
  hungarian = "hu",
  romanian = "ro",
  polish = "pl",
  slovenian = "sl",
  slovak = "sk",
  serbian = "sr",
  ukrainian = "uk"
)

parse_filename <- function(filename) {
  filename_lower <- tolower(filename)
  
  # Compact format, e.g. cz-huwiki.xlsx
  compact <- str_match(
    filename_lower,
    "^([a-z]{2})-([a-z]{2})wiki\\.xlsx$"
  )
  
  if (!is.na(compact[1, 2])) {
    return(tibble(
      author_language = compact[1, 2],
      wiki_language = compact[1, 3]
    ))
  }
  
  # Model-comparison format, e.g. cz-author-polish-wiki-llama70b.xlsx
  expanded <- str_match(
    filename_lower,
    "^([a-z]{2})-author-([a-z]+)-wiki-"
  )
  
  if (!is.na(expanded[1, 2])) {
    wiki_code <- wiki_name_to_code[[expanded[1, 3]]]
    
    if (is.null(wiki_code)) {
      stop("Unknown Wiki language in filename: ", filename)
    }
    
    return(tibble(
      author_language = expanded[1, 2],
      wiki_language = wiki_code
    ))
  }
  
  stop("Could not parse author and Wiki languages from filename: ", filename)
}

count_file <- function(path, source_name) {
  filename <- basename(path)
  language_info <- parse_filename(filename)
  
  dat <- read_excel(path)
  names(dat) <- tolower(names(dat))
  
  if (!all(c("id", "writer", "language") %in% names(dat))) {
    stop("Expected id, writer, and language columns in: ", filename)
  }
  
  # Keep one row per ID in each file, retaining the first row's other fields.
  dat <- dat |>
    mutate(id = as.character(id)) |>
    filter(!is.na(id), id != "") |>
    distinct(id, .keep_all = TRUE) |>
    mutate(
      writer_text = toupper(as.character(writer)),
      writer_is_true = !is.na(writer) &
        writer_text %in% c("TRUE", "1"),
      language_text = as.character(language),
      language_missing = is.na(language_text) |
        trimws(language_text) == "",
      status = case_when(
        is.na(writer) ~ "ERROR",
        writer_is_true & language_missing ~ "NOLANG",
        TRUE ~ "CLASSIFIABLE"
      )
    )
  
  n_ids <- nrow(dat)
  n_error <- sum(dat$status == "ERROR")
  n_nolang <- sum(dat$status == "NOLANG")
  
  tibble(
    source = source_name,
    file = filename,
    author_language = language_info$author_language,
    wiki_language = language_info$wiki_language,
    unique_ids = n_ids,
    ERROR = n_error,
    ERROR_ratio = if (n_ids > 0) n_error / n_ids else NA_real_,
    ERROR_percent = if (n_ids > 0) 100 * n_error / n_ids else NA_real_,
    NOLANG = n_nolang,
    NOLANG_ratio = if (n_ids > 0) n_nolang / n_ids else NA_real_,
    NOLANG_percent = if (n_ids > 0) 100 * n_nolang / n_ids else NA_real_
  )
}

per_file <- imap_dfr(folders, function(folder, source_name) {
  if (!dir.exists(folder)) {
    stop("Folder does not exist: ", folder)
  }
  
  files <- list.files(
    folder,
    pattern = "\\.xlsx$",
    full.names = TRUE
  ) |>
    sort()
  
  if (length(files) == 0) {
    stop("No Excel files found in: ", folder)
  }
  
  map_dfr(files, count_file, source_name = source_name)
})

summarise_rates <- function(data, groups) {
  data |>
    group_by(across(all_of(groups))) |>
    summarise(
      unique_ids = sum(unique_ids),
      ERROR = sum(ERROR),
      ERROR_ratio = ERROR / unique_ids,
      ERROR_percent = 100 * ERROR_ratio,
      NOLANG = sum(NOLANG),
      NOLANG_ratio = NOLANG / unique_ids,
      NOLANG_percent = 100 * NOLANG_ratio,
      .groups = "drop"
    )
}

# One row per source/model and author–Wiki comparison.
by_language_wiki <- summarise_rates(
  per_file,
  c("source", "author_language", "wiki_language")
)

# Combined rates across all files within each source/model folder.
overall <- summarise_rates(per_file, "source")

print(per_file)
print(by_language_wiki)
print(overall)

write_xlsx(
  list(
    file_level = per_file,
    by_language_wiki = by_language_wiki,
    overall = overall
  ),
  file.path(root, "error-nolang-ratios.xlsx")
)
