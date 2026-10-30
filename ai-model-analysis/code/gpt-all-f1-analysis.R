library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(writexl)

root <- "ai-model-analysis"
gpt_all_dir <- file.path(root, "gpt-all")
final_tables <- "final-visegrad"

valid_file <- file.path(final_tables, "all-unique-authors-update.xlsx")
deleted_file <- file.path(
  final_tables,
  "deleted.xlsx"
)

# Author-language code -> gold-standard language and deleted-record sheet.
author_lang <- c(
  cz = "Cseh",
  hu = "Magyar",
  sk = "Szlovák",
  pl = "Lengyel"
)

deleted_sheet <- c(
  cz = 4,
  hu = 3,
  sk = 2,
  pl = 1
)

# Author-language code -> language code expected in the model output.
output_lang_code <- c(
  cz = "cs",
  hu = "hu",
  sk = "sk",
  pl = "pl"
)

wiki_names <- c(
  cz = "czech",
  hu = "hungarian",
  sk = "slovak",
  pl = "polish"
)

# Extract IDs from a data frame whose ID column may be id, ID, Id, or cid.
extract_ids <- function(df, candidates = c("id", "cid")) {
  names(df) <- tolower(names(df))
  id_col <- intersect(candidates, names(df))
  
  if (length(id_col) == 0) {
    stop(
      "No ID column found. Available columns: ",
      paste(names(df), collapse = ", ")
    )
  }
  
  tibble(id = as.character(df[[id_col[[1]]]])) |>
    filter(!is.na(id), id != "") |>
    distinct(id)
}

# Load valid-author reference.
valid_ref <- read_excel(valid_file)
names(valid_ref) <- tolower(names(valid_ref))

if (!all(c("id", "lang") %in% names(valid_ref))) {
  stop("The valid-author file must contain ID and Lang columns.")
}

# Load deleted/invalid IDs, using the relevant sheet for each author language.
invalid_ids_by_author <- set_names(
  map(names(author_lang), function(code) {
    read_excel(
      deleted_file,
      sheet = deleted_sheet[[code]]
    ) |>
      extract_ids(c("id", "cid")) |>
      pull(id)
  }),
  names(author_lang)
)

score_file <- function(path) {
  filename <- basename(path)
  
  # Filename format: author-code + wiki-code + "wiki.xlsx"
  # Example: cz-huwiki.xlsx
  filename_parts <- str_match(
    tolower(filename),
    "^([a-z]{2})-([a-z]{2})wiki\\.xlsx$"
  )
  
  author_code <- filename_parts[, 2]
  wiki_code <- filename_parts[, 3]
  
  if (is.na(author_code) || is.na(wiki_code)) {
    stop("Could not parse author and Wiki languages from filename: ", filename)
  }
  
  if (!author_code %in% names(author_lang)) {
    stop("Unknown author-language code in filename: ", filename)
  }
  
  wiki_name <- wiki_names[[wiki_code]]
  
  if (is.null(wiki_name)) {
    stop("Unknown Wiki-language code in filename: ", filename)
  }
  
  # Read the file and deduplicate by ID, retaining the first row and its
  # other fields, including length.
  dat <- read_excel(path)
  names(dat) <- tolower(names(dat))
  
  if (!all(c("id", "writer", "language") %in% names(dat))) {
    stop("Expected id, writer, and language columns in: ", filename)
  }
  
  dat <- dat |>
    mutate(id = as.character(id)) |>
    distinct(id, .keep_all = TRUE)
  
  valid_ids <- valid_ref |>
    filter(lang == author_lang[[author_code]]) |>
    extract_ids("id") |>
    pull(id)
  
  invalid_ids <- invalid_ids_by_author[[author_code]]
  
  overlap <- intersect(valid_ids, invalid_ids)
  if (length(overlap) > 0) {
    stop(
      "IDs occur in both valid and invalid reference lists for ",
      author_code, ": ",
      paste(overlap, collapse = ", ")
    )
  }
  
  target_lang <- output_lang_code[[author_code]]
  
  dat <- dat |>
    mutate(
      writer_text = toupper(as.character(writer)),
      language_text = as.character(language),
      writer_is_true = !is.na(writer) &
        writer_text %in% c("TRUE", "1"),
      `valid-writer` = case_when(
        writer_is_true & is.na(language_text) ~ "NOLANG",
        is.na(writer) ~ "ERROR",
        writer_is_true &
          str_detect(coalesce(language_text, ""), fixed(target_lang)) ~ "YES",
        !is.na(writer) ~ "NO",
        TRUE ~ NA_character_
      ),
      type = case_when(
        id %in% valid_ids ~ "valid",
        id %in% invalid_ids ~ "invalid",
        TRUE ~ NA_character_
      )
    )
  
  # Score only records with a gold-standard label and a classifiable YES/NO output.
  scored <- dat |>
    filter(
      !is.na(type),
      `valid-writer` %in% c("YES", "NO")
    )
  
  tp <- sum(scored$type == "valid" & scored$`valid-writer` == "YES")
  fp <- sum(scored$type == "invalid" & scored$`valid-writer` == "YES")
  tn <- sum(scored$type == "invalid" & scored$`valid-writer` == "NO")
  fn <- sum(scored$type == "valid" & scored$`valid-writer` == "NO")
  
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  
  f1 <- if (!is.na(precision) && !is.na(recall) && precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  
  unique_ids <- n_distinct(dat$id)
  error_n <- sum(dat$`valid-writer` == "ERROR", na.rm = TRUE)
  nolang_n <- sum(dat$`valid-writer` == "NOLANG", na.rm = TRUE)
  classifiable_n <- sum(
    dat$`valid-writer` %in% c("YES", "NO"),
    na.rm = TRUE
  )
  
  tibble(
    model = "gpt-oss:120b",
    file = filename,
    author_language = author_lang[[author_code]],
    wiki_language = wiki_name,
    unique_ids = unique_ids,
    scored = nrow(scored),
    classifiable = classifiable_n,
    analyzed_ratio = classifiable_n / unique_ids,
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn,
    ERROR = error_n,
    NOLANG = nolang_n,
    without_gold_label = sum(is.na(dat$type)),
    precision = precision,
    recall = recall,
    F1 = f1
  )
}

files <- list.files(
  gpt_all_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
) |>
  sort()

if (length(files) != 12) {
  stop("Expected 12 Excel files in gpt-all; found ", length(files))
}

per_dataset <- map_dfr(files, score_file)

# Pooled metrics: calculate from the summed confusion-matrix counts.
pooled <- per_dataset |>
  summarise(
    datasets = n(),
    scored = sum(scored),
    TP = sum(TP),
    FP = sum(FP),
    TN = sum(TN),
    FN = sum(FN)
  ) |>
  mutate(
    precision = ifelse(TP + FP > 0, TP / (TP + FP), NA_real_),
    recall = ifelse(TP + FN > 0, TP / (TP + FN), NA_real_),
    F1 = ifelse(
      !is.na(precision) & !is.na(recall) & precision + recall > 0,
      2 * precision * recall / (precision + recall),
      NA_real_
    )
  )

# Macro-average: mean of the 12 per-dataset metrics.
macro_average <- per_dataset |>
  summarise(
    datasets = n(),
    mean_precision = mean(precision, na.rm = TRUE),
    mean_recall = mean(recall, na.rm = TRUE),
    mean_F1 = mean(F1, na.rm = TRUE)
  )

print(per_dataset)
print(pooled)
print(macro_average)

write_xlsx(
  list(
    per_dataset = per_dataset,
    pooled = pooled,
    macro_average = macro_average
  ),
  file.path(root, "gpt-all-f1-results.xlsx")
)
