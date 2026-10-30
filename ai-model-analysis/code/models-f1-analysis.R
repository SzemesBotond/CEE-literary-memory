library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(writexl)

root <- "ai-model-analysis"
final_tables <- "final-tabels"

model_dirs <- c(
  gemma27  = file.path(root, "gemma27"),
  gpt_oss120 = file.path(root, "gpt-os-120"),
  llama8b  = file.path(root, "llama8b"),
  llama70b = file.path(root, "llama70b")
)

# Map filename author-language prefixes to the gold-standard labels,
# deleted-record sheet numbers, and language codes expected in model output.
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

output_lang_code <- c(
  cz = "cs",
  hu = "hu",
  sk = "sk",
  pl = "pl"
)

valid_ref <- read_excel(
  file.path(final_tables, "all-unique-authors-update.xlsx")
)

deleted_file <- file.path(
  final_tables,
  "deleted.xlsx"
)

extract_ids <- function(df, candidates = c("id", "cid")) {
  names(df) <- tolower(names(df))
  id_col <- intersect(candidates, names(df))
  
  if (length(id_col) == 0) {
    stop("No ID column found. Available columns: ",
         paste(names(df), collapse = ", "))
  }
  
  df |>
    transmute(id = as.character(.data[[id_col[[1]]]])) |>
    filter(!is.na(id), id != "") |>
    distinct(id)
}

score_file <- function(path, model_name) {
  filename <- basename(path)
  
  # Get the author-language prefix, e.g. "cz" from "cz-author-polish-wiki-..."
  author_code <- str_match(filename, "^([a-z]{2})-author-")[, 2]
  
  if (is.na(author_code) || !author_code %in% names(author_lang)) {
    stop("Could not identify author language from filename: ", filename)
  }
  
  # Read and deduplicate by ID; .keep_all retains columns such as length.
  dat <- read_excel(path)
  names(dat) <- tolower(names(dat))
  
  dat <- dat |>
    distinct(id, .keep_all = TRUE) |>
    mutate(id = as.character(id))
  
  valid_ids <- valid_ref |>
    filter(tolower(Lang) == tolower(author_lang[[author_code]])) |>
    extract_ids(c("id")) |>
    pull(id)
  
  deleted_ids <- read_excel(
    deleted_file,
    sheet = deleted_sheet[[author_code]]
  ) |>
    extract_ids(c("id", "cid")) |>
    pull(id)

    overlap <- intersect(valid_ids, deleted_ids)
  if (length(overlap) > 0) {
    stop(
      "Some IDs appear in both the valid and deleted reference lists for ",
      author_code, ": ",
      paste(overlap, collapse = ", ")
    )
  }
  
  target_lang <- output_lang_code[[author_code]]
  
  dat <- dat |>
    mutate(
      writer_text = as.character(writer),
      language_text = as.character(language),
      `valid-writer` = case_when(
        writer_text == "TRUE" & is.na(language_text) ~ "NOLANG",
        is.na(writer) ~ "ERROR",
        writer_text == "TRUE" &
          str_detect(coalesce(language_text, ""), fixed(target_lang)) ~ "YES",
        !is.na(writer) ~ "NO",
        TRUE ~ NA_character_
      ),
      type = case_when(
        id %in% valid_ids ~ "valid",
        id %in% deleted_ids ~ "invalid",
        TRUE ~ NA_character_
      )
    )
  
  # Only score records with a gold-standard label and a classifiable YES/NO output.
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
  
  tibble(
    model = model_name,
    file = filename,
    author_language = author_lang[[author_code]],
    wiki_language = str_match(filename, "^[^-]+-author-([^-]+)-wiki-")[, 2],
    unique_ids = n_distinct(dat$id),
    scored = nrow(scored),
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn,
    ERROR = sum(dat$`valid-writer` == "ERROR", na.rm = TRUE),
    NOLANG = sum(dat$`valid-writer` == "NOLANG", na.rm = TRUE),
    without_gold_label = sum(is.na(dat$type)),
    precision = precision,
    recall = recall,
    F1 = f1
  )
}

f1_results <- imap_dfr(model_dirs, function(model_dir, model_name) {
  files <- list.files(
    model_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE
  ) |>
    sort()
  
  if (length(files) != 4) {
    stop(
      "Expected 4 Excel files in ", model_dir,
      "; found ", length(files)
    )
  }
  
  map_dfr(files, score_file, model_name = model_name)
})

print(f1_results)

write_xlsx(
  f1_results,
  file.path(root, "f1-metrics-four-models.xlsx")
)
