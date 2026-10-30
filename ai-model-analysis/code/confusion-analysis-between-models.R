library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)

# --- classifier ---
lang_by_index <- c("cs", "cs", "cs", "hu")

add_valid_writer <- function(df, idx, lang_vec = lang_by_index) {
  if (idx > length(lang_vec)) {
    stop("No language defined for file index ", idx)
  }
  
  lang <- lang_vec[idx]
  
  df %>%
    mutate(
      `valid-writer` = case_when(
        writer == "TRUE" & is.na(language)       ~ "NOLANG",
        is.na(writer)                            ~ "ERROR",
        writer == "TRUE" & grepl(lang, language) ~ "YES",
        !is.na(writer)                           ~ "NO",
        TRUE                                     ~ NA_character_
      )
    )
}


# --- main comparison function ---
compare_folders_by_index <- function(folder_a, folder_b) {
  files_a <- list.files(folder_a, pattern = "\\.xlsx$", full.names = TRUE) |> sort()
  files_b <- list.files(folder_b, pattern = "\\.xlsx$", full.names = TRUE) |> sort()
  
  n <- min(length(files_a), length(files_b))
  if (length(files_a) != length(files_b)) {
    message("Warning: folders contain different number of .xlsx files. Comparing first ", n, " pairs only.")
  }
  
  map_dfr(seq_len(n), function(i) {
    a <- read_excel(files_a[i]) |>
      distinct(id, .keep_all = TRUE) |>
      add_valid_writer(i)
    
    b <- read_excel(files_b[i]) |>
      distinct(id, .keep_all = TRUE) |>
      add_valid_writer(i)
    
    a2 <- a %>% select(id, vw_a = `valid-writer`)
    b2 <- b %>% select(id, vw_b = `valid-writer`)
    
    j <- full_join(a2, b2, by = "id") %>%
      mutate(
        in_a = !is.na(vw_a),
        in_b = !is.na(vw_b),
        comparable = in_a & in_b,
        agree = comparable & (vw_a == vw_b),
        disagree = comparable & (vw_a != vw_b)
      )
    
    comparable_n <- sum(j$comparable, na.rm = TRUE)
    agree_n      <- sum(j$agree, na.rm = TRUE)
    disagree_n   <- sum(j$disagree, na.rm = TRUE)
    
    tibble(
      pair_index = i,
      file_a = basename(files_a[i]),
      file_b = basename(files_b[i]),
      ids_in_both = comparable_n,
      agree = agree_n,
      disagree = disagree_n,
      agree_rate = ifelse(comparable_n > 0, agree_n / comparable_n, NA_real_),
      only_in_a = sum(j$in_a & !j$in_b, na.rm = TRUE),
      only_in_b = sum(!j$in_a & j$in_b, na.rm = TRUE)
    )
  })
}


# --- confusion table by file ---
confusion_by_file <- function(folder_a, folder_b, lang_vec = lang_by_index) {
  files_a <- list.files(folder_a, pattern = "\\.xlsx$", full.names = TRUE) |> sort()
  files_b <- list.files(folder_b, pattern = "\\.xlsx$", full.names = TRUE) |> sort()
  
  len_a <- length(files_a)
  len_b <- length(files_b)
  
  if (len_a == 0 || len_b == 0) {
    stop("One or both folders contain no .xlsx files")
  }
  
  n <- min(len_a, len_b)
  if (len_a != len_b) {
    message("Warning: different number of files. Comparing first ", n, " pairs only.")
  }
  
  purrr::map_dfr(seq_len(n), function(i) {
    a <- read_excel(files_a[i]) |>
      distinct(id, .keep_all = TRUE) |>
      add_valid_writer(idx = i, lang_vec = lang_vec) |>
      select(id, length, vw_a = `valid-writer`)
    
    b <- read_excel(files_b[i]) |>
      distinct(id, .keep_all = TRUE) |>
      add_valid_writer(idx = i, lang_vec = lang_vec) |>
      select(id, length, vw_b = `valid-writer`)
    
    inner_join(a, b, by = "id") %>%
      count(vw_a, vw_b, name = "n") %>%
      mutate(
        pair_index = i,
        file_a = basename(files_a[i]),
        file_b = basename(files_b[i]),
        lang_code = lang_vec[(i - 1) %% length(lang_vec) + 1]
      ) %>%
      select(pair_index, file_a, file_b, lang_code, vw_a, vw_b, n)
  })
}


# --- YES/NO conflicts ---
yes_no_conflicts <- function(folder_a, folder_b, lang_vec = lang_by_index) {
  files_a <- list.files(folder_a, pattern = "\\.xlsx$", full.names = TRUE) |> sort()
  files_b <- list.files(folder_b, pattern = "\\.xlsx$", full.names = TRUE) |> sort()
  
  len_a <- length(files_a)
  len_b <- length(files_b)
  
  if (len_a == 0 || len_b == 0) {
    stop("One or both folders contain no .xlsx files")
  }
  
  n <- min(len_a, len_b)
  
  purrr::map_dfr(seq_len(n), function(i) {
    a <- read_excel(files_a[i]) |>
      distinct(id, .keep_all = TRUE) |>
      add_valid_writer(idx = i, lang_vec = lang_vec) |>
      select(id, length, vw_a = `valid-writer`)
    
    b <- read_excel(files_b[i]) |>
      distinct(id, .keep_all = TRUE) |>
      add_valid_writer(idx = i, lang_vec = lang_vec) |>
      select(id, length, vw_b = `valid-writer`)
    
    inner_join(a, b, by = "id") |>
      filter(
        (vw_a == "YES" & vw_b == "NO") |
          (vw_a == "NO"  & vw_b == "YES")
      ) |>
      mutate(
        pair_index = i,
        file_a = basename(files_a[i]),
        file_b = basename(files_b[i]),
        lang_code = lang_vec[(i - 1) %% length(lang_vec) + 1],
        conflict_type = paste(vw_a, "vs", vw_b)
      ) |>
      select(
        pair_index, file_a, file_b, lang_code,
        id, vw_a, vw_b, conflict_type
      )
  })
}

folder1 <- "gpt-os-120"
folder2 <- "llama70b"

summary_tbl <- compare_folders_by_index(folder1, folder2)
summary_tbl



write_xlsx(
  summary_tbl,
  path = "gpt-llama70b-comparison.xlsx"
)


conf_tbl <- confusion_by_file(folder1, folder2)
write_xlsx(
  conf_tbl,
  path = "gpt-llama70b-confusion.xlsx"
)


yesno_tbl <- yes_no_conflicts(folder1, folder2)
write_xlsx(
  yesno_tbl,
  path = "gpt-llama70b-confusion-yesno.xlsx"
)
