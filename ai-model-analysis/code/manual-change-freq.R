library(readxl)

root <- "ai-model-analysis/ai-human-analysis"

# Select language folders such as cz, hu, and ro; exclude folders such as 00-analysis.
language_folders <- list.dirs(
  root,
  recursive = FALSE,
  full.names = TRUE
)

language_folders <- language_folders[
  grepl("^[a-z]{2}$", basename(language_folders), ignore.case = TRUE)
]

results <- list()

for (folder in language_folders) {
  files <- list.files(
    folder,
    pattern = "^debated.*\\.(xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  for (file in files) {
    data <- read_excel(file, .name_repair = "unique")
    
    human_col <- which(tolower(trimws(names(data))) == "human_analysis")
    
    if (length(human_col) != 1) {
      warning("Skipping file: could not identify one human_analysis column in ", file)
      next
    }
    
    other_cols <- setdiff(seq_along(data), human_col)
    human_values <- toupper(trimws(as.character(data[[human_col]])))
    
    contradictory_annotations <- 0L
    matching_annotations <- 0L
    excluded_rows <- 0L
    
    for (i in seq_len(nrow(data))) {
      human_value <- human_values[i]
      
      # Only compare rows where human_analysis is YES or NO.
      if (is.na(human_value) || !human_value %in% c("YES", "NO")) {
        excluded_rows <- excluded_rows + 1L
        next
      }
      
      other_values <- vapply(
        other_cols,
        function(j) toupper(trimws(as.character(data[[j]][i]))),
        character(1)
      )
      
      # Count only other cells containing YES or NO.
      comparable <- !is.na(other_values) &
        other_values %in% c("YES", "NO")
      
      contradictory_annotations <- contradictory_annotations +
        sum(comparable & other_values != human_value)
      
      matching_annotations <- matching_annotations +
        sum(comparable & other_values == human_value)
    }
    
    results[[length(results) + 1]] <- data.frame(
      language = basename(folder),
      file = basename(file),
      contradictory_annotations = contradictory_annotations,
      matching_annotations = matching_annotations,
      compared_annotations = contradictory_annotations + matching_annotations,
      rows_excluded_invalid_human_analysis = excluded_rows
    )
  }
}

if (length(results) == 0) {
  stop("No matching debated Excel files were found.")
}

per_file <- do.call(rbind, results)

totals <- data.frame(
  language = "ALL",
  file = "ALL FILES",
  contradictory_annotations = sum(per_file$contradictory_annotations),
  matching_annotations = sum(per_file$matching_annotations),
  compared_annotations = sum(per_file$compared_annotations),
  rows_excluded_invalid_human_analysis =
    sum(per_file$rows_excluded_invalid_human_analysis)
)

output <- rbind(per_file, totals)

print(output)

write.csv(
  output,
  file.path(root, "annotation_comparison_summary.csv"),
  row.names = FALSE
)

cat(sprintf(
  "\nAcross all files, %d annotation instances contradicted human_analysis and %d matched it, out of %d comparable annotations.\n",
  totals$contradictory_annotations,
  totals$matching_annotations,
  totals$compared_annotations
))