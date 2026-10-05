# Run this script first, then memory_scores.R, canon.R, and overall_stat.R.
library(readxl)
library(dplyr)
library(ggplot2)
library(waffle)

# Relative paths: edit these folders as needed.
data_dir <- "data"
figure_dir <- "figures"
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

colors <- c(Hungarian = "#000000", Polish = "#666666",
            Czech = "lightgray", Slovak = "#999999")

# Read the original twelve worksheets, retaining the first seven columns.
hu_pol <- read_excel(file.path(data_dir, "hungarian-author-final.xlsx"), sheet = "pl-wp")[, 1:7]
hu_cz  <- read_excel(file.path(data_dir, "hungarian-author-final.xlsx"), sheet = "cz-wp")[, 1:7]
hu_sl  <- read_excel(file.path(data_dir, "hungarian-author-final.xlsx"), sheet = "sl-wp")[, 1:7]
pol_hu <- read_excel(file.path(data_dir, "polish-author-final.xlsx"), sheet = "hu-wp")[, 1:7]
pol_cz <- read_excel(file.path(data_dir, "polish-author-final.xlsx"), sheet = "cz-wp")[, 1:7]
pol_sl <- read_excel(file.path(data_dir, "polish-author-final.xlsx"), sheet = "sl-wp")[, 1:7]
cz_hu  <- read_excel(file.path(data_dir, "czech-author-final.xlsx"), sheet = "hu-wp")[, 1:7]
cz_pol <- read_excel(file.path(data_dir, "czech-author-final.xlsx"), sheet = "pl-wp")[, 1:7]
cz_sl  <- read_excel(file.path(data_dir, "czech-author-final.xlsx"), sheet = "sl-wp")[, 1:7]
sl_hu  <- read_excel(file.path(data_dir, "slovak-author-final.xlsx"), sheet = "hu-wp")[, 1:7]
sl_pol <- read_excel(file.path(data_dir, "slovak-author-final.xlsx"), sheet = "pl-wp")[, 1:7]
sl_cz  <- read_excel(file.path(data_dir, "slovak-author-final.xlsx"), sheet = "cz-wp")[, 1:7]

# Group authors by their literary language and retain the Wikipedia labels.
hu_all  <- bind_rows(Polish = hu_pol, Czech = hu_cz, Slovak = hu_sl, .id = "Wikipedia")
pol_all <- bind_rows(Hungarian = pol_hu, Czech = pol_cz, Slovak = pol_sl, .id = "Wikipedia")
cz_all  <- bind_rows(Hungarian = cz_hu, Polish = cz_pol, Slovak = cz_sl, .id = "Wikipedia")
sl_all  <- bind_rows(Hungarian = sl_hu, Polish = sl_pol, Czech = sl_cz, .id = "Wikipedia")
author_groups <- list(Hungarian = hu_all, Polish = pol_all, Czech = cz_all, Slovak = sl_all)

# Group the foreign authors appearing on each Wikipedia.
hu_wiki  <- bind_rows(cz_hu, pol_hu, sl_hu)
pol_wiki <- bind_rows(cz_pol, hu_pol, sl_pol)
cz_wiki  <- bind_rows(pol_cz, hu_cz, sl_cz)
sl_wiki  <- bind_rows(pol_sl, hu_sl, cz_sl)
wiki_groups <- list(Hungarian = hu_wiki, Polish = pol_wiki, Czech = cz_wiki, Slovak = sl_wiki)

# Two original proportions: representation of a tradition and composition of a site.
author_totals <- sapply(author_groups, function(x) n_distinct(x$cid))
wiki_totals <- sapply(wiki_groups, nrow)
distributions <- bind_rows(author_groups, .id = "Language") %>%
  count(Language, Wikipedia, name = "Number") %>%
  mutate(Coverage = Number / author_totals[Language],
         Composition = Number / wiki_totals[Wikipedia])

# Author distributions across the three foreign-language Wikipedias.
for (language in names(author_groups)) {
  parts <- distributions %>% filter(Language == language)
  fig <- ggplot(parts, aes(Wikipedia, Coverage * 100, fill = Wikipedia)) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(round(Coverage * 100), "%")), vjust = -0.3) +
    scale_fill_manual(values = colors) +
    ylim(0, 105) +
    labs(title = paste(language, "authors in foreign Wikipedias"),
         x = "", y = "Percentage") +
    theme_bw() + theme(legend.position = "none")
  ggsave(file.path(figure_dir, paste0("04-", tolower(language), "-authors.png")),
         fig, width = 15, height = 15, units = "cm", bg = "white", dpi = 300)
}

# Wikipedia composition by the language of foreign authors.
for (wikipedia in names(wiki_groups)) {
  parts <- distributions %>% filter(Wikipedia == wikipedia)
  cells <- round(parts$Composition * 100)
  # Adjust rounding so the waffle has exactly 100 squares.
  cells[which.max(cells)] <- cells[which.max(cells)] + 100 - sum(cells)
  names(cells) <- paste0(parts$Language, " (", round(parts$Composition * 100), "%)")
  fig <- waffle(cells, rows = 10, colors = unname(colors[parts$Language])) +
    labs(title = paste(wikipedia, "Wikipedia"),
         subtitle = "Distribution of foreign authors by language")
  ggsave(file.path(figure_dir, paste0("05-", tolower(wikipedia), "-wiki-authors.png")),
         fig, width = 15, height = 15, units = "cm", bg = "white", dpi = 300)
}
