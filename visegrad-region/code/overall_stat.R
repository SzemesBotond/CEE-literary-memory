# Run distributions.R first.
library(dplyr)
library(ggplot2)

all_authors <- bind_rows(author_groups, .id = "Language")
unique_authors <- all_authors %>% distinct(Language, cid, .keep_all = TRUE)
all_wiki <- bind_rows(wiki_groups, .id = "Wikipedia")

# Number of authors: appearances, unique authors, and Wikipedia totals.
num <- bind_rows(
  all_authors %>% count(Language, name = "Num") %>%
    mutate(Facet = "by Language (incl. duplicates)"),
  unique_authors %>% count(Language, name = "Num") %>%
    mutate(Facet = "by Language (unique)"),
  all_wiki %>% count(Wikipedia, name = "Num") %>%
    rename(Language = Wikipedia) %>% mutate(Facet = "by Wikipedia")
)
fig <- ggplot(num, aes(Language, Num, fill = Language)) +
  geom_col() +
  facet_wrap(~Facet, scales = "free") +
  scale_fill_manual(values = colors, guide = "none") +
  labs(title = "Number of Authors", x = "", y = "Number") + theme_bw()
ggsave(file.path(figure_dir, "01-num-authors.png"), fig,
       width = 40, height = 15, units = "cm", bg = "white", dpi = 300)

# Gender by author language (unique authors) and by Wikipedia (appearances).
gender <- bind_rows(
  unique_authors %>% count(Language, nem) %>%
    mutate(Group = paste(Language, "Authors")),
  all_wiki %>% count(Wikipedia, nem) %>%
    mutate(Group = paste(Wikipedia, "Wikipedia"))
) %>%
  group_by(Group) %>% mutate(freq = n / sum(n)) %>% ungroup() %>%
  mutate(Gender = case_when(
    nem == "férfi" ~ "Man",
    nem == "nő" ~ "Woman",
    nem == "http://www.wikidata.org/entity/Q1052281" ~ "Trans woman",
    is.na(nem) ~ "Unknown",
    TRUE ~ nem
  ))
fig <- ggplot(gender, aes("", freq, fill = Gender)) +
  geom_col() +
  facet_wrap(~Group, ncol = 2) +
  geom_text(aes(label = ifelse(round(freq * 100) == 0, "", round(freq * 100))),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c(Man = "lightblue", Woman = "pink",
                               "Trans woman" = "green", Unknown = "gray")) +
  labs(title = "Distribution of Gender", x = "", y = "") + theme_minimal()
ggsave(file.path(figure_dir, "02-gender.png"), fig,
       width = 10, height = 17, units = "cm", bg = "white", dpi = 300)

# Birth years: use the original first-year-before-comma conversion.
auth_year <- all_authors %>%
  mutate(Year = as.numeric(gsub(",.*", "", születési_idő)))
mean_year <- auth_year %>% group_by(Language) %>%
  summarise(Mean = mean(Year, na.rm = TRUE))
median_year <- auth_year %>% group_by(Language) %>%
  summarise(Median = median(Year, na.rm = TRUE))
year_by_wiki <- auth_year %>% group_by(Language, Wikipedia) %>%
  summarise(Mean = mean(Year, na.rm = TRUE), Median = median(Year, na.rm = TRUE),
            .groups = "drop")

# Individual tradition plots, as displayed in the original script.
for (language in names(author_groups)) {
  fig <- ggplot(filter(auth_year, Language == language), aes(Year, color = Wikipedia)) +
    geom_density(adjust = 1.5) + scale_color_manual(values = colors) +
    labs(title = paste(language, "authors' year of birth")) + theme_bw()
  print(fig)
}

# Combined birth-year plot counts each foreign-Wikipedia appearance.
fig <- ggplot(auth_year, aes(Year, color = Language)) +
  geom_density(adjust = 1.5, linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1200, 2020, 100), limits = c(1200, 2020)) +
  scale_color_manual(values = colors) +
  labs(title = "Authors' Year of Birth by Language", x = "Year", y = "Density") + theme_bw()
ggsave(file.path(figure_dir, "03-birth-of-year.png"), fig,
       width = 20, height = 15, units = "cm", bg = "white", dpi = 300)
