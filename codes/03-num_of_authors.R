library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(ggplot2)

df <- read_excel("merged_authors.xlsx") # see code 02

colorcode <- c("cz" = "#11457E",
               "hr" = "#FF0000",
               "hu" = "#436F4D",
               "pl" = "#DC143C",
               "ro" = "#FFD700",
               "sk" = "#0B4EA2",
               "sl" = "#1D854C",
               "sr" = "#C6363C",
               "uk" = "#0057B7")

# Distribution of birth years
df1 <- df %>%
  mutate(
    # Extract first 4 digits from születési_idő aka date of birth
    birth_year = str_extract(születési_idő, "^\\d{4}"),
    birth_year = as.numeric(birth_year)
  ) %>%
  filter(!is.na(birth_year))   # drop NAs

# Overall density
overall_density <- ggplot(df1, aes(x = birth_year)) +
  geom_density(size = 1, color = "black") +  # thicker overall line
  geom_density(aes(color = language), size = 0.7, alpha = 0.7) +  # by language
  #geom_density(aes(color = wiki), size = 0.7, alpha = 0.7) + # by wiki
  xlim(1400,2000)+
  labs(
    title = "",
    x = "Birth year",
    y = "Density"
  ) +
  scale_color_manual(
    values = colorcode)+
  theme_minimal()

overall_density
ggsave("04-year-density-author.png", overall_density, 
       width = 45, height = 20, units = "cm",
       dpi = 300,
       bg= "white")

###################################xx

# Genger distribution
colorcode1 = c(
  "male" = "lightblue",
  "female" = "pink",
  "trans man" = "darkblue",
  "trans woman" = "red",
  "unknown" = "grey"
)

unique(df$nem)

df_clean <- df %>%
  filter(!is.na(nem)) %>%
  mutate(nem = factor(nem, levels = names(colorcode1)))

unique(df_clean$nem)

# Facet by language
p_language <- ggplot(df_clean, aes(x = language, fill = nem)) +
  geom_bar(position = "stack") +
  labs(
    title = "Distribution of gender by language",
    x = "Language", y = "Count", fill = "Gender"
  ) +scale_fill_manual(values = colorcode1)+
  theme_minimal()

# Facet by wiki
p_wiki <- ggplot(df_clean, aes(x = wiki, fill = nem)) +
  geom_bar(position = "stack") +
  labs(
    title = "Distribution of gender by wiki",
    x = "Wiki", y = "Count", fill = "Gender"
  ) +
  scale_fill_manual(
    values = colorcode1)+
  theme_minimal()

# Show plots
p_language
p_wiki

ggsave("03-gender-lang.png", p_language, 
       width = 35, height = 15, units = "cm",
       dpi = 300,
       bg= "white")
#########

#  Size

colorcode_author <- c(
  "cz" = "#11457E",
  "hr" = "#FF0000",
  "hu" = "#436F4D",
  "pl" = "#DC143C",
  "ro" = "#FFD700",
  "sk" = "#0B4EA2",
  "sl" = "#1D854C",
  "sr" = "#C6363C",
  "uk" = "#0057B7"
)

colorcode_wiki <- c("cz-wiki" = "#11457E",
               "hr-wiki" = "#FF0000",
               "hu-wiki" = "#436F4D",
               "pl-wiki" = "#DC143C",
               "ro-wiki" = "#FFD700",
               "sk-wiki" = "#0B4EA2",
               "sl-wiki" = "#1D854C",
               "sr-wiki" = "#C6363C",
               "uk-wiki" = "#0057B7")

# 1. Number of rows per wiki
p_wiki <- df %>%
  group_by(wiki) %>%
  summarise(unique_authors = n_distinct(author_id))  %>%
  ggplot(aes(x = wiki, y = unique_authors, fill = wiki)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of authors per Wiki", x = "Wiki", y = "Count") +
  theme_minimal() +
  scale_fill_manual(
    values = colorcode_wiki)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_wiki

# 2. Number of unique author_id per wiki
p_language_unique <- df %>%
  group_by(language) %>%
  summarise(unique_authors = n_distinct(author_id)) %>%
  ggplot(aes(x = language, y = unique_authors, fill = language)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of unique authors per language", x = "Wiki", y = "Unique authors") +
  theme_minimal() +
  scale_fill_manual(
    values = colorcode_author)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_language_unique

# 3. Number of rows per language
p_language <- df %>%
  group_by(language) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = language, y = count, fill = language)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorcode) +
  labs(title = "Number of authors per language", x = "Language", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_language

ggsave("01-num_of_authors_wiki.png", p_wiki, 
       width = 35, height = 15, units = "cm",
       dpi = 300,
       bg= "white")
