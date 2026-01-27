library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)
library(grid)

# Read data + build weighted edges from shared author_ids

df <- read_excel("merged_authors.xlsx") %>%
  transmute(
    author_id = as.character(author_id),
    wiki = as.character(wiki)
  ) %>%
  filter(!is.na(author_id), !is.na(wiki), wiki != "") %>%
  distinct(author_id, wiki)  # avoid double-counting same author in same wiki

# Create wiki-wiki pairs per author, then count across authors
edges <- df %>%
  group_by(author_id) %>%
  summarise(wikis = list(sort(unique(wiki))), .groups = "drop") %>%
  mutate(
    pairs = map(wikis, ~{
      if (length(.x) < 2) return(list())
      combn(.x, 2, simplify = FALSE)
    })
  ) %>%
  select(author_id, pairs) %>%
  unnest(pairs) %>%
  reframe(                               
    from = map_chr(pairs, 1),
    to   = map_chr(pairs, 2)
  ) %>%
  count(from, to, name = "weight") %>%
  arrange(desc(weight))

# Create igraph object
g <- graph_from_data_frame(edges, directed = FALSE)

#  Weighted layout with set.seed to reproduceibality
set.seed(123)
coords <- layout_with_fr(g, weights = E(g)$weight)

custom_labels <- c(
  "cz-wiki" = "Cz",
  "hr-wiki" = "Hr",
  "hu-wiki" = "Hu",
  "pl-wiki" = "Pl",
  "ro-wiki" = "Ro",
  "sk-wiki" = "Sk",
  "sl-wiki" = "Sl",
  "sr-wiki" = "Sr",
  "uk-wiki" = "Uk"
)


colorcode <- c(
  "cz-wiki" = "#11457E",
  "hr-wiki" = "#FF0000",
  "hu-wiki" = "#436F4D",
  "pl-wiki" = "#DC143C",
  "ro-wiki" = "#FFD700",
  "sk-wiki" = "#0B4EA2",
  "sl-wiki" = "#1D854C",
  "sr-wiki" = "#C6363C",
  "uk-wiki" = "#0057B7"
)

plot <- ggraph(g, layout = "manual", x = coords[, 1], y = coords[, 2]) +
    geom_edge_link(aes(width = weight),
                 color = "grey30",
                 alpha = 0.6,
                 show.legend = FALSE) +
  geom_node_point(aes(color = name),
                  size = 6,
                  show.legend = FALSE) +
  geom_node_label(aes(label = custom_labels[name], color = name),
                  fontface = "bold",
                  fill = "white",
                  label.padding = unit(0.25, "lines"),
                  label.r = unit(0.2, "lines"),
                  label.size = 0,
                  repel = TRUE,
                  show.legend = FALSE) +
  
  scale_color_manual(values = colorcode, na.value = "grey50") +
  scale_edge_width(range = c(0.2, 3)) +
  
  theme_void() +
  ggtitle("Network of Literary Memory in CEE",
          subtitle = "based on the number of shared foreign authors")

ggsave(
  "11-network-shared-authors-wikis.png",
  plot,
  width = 20, height = 15, units = "cm",
  dpi = 300,
  bg= "white")
