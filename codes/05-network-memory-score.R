library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)

memory_scores$author <- gsub("\n.*", "", memory_scores$author)


# Sum memory scores for unordered pairs
edges <- memory_scores %>%
  mutate(node1 = pmin(author, wiki),
         node2 = pmax(author, wiki)) %>%
  group_by(node1, node2) %>%
  summarise(weight = sum(memory_score), .groups = "drop") %>%
  rename(source = node1, target = node2)

# Create igraph
g <- graph_from_data_frame(edges, directed = FALSE)

# Weighted FR layout with set.seed to reproducibality
set.seed(123) 
coords <- layout_with_fr(g, weights = E(g)$weight)

#  Short labels (customize as needed)
custom_labels <- setNames(
  toupper(unique(c(edges$source, edges$target))),  # e.g., "SK", "HR", etc.
  unique(c(edges$source, edges$target))
)

# Manual colors
colorcode <- c(
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

# Rotate coordinates

coords_rot <- cbind(
  x = -coords[,1],      
  y = coords[,2]      
)


# Plot with rotated coordinates
plot <- ggraph(g, layout = "manual", x = coords_rot[,1], y = coords_rot[,2]) +
  # edges
  geom_edge_link(aes(width = weight), color = "grey30", alpha = 0.6, show.legend = FALSE) +
  geom_node_point(aes(color = name), size = 6, show.legend = FALSE) +
  geom_node_label(aes(label = custom_labels[name], color = name),
                  fontface = "bold",
                  fill = "white",
                  label.padding = unit(0.25, "lines"),
                  label.r = unit(0.2, "lines"),
                  label.size = 0,
                  repel = TRUE,
                  show.legend = FALSE) +
  scale_color_manual(values = colorcode) +
  scale_edge_width(range = c(0.2, 3)) +
  theme_void() +
  ggtitle("Network of Literary Memory", subtitle = "Weighted by summed memory score")


ggsave(
  "10-network-memory-score.png",
  plot,
  width = 15, height = 15, units = "cm",
  dpi = 300,
  bg= "white")
