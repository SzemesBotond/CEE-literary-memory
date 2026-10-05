# Run distributions.R first.
library(dplyr)
library(ggplot2)

memory_scores <- distributions %>%
  mutate(Memory_score = Coverage * Composition * 100,
         Author_language = paste(Language, "in", Wikipedia))

fig <- ggplot(memory_scores,
              aes(reorder(Author_language, Memory_score), Memory_score, fill = Wikipedia)) +
  geom_col() +
  scale_fill_manual(values = colors) +
  labs(title = "Memory Scores in the Visegrad Region",
       x = "Author language and Wikipedia", y = "Memory score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(figure_dir, "07-memory-score.png"), fig,
       width = 20, height = 15, units = "cm", bg = "white", dpi = 300)
