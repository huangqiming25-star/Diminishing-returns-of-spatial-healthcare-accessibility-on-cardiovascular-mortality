library(ggplot2)
library(dplyr)
library(ggdist)

df <- read.csv(
  "D:/heathycare data/data and code/TEST/FIG1/data1e.csv",
  stringsAsFactors = FALSE
)

colnames(df) <- make.names(colnames(df))

df$Continent <- as.factor(df$Continent)

df <- df %>%
  filter(
    !is.na(Cardiovascular.diseases),
    !is.na(all),
    !is.na(Continent)
  )

df <- df %>%
  mutate(
    all_group = factor(
      dplyr::ntile(all, 4),
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

my_colors <- c(
  "#07ddfb",
  "#fb5454",
  "#179dfc",
  "#fc7c1f",
  "#8484fc",
  "#fcfc00"
)

p <- ggplot(
  df,
  aes(x = all_group, y = Cardiovascular.diseases)
) +
  stat_halfeye(
    aes(y = Cardiovascular.diseases),
    side = "right",
    width = 0.6,
    slab_alpha = 0.3,
    slab_fill = "gray60",
    color = "black",
    adjust = 1.5,
    position = position_nudge(x = 0.25)
  ) +
  geom_jitter(
    aes(color = Continent),
    width = 0.1,
    size = 3.5,
    alpha = 0.5,
    shape = 16
  ) +
  geom_boxplot(
    width = 0.25,
    fill = NA,
    color = "black",
    size = 1.2,
    outlier.shape = NA
  ) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(limits = c(100, 800)) +
  theme_classic(base_size = 22) +
  theme(
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(size = 2, color = "black")
  ) +
  labs(
    x = "Healthcare Accessibility",
    y = "CVD Death",
    color = "Continent"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 8,
        alpha = 0.8,
        shape = 16
      )
    )
  )

print(p)

ggsave(
  "D:/healthy_cvd_data/CVD_boxplot1.pdf",
  plot = p,
  width = 8,
  height = 6,
  device = cairo_pdf
)