library(ggplot2)
library(dplyr)
library(scales)

df <- read.csv(
  "D:/heathycare data/Bayesian_income.csv",
  stringsAsFactors = FALSE
)

colnames(df) <- make.names(colnames(df))

df <- df %>%
  filter(
    !is.na(GDP),
    !is.na(all),
    !is.na(Cardiovascular.diseases),
    !is.na(income)
  )

df <- df %>%
  mutate(
    log_cvd = log10(Cardiovascular.diseases + 1e-6)
  )

df <- df %>%
  mutate(
    income_class = factor(
      income,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Low",
        "Lower-Mid",
        "Upper-Mid",
        "High"
      )
    )
  )

income_colors <- c(
  "Low" = "gray80",
  "Lower-Mid" = "#179dfc",
  "Upper-Mid" = "#ffcc00",
  "High" = "#fb5454"
)

stats_df <- df %>%
  group_by(income_class) %>%
  summarise(
    r_val = round(cor(all, log_cvd), 2),
    p_val = signif(
      summary(lm(log_cvd ~ all))$coefficients[2, 4],
      3
    ),
    n_val = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      "p=", p_val,
      ", r=", r_val,
      ", n=", n_val
    ),
    x_pos = c(0.95, 1.8, 2.6, 3.2),
    y_pos = c(2.5, 2.3, 2.1, 1.9)
  )

p <- ggplot(
  df,
  aes(x = all, y = log_cvd)
) +
  geom_jitter(
    aes(color = income_class),
    width = 0.01 * diff(range(df$all)),
    height = 0.05 * diff(range(df$log_cvd)),
    alpha = 0.6,
    size = 8,
    shape = 16
  ) +
  geom_smooth(
    aes(group = income_class),
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 1.5,
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_text(
    data = stats_df,
    aes(
      x = x_pos,
      y = y_pos,
      label = label
    ),
    hjust = 1,
    vjust = 1,
    fontface = "bold",
    size = 5
  ) +
  scale_color_manual(
    values = income_colors,
    name = "Income Level"
  ) +
  labs(
    x = expression(italic("Healthcare Accessibility (All)")),
    y = expression(paste("log"[10], "(CVD Deaths)")),
    title = "Relationship between Healthcare Accessibility and CVD Mortality"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_rect(
      fill = alpha("white", 0.6),
      color = "black",
      size = 1
    ),
    legend.title = element_text(face = "bold", size = 18),
    legend.text = element_text(face = "bold", size = 16),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 1.5
    ),
    panel.grid.major = element_line(
      color = "grey50",
      size = 0.5
    ),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(1.5, NA))

print(p)

ggsave(
  "D:/heathycare data/figf.pdf",
  plot = p,
  width = 10,
  height = 8,
  units = "in",
  device = cairo_pdf
)