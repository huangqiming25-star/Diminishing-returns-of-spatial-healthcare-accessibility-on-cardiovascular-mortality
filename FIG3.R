library(tidyverse)
library(ggrepel)

df <- read_csv("D:/heathycare data/picture4/all_ad.csv")
df1 <- read_csv("D:/heathycare data/picture4/hosptal_ad.csv")
df2 <- read_csv("D:/heathycare data/picture4/clinic_ad.csv")
df3 <- read_csv("D:/heathycare data/picture4/pharmacy_ad.csv")

df_plot <- df %>%
  filter(MA > 0) %>%
  mutate(
    x = GDP,
    y = median,
    MA_category = cut(MA,
                      breaks = 5,
                      labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                      include.lowest = TRUE)
  )

label_df <- df_plot %>%
  arrange(desc(y)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    df_plot %>%
      arrange(desc(x)) %>%
      slice_head(n = 10)
  ) %>%
  distinct()

continent_colors <- c(
  "Asia" = "#E69F00",
  "Europe" = "#56B4E9",
  "Africa" = "#009E73",
  "North America" = "#F0E442",
  "South America" = "#D55E00",
  "Oceania" = "#CC79A7"
)

p <- ggplot(df_plot, aes(x = x, y = y)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    color = "black",
    fill = NA,
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = Continent,
      size = MA_category
    ),
    alpha = 0.8,
    stroke = 0.4
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = Country),
    size = 5,
    color = "black",
    max.overlaps = 50,
    min.segment.length = 0.2,
    seed = 42,
    force = 1.5,
    force_pull = 0.5,
    box.padding = 0.5,
    point.padding = 0.4,
    segment.color = "grey60",
    segment.linetype = "dashed",
    segment.size = 1,
    segment.alpha = 0.8,
    direction = "both",
    nudge_x = 0.1,
    nudge_y = 0.1,
    max.time = 1,
    max.iter = 10000
  ) +
  scale_color_manual(
    values = continent_colors,
    guide = guide_legend(
      title = "Continent",
      override.aes = list(size = 10)
    )
  ) +
  scale_size_manual(
    name = "MA\n(Categories)",
    values = c(12, 15, 25, 35, 45),
    guide = guide_legend(
      title = "MA\n(Categories)",
      title.position = "top",
      title.hjust = 0.5,
      order = 2,
      override.aes = list(color = "grey50")
    )
  ) +
  scale_x_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, 30000)
  ) +
  scale_y_continuous(
    limits = c(-1, 90000),
    breaks = seq(0, 90000, 30000),
    expand = expansion(mult = 0.1)
  ) +
  labs(
    x = "GDP",
    y = "avoidable deaths",
    title = "MA vs GDP"
  ) +
  theme_classic() +
  theme(
    plot.background = element_rect(color = "black", linewidth = 1, fill = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.5),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(8, 8, 8, 8),
    legend.key.height = unit(0.4, "cm"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 15))
  ) +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

df_plot1 <- df1 %>%
  filter(hospital > 0) %>%
  mutate(
    x = GDP,
    y = median,
    hospital_category = cut(hospital,
                            breaks = 5,
                            labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                            include.lowest = TRUE)
  )

label_df1 <- df_plot1 %>%
  arrange(desc(y)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    df_plot1 %>%
      arrange(desc(x)) %>%
      slice_head(n = 10)
  ) %>%
  distinct()

p1 <- ggplot(df_plot1, aes(x = x, y = y)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    color = "black",
    fill = NA,
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = Continent,
      size = hospital_category
    ),
    alpha = 0.8,
    stroke = 0.4
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = Country),
    size = 5,
    color = "black",
    max.overlaps = 50,
    min.segment.length = 0.2,
    seed = 42,
    force = 1.5,
    force_pull = 0.5,
    box.padding = 0.5,
    point.padding = 0.4,
    segment.color = "grey60",
    segment.linetype = "dashed",
    segment.size = 1,
    segment.alpha = 0.8,
    direction = "both",
    nudge_x = 0.1,
    nudge_y = 0.1,
    max.time = 1,
    max.iter = 10000
  ) +
  scale_color_manual(
    values = continent_colors,
    guide = guide_legend(
      title = "Continent",
      override.aes = list(size = 10)
    )
  ) +
  scale_size_manual(
    name = "Hospital\n(Categories)",
    values = c(12, 15, 25, 35, 45),
    guide = guide_legend(
      title = "Hospital\n(Categories)",
      title.position = "top",
      title.hjust = 0.5,
      order = 2,
      override.aes = list(color = "grey50")
    )
  ) +
  scale_x_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, 30000)
  ) +
  scale_y_continuous(
    limits = c(-1, 80000),
    breaks = seq(0, 80000, 20000),
    expand = expansion(mult = 0.1)
  ) +
  labs(
    x = "GDP",
    y = "avoidable deaths",
    title = "Hospital vs GDP"
  ) +
  theme_classic() +
  theme(
    plot.background = element_rect(color = "black", linewidth = 1, fill = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.5),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(8, 8, 8, 8),
    legend.key.height = unit(0.4, "cm"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 15))
  ) +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

df_plot2 <- df2 %>%
  filter(clinic > 0) %>%
  mutate(
    x = GDP,
    y = median,
    clinic_category = cut(clinic,
                          breaks = 5,
                          labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                          include.lowest = TRUE)
  )

label_df2 <- df_plot2 %>%
  arrange(desc(y)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    df_plot2 %>%
      arrange(desc(x)) %>%
      slice_head(n = 10)
  ) %>%
  distinct()

p2 <- ggplot(df_plot2, aes(x = x, y = y)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    color = "black",
    fill = NA,
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = Continent,
      size = clinic_category
    ),
    alpha = 0.8,
    stroke = 0.4
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = Country),
    size = 5,
    color = "black",
    max.overlaps = 50,
    min.segment.length = 0.2,
    seed = 42,
    force = 1.5,
    force_pull = 0.5,
    box.padding = 0.5,
    point.padding = 0.4,
    segment.color = "grey60",
    segment.linetype = "dashed",
    segment.size = 1,
    segment.alpha = 0.8,
    direction = "both",
    nudge_x = 0.1,
    nudge_y = 0.1,
    max.time = 1,
    max.iter = 10000
  ) +
  scale_color_manual(
    values = continent_colors,
    guide = guide_legend(
      title = "Continent",
      override.aes = list(size = 10)
    )
  ) +
  scale_size_manual(
    name = "Clinic\n(Categories)",
    values = c(12, 15, 25, 35, 45),
    guide = guide_legend(
      title = "Clinic\n(Categories)",
      title.position = "top",
      title.hjust = 0.5,
      order = 2,
      override.aes = list(color = "grey50")
    )
  ) +
  scale_x_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, 30000)
  ) +
  scale_y_continuous(
    limits = c(-1, 90000),
    breaks = seq(0, 90000, 30000),
    expand = expansion(mult = 0.1)
  ) +
  labs(
    x = "GDP",
    y = "avoidable deaths",
    title = "Clinic vs GDP"
  ) +
  theme_classic() +
  theme(
    plot.background = element_rect(color = "black", linewidth = 1, fill = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.5),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(8, 8, 8, 8),
    legend.key.height = unit(0.4, "cm"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 15))
  ) +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

df_plot3 <- df3 %>%
  filter(pharmacy > 0) %>%
  mutate(
    x = GDP,
    y = median,
    pharmacy_category = cut(pharmacy,
                            breaks = 5,
                            labels = c("Very Low", "Low", "Medium", "High", "Very High"),
                            include.lowest = TRUE)
  )

label_df3 <- df_plot3 %>%
  arrange(desc(y)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    df_plot3 %>%
      arrange(desc(x)) %>%
      slice_head(n = 10)
  ) %>%
  distinct()

p3 <- ggplot(df_plot3, aes(x = x, y = y)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    color = "black",
    fill = NA,
    linewidth = 1
  ) +
  geom_point(
    aes(
      color = Continent,
      size = pharmacy_category
    ),
    alpha = 0.8,
    stroke = 0.4
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = Country),
    size = 5,
    color = "black",
    max.overlaps = 50,
    min.segment.length = 0.2,
    seed = 42,
    force = 1.5,
    force_pull = 0.5,
    box.padding = 0.5,
    point.padding = 0.4,
    segment.color = "grey60",
    segment.linetype = "dashed",
    segment.size = 1,
    segment.alpha = 0.8,
    direction = "both",
    nudge_x = 0.1,
    nudge_y = 0.1,
    max.time = 1,
    max.iter = 10000
  ) +
  scale_color_manual(
    values = continent_colors,
    guide = guide_legend(
      title = "Continent",
      override.aes = list(size = 10)
    )
  ) +
  scale_size_manual(
    name = "Pharmacy\n(Categories)",
    values = c(12, 15, 25, 35, 45),
    guide = guide_legend(
      title = "Pharmacy\n(Categories)",
      title.position = "top",
      title.hjust = 0.5,
      order = 2,
      override.aes = list(color = "grey50")
    )
  ) +
  scale_x_continuous(
    limits = c(0, 150000),
    breaks = seq(0, 150000, 30000)
  ) +
  scale_y_continuous(
    limits = c(-1, 100000),
    breaks = seq(0, 90000, 30000),
    expand = expansion(mult = 0.1)
  ) +
  labs(
    x = "GDP",
    y = "avoidable deaths",
    title = "Pharmacy vs GDP"
  ) +
  theme_classic() +
  theme(
    plot.background = element_rect(color = "black", linewidth = 1, fill = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.25),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.5),
    legend.spacing.y = unit(0.3, "cm"),
    legend.margin = margin(8, 8, 8, 8),
    legend.key.height = unit(0.4, "cm"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 15))
  ) +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

print(p)
print(p1)
print(p2)
print(p3)

ggsave("D:/heathycare data/picture4/MA_vs_GDP.pdf", p, width = 10, height = 10, device = cairo_pdf)
ggsave("D:/heathycare data/picture4/Hospital_vs_GDP.pdf", p1, width = 10, height =10, device = cairo_pdf)
ggsave("D:/heathycare data/picture4/Clinic_vs_GDP.pdf", p2, width = 10, height = 10, device = cairo_pdf)
ggsave("D:/heathycare data/picture4/Pharmacy_vs_GDP.pdf", p3, width = 10, height = 10, device = cairo_pdf)

ggsave(
  filename = "median_dis_scatter.pdf",
  plot = p,
  width = 8,
  height = 6,
  device = cairo_pdf
)