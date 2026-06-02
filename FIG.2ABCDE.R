library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores())

library(performance)
library(ncf)
library(ggplot2)

div.table <- read.csv(
  "D:/heathycare data/data and code/TEST/FIG2/data2.csv",
  sep = ","
)

print(div.table)

div.table <- div.table %>%
  filter(
    all > 0,
    pharmacy > 0,
    hospital > 0,
    clinic > 0,
    GDP > 0,
    PM25 > 0,
    Cardiovascular.diseases > 0
  )

div.table$all.log.sc <- scale(log(div.table$all))
div.table$pharmacy.log.sc <- scale(log(div.table$pharmacy))
div.table$hospital.log.sc <- scale(log(div.table$hospital))
div.table$clinic.log.sc <- scale(log(div.table$clinic))
div.table$GDP.log.sc <- scale(log(div.table$GDP))

div.table$PM25.log.sc <- scale(log(div.table$PM25))


all.q1.logn_inter <- brm(
  formula = Cardiovascular.diseases ~
    all.log.sc +
    I(all.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 30000,
  chains = 4,
  thin = 10,
  refresh = 0,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  )
)

summary(all.q1.logn_inter)

r2_bayes(all.q1.logn_inter)

loo_all_inter <- loo(all.q1.logn_inter)
waic_all_inter <- waic(all.q1.logn_inter)

print(loo_all_inter)
print(waic_all_inter)

hospital.q1.logn_inter <- brm(
  formula = Cardiovascular.diseases ~
    hospital.log.sc +
    I(hospital.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 30000,
  chains = 4,
  thin = 10,
  refresh = 0,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  )
)

summary(hospital.q1.logn_inter)

r2_bayes(hospital.q1.logn_inter)

loo_hospital_inter <- loo(hospital.q1.logn_inter)
waic_hospital_inter <- waic(hospital.q1.logn_inter)

print(loo_hospital_inter)
print(waic_hospital_inter)

clinic.q1.logn_inter <- brm(
  formula = Cardiovascular.diseases ~
    clinic.log.sc +
    I(clinic.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 30000,
  chains = 4,
  thin = 10,
  refresh = 0,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  )
)

summary(clinic.q1.logn_inter)

r2_bayes(clinic.q1.logn_inter)

loo_clinic_inter <- loo(clinic.q1.logn_inter)
waic_clinic_inter <- waic(clinic.q1.logn_inter)

print(loo_clinic_inter)
print(waic_clinic_inter)

pharmacy.q1.logn_inter <- brm(
  formula = Cardiovascular.diseases ~
    pharmacy.log.sc +
    I(pharmacy.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 30000,
  chains = 4,
  thin = 10,
  refresh = 0,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  )
)

summary(pharmacy.q1.logn_inter)

r2_bayes(pharmacy.q1.logn_inter)

loo_pharmacy_inter <- loo(pharmacy.q1.logn_inter)
waic_pharmacy_inter <- waic(pharmacy.q1.logn_inter)

print(loo_pharmacy_inter)
print(waic_pharmacy_inter)

all_Str <- brm(
  formula = Stroke ~
    all.log.sc +
    I(all.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 10000,
  chains = 4,
  thin = 10,
  refresh = 0
)

summary(all_Str)

r2_bayes(all_Str)

all_IHD <- brm(
  formula = Ischemic.heart.disease ~
    all.log.sc +
    I(all.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 10000,
  chains = 4,
  thin = 10,
  refresh = 0
)

summary(all_IHD)

r2_bayes(all_IHD)

all_HHD <- brm(
  formula = Hypertensive.heart.disease ~
    all.log.sc +
    I(all.log.sc^2) +
    GDP.log.sc +
    PM25.log.sc +
    (1 | Country),
  family = lognormal(),
  data = div.table,
  warmup = 4000,
  iter = 10000,
  chains = 4,
  thin = 10,
  refresh = 0
)

summary(all_HHD)

r2_bayes(all_HHD)

generate_predictions_with_multiple_ci <- function(
    model,
    var_name,
    n_points = 1500
) {
  
  model_data <- model.frame(model)
  
  seq_points <- seq(
    from = min(model_data[[var_name]], na.rm = TRUE),
    to = max(model_data[[var_name]], na.rm = TRUE),
    length.out = n_points
  )
  
  new_data <- model_data[1, , drop = FALSE][rep(1, n_points), ]
  
  new_data[[var_name]] <- seq_points
  
  pred <- posterior_epred(
    model,
    newdata = new_data,
    re_formula = NA,
    summary = FALSE
  )
  
  result <- data.frame(
    var = seq_points,
    estimate__ = colMeans(pred),
    lower__95 = apply(pred, 2, quantile, 0.025),
    upper__95 = apply(pred, 2, quantile, 0.975),
    lower__80 = apply(pred, 2, quantile, 0.10),
    upper__80 = apply(pred, 2, quantile, 0.90),
    lower__50 = apply(pred, 2, quantile, 0.25),
    upper__50 = apply(pred, 2, quantile, 0.75)
  )
  
  names(result)[1] <- var_name
  
  return(result)
}

all_df_inter <- generate_predictions_with_multiple_ci(
  all.q1.logn_inter,
  "all.log.sc",
  n_points = 1500
)

hospital_df_inter <- generate_predictions_with_multiple_ci(
  hospital.q1.logn_inter,
  "hospital.log.sc",
  n_points = 1500
)

clinic_df_inter <- generate_predictions_with_multiple_ci(
  clinic.q1.logn_inter,
  "clinic.log.sc",
  n_points = 1500
)

pharmacy_df_inter <- generate_predictions_with_multiple_ci(
  pharmacy.q1.logn_inter,
  "pharmacy.log.sc",
  n_points = 1500
)

df <- all_df_inter %>%
  mutate(
    x = exp(
      all.log.sc *
        attr(div.table$all.log.sc, "scaled:scale") +
        attr(div.table$all.log.sc, "scaled:center")
    ),
    y = estimate__
  ) %>%
  filter(x >= 0.01, x <= 2.5) %>%
  arrange(x) %>%
  mutate(
    dy = c(NA, diff(y)),
    dx = c(NA, diff(x)),
    dydx = dy / dx
  )

abs_dydx <- abs(na.omit(df$dydx))

q <- quantile(
  abs_dydx,
  probs = c(0.25, 0.5)
)

print(q)

threshold_points <- sapply(q, function(tgt) {
  idx <- which.min(abs(abs(df$dydx) - tgt))
  return(df$x[idx])
})

names(threshold_points) <- c(
  "Q1 (75%)",
  "Q3 (50%)"
)

library(purrr)

post <- posterior_samples(all.q1.logn_inter)

b0 <- post$b_Intercept
b1 <- post$b_all.log.sc
b2 <- post$b_Iall.log.scE2
sigma <- post$sigma

z_center <- attr(div.table$all.log.sc, "scaled:center")
z_scale <- attr(div.table$all.log.sc, "scaled:scale")

Z <- matrix(
  div.table$all.log.sc,
  nrow = length(b0),
  ncol = nrow(div.table),
  byrow = TRUE
)

mu_post <- Z * b1 + Z^2 * b2 + b0

Ey_post <- exp(mu_post + sigma^2 / 2)

dmu_dz_post <- b1 + 2 * b2 * Z

A <- exp(Z * z_scale + z_center)

dE_dA_post <- Ey_post * dmu_dz_post / (z_scale * A)

dE_dA_mean <- colMeans(dE_dA_post)

dE_dA_lower <- apply(
  dE_dA_post,
  2,
  quantile,
  0.025
)

dE_dA_upper <- apply(
  dE_dA_post,
  2,
  quantile,
  0.975
)

dydA_df <- div.table %>%
  select(
    Country,
    year,
    all,
    GDP,
    Continent,
    PM25,
    Cardiovascular.diseases
  ) %>%
  mutate(
    A = A[1, ],
    dE_dA_mean = dE_dA_mean,
    dE_dA_lower = dE_dA_lower,
    dE_dA_upper = dE_dA_upper
  )

head(dydA_df)

write.csv(
  dydA_df,
  "D:/heathycare data/picture2/marginal_effect.csv",
  row.names = FALSE
)

p <- ggplot() +
  labs(
    x = "Healthcare accessibility",
    y = "CVD mortality"
  ) +
  geom_line(
    data = all_df_inter,
    aes(
      x = exp(
        all.log.sc *
          attr(div.table$all.log.sc, 'scaled:scale') +
          attr(div.table$all.log.sc, 'scaled:center')
      ) - 0.01,
      y = estimate__
    ),
    inherit.aes = FALSE,
    size = 1.5,
    alpha = 1,
    color = "#C82423",
    na.rm = TRUE
  ) +
  geom_ribbon(
    data = all_df_inter,
    aes(
      x = exp(
        all.log.sc *
          attr(div.table$all.log.sc, 'scaled:scale') +
          attr(div.table$all.log.sc, 'scaled:center')
      ) - 0.01,
      ymin = lower__95,
      ymax = upper__95
    ),
    inherit.aes = FALSE,
    fill = "#FEB2B4",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  geom_ribbon(
    data = all_df_inter,
    aes(
      x = exp(
        all.log.sc *
          attr(div.table$all.log.sc, 'scaled:scale') +
          attr(div.table$all.log.sc, 'scaled:center')
      ) - 0.01,
      ymin = lower__80,
      ymax = upper__80
    ),
    inherit.aes = FALSE,
    fill = "#F0988C",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  geom_ribbon(
    data = all_df_inter,
    aes(
      x = exp(
        all.log.sc *
          attr(div.table$all.log.sc, 'scaled:scale') +
          attr(div.table$all.log.sc, 'scaled:center')
      ) - 0.01,
      ymin = lower__50,
      ymax = upper__50
    ),
    inherit.aes = FALSE,
    fill = "#EF7A6D",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = 0.153,
    linetype = "dashed",
    color = "black",
    size = 1.2
  ) +
  geom_vline(
    xintercept = 0.598,
    linetype = "dashed",
    color = "red",
    size = 1.2
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.line = element_line(color = "black", size = 1),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 1.5
    ),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key.size = unit(3, "lines"),
    legend.key.width = unit(3, "lines"),
    legend.position = c(0.85, 0.9),
    legend.background = element_rect(
      fill = alpha('white', 0.7),
      color = NA
    ),
    legend.box.background = element_rect(
      color = "black",
      size = 0.8
    ),
    legend.key = element_blank()
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 8, alpha = 1)
    ),
    size = guide_legend(
      override.aes = list(alpha = 0.7)
    )
  ) +
  scale_x_continuous(
    expand = c(0.05, 0.05),
    breaks = c(-1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5),
    limits = c(0.0, 2.5)
  ) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    limits = c(150, 280)
  )

print(p)

ggsave(
  "D:/heathycare data/picture2/healthycare_cvd.pdf",
  plot = p,
  width = 5,
  height = 5
)

div.table <- div.table %>%
  mutate(
    all_category = factor(
      case_when(
        all < 0.153 ~ "Low",
        all >= 0.153 & all < 0.598 ~ "Mid",
        all >= 0.598 ~ "High"
      ),
      levels = c("Low", "Mid", "High")
    )
  )

continent_summary <- div.table %>%
  group_by(Continent, all_category) %>%
  summarise(
    count = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    percentage = count / sum(count) * 100
  ) %>%
  ungroup()

p_continent <- ggplot(
  continent_summary,
  aes(
    x = Continent,
    y = percentage,
    fill = all_category
  )
) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#FEB2B4",
      "Mid" = "#F0988C",
      "High" = "#C82423"
    ),
    name = "Access Level"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Healthcare Access Category Distribution by Continent",
    x = "Continent",
    y = "Percentage (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      family = "sans"
    ),
    axis.title = element_text(
      size = 12,
      family = "sans",
      face = "bold"
    ),
    axis.text = element_text(
      size = 10,
      family = "sans",
      color = "black"
    ),
    axis.line = element_line(
      color = "black",
      linewidth = 0.5
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.5
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      family = "sans",
      size = 11
    ),
    legend.text = element_text(
      family = "sans",
      size = 10
    ),
    legend.key = element_rect(
      color = "black",
      linewidth = 0.3
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    text = element_text(family = "sans"),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

print(p_continent)

ggsave(
  "D:/heathycare data/picture2/continent_distribution.pdf",
  p_continent,
  bg = "white"
)