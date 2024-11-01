library(tidyverse)

dat1 <- ku_pid5fsf

dat2 <- score_pid5fsf(dat1)
dat3 <- validity_pid5fsf(dat1)

dat <- bind_cols(dat2, dat3)
dat

dati <-
dati



dat |>
  select(starts_with("d_")) |>
  rename(
    Psychoticism = d_psycho,
    `Negative Affectivity` = d_negati,
    Disinhibition = d_disinh,
    Detachment = d_detatc,
    Antagonism = d_antago
  ) |>
  #mutate(across(everything(), \(x) ecdf(x)(x))) |> # Percentile
  mutate(across(everything(), \(x) ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)) * 10) + 50) |>
  slice(5) |>
  pivot_longer(cols = everything(), names_to = "scale", values_to = "score") |>
  ggplot(aes(x = score, y = scale)) +
  geom_col() +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "T Score", y = NULL)

dat |>
  select(starts_with("d_")) |>
  rename(
    Psychoticism = d_psycho,
    `Negative Affectivity` = d_negati,
    Disinhibition = d_disinh,
    Detachment = d_detatc,
    Antagonism = d_antago
  ) |>
  #mutate(across(everything(), \(x) ecdf(x)(x))) |> # Percentile
  mutate(across(everything(), \(x) ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)) * 10) + 50) |>
  slice(5) |>
  pivot_longer(cols = everything(), names_to = "scale", values_to = "score") |>
  arrange(scale) |>
  ggplot(aes(x = fct_rev(scale), y = score, group = 1)) +
  geom_rect(
    ymin = 0,
    ymax = 20,
    xmin = -Inf,
    xmax = Inf,
    fill = "#1a9641",
    alpha = 1/20
  ) +
  geom_rect(
    ymin = 20,
    ymax = 40,
    xmin = -Inf,
    xmax = Inf,
    fill = "#a6d96a",
    alpha = 1/20
  ) +
  geom_rect(
    ymin = 60,
    ymax = 80,
    xmin = -Inf,
    xmax = Inf,
    fill = "#fdae61",
    alpha = 1/20
  ) +
  geom_rect(
    ymin = 80,
    ymax = 100,
    xmin = -Inf,
    xmax = Inf,
    fill = "#d7191c",
    alpha = 1/20
  ) +
  #geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_label(aes(label = round(score))) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0, 0)) +
  labs(y = "T Score", x = NULL) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(linetype = "dashed"),
    plot.margin = margin(5.5, 20, 5.5, 5.5)
  )

dat |>
  select(starts_with("f_")) |>
  mutate(across(everything(), \(x) ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)) * 10) + 50) |>
  slice(5) |>
  pivot_longer(cols = everything(), names_to = "scale", values_to = "score") |>
  mutate(Domain = factor(rep(1:5, each = 5))) |>
  ggplot(aes(x = fct_rev(scale), y = score, group = 1)) +
  facet_wrap(~Domain, ncol = 1, scales = "free_y") +
  geom_rect(
    ymin = 0,
    ymax = 20,
    xmin = -Inf,
    xmax = Inf,
    fill = "#1a9641",
    alpha = 1/50
  ) +
  geom_rect(
    ymin = 20,
    ymax = 40,
    xmin = -Inf,
    xmax = Inf,
    fill = "#a6d96a",
    alpha = 1/50
  ) +
  geom_rect(
    ymin = 60,
    ymax = 80,
    xmin = -Inf,
    xmax = Inf,
    fill = "#fdae61",
    alpha = 1/50
  ) +
  geom_rect(
    ymin = 80,
    ymax = 100,
    xmin = -Inf,
    xmax = Inf,
    fill = "#d7191c",
    alpha = 1/50
  ) +
  geom_line(linewidth = 1) +
  geom_label(aes(label = round(score))) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0, 0)) +
  labs(y = "T Score", x = NULL) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(linetype = "dashed"),
    plot.margin = margin(5.5, 20, 5.5, 5.5),
    strip.background = element_rect(fill = "grey95")
  )
