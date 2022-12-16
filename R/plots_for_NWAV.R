<<<<<<< HEAD
# -------------------------------------------------------------------------
# File: plots_for_NWAV.R
#
# Description:
# Code for creating the plots for the NWAV presentation
# -------------------------------------------------------------------------

# Libs
library(tidyverse); library(here); library(scales)
library(patchwork); library(ggthemes); library(ggcharts)
library(ggtext); library(RColorBrewer)

source(here("scripts_R", "custom_functions.R"))

# set theme and colors
text_col <- "grey90"
bg_col <- "#000000"

theme_nwav <- function (text_col = "grey90", bg_col = "#000000") {
  theme_classic() %+replace%
    theme(
      axis.ticks.x = element_line(color = text_col),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      plot.caption = element_text(size = rel(1), color = text_col),
      panel.background = element_rect(fill = bg_col),
      strip.background = element_rect(fill = bg_col),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = rel(1.3), hjust = 0, color = text_col),
      axis.text.y = element_text(size = rel(1.4), color = text_col),
      axis.text.x = element_text(size = rel(1.2), color = text_col),
      axis.title.x = element_text(color = text_col),
      axis.title.y = element_text(color = text_col),
      plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col, hjust = .1),
      legend.background = element_rect(fill = bg_col, color = bg_col),
      legend.text = element_text(color = text_col, size = rel(1.4))
    )
  }

theme_set(theme_nwav())


# Load datasets -----------------------------------------------------------

glowbe_data_processed <- here("data_processed", "data_BE_sat_glowbe.rds") %>%
  readRDS()

# subset the UK data
glowbe_data_uk <- glowbe_data_processed %>%
  dplyr::filter(country_code == "GB") %>%
  mutate(
    across(25:26, ~ fct_recode(.x, Yes = "y", No = "n")),
    dist_to_post_vp = as.numeric(dist_to_post_vp) - 1,
    subj_person = ifelse(grepl("_n", subj_tagged), "Noun", subj_person) %>%
      factor(levels = c("1P", "2P", "3P", "Noun"))
  )

bank_of_E_data_processed <- here("data_processed", "data_BE_sat_bank_of_E.rds") %>%
  readRDS()

bnc_data_processed <- here("data_processed", "data_BE_sat_bnc.rds") %>%
  readRDS()


# Glowbe ------------------------------------------------------------------

## set colors ------------
glowbe_dark <- rep("grey30", 20)
glowbe_text <- rep("grey60", 20)
names(glowbe_dark) <- c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                    "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM")
names(glowbe_text) <- names(glowbe_dark)
glowbe_dark["GB"] <- "#9ad2fc"
glowbe_text["GB"] <- "#9ad2fc"


## plot glowbe percentages ------------

p3 <- glowbe_data_processed %>%
  dplyr::filter(country_code != "XX") %>%
  droplevels() %>%
  group_by(country_code, variant) %>%
  count() %>%
  group_by(country_code) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%"),
    y = -.01,
    country_code = factor(
      country_code,
      levels = c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
      "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM"))
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(country_code, prop, fill = country_code)) +
  geom_col(width = .8) +
  geom_text(aes(label = country_code, y = y, color = country_code), size = 5,
            fontface = "bold") +
  # geom_text(aes(label = perc), color = uk_text, nudge_y = .005, fontface = "bold") +
  labs(x = "", y = "", title = "*BE sat/stood* as percentage of all tokens") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(guide = "none", values = glowbe_dark) +
  scale_color_manual(guide = "none", values = glowbe_text) +
  theme_nwav() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line.y = element_line(color = text_col),
    axis.text.x = element_blank(),
    axis.ticks.y = element_line(color = text_col),
    axis.ticks.x = element_blank()
    ) +
  lemon::coord_capped_cart(left = "bottom", gap = 0)


## pyramid plot of raw counts and normalized frequencies ------------

glowbe_counts <- glowbe_data_processed %>%
  dplyr::filter(country_code != "XX") %>%
  droplevels() %>%
  group_by(country_code, variant) %>%
  mutate(
    country_code = factor(
      country_code,
      levels = c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                 "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM"))
    ) %>%
  count() %>%
  ungroup() %>%
  mutate(
    words_mil = rep(
      c(386.8, 134.8, 387.6, 101, 148.2, 81.4, 96.4, 46.6, 51.4,
        39.5, 43, 41.6, 43.2, 40.5, 45.4, 42.6, 38.8, 41.1, 35.2, 39.6),
      each = 2),
    per_mil = n/words_mil
  )

### Left plot
p1 <- glowbe_counts %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(country_code, per_mil)) +
  geom_col(fill = glowbe_dark, width = .6) +
  geom_text(aes(label = n), hjust = 1.2, color = glowbe_dark, fontface = "bold", size = 3) +
  scale_fill_manual(name = "", values = glowbe_text) +
  labs(x = "", y = "", title = "*sat/stood*") +
  scale_y_reverse(limits = c(6.5,0)) +
  theme_nwav() +
  ggcharts:::pyramid_theme("left") +
  theme(plot.title = ggtext::element_markdown(hjust = .9, color = text_col),
        axis.line.x = element_line(color = text_col),
        axis.text = element_text(color = text_col),
        axis.ticks = element_line(color = text_col)) +
  lemon::coord_capped_flip(bottom = "right", gap = 0)

### Right plot
p2 <- glowbe_counts %>%
  dplyr::filter(variant == "ing") %>%
  ggplot(aes(country_code, per_mil)) +
  geom_col(fill = glowbe_dark, width = .6) +
  geom_text(aes(label = n), hjust = -.2, color = glowbe_dark, fontface = "bold", size = 3) +
  scale_fill_manual(name = "", values = glowbe_text) +
  labs(x = "", y = "", title = "*sitting/standing*") +
  ylim(0, 42) +
  theme_nwav() +
  ggcharts:::pyramid_theme("right") +
  theme(plot.title = ggtext::element_markdown(hjust = .1, color = text_col),
        axis.line.x = element_line(color = text_col),
        axis.text.y = element_text(size = rel(.9), color = text_col),
        axis.text = element_text(color = text_col),
        axis.ticks = element_line(color = text_col)) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

### Combined
p_comb <- p3/(p1 + p2) +
  plot_annotation(
    caption = "Frequency per million words",
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 14))
  )

ggsave(
  "glowbe_frequencies.emf",
  device = "wmf",
  plot = p_comb,
  path = here("figures"),
  width = 10,
  height = 6.8
)

ggsave(
  "glowbe_frequencies.png",
  device = "png",
  plot = p_comb,
  path = here("figures"),
  width = 10,
  height = 6.8
)



## Distance to post VP plot ------------

glowbe_HE_barplot <- glowbe_data_uk %>%
  filter(postmodifier_vp == "Yes") %>%
  # mutate(dist_to_post_vp = factor(dist_to_post_vp, levels = c("1", "2", "3", "4"))) %>%
  mutate(dist_to_post_vp = as.numeric(dist_to_post_vp)) %>%
  BarPlot(
    x = dist_to_post_vp,
    fill = variant,
    sort = F
  )

ggsave(
  "glowbe_horror_aequi.emf",
  device = "wmf",
  plot = glowbe_HE_barplot,
  path = here("figures"),
  width = 4.8,
  height = 3.2
)

ggsave(
  "glowbe_horror_aequi.png",
  device = "png",
  plot = glowbe_HE_barplot,
  path = here("figures"),
  width = 4.8,
  height = 3.2
)

## Test for horror aequi effects ------------

d <- glowbe_data_uk %>%
  filter(postmodifier_vp == "Yes") %>%
  mutate(across(19:20, as.factor),
         variant = relevel(variant, ref = "ing"),
         dist_to_post_vp = as.factor(dist_to_post_vp))

# Use Helmert coding: 1 vs. mean(2,3,4), 2 vs. mean(3,4), 3 vs. 4
(my_helmert <-  matrix(c(3/4, -1/4, -1/4, -1/4, 0, 2/3, -1/3, -1/3, 0, 0, 1/
                        2, -1/2), ncol = 3))

contrasts(d$dist_to_post_vp) <- my_helmert

summary(glowbe_HE_model <- glm(variant ~ verb * (dist_to_post_vp + subj_person),
                       data = d, family = binomial))

glowbe_HE_model <- effects::Effect(c("dist_to_post_vp", "verb"), glowbe_HE_model,
                xlevels = list(dist_to_post_vp = 0:3, verb = c("sit", "stand"))) %>%
  as.data.frame() %>%
  ggplot(aes(x = dist_to_post_vp, y = fit)) +
  geom_line(aes(group = 1), color = text_col, linetype = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = verb), width = .1) +
  geom_point(aes(color = verb), size = 4) +
  facet_wrap(~verb) +
  labs(x = "# of words between main verb and postmodifying -ing form",
       y = "Probability of *sat/stood*") +
  scale_color_manual(guide = "none", values = c("#9ad2fc", "#fca49a")) +
  theme(
    axis.text.x = element_text(size = 12),
    strip.text = element_blank(),
    axis.title.y = ggtext::element_markdown(angle = 90,
                                            padding = unit(c(0, 0, 5, 0), "pt")),
    axis.line.y = element_line(color = text_col)
    )

ggsave(
  "glowbe_horror_aequi_model.emf",
  plot = glowbe_HE_model,
  device = "wmf",
  path = here("figures"),
  width = 4.8,
  height = 3.2
)

ggsave(
  "glowbe_horror_aequi_model.png",
  plot = glowbe_HE_model,
  device = "png",
  path = here("figures"),
  width = 4.8,
  height = 3.2
)


# BofE distributions ------------------------------------------------------

## Plot register by country proportions ------------
bofe_dark <- rep("grey30", 6)
bofe_text <- rep("grey60", 6)
names(bofe_dark) <- unique(bank_of_E_data_processed$country)
names(bofe_text) <- names(glowbe_dark)
bofe_dark["GB"] <- "#9ad2fc"
bofe_text["GB"] <- "#9ad2fc"

bank_of_E_plot <- bank_of_E_data_processed %>%
  filter(!text_genre %in% c("ephemera", "journal")) %>%
  mutate(
    across(c(country, text_genre, variant), ~ as.factor(.x)),
    country = fct_recode(country, GB = "UK")
    ) %>%
  group_by(country, text_genre, variant) %>%
  count(.drop = FALSE) %>%
  ungroup() %>%
  complete(country, nesting(text_genre, variant), fill = list(n = 0)) %>%
  group_by(country, text_genre) %>%
  mutate(
    prop = n/sum(n),
    prop = ifelse(is.na(prop), 0, prop),
    perc = paste0(round(100*prop), "%")
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(reorder(country, prop), prop, fill = n)) +
  facet_wrap(~ text_genre) +
  geom_col(width = .8) +
  theme_nwav() +
  geom_text(aes(label = perc, color = n), size = 5, fontface = "bold",
            nudge_y = .01, hjust = 0) +
  labs(x = "", y = "Percentage *BE sat/stood*") +
  scale_y_continuous(expand = expansion(mult = .02),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient(name = "# of tokens", high = "#9ad2fc", low = "#000000") +
  scale_color_gradient(guide = "none", high = "#9ad2fc", low = "#222222") +
  theme(
    legend.position = c(.9,.85),
    legend.title = element_text(color = text_col, size = 12),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = text_col),
    axis.text.y = element_text(hjust = 1),
    axis.ticks.x = element_line(color = text_col),
    axis.ticks.y = element_blank(),
    axis.title.x = ggtext::element_markdown(color = text_col),
    axis.title.y = ggtext::element_markdown(color = text_col)
  ) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

ggsave(
  "bank_of_E_genre.emf",
  device = "wmf",
  plot = bank_of_E_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)

ggsave(
  "bank_of_E_genre.png",
  device = "png",
  plot = bank_of_E_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)


## Plot register over time proportions for UK ------------
reg_cols <- c("#6F69AC", "#95DAC1", "#FFEBA1", "#FD6F96") %>% rev()

### Combined proportion over time
bank_of_E_data_processed %>%
  mutate(country = fct_recode(country, GB = "UK")) %>%
  filter(country == "GB") %>%
  group_by(year, variant, .drop = F) %>%
  count() %>%
  group_by(year) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%"),
    y = .01
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(year, prop)) +
  geom_line(aes(group = 1), color = text_col, size = 1)

### Prop over time by register
bank_of_E_genre_time_plot <- bank_of_E_data_processed %>%
  filter(!text_genre %in% c("ephemera", "journal")) %>%
  mutate(country = fct_recode(country, GB = "UK")) %>%
  filter(country == "GB") %>%
  group_by(year, text_genre, variant, .drop = F) %>%
  count() %>%
  group_by(year, text_genre) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%"),
    y = .01
  ) %>%
  dplyr::filter(variant == "ed") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, prop)) +
  geom_line(aes(color = text_genre), size = 2) +
  scale_color_manual(values = reg_cols) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "*BE sat/stood* as percentage of all tokens") +
  theme(
    legend.position = c(0.05,1),
    legend.justification = c(0,1),
    axis.line.x = element_line(color = text_col),
    axis.title.y = ggtext::element_markdown(angle = 90,
                                            padding = unit(c(0, 0, 5, 0), "pt")))

ggsave(
  "bank_of_E_genre_time_UK.emf",
  device = "wmf",
  plot = bank_of_E_genre_time_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)

ggsave(
  "bank_of_E_genre_time_UK.png",
  device = "png",
  plot = bank_of_E_genre_time_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)


# BNC distributions -------------------------------------------------------

## Plot proportions by dialect region ------------
bnc_region4_plot <- bnc_data_processed %>%
  dplyr::filter(!is.na(region4)) %>%
  group_by(region4, variant, .drop = F) %>%
  count() %>%
  group_by(region4) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%")
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(reorder(region4, prop), prop)) +
  geom_col(aes(fill = n), width = .8) +
  geom_text(aes(label = perc, y = prop, color = n), size = 5, hjust = 0, nudge_y = .02,
            fontface = "bold") +
  labs(x = "", y = "Percentage *BE sat/stood*") +
  scale_y_continuous(expand = expansion(mult = .1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient(name = "# of tokens", high = "#9ad2fc", low = "#000000") +
  scale_color_gradient(guide = "none", high = "#9ad2fc", low = "#222222") +
  theme(
    legend.position = c(.9,.3),
    legend.title = element_text(color = text_col, size = 12),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = text_col),
    axis.text.y = element_text(hjust = 1),
    axis.ticks.x = element_line(color = text_col),
    axis.ticks.y = element_blank(),
    axis.title.x = ggtext::element_markdown(color = text_col),
    axis.title.y = ggtext::element_markdown(color = text_col)
  ) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

ggsave(
  "bnc_region4_dist.emf",
  device = "wmf",
  plot =bnc_region4_plot,
  path = here("figures"),
  width = 6,
  height = 4.1
)

ggsave(
  "bnc_region4_dist.png",
  device = "png",
  plot = bnc_region4_plot,
  path = here("figures"),
  width = 6,
  height = 4.1
)


## Plot sex effect  ------------
### relevel the response to predict 'sat'
bnc_data_processed2 <- bnc_data_processed %>%
  mutate(variant = as.factor(variant),
         variant = relevel(variant, ref = "ing"))

summary(bnc_gender_model <- glm(variant ~ verb * gender, bnc_data_processed2, family = binomial))

### sit on left, stand on right
bnc_gender_effect_plot <- bnc_gender_model %>%
  effects::Effect(c("gender", "verb"), .) %>%
  as.data.frame() %>%
  ggplot(aes(gender, fit)) +
  geom_line(aes(group = verb), linetype = 2, color = "grey") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = gender), width = .1) +
  geom_point(aes(color = gender), size = 4)+
  facet_wrap(~verb) +
  scale_color_manual(guide = "none", values = c("#FEC260", "#d460fe")) +
  labs(x = "Speaker sex",
       y = "Probability of *sat/stood*") +
  theme(
    axis.text.x = element_text(size = 14),
    strip.text = element_blank(),
    axis.title.y = ggtext::element_markdown(angle = 90,
                                            padding = unit(c(0, 0, 5, 0), "pt")),
    axis.line.y = element_line(color = text_col)
  )

ggsave(
  "bnc_gender_model.emf",
  plot = bnc_gender_effect_plot,
  device = "wmf",
  path = here("figures"),
  width = 6,
  height = 3.4
)

ggsave(
  "bnc_gender_model.png",
  plot = bnc_gender_effect_plot,
  device = "png",
  path = here("figures"),
  width = 6,
  height = 3.4
=======
# -------------------------------------------------------------------------
# File: plots_for_NWAV.R
#
# Description:
# Code for creating the plots for the NWAV presentation
# -------------------------------------------------------------------------

# Libs
library(tidyverse); library(here); library(scales)
library(patchwork); library(ggthemes); library(ggcharts)
library(ggtext); library(RColorBrewer)

source(here("scripts_R", "custom_functions.R"))

# set theme and colors
text_col <- "grey90"
bg_col <- "#000000"

theme_nwav <- function (text_col = "grey90", bg_col = "#000000") {
  theme_classic() %+replace%
    theme(
      axis.ticks.x = element_line(color = text_col),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      plot.caption = element_text(size = rel(1), color = text_col),
      panel.background = element_rect(fill = bg_col),
      strip.background = element_rect(fill = bg_col),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = rel(1.3), hjust = 0, color = text_col),
      axis.text.y = element_text(size = rel(1.4), color = text_col),
      axis.text.x = element_text(size = rel(1.2), color = text_col),
      axis.title.x = element_text(color = text_col),
      axis.title.y = element_text(color = text_col),
      plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col, hjust = .1),
      legend.background = element_rect(fill = bg_col, color = bg_col),
      legend.text = element_text(color = text_col, size = rel(1.4))
    )
  }

theme_set(theme_nwav())


# Load datasets -----------------------------------------------------------

glowbe_data_processed <- here("data_processed", "data_BE_sat_glowbe.rds") %>%
  readRDS()

# subset the UK data
glowbe_data_uk <- glowbe_data_processed %>%
  dplyr::filter(country_code == "GB") %>%
  mutate(
    across(25:26, ~ fct_recode(.x, Yes = "y", No = "n")),
    dist_to_post_vp = as.numeric(dist_to_post_vp) - 1,
    subj_person = ifelse(grepl("_n", subj_tagged), "Noun", subj_person) %>%
      factor(levels = c("1P", "2P", "3P", "Noun"))
  )

bank_of_E_data_processed <- here("data_processed", "data_BE_sat_bank_of_E.rds") %>%
  readRDS()

bnc_data_processed <- here("data_processed", "data_BE_sat_bnc.rds") %>%
  readRDS()


# Glowbe ------------------------------------------------------------------

## set colors ------------
glowbe_dark <- rep("grey30", 20)
glowbe_text <- rep("grey60", 20)
names(glowbe_dark) <- c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                    "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM")
names(glowbe_text) <- names(glowbe_dark)
glowbe_dark["GB"] <- "#9ad2fc"
glowbe_text["GB"] <- "#9ad2fc"


## plot glowbe percentages ------------

p3 <- glowbe_data_processed %>%
  dplyr::filter(country_code != "XX") %>%
  droplevels() %>%
  group_by(country_code, variant) %>%
  count() %>%
  group_by(country_code) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%"),
    y = -.01,
    country_code = factor(
      country_code,
      levels = c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
      "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM"))
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(country_code, prop, fill = country_code)) +
  geom_col(width = .8) +
  geom_text(aes(label = country_code, y = y, color = country_code), size = 5,
            fontface = "bold") +
  # geom_text(aes(label = perc), color = uk_text, nudge_y = .005, fontface = "bold") +
  labs(x = "", y = "", title = "*BE sat/stood* as percentage of all tokens") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(guide = "none", values = glowbe_dark) +
  scale_color_manual(guide = "none", values = glowbe_text) +
  theme_nwav() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line.y = element_line(color = text_col),
    axis.text.x = element_blank(),
    axis.ticks.y = element_line(color = text_col),
    axis.ticks.x = element_blank()
    ) +
  lemon::coord_capped_cart(left = "bottom", gap = 0)


## pyramid plot of raw counts and normalized frequencies ------------

glowbe_counts <- glowbe_data_processed %>%
  dplyr::filter(country_code != "XX") %>%
  droplevels() %>%
  group_by(country_code, variant) %>%
  mutate(
    country_code = factor(
      country_code,
      levels = c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                 "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM"))
    ) %>%
  count() %>%
  ungroup() %>%
  mutate(
    words_mil = rep(
      c(386.8, 134.8, 387.6, 101, 148.2, 81.4, 96.4, 46.6, 51.4,
        39.5, 43, 41.6, 43.2, 40.5, 45.4, 42.6, 38.8, 41.1, 35.2, 39.6),
      each = 2),
    per_mil = n/words_mil
  )

### Left plot
p1 <- glowbe_counts %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(country_code, per_mil)) +
  geom_col(fill = glowbe_dark, width = .6) +
  geom_text(aes(label = n), hjust = 1.2, color = glowbe_dark, fontface = "bold", size = 3) +
  scale_fill_manual(name = "", values = glowbe_text) +
  labs(x = "", y = "", title = "*sat/stood*") +
  scale_y_reverse(limits = c(6.5,0)) +
  theme_nwav() +
  ggcharts:::pyramid_theme("left") +
  theme(plot.title = ggtext::element_markdown(hjust = .9, color = text_col),
        axis.line.x = element_line(color = text_col),
        axis.text = element_text(color = text_col),
        axis.ticks = element_line(color = text_col)) +
  lemon::coord_capped_flip(bottom = "right", gap = 0)

### Right plot
p2 <- glowbe_counts %>%
  dplyr::filter(variant == "ing") %>%
  ggplot(aes(country_code, per_mil)) +
  geom_col(fill = glowbe_dark, width = .6) +
  geom_text(aes(label = n), hjust = -.2, color = glowbe_dark, fontface = "bold", size = 3) +
  scale_fill_manual(name = "", values = glowbe_text) +
  labs(x = "", y = "", title = "*sitting/standing*") +
  ylim(0, 42) +
  theme_nwav() +
  ggcharts:::pyramid_theme("right") +
  theme(plot.title = ggtext::element_markdown(hjust = .1, color = text_col),
        axis.line.x = element_line(color = text_col),
        axis.text.y = element_text(size = rel(.9), color = text_col),
        axis.text = element_text(color = text_col),
        axis.ticks = element_line(color = text_col)) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

### Combined
p_comb <- p3/(p1 + p2) +
  plot_annotation(
    caption = "Frequency per million words",
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 14))
  )

ggsave(
  "glowbe_frequencies.emf",
  device = "wmf",
  plot = p_comb,
  path = here("figures"),
  width = 10,
  height = 6.8
)

ggsave(
  "glowbe_frequencies.png",
  device = "png",
  plot = p_comb,
  path = here("figures"),
  width = 10,
  height = 6.8
)



## Distance to post VP plot ------------

glowbe_HE_barplot <- glowbe_data_uk %>%
  filter(postmodifier_vp == "Yes") %>%
  # mutate(dist_to_post_vp = factor(dist_to_post_vp, levels = c("1", "2", "3", "4"))) %>%
  mutate(dist_to_post_vp = as.numeric(dist_to_post_vp)) %>%
  BarPlot(
    x = dist_to_post_vp,
    fill = variant,
    sort = F
  )

ggsave(
  "glowbe_horror_aequi.emf",
  device = "wmf",
  plot = glowbe_HE_barplot,
  path = here("figures"),
  width = 4.8,
  height = 3.2
)

ggsave(
  "glowbe_horror_aequi.png",
  device = "png",
  plot = glowbe_HE_barplot,
  path = here("figures"),
  width = 4.8,
  height = 3.2
)

## Test for horror aequi effects ------------

d <- glowbe_data_uk %>%
  filter(postmodifier_vp == "Yes") %>%
  mutate(across(19:20, as.factor),
         variant = relevel(variant, ref = "ing"),
         dist_to_post_vp = as.factor(dist_to_post_vp))

# Use Helmert coding: 1 vs. mean(2,3,4), 2 vs. mean(3,4), 3 vs. 4
(my_helmert <-  matrix(c(3/4, -1/4, -1/4, -1/4, 0, 2/3, -1/3, -1/3, 0, 0, 1/
                        2, -1/2), ncol = 3))

contrasts(d$dist_to_post_vp) <- my_helmert

summary(glowbe_HE_model <- glm(variant ~ verb * (dist_to_post_vp + subj_person),
                       data = d, family = binomial))

glowbe_HE_model <- effects::Effect(c("dist_to_post_vp", "verb"), glowbe_HE_model,
                xlevels = list(dist_to_post_vp = 0:3, verb = c("sit", "stand"))) %>%
  as.data.frame() %>%
  ggplot(aes(x = dist_to_post_vp, y = fit)) +
  geom_line(aes(group = 1), color = text_col, linetype = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = verb), width = .1) +
  geom_point(aes(color = verb), size = 4) +
  facet_wrap(~verb) +
  labs(x = "# of words between main verb and postmodifying -ing form",
       y = "Probability of *sat/stood*") +
  scale_color_manual(guide = "none", values = c("#9ad2fc", "#fca49a")) +
  theme(
    axis.text.x = element_text(size = 12),
    strip.text = element_blank(),
    axis.title.y = ggtext::element_markdown(angle = 90,
                                            padding = unit(c(0, 0, 5, 0), "pt")),
    axis.line.y = element_line(color = text_col)
    )

ggsave(
  "glowbe_horror_aequi_model.emf",
  plot = glowbe_HE_model,
  device = "wmf",
  path = here("figures"),
  width = 4.8,
  height = 3.2
)

ggsave(
  "glowbe_horror_aequi_model.png",
  plot = glowbe_HE_model,
  device = "png",
  path = here("figures"),
  width = 4.8,
  height = 3.2
)


# BofE distributions ------------------------------------------------------

## Plot register by country proportions ------------
bofe_dark <- rep("grey30", 6)
bofe_text <- rep("grey60", 6)
names(bofe_dark) <- unique(bank_of_E_data_processed$country)
names(bofe_text) <- names(glowbe_dark)
bofe_dark["GB"] <- "#9ad2fc"
bofe_text["GB"] <- "#9ad2fc"

bank_of_E_plot <- bank_of_E_data_processed %>%
  filter(!text_genre %in% c("ephemera", "journal")) %>%
  mutate(
    across(c(country, text_genre, variant), ~ as.factor(.x)),
    country = fct_recode(country, GB = "UK")
    ) %>%
  group_by(country, text_genre, variant) %>%
  count(.drop = FALSE) %>%
  ungroup() %>%
  complete(country, nesting(text_genre, variant), fill = list(n = 0)) %>%
  group_by(country, text_genre) %>%
  mutate(
    prop = n/sum(n),
    prop = ifelse(is.na(prop), 0, prop),
    perc = paste0(round(100*prop), "%")
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(reorder(country, prop), prop, fill = n)) +
  facet_wrap(~ text_genre) +
  geom_col(width = .8) +
  theme_nwav() +
  geom_text(aes(label = perc, color = n), size = 5, fontface = "bold",
            nudge_y = .01, hjust = 0) +
  labs(x = "", y = "Percentage *BE sat/stood*") +
  scale_y_continuous(expand = expansion(mult = .02),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient(name = "# of tokens", high = "#9ad2fc", low = "#000000") +
  scale_color_gradient(guide = "none", high = "#9ad2fc", low = "#222222") +
  theme(
    legend.position = c(.9,.85),
    legend.title = element_text(color = text_col, size = 12),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = text_col),
    axis.text.y = element_text(hjust = 1),
    axis.ticks.x = element_line(color = text_col),
    axis.ticks.y = element_blank(),
    axis.title.x = ggtext::element_markdown(color = text_col),
    axis.title.y = ggtext::element_markdown(color = text_col)
  ) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

ggsave(
  "bank_of_E_genre.emf",
  device = "wmf",
  plot = bank_of_E_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)

ggsave(
  "bank_of_E_genre.png",
  device = "png",
  plot = bank_of_E_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)


## Plot register over time proportions for UK ------------
reg_cols <- c("#6F69AC", "#95DAC1", "#FFEBA1", "#FD6F96") %>% rev()

### Combined proportion over time
bank_of_E_data_processed %>%
  mutate(country = fct_recode(country, GB = "UK")) %>%
  filter(country == "GB") %>%
  group_by(year, variant, .drop = F) %>%
  count() %>%
  group_by(year) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%"),
    y = .01
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(year, prop)) +
  geom_line(aes(group = 1), color = text_col, size = 1)

### Prop over time by register
bank_of_E_genre_time_plot <- bank_of_E_data_processed %>%
  filter(!text_genre %in% c("ephemera", "journal")) %>%
  mutate(country = fct_recode(country, GB = "UK")) %>%
  filter(country == "GB") %>%
  group_by(year, text_genre, variant, .drop = F) %>%
  count() %>%
  group_by(year, text_genre) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%"),
    y = .01
  ) %>%
  dplyr::filter(variant == "ed") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, prop)) +
  geom_line(aes(color = text_genre), size = 2) +
  scale_color_manual(values = reg_cols) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "*BE sat/stood* as percentage of all tokens") +
  theme(
    legend.position = c(0.05,1),
    legend.justification = c(0,1),
    axis.line.x = element_line(color = text_col),
    axis.title.y = ggtext::element_markdown(angle = 90,
                                            padding = unit(c(0, 0, 5, 0), "pt")))

ggsave(
  "bank_of_E_genre_time_UK.emf",
  device = "wmf",
  plot = bank_of_E_genre_time_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)

ggsave(
  "bank_of_E_genre_time_UK.png",
  device = "png",
  plot = bank_of_E_genre_time_plot,
  path = here("figures"),
  width = 8.7,
  height = 5
)


# BNC distributions -------------------------------------------------------

## Plot proportions by dialect region ------------
bnc_region4_plot <- bnc_data_processed %>%
  dplyr::filter(!is.na(region4)) %>%
  group_by(region4, variant, .drop = F) %>%
  count() %>%
  group_by(region4) %>%
  mutate(
    prop = n/sum(n),
    perc = paste0(round(100*prop), "%")
  ) %>%
  dplyr::filter(variant == "ed") %>%
  ggplot(aes(reorder(region4, prop), prop)) +
  geom_col(aes(fill = n), width = .8) +
  geom_text(aes(label = perc, y = prop, color = n), size = 5, hjust = 0, nudge_y = .02,
            fontface = "bold") +
  labs(x = "", y = "Percentage *BE sat/stood*") +
  scale_y_continuous(expand = expansion(mult = .1),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient(name = "# of tokens", high = "#9ad2fc", low = "#000000") +
  scale_color_gradient(guide = "none", high = "#9ad2fc", low = "#222222") +
  theme(
    legend.position = c(.9,.3),
    legend.title = element_text(color = text_col, size = 12),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = text_col),
    axis.text.y = element_text(hjust = 1),
    axis.ticks.x = element_line(color = text_col),
    axis.ticks.y = element_blank(),
    axis.title.x = ggtext::element_markdown(color = text_col),
    axis.title.y = ggtext::element_markdown(color = text_col)
  ) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

ggsave(
  "bnc_region4_dist.emf",
  device = "wmf",
  plot =bnc_region4_plot,
  path = here("figures"),
  width = 6,
  height = 4.1
)

ggsave(
  "bnc_region4_dist.png",
  device = "png",
  plot = bnc_region4_plot,
  path = here("figures"),
  width = 6,
  height = 4.1
)


## Plot sex effect  ------------
### relevel the response to predict 'sat'
bnc_data_processed2 <- bnc_data_processed %>%
  mutate(variant = as.factor(variant),
         variant = relevel(variant, ref = "ing"))

summary(bnc_gender_model <- glm(variant ~ verb * gender, bnc_data_processed2, family = binomial))

### sit on left, stand on right
bnc_gender_effect_plot <- bnc_gender_model %>%
  effects::Effect(c("gender", "verb"), .) %>%
  as.data.frame() %>%
  ggplot(aes(gender, fit)) +
  geom_line(aes(group = verb), linetype = 2, color = "grey") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = gender), width = .1) +
  geom_point(aes(color = gender), size = 4)+
  facet_wrap(~verb) +
  scale_color_manual(guide = "none", values = c("#FEC260", "#d460fe")) +
  labs(x = "Speaker sex",
       y = "Probability of *sat/stood*") +
  theme(
    axis.text.x = element_text(size = 14),
    strip.text = element_blank(),
    axis.title.y = ggtext::element_markdown(angle = 90,
                                            padding = unit(c(0, 0, 5, 0), "pt")),
    axis.line.y = element_line(color = text_col)
  )

ggsave(
  "bnc_gender_model.emf",
  plot = bnc_gender_effect_plot,
  device = "wmf",
  path = here("figures"),
  width = 6,
  height = 3.4
)

ggsave(
  "bnc_gender_model.png",
  plot = bnc_gender_effect_plot,
  device = "png",
  path = here("figures"),
  width = 6,
  height = 3.4
>>>>>>> 5b19058cfb251bfd006380c1fa908f8015b0fb54
)