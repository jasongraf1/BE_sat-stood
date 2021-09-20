# -------------------------------------------------------------------------
# File: plots_for_NWAV.R
#
# Description:
# Code for creating the plots for the NWAV presentation
# -------------------------------------------------------------------------

# Libs
library(tidyverse); library(here);
library(patchwork); library(ggthemes); library(ggcharts)
library(ggtext); library(RColorBrewer)

# set theme and colors

uk_dark <- rep("grey30", 20)
uk_text <- rep("grey60", 20)
names(uk_dark) <- c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                    "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM")
names(uk_text) <- names(uk_dark)
uk_dark["GB"] <- "#9ad2fc"
uk_text["GB"] <- "#9ad2fc"

text_col <- "grey90"
bg_col <- "#000000"

theme_set(theme_classic())

theme_update(
  axis.ticks.x = element_line(color = text_col),
  plot.background = element_rect(fill = bg_col, color = bg_col),
  panel.background = element_rect(fill = bg_col),
  strip.text = element_text(size = rel(1.3), hjust = 0),
  axis.text.y = element_text(size = rel(1.4), color = text_col),
  axis.text.x = element_text(size = rel(1.2), color = text_col),
  axis.title.x = element_text(color = text_col),
  plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col),
  panel.grid.major.x = element_blank(),
  plot.caption = element_text(size = rel(1), color = text_col),
  legend.background = element_rect(fill = bg_col, color = bg_col),
  legend.text = element_text(color = text_col, size = rel(1.4))
)

# Glowbe ------------------------------------------------------------------

glowbe_data_processed <- here("data_processed", "data_BE_sat_glowbe.rds")

# glowbe percentages
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
  scale_fill_manual(guide = "none", values = uk_dark) +
  scale_color_manual(guide = "none", values = uk_text) +
  theme(axis.line.y = element_line(color = text_col),
        axis.text.x = element_blank(),
        axis.ticks.y = element_line(color = text_col),
        axis.ticks.x = element_blank()) +
  lemon::coord_capped_cart(left = "bottom", gap = 0)

# pyramid plot
glowbe_counts <- glowbe_data_processed %>%
  dplyr::filter(country_code != "XX") %>%
  droplevels() %>%
  group_by(country_code, variant) %>%
  mutate(country_code = factor(
    country_code,
    levels = c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
               "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM"))
    ) %>%
  count()

glowbe_counts$words_mil <- rep(
  round(c(386.8, 134.8, 387.6, 101, 148.2, 81.4, 96.4,
                                 46.6, 51.4, 39.5, 43, 41.6, 43.2, 40.5, 45.4,
                                 42.6, 38.8, 41.1, 35.2, 39.6), 2),
      each = 2)
  )

p1 <- glowbe_counts %>%
  dplyr::filter(variant == "ed") %>%
  mutate(per_mil = n/words_mil) %>%
  ggplot(aes(country_code, per_mil)) +
  geom_col(fill = uk_dark, width = .6) +
  geom_text(aes(label = n), hjust = 1.2, color = uk_dark, fontface = "bold", size = 3) +
  # coord_flip() +
  scale_fill_manual(name = "", values = uk_light) +
  labs(
    x = "", y = "",
    title = "*sat/stood*"
  ) +
  scale_y_reverse(limits = c(6.5,0)) +
  # theme_nwav() +
  ggcharts:::pyramid_theme("left") +
  theme(plot.title = ggtext::element_markdown(hjust = .9, color = text_col),
        axis.line.x = element_line(color = text_col),
        axis.text = element_text(color = text_col),
        axis.ticks = element_line(color = text_col)) +
  lemon::coord_capped_flip(bottom = "right", gap = 0)

p2 <- glowbe_counts %>%
  dplyr::filter(variant == "ing") %>%
  mutate(per_mil = n/words_mil) %>%
  ggplot(aes(country_code, per_mil)) +
  geom_col(fill = uk_dark, width = .6) +
  geom_text(aes(label = n), hjust = -.2, color = uk_dark, fontface = "bold", size = 3) +
  # coord_flip() +
  labs(x = "", y = "", title = "*sitting/standing*") +
  ylim(0, 42) +
  ggcharts:::pyramid_theme("right") +
  theme(plot.title = ggtext::element_markdown(hjust = .1, color = text_col),
        axis.line.x = element_line(color = text_col),
        axis.text.y = element_text(size = rel(.9), color = text_col),
        axis.text = element_text(color = text_col),
        axis.ticks = element_line(color = text_col)) +
  lemon::coord_capped_flip(bottom = "left", gap = 0)

p3/(p1 + p2) +
  plot_annotation(
    # tag_levels = c("A", "1"), tag_suffix = '.',
    caption = "Frequency per million words",
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 14))
  ) &
  theme(plot.tag = element_text(size = 10))

ggsave(
  "glowbe_frequencies.emf",
  device = "wmf",
  path = here("figures"),
  width = 10,
  height = 6.8
)

