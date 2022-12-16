# -------------------------------------------------------------------------
# File: plots_for_ICAME.R
#
# Description:
# Code for creating the plots for the ICAME presentation
# -------------------------------------------------------------------------

# Libs
library(tidyverse); library(here); library(scales)
library(patchwork); library(ggthemes); library(ggcharts)
library(ggtext); library(RColorBrewer)

# set theme and colors
text_col <- "grey10"
bg_col <- "white"

theme_icame <- function (text_col = "grey10", bg_col = "white") {
  theme_classic() %+replace%
    theme(
      axis.ticks.x = element_line(color = text_col),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      plot.caption = element_text(size = rel(1), color = text_col),
      panel.background = element_rect(fill = bg_col, color = bg_col),
      strip.background = element_rect(fill = bg_col, color = bg_col),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = rel(1.3), hjust = 0, color = text_col),
      axis.text.y = element_text(size = rel(1.3), color = text_col),
      axis.text.x = element_text(size = rel(1.2), color = text_col),
      axis.title.x = ggtext::element_markdown(color = text_col),
      axis.title.y = ggtext::element_markdown(color = text_col),
      plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col, hjust = .1),
      legend.background = element_rect(fill = bg_col, color = bg_col),
      legend.text = element_text(color = text_col, size = rel(1.4))
    )
}

theme_set(theme_icame())


# Load datasets -----------------------------------------------------------

glowbe_data_processed <- here("data_processed", "data_BE_sat_glowbe.txt") %>%
  vroom::vroom(delim = "\t") %>%
  dplyr::filter(country_code != "XX") %>%
  dplyr::filter(is.na(include))

# subset the UK data
glowbe_data_uk <- glowbe_data_processed %>%
  dplyr::filter(country_code == "GB") %>%
  mutate(
    across(25:26, ~ fct_recode(.x, Yes = "y", No = "n")),
    dist_to_post_vp = as.numeric(dist_to_post_vp) - 1,
    subj_person = ifelse(grepl("_n", subj_tagged), "Noun", subj_person) %>%
      factor(levels = c("1P", "2P", "3P", "Noun"))
  )

# Glowbe ------------------------------------------------------------------

## set colors ------------
glowbe_dark <- rep("grey70", 20)
glowbe_text <- rep("grey50", 20)
names(glowbe_dark) <- c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                        "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM")
names(glowbe_text) <- names(glowbe_dark)
glowbe_dark["GB"] <- "#0207a6"
glowbe_text["GB"] <- "#0207a6"


## plot glowbe percentages ------------

p3 <- glowbe_data_processed %>%
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
  labs(x = "", y = "Perc. *BE sat/stood*", title = "*BE sat/stood* in GloWbE") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(guide = "none", values = glowbe_dark) +
  scale_color_manual(guide = "none", values = glowbe_text) +
  theme_icame() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line.y = element_line(color = text_col),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = ggtext::element_markdown(angle = 90, size = 12,
                                            color = text_col),
    axis.ticks.y = element_line(color = text_col),
    axis.ticks.x = element_blank()
  ) +
  lemon::coord_capped_cart(left = "bottom", gap = 0)


## pyramid plot of raw counts and normalized frequencies ------------

glowbe_counts <- glowbe_data_processed %>%
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
  geom_text(aes(label = n), hjust = 1.2, color = glowbe_text, fontface = "bold", size = 3, vjust = .5) +
  scale_fill_manual(name = "", values = glowbe_text) +
  labs(x = "", y = "", title = "*sat/stood*") +
  scale_y_reverse(limits = c(6.5,0)) +
  theme_icame() +
  annotate("text", y = 6, x = "LK", label = "Values beside bars\nshow raw counts",
           color = text_col, hjust = 0, vjust = 0) +
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
  geom_text(aes(label = n), hjust = -.2, color = glowbe_text, fontface = "bold", size = 3) +
  scale_fill_manual(name = "", values = glowbe_text) +
  labs(x = "", y = "", title = "*sitting/standing*") +
  ylim(0, 42) +
  theme_icame() +
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
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 12))
  )

ggsave(
  "glowbe_frequencies_ICAME.emf",
  device = "wmf",
  plot = p_comb,
  path = here("figures"),
  width = 10,
  height = 6.8
)

ggsave(
  "glowbe_frequencies_ICAME.png",
  device = "png",
  plot = p_comb,
  path = here("figures"),
  width = 10,
  height = 6.8
)


