# -------------------------------------------------------------------------
# File: plot_functions.R
#
# Description:
# Custom defined functions for plot the corpus data in the *BE sat/stood*
# project
# -------------------------------------------------------------------------

extrafont::loadfonts(device = "win")

# theme_set(theme_classic(base_family = "serif"))

# Create custom theme for pretty plots
theme_light <- function (text_col = "grey10", bg_col = "white") {
  require(ggplot2)
  theme_classic() %+replace%
    theme(
      # text = element_text(family = "serif"),
      axis.ticks.x = element_line(color = text_col),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      plot.caption = element_text(size = rel(1), color = text_col),
      panel.background = element_rect(fill = bg_col, color = bg_col),
      strip.background = element_rect(fill = bg_col, color = bg_col),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = rel(1.3), hjust = 0, color = text_col),
      axis.text.y = element_text(size = rel(1.4), color = text_col),
      axis.text.x = element_text(size = rel(1.2), color = text_col),
      axis.title.x = element_text(color = text_col),
      axis.title.y = element_text(color = text_col),
      plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col,
                                            hjust = .1),
      legend.background = element_rect(fill = bg_col, color = bg_col),
      legend.text = element_text(color = text_col, size = rel(1.4))
    )
}

theme_dark <- function (text_col = "white", bg_col = "black") {
  require(ggplot2)
  theme_classic() %+replace%
    theme(
      axis.ticks.x = element_line(color = text_col),
      plot.background = element_rect(fill = bg_col, color = bg_col),
      plot.caption = element_text(size = rel(1), color = text_col),
      panel.background = element_rect(fill = bg_col, color = bg_col),
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

# Plot for glowbe frequencies
PlotGlowbeFrequencies <- function(df, file, theme = c("dark", "light", "bw"), w = 10,
                                  h = 6.8, dev = c("png", "pdf", "wmf"), dpi = 320){
  require(tidyverse)
  require(patchwork)
  require(ggcharts)
  require(lemon)

  varieties <- c("US", "CA", "GB", "IE", "AU", "NZ", "IN", "LK", "PK", "BD",
                 "SG", "MY", "PH", "HK", "ZA", "NG", "GH", "KE", "TZ", "JM")

  device <- match.arg(dev)
  if(device == "pdf"){
    update_geom_defaults("text", list(family = "serif"))
  }

  theme <- match.arg(theme)
  if(theme == "dark"){
    text_col <- "white"
    bg_col <- "#000000"
    glowbe_dark <- rep("grey30", 20)
    glowbe_text <- rep("grey60", 20)
    names(glowbe_dark) <- varieties
    names(glowbe_text) <- varieties
    glowbe_dark["GB"] <- "#9ad2fc"
    glowbe_text["GB"] <- "#9ad2fc"
  } else if (theme == "light") {
    text_col <- "black"
    bg_col <- "white"
    glowbe_dark <- rep("grey70", 20)
    glowbe_text <- rep("grey50", 20)
    names(glowbe_dark) <- varieties
    names(glowbe_text) <- varieties
    glowbe_dark["GB"] <- "#0207a6"
    glowbe_text["GB"] <- "#0207a6"
  } else if (theme == "bw"){
    text_col <- "black"
    bg_col <- "white"
    glowbe_dark <- rep("grey70", 20)
    glowbe_text <- rep("grey50", 20)
    names(glowbe_dark) <- varieties
    names(glowbe_text) <- varieties
    glowbe_dark["GB"] <- "black"
    glowbe_text["GB"] <- "black"
  }

  p_perc <- df %>%
    group_by(country_code, variant) %>%
    count() %>%
    group_by(country_code) %>%
    mutate(
      prop = n/sum(n),
      perc = paste0(round(100*prop), "%"),
      y = -.01,
      country_code = factor(
        country_code,
        levels = varieties)
    ) %>%
    dplyr::filter(variant == "ed") %>%
    ggplot(aes(country_code, prop, fill = country_code)) +
    geom_col(width = .8) +
    geom_text(aes(label = country_code, y = y, color = country_code), size = 5,
              fontface = "bold") +
    labs(x = "", y = "", title = "*BE sat/stood* as percentage of all tokens") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(guide = "none", values = glowbe_dark) +
    scale_color_manual(guide = "none", values = glowbe_text)

  if(theme != "dark"){
    p_perc <- p_perc + theme_light()
  } else {
    p_perc <- p_perc + theme_dark()
  }

  p_perc <- p_perc +
    theme(
      panel.grid.major.x = element_blank(),
      axis.line.y = element_line(color = text_col),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_line(color = text_col),
      axis.ticks.x = element_blank()
    ) +
    lemon::coord_capped_cart(left = "bottom")

  # make the pyramid plot

  glowbe_counts <- df %>%
    dplyr::filter(country_code != "XX") %>%
    droplevels() %>%
    group_by(country_code, variant) %>%
    mutate(
      country_code = factor(
        country_code,
        levels = varieties)
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

  p_left <- glowbe_counts %>%
    dplyr::filter(variant == "ed") %>%
    ggplot(aes(country_code, per_mil)) +
    geom_col(fill = glowbe_dark, width = .6) +
    geom_text(aes(label = n), hjust = 1.2, color = glowbe_dark, fontface = "bold", size = 3) +
    scale_fill_manual(name = "", values = glowbe_text) +
    labs(x = "", y = "", title = "*sat/stood*") +
    scale_y_reverse(limits = c(6.5,0)) +
    ggcharts:::pyramid_theme("left")

  p_right <- glowbe_counts %>%
    dplyr::filter(variant == "ing") %>%
    ggplot(aes(country_code, per_mil)) +
    geom_col(fill = glowbe_dark, width = .6) +
    geom_text(aes(label = n), hjust = -.2, color = glowbe_dark, fontface = "bold", size = 3) +
    scale_fill_manual(name = "", values = glowbe_text) +
    labs(x = "", y = "", title = "*sitting/standing*") +
    ylim(0, 42) +
    ggcharts:::pyramid_theme("right")

  if(theme != "dark"){
    p_left <- p_left + theme_light()
    p_right <- p_right + theme_light()
  } else {
    p_left <- p_left + theme_dark()
    p_right <- p_right + theme_dark()
  }

  p_left <- p_left +
    theme(plot.title = ggtext::element_markdown(hjust = .9, color = text_col),
          axis.line.x = element_line(color = text_col),
          axis.text.x = element_text(color = text_col),
          axis.ticks.x = element_line(color = text_col),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
    lemon::coord_capped_flip(bottom = "right", gap = 0)

  p_right <- p_right +
    theme(plot.title = ggtext::element_markdown(hjust = .1, color = text_col),
          axis.line.x = element_line(color = text_col),
          axis.text.x = element_text(color = text_col),
          axis.ticks.x = element_line(color = text_col),
          axis.text.y = element_text(size = rel(.9), color = text_col),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
    lemon::coord_capped_flip(bottom = "left", gap = 0)

  p_combined <- p_perc/(p_left + p_right) +
    plot_annotation(
      caption = "Frequency per million words",
      theme =
        theme(plot.caption = element_text(hjust = 0.5, size = 14, color = "white"),
              plot.background = element_rect(fill = bg_col))
    )

  device <- match.arg(dev)

  if(device == "pdf"){
    p_combined <- p_combined +
      theme(
        text = element_text(family = "serif"),
        axis.text.x = element_text(family = "serif"),
        axis.text.y = element_text(family = "serif"),
        axis.title = element_text(family = "serif"),
        plot.title = element_text(family = "serif"),
        strip.text = element_text(family = "serif"),
        plot.caption = element_text(family = "serif")
        )
  }

  here("figures", file) %>%
    ggsave(p_combined, device = device, height = h, width = w)

  return(here("figures", file))
}


PlotBars <- function(data, file, bar_colors = c("#FEC260", "cornflowerblue"),
                     text_colors = c("black", "black"), w = 4.8, h = 3.2,
                     dev = c("png", "pdf", "wmf"), dpi = 320){
  require(dplyr)
  require(ggplot2)

  data <- data %>%
    dplyr::filter(postmodifier_vp == "y") %>%
    mutate(variant = as.factor(variant),
           variant = relevel(variant, ref = "ed"),
           dist_to_post_vp = as.integer(dist_to_post_vp) %>%
             as.factor())

  x_levs <- data %>%
    pull(dist_to_post_vp) %>%
    levels()

  n_x_levs <- length(x_levs)

  fill_levs <- data %>%
    pull(variant) %>%
    as.factor() %>%
    levels()

  counts <- data %>%
    group_by(dist_to_post_vp, variant) %>%
    count() %>%
    mutate(
      place = ifelse(variant == fill_levs[1], .98, .02),
      color = ifelse(variant == fill_levs[1], "a", "b"))

  x_nlevs <- data %>%
    pull(dist_to_post_vp) %>%
    nlevels()

  p <- data %>%
    ggplot(aes(x = dist_to_post_vp, fill = variant)) +
    geom_bar(position = "fill", color = "#000000", width = .8) +
    geom_text(
      data = counts,
      size = 6,
      fontface = "bold",
      aes(x = dist_to_post_vp, y = place, label = n, color = color),
      hjust = rep(c(1, 0), length(x_levs))
    ) +
    labs(x = "", y = "Percentage of tokens") +
    scale_fill_manual(guide = "none", values = bar_colors) +
    scale_color_manual(guide = "none", values = text_colors) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    theme_dark() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 16),
      axis.line.x = element_line(color = "white"),
      axis.ticks.y = element_blank()
    ) +
    lemon::coord_capped_flip(bottom = 'both', gap = 0)

  here("figures", file) %>%
    ggsave(p, device = device, height = h, width = w)
  return(here("figures", file))
}


PlotTwitterMap <- function(data, map, file, verb = c("sit", "stand"),
                           theme = c("dark", "blue", "bw")) {
  # earlier version 
  # {rgdal} is being retired in 2023, so migrate to PlotTwitterMap2()
  require(sp); 
  require(rgdal); 
  require(classInt); 
  require(plotrix); 
  require(Hmisc)

  projection <- "+proj=longlat +datum=WGS84"
  map <- spTransform(map, CRS(projection))
  map <- map[order(map$name), ]

  data <- arrange(data, REGION)

  v <- match.arg(verb)
  # pull out the percentages to color by
  if(v == "sit"){
    verb_percents <- 100*(data$SAT/(data$SAT + data$SITTING))
    label <- "BE Sat/Sitting"
  } else if(v == "stand"){
    verb_percents <- 100*(data$STOOD/(data$STOOD + data$STANDING))
    label <- "BE Stood/Standing"
  }

  # Colour Palettes
  color <- match.arg(theme)
  if (color == "blue"){
    # blue and yellow theme
    red <- "#F5D042"
    blue <- "#2131FE"
    textcol <- "white"
    bgcol = "white"
    legendcol = "black"
  } else if (color == "dark") {
    red <- "#E6401A"
    blue <- "#1A2666"
    textcol <- "white"
    bgcol = "black"
    legendcol = "white"
  } else if (color == "bw") {
    red <- "#E6401A"
    blue <- "#1A2666"
    textcol <- "white"
    bgcol = "black"
    legendcol = "white"
  }

  breaks <- 10
  colfunc <- colorRampPalette(c(blue, red), bias = 1)
  colpal <- colfunc(breaks)
  class <- classIntervals(verb_percents, breaks, style = "quantile", intervalClosure = "right")
  colors <- findColours(class, colpal)

  # Map
  png(here::here("figures", file), width = 1800, height = 2800, res = 300)
  par(mar = c(0, 0, 0, 0), bg = bgcol, family = "sans")
  plot(map, xlim = c(-6, -1), ylim = c(50.00, 59.4), col = colors, lwd = .25)

  # Legend
  text(0.5, 58.4, label, cex = 1.2, col = legendcol)
  gradient.rect(0.20, 55.8, .80, 57.8,
                col = colpal, gradient = "y"
  )
  text(.87, 55.84, round(min(class$brks), 1), cex = .75, pos = 4, col = legendcol)
  text(.87, 56.80, round(class$brks[breaks / 2 + 1], 1), cex = .75, pos = 4, col = legendcol)
  text(.87, 57.70, round(max(class$brks), 1), cex = .75, pos = 4, col = legendcol)

  # London inset
  subplot(plot(map, col = colors, lwd = .25, xlim = c(-.48, .238), ylim = c(51.39, 51.64)),
          x = -6.78, y = 52.9, size = c(1.25, 1.18)
  )
  rect(-.52, 51.29, .26, 51.75, lwd = .6, border = "white")
  text(-7.54, 52.14, "London", cex = .75, col = legendcol)

  # Text for bottom corner
  text(0.98, 49.85, "UK Twitter 2014", cex = .75, col = legendcol)

  # City Names
  text(-1.898575, 52.489471, "Birmingham", cex = .5, col = textcol)
  text(-2.244644, 53.483959 + .01, "Manchester", cex = .5, col = textcol)
  text(-1.548567, 53.801277, "Leeds", cex = .5, col = textcol)
  text(-2.59665 + .06, 51.45523 - .003, "Bristol", cex = .5, col = textcol)
  text(-4.251433, 55.860916, "Glasgow", cex = .5, col = "white")
  text(-3.179090 - .21, 51.481583 + .01 - .003, "Cardiff", cex = .5, col = textcol)
  text(-1.466667, 53.383331, "Sheffield", cex = .5, col = textcol)
  text(-1.150000, 52.950001, "Nottingham", cex = .5, col = textcol)
  text(-5.926437 - .14, 54.607868 - .04, "Belfast", cex = .5, col = "white")
  text(-3.188267 + .006, 55.953251 - .06, "Edinburgh", cex = .5, col = "white")
  text(-2.983333 + .2, 53.400002 - .015, "Liverpool", cex = .5, col = textcol)
  text(-1.600000 - .18, 54.966667 - .003, "Newcastle", cex = .5, col = textcol)
  text(1.297355, 52.630886, "Norwich", cex = .5, col = textcol)
  text(-1.133333, 52.633331, "Leicester", cex = .5, col = textcol)
  text(-1.404351, 50.909698, "Southampton", cex = .5, col = textcol)

  dev.off()

  return(here::here("figures", file))
}

PlotTwitterMap2 <- function(data, map, file, verb = c("sit", "stand"),
                           theme = c("dark", "blue", "bw")) {
  # updated version using {sf} and {ggplot2} 
  require(ggplot2)
  require(patchwork);
  require(sf); 
  require(mapsf); 
  require(classInt); 
  require(plotrix); 
  require(Hmisc)
  
  data <- data |> 
    rename(name = "REGION") |> 
    mutate(
      SAT_PERC = 100*(SAT/(SAT + SITTING)),
      STOOD_PERC = 100*(STOOD/(STOOD + STANDING))
    )
  
  # add data to map df
  map <- map |> 
    left_join(
      data, by = "name"
    )
  
  # match the verb and theme elements
  v <- match.arg(verb)
  color <- match.arg(theme)
  if (color == "blue"){
    # blue and yellow theme
    red <- "#F5D042"
    blue <- "#2131FE"
    textcol <- "white"
    bgcol = "white"
    legendcol = "black"
  } else if (color == "dark") {
    red <- "#E6401A"
    blue <- "#1A2666"
    textcol <- "white"
    bgcol = "black"
    legendcol = "white"
  } else if (color == "bw") {
    red <- "grey10"
    blue <- "grey90"
    textcol <- "white"
    bgcol = "white"
    legendcol = "black"
  }
  
  breaks <- 10
  colfunc <- colorRampPalette(c(blue, red), bias = 1)
  colpal <- colfunc(breaks)
  # pull out the percentages to color by
  if(v == "sit"){
    label <- "BE sat/sitting"
    class <- classIntervals(data$SAT_PERC, breaks, style = "quantile", intervalClosure = "right")
    color_breaks <- quantile(data$SAT_PERC, seq(.1, .9, .1)) |> 
      as.vector()
    data_range <- range(data$SAT_PERC)
    data_median <- median(data$SAT_PERC)
  } else if(v == "stand"){
    label <- "BE stood/standing"
    class <- classIntervals(data$STOOD_PERC, breaks, style = "quantile", intervalClosure = "right")
    color_breaks <- quantile(data$STOOD_PERC, seq(.1, .9, .1)) |> 
      as.vector()
    data_range <- range(data$STOOD_PERC)
    data_median <- median(data$STOOD_PERC)
  }
  colors <- findColours(class, colpal)
  
  # Map
  # png(here::here("figures", file), width = 1800, height = 2800, res = 300)
  if(v == "sit"){
    p_main <- ggplot(map) +
      geom_sf(aes(fill = SAT_PERC), linewidth = .04, color = legendcol)
  } else {
    p_main <- ggplot(map) +
      geom_sf(aes(fill = STOOD_PERC), linewidth = .04, color = legendcol)
  }
  p_main <- p_main +
    ylim(50.1, 59.4) +
    scale_fill_gradientn(
      limits = data_range,
      colors = colpal[c(1, seq_along(colpal), length(colpal))],
      values = c(0, scales::rescale(color_breaks, from = data_range), 1),
      breaks = c(data_range[1], mean(data_range), data_range[2]),
      labels = c(round(min(data_range[1]), 1), round(data_median), round(data_range[2], 1)),
      name = label
    ) + 
    geom_rect(aes(xmin = -.48, xmax = .238, ymin = 51.3, ymax = 51.71), 
              linewidth = .25, color = "white", fill = NA) +
    labs(caption = "UK Twitter 2014") +
    theme_void() +
    theme(
      legend.title = element_text(vjust = 1.5, color = legendcol, hjust = .5),
      legend.text = element_text(color = legendcol),
      legend.position = c(.9, .7),
      plot.background = element_rect(fill = bgcol, color = NA),
      plot.caption = element_text(color = legendcol, size = 10),
      # plot.caption.position = c(.99, .01)
      ) +
    annotate(geom = "text", x = -1.898575, y = 52.489471, label = "Birmingham", size = 2, color = "white") +
    annotate(geom = "text", x = -2.244644, y = 53.483959 + .01, label = "Manchester", size = 2, color = "white") +
    annotate(geom = "text", x = -1.548567, y = 53.801277, label = "Leeds", size = 2, color = "white") +
    annotate(geom = "text", x = -2.59665 + .06, y = 51.45523 - .003, label = "Bristol", size = 2, color = "white") +
    annotate(geom = "text", x = -4.251433, y = 55.860916, label = "Glasgow", size = 2, col = "white") +
    annotate(geom = "text", x = -3.179090 - .21, y = 51.481583 + .01 - .003, label = "Cardiff", size = 2, color = "white") +
    annotate(geom = "text", x = -1.466667, y = 53.383331, label = "Sheffield", size = 2, color = "white") +
    annotate(geom = "text", x = -1.150000, y = 52.950001, label = "Nottingham", size = 2, color = "white") +
    annotate(geom = "text", x = -5.926437 - .14, y = 54.607868 - .04, label = "Belfast", size = 2, col = "white") +
    annotate(geom = "text", x = -3.188267 + .006, y = 55.953251 - .06, label = "Edinburgh", size = 2, col = "white") +
    annotate(geom = "text", x = -2.983333 + .2, y = 53.400002 - .015, label = "Liverpool", size = 2, color = "white") +
    annotate(geom = "text", x = -1.600000 - .18, y = 54.966667 - .003, label = "Newcastle", size = 2, color = "white") +
    annotate(geom = "text", x = 1.297355, y = 52.630886, label = "Norwich", size = 2, color = "white") +
    annotate(geom = "text", x = -1.133333, y = 52.633331, label = "Leicester", size = 2, color = "white") +
    annotate(geom = "text", x = -1.404351, y = 50.909698, label = "Southampton", size = 2, color = "white") 
  # London inset
  if(v == "sit"){
    p_london <- ggplot(map) +
      geom_sf(aes(fill = SAT_PERC), linewidth = .05, color = legendcol)
  } else {
    p_london <- ggplot(map) +
      geom_sf(aes(fill = STOOD_PERC), linewidth = .05, color = legendcol)
  }
  p_london <- p_london +
    xlim(-.48, .238) +
    ylim(51.3, 51.71) +
    scale_fill_gradientn(
      limits = data_range,
      colors = colpal[c(1, seq_along(colpal), length(colpal))],
      values = c(0, scales::rescale(color_breaks, from = data_range), 1),
      breaks = c(data_range[1], 50, data_range[2]),
      labels = c(round(min(data_range[1]), 1), round(data_median), round(data_range[2], 1))
    ) +
    labs(caption = "London") +
    theme_void() + guides(fill = "none") +
    theme(plot.caption = element_text(size = 10, hjust = 0, color = legendcol),
          panel.border = element_rect(fill = NA, color = legendcol))
  
  p <- p_main + inset_element(p_london, left = 0.01, bottom = 0.1, right = .31, top = .45)
  
  # dev.off()
  ggsave(here::here("figures", file), plot = p, device = "png", width = 1800, 
         height = 2800, units = "px", dpi = 300)
  
  return(here::here("figures", file))
}


PlotPartialEffects <- function(mod, file,  w = 4.8, h = 3.2, dev = c("png", "pdf", "wmf"), dpi = 320){
  require(effects)

  p <- effects::Effect(c("dist_to_post_vp", "verb"), mod) %>%
    as_tibble() %>%
    mutate(dist_to_post_vp = as.numeric(dist_to_post_vp) %>%
             round() %>% as.factor()) %>%
    ggplot(aes(x = dist_to_post_vp, y = fit, color = verb)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
    geom_point(size = 4) +
    geom_line(aes(group = verb), color = "grey80", linetype = "dashed") +
    facet_wrap(~verb) +
    scale_color_manual(guide = "none", values = c("#66ff66", "#66ff66")) +
    scale_x_discrete(labels = 0:3) +
    theme_dark() +
    labs(x = "# words between main verb and postmodifying verb",
         y = "Predicted probability of *sat/stood*")+
    theme(
      axis.text.x = element_text(size = 12),
      strip.text = element_blank(),
      axis.title.y = ggtext::element_markdown(angle = 90,
                                              padding = unit(c(0, 0, 5, 0), "pt")),
      axis.line.y = element_line(color = "white")
    )

  device <- match.arg(dev)
  here("figures", file) %>%
    ggsave(p, device = device, height = h, width = w)

  return(here("figures", file))

}
