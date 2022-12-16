# -------------------------------------------------------------------------
# File: custom_functions.R
#
# Description:
# Custom defined functions for data wrangling and analysis of the corpus data
# in the *BE sat/stood* project
# -------------------------------------------------------------------------

#
ReadDataset <- function(file, delim = "\t"){
  df <- vroom::vroom(file, delim = delim, escape_double = FALSE, trim_ws = TRUE,
                     col_types = "c") %>%
    janitor::clean_names() %>%
    dplyr::mutate(across(.cols = everything(), as.character))
}

# strip the "<<<" from the Query_item columns
StripBrackets <- function(df){
  df <- df %>%
    dplyr::mutate(
      query_item = stringr::str_remove_all(query_item, "(<<<|>>>)") %>%
        stringr::str_trim(),
      tagged_query_item = stringr::str_remove_all(tagged_query_item, "(<<<|>>>)") %>%
        stringr::str_trim()
    )
  return(df)
}

CodeCountry <- function(country) {
  code <- case_when(
    country == "Australia" ~ "AU",
    country == "Bangladesh" ~ "BD",
    country == "Canada" ~ "CA",
    country == "Great Britain" ~ "GB",
    country == "Ghana" ~ "GH",
    country == "Hong Kong" ~ "HK",
    country == "Ireland" ~ "IE",
    country == "India" ~ "IN",
    country == "Jamaica" ~ "JM",
    country == "Kenya" ~ "KE",
    country == "Sri Lanka" ~ "LK",
    country == "Malaysia" ~ "MY",
    country == "Nigeria" ~ "NG",
    country == "New Zealand" ~ "NZ",
    country == "Philippines" ~ "PH",
    country == "Pakistan" ~ "PK",
    country == "Singapore" ~ "SG",
    country == "Tanzania" ~ "TZ",
    country == "South Africa" ~ "ZA",
    country == "United States" ~ "US",
    TRUE ~ "XX"
  )
  return(code)
}

# functions for annotation ------------------------------------------------

# get the verb
GetVerb <- function(token){
  if(grepl("(sat|sitting|sitten)", token)){
    v <- "sit"
  } else v <- "stand"
  return(v)
}

# get the variant (-ed vs. -ing form) for the token
GetVariant <- function(token){
  form <- stringr::str_split(token, " ", simplify = T) %>%
    last()
  variant <- ifelse(form %in% c("sat", "stood", "sitten"), "ed", "ing")
  return(variant)
}

# make a column with simplified context, e.g. 2 words to the left and 6 words to
# the right of the token
MakeContext <- function(before, item, after, n1 = 1, n2 = 6){
  before_words <- stringr::str_split(before, " ", simplify = T)
  len_b <- length(before_words)
  after_words <- stringr::str_split(after, " ", simplify = T)
  text <- c(before_words[(len_b - n1):len_b], item, after_words[1:n2])

  return(paste(text, collapse = " "))
}

# get the following material after the token
GetPostmodifierPP <- function(after){
  first_word <- stringr::str_split(after, " ", simplify = T) %>%
    first()
  if(grepl("_(rp|ii|RP|II)", first_word)){
    postm <- "y"
  } else postm <- "n"

  return(postm)
}

GetPostmodifierVP <- function(after){
  words <- stringr::str_split(after, " ") %>%
    unlist()
  first_words <- words[1:4]
  if(any(grepl("_(v.g|V.G)", first_words))){
    postm <- "y"
  } else postm <- "n"

  return(postm)
}

CheckHorroAequi <- function(after, post_vp){
  if(post_vp == "y"){
    words <- stringr::str_split(after, " ") %>%
      unlist()
    first_words <- words[1:4]
    horror <- which(grepl("_(v.g|V.G)", first_words))[1]
  } else horror <- NA

  return(horror)
}

# get the subject of the token
GetSubject <- function(token, before){
  if(grepl("^'", token)){
    subj <- stringr::str_split(before, " ") %>%
      unlist() %>%
      last() %>%
      tolower()
  } else if(grepl("am", token)) {
    subj <- "i"
  } else {
    # take the last noun or pronoun in the preceding context as the most likely
    # subject
    before_words <- stringr::str_split(before, " ") %>%
      unlist()
    subj <- before_words[grepl("_[np]\\S+", before_words, ignore.case = T)] %>%
      last() %>%
      tolower()
  }
  return(subj)
}

GetSubjectPerson <- function(subj){
  if(is.na(subj)){
    pers <- "NA"
  } else if(subj %in% c("i", "we")){
    pers  <- "1P"
  } else if(subj == "you"){
    pers <- "2P"
  } else {
    pers <- "3P"
  }
  return(pers)
}

GetTenseAspect <- function(token, before){
  if(grepl("_vb[mzr]", token, ignore.case = T)){
    tense <- "pres"
  } else if(grepl("_vbd", token, ignore.case = T)){
    tense <- "past"
  } else {
    before_words <- stringr::str_split(before, " ") %>%
      unlist()
    last_word <- before_words[grepl("_(vm|vh|to)", before_words, ignore.case = T)] %>%
      last()
    if(grepl("_vm", last_word, ignore.case = T)){
      tense <- "modal"
    } else if(grepl("_vh[0gzi]", last_word, ignore.case = T)){
      tense <- "presperf"
    } else if(grepl("_vhd", last_word, ignore.case = T)){
      tense <- "pastperf"
    } else if(grepl("_to", last_word, ignore.case = T)){
      tense <- "inf"
    } else tense <- "NA"
  }

  return(tense)
}


# functions for tables ----------------------------------------------------

MakeCorpusTable <- function(df){
  header <- c("", "sat", "sat", "sitting", "sitting",
              "stood", "stood", "standing", "standing") %>%
    as.list()
  names(header) <- paste0("V",1:9)

 df %>%
  flextable() %>%
    colformat_char(j = c(3,5,7,9), prefix = "(", suffix = "%)") %>%
    set_header_labels(
      values = header
    ) %>%
    merge_h(part = "header") %>%
    italic(i = 1, j = 2:9, part = "header", italic = TRUE) %>%
    width(j = c(3,5,7,9), width = .5) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 12) %>%
    fontsize(j = c(3,5,7,9), size = 10, part = "body") %>%
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(j = 2:9, align = "right", part = "body") %>%
    align(j = 2:9, align = "center", part = "header") %>%
    vline(j = 5)
}

MakeHistoricalTable <- function(df){
  header <- c("", "Period", "sat", "sitting", "stood", "standing") %>%
    as.list()

  names(header) <- names(df)

  df %>%
    flextable() %>%
    set_header_labels(
      values = header
    ) %>%
    # merge_h(part = "header") %>%
    italic(j = 3:6, part = "header", italic = TRUE) %>%
    width(j = 2, width = .5) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 12) %>%
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(j = 3:6, align = "right", part = "all") %>%
    align(j = 1:2, align = "left", part = "all")
}


# functions for plotting --------------------------------------------------

BarPlot <- function(data, x, fill,
                    bar_colors = c("#FEC260", "#A12568"),
                    text_colors = c("grey10", "white"),
                    facet_by = NULL, sort = TRUE,
                    caption = ""
  ){
  require(dplyr)
  require(ggplot2)

  x_var <- rlang::enquo(x)
  fill_var <- rlang::enquo(fill)

  data <- data %>%
    mutate(across(c(!! x_var, !! fill_var), as.factor))

  x_levs <- data %>%
    pull(!!x_var) %>%
    levels()

  n_x_levs <- length(x_levs)

  fill_levs <- data %>%
    pull(!!fill_var) %>%
    as.factor() %>%
    levels()

  if(is.null(facet_by)){
    counts <- data %>%
      group_by(!! x_var, !! fill_var) %>%
      summarise(n = n()) %>%
      mutate(
        place = ifelse(!! fill_var == fill_levs[1], .98, .02),
        color = ifelse(!! fill_var == fill_levs[1], "a", "b"))
    x_nlevs <- data %>%
      pull(!!x_var) %>%
      nlevels()
    if(sort == T){
      p <- data %>%
        ggplot(aes(x = fct_reorder(!! x_var, as.numeric(!! fill_var), .fun = mean),
                 fill = !! fill_var))
    } else {
      p <- data %>%
        ggplot(aes(x = !! x_var, fill = !! fill_var))
    }
    p <- p + geom_bar(position = "fill", color = "#000000", width = .8) +
      geom_text(
        data = counts,
        size = 6,
        fontface = "bold",
        aes(x = !! x_var, y = place, label = n, color = color),
        hjust = rep(c(1, 0), length(x_levs))
      ) +
      # annotate(geom = "text", y = 1, x = n_x_levs + 0.5, hjust = 1, fontface = 'italic',
      #          label = "-ed", color = "grey90", vjust = 0, size = 6) +
      # annotate(geom = "text", y = 0, x = n_x_levs + 0.5,  hjust = 0, fontface = 'italic',
      #          label = "-ing", color = "grey90", vjust = 0, size = 6) +
      labs(x = "", y = "Percentage of tokens", caption = caption) +
      scale_fill_manual(guide = "none", values = bar_colors) +
      scale_color_manual(guide = "none", values = text_colors) +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      # coord_flip() +
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col, hjust = .1),
        strip.text = element_text(hjust = 0, size = 14, face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.line.x = element_line(color = "grey90"),
        axis.ticks.y = element_blank()
      ) +
      lemon::coord_capped_flip(bottom = 'both', gap = 0)
  } else {
    facet_var <- rlang::enquo(facet_by)

    counts <- data %>%
      group_by(!!x_var, !!fill_var, !!facet_var) %>%
      summarise(n = n()) %>%
      mutate(
        place = ifelse(!! fill_var == fill_levs[1], .98, .02),
        color = ifelse(!! fill_var == fill_levs[1], "a", "b"))
    x_nlevs <- data %>%
      pull(!!x_var) %>%
      nlevels()
    p <- data %>%
      ggplot(aes(x = fct_reorder(!! x_var, as.numeric(!! fill_var), .fun = mean),
                 fill = !! fill_var)) +
      geom_bar(position = "fill", color = "#000000", width = .8) +
      geom_text(
        data = counts,
        size = 6,
        fontface = "bold",
        aes(x = !! x_var, y = place, label = n, color = color),
        hjust = rep(c(1, 0), length(x_levs))
      ) +
      annotate(geom = "text", y = 1, x = n_x_levs + 0.5, hjust = 1, fontface = 'italic',
               label = "-ed", color = "grey90", vjust = 0, size = 6) +
      annotate(geom = "text", y = 0, x = n_x_levs + 0.5,  hjust = 0, fontface = 'italic',
               label = "-ing", color = "grey90", vjust = 0, size = 6) +
      labs(x = "", y = "Percentage of tokens", caption = caption) +
      scale_fill_manual(guide = "none", values = bar_colors) +
      scale_color_manual(guide = "none", values = text_colors) +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      facet_wrap(vars(!!facet_var), ncol = 2) +
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        plot.title = ggtext::element_markdown(size = rel(1.3), color = text_col, hjust = .1),
        strip.text = element_text(hjust = 0, size = 14, face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.line.x = element_line(color = "grey90"),
        axis.ticks.y = element_blank()
      ) +
      lemon::coord_capped_flip(bottom = 'both', gap = 0)
  }

  return(p)
}

