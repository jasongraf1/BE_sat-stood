# ----------------------------------------------------------------------------
# file: table_functions.R
# author: Jason Grafmiller
# date: 2022-12-16
# description: 
# Custom defined functions for creating summary tables for the *BE sat/stood* project
# ----------------------------------------------------------------------------

suppressMessages(library(tidyverse))
suppressMessages(library(flextable))

MakeCorpusTable <- function(df, type = c("html", "latex"), file = "table_corpus_summary.tex"){
  require(here)
  require(flextable)
  require(knitr)
  require(kableExtra)
  
  type <- match.arg(type)
  
  if(type == "html"){
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
      # font(fontname = "Times New Roman", part = "all") %>%
      fontsize(size = 12) %>%
      fontsize(j = c(3,5,7,9), size = 10, part = "body") %>%
      theme_booktabs() %>%
      set_table_properties(width = 1, layout = "autofit") %>%
      align(j = 2:9, align = "right", part = "body") %>%
      align(j = 2:9, align = "center", part = "header") %>%
      vline(j = 5)
  } else if (type == "latex"){
    df2 <- df |> 
      mutate(across(c(3, 5, 7, 9), ~paste0("(", .x, "%)")))
    nrows <- nrow(df)
    
    tex <- df2 %>%
      kable(
        format = "latex",
        align = "lrrrrrrrr",
        booktabs = T,
        col.names = NULL,
        label = "tbl-corpus-summary",
        caption = "Counts and proportions of \\emph{be sat/stood} variants across corpora",
      ) %>%
      add_header_above(c(" " = 1, "sat" = 2, "sitting" = 2, "stood" = 2, "standing" = 2)) |> 
      row_spec(nrows, extra_latex_after = "\\bottomrule") 
    # Add extra \toprule
    tex <- gsub("\\\\toprule", "\\\\toprule\\\\toprule", tex)
    # write to file
    writeLines(tex, here::here("TeX", file))
  }
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