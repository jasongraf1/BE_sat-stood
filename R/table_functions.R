# -------------------------------------------------------------------------
# File: table_functions.R
#
# Description:
# Custom defined functions for creating summary tables for the *BE sat/stood* project

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