# -------------------------------------------------------------------------
# File: custom_functions.R
#
# Description:
# Custom defined functions for dat wrangling and analysis of the corpus data
# in the *BE sat/stood* project
# -------------------------------------------------------------------------

#
ReadDataset <- function(file, delim = "\t"){
  df <- read_delim(file, delim = delim, escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols()) %>%
    rename_with(.fn = ~ str_replace_all(.x, " ", "_"), .cols = everything()) %>%
    mutate(across(.cols = everything(), as.character))
}

# strip the "<<<" from the Query_item columns
StripBrackets <- function(df){
  df <- df %>%
    mutate(
      Query_item = str_remove_all(Query_item, "(<<<|>>>)") %>%
        str_trim(),
      Tagged_query_item = str_remove_all(Tagged_query_item, "(<<<|>>>)") %>%
        str_trim()
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
  if(grepl("(sat|sitting)", token)){
    v <- "sit"
  } else v <- "stand"
  return(v)
}

# get the variant (-ed vs. -ing form) for the token
GetVariant <- function(token){
  form <- str_split(token, " ", simplify = T) %>%
    last()
  variant <- ifelse(form %in% c("sat", "stood"), "ed", "ing")
  return(variant)
}

# make a column with simplified context, e.g. 2 words to the left and 6 words to
# the right of the token
MakeContext <- function(before, item, after, n1 = 1, n2 = 6){
  before_words <- str_split(before, " ", simplify = T)
  len_b <- length(before_words)
  after_words <- str_split(after, " ", simplify = T)
  text <- c(before_words[(len_b - n1):len_b], item, after_words[1:n2])

  return(paste(text, collapse = " "))
}

# get the following material after the token
GetPostmodifier <- function(after){
  first_word <- str_split(after, " ", simplify = T) %>%
    first()
  if(grepl("_(rp|ii|RP|II)", first_word)){
    postm <- "PP"
  } else if(grepl("_(v.g|V.G)", first_word)){
    postm <- "VP"
  } else postm <- "none"

  return(postm)
}

# get the subject of the token
GetSubject <- function(token, before){
  if(grepl("^'", token)){
    subj <- str_split(before, " ") %>%
      unlist() %>%
      last() %>%
      tolower()
  } else if(grepl("am", token)) {
    subj <- "i"
  } else {
    # take the last noun or pronoun in the preceding context as the most likely
    # subject
    before_words <- str_split(before, " ") %>%
      unlist()
    subj <- before_words[grepl("_[np]\\S+", before_words, ignore.case = T)] %>%
      last() %>%
      tolower()
  }
  return(str_remove(subj, "_.*"))
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
    before_words <- str_split(before, " ") %>%
      unlist()
    last_word <- before_words[grepl("_(vm|vh|to)", before_words, ignore.case = T)] %>%
      last()
    if(grepl("_vm", last_word, ignore.case = T)){
      tense <- "modal"
    } else if(grepl("_vh0", last_word, ignore.case = T)){
      tense <- "presperf"
    } else if(grepl("_vhd", last_word, ignore.case = T)){
      tense <- "pastperf"
    } else if(grepl("_to", last_word, ignore.case = T)){
      tense <- "inf"
    } else tense <- "NA"
  }

  return(tense)
}