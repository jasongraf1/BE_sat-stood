# -------------------------------------------------------------------------
# File: data_munge.R
#
# Description:
# Load in raw datasets from the various corpora and combine them into single
# datasets. All BNC, GloWbE, EEBO and BofE data were downloaded from CQPweb at
# Lancaster (https://cqpweb.lancs.ac.uk/) and Birmingham
# (https://cqpweb.bham.ac.uk/)
# -------------------------------------------------------------------------

# Libs
library(tidyverse)
library(here)

source(here("scripts_R", "custom_functions.R"))

# GloWbE data ----------------------------------------------------------------

# There are some parsing erros in the data, but we ignore these for now
glowbe_data_raw <- here("data_raw") %>%
  list.files(pattern = "glowbe", full.names = TRUE) %>%
  map_df(ReadDataset)

glowbe_data_raw %>%
  glimpse()

glowbe_data_processed <- glowbe_data_raw %>%
  StripBrackets() %>%
  mutate(
    variant = map_chr(query_item, GetVariant),
    verb = map_chr(query_item, GetVerb),
    subj_tagged = map2_chr(query_item, tagged_context_before, GetSubject),
    subj = str_remove(subj_tagged, "_.*"),
    subj_person = map_chr(subj, GetSubjectPerson),
    tense_aspect = map2_chr(tagged_query_item, tagged_context_before, GetTenseAspect),
    postmodifier_pp = map_chr(tagged_context_after, GetPostmodifierPP),
    postmodifier_vp = map_chr(tagged_context_after, GetPostmodifierVP),
    horror_aequi = map_chr(tagged_context_after, CheckHorroAequi),
    token_simple = pmap_chr(list(context_before, query_item, context_after), MakeContext)
    )

# Cases of passive "BE stood down" are common in AUS and NZ but are not reliable hits.
# Genuine uses of "BE stood down" are rare, so the false negative rate is likely
# to be low enough to exclude these here
glowbe_data_processed <- glowbe_data_processed %>%
  dplyr::filter(!(verb == "stand" & grepl("down ", context_after))) %>%
  mutate(
    country_code = map_chr(country, CodeCountry)
  )

# check data
# glowbe_data_processed %>%
#   glimpse()

# save as compact file
glowbe_data_processed %>%
  rownames_to_column("token_ID") %>%
  saveRDS(here("data_processed", "data_BE_sat_glowbe.rds"))

rm(glowbe_data_raw, glowbe_data_processed)

# BNC data ----------------------------------------------------------------

# BNC files were downloaded using the restricted query to collect metadata for
# speaker gender and dialect region. Basically download separate files for each
# gender, dialect region level 3 (North, South, Midlands, Wales) and dialect region
# 4 (Northwest, Northeast, Liverpool, London, Yorkshire, West Midlands, etc.).
# All these datasets are then combined.
ReadDataset2 <- function(file, delim = "\t"){
  location <- str_remove_all(file, "(.*BE_sat_|_BNC2014.txt)")
  df <- read_delim(file, delim = delim, escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols()) %>%
    janitor::clean_names() %>%
    mutate(
      across(.cols = everything(), as.character),
      region4 = location
    )
}

bnc_data_region4_raw <- here("data_raw") %>%
  list.files(pattern = "BNC", full.names = TRUE) %>%
  str_subset("(F|M|NEG|r3)", negate = T) %>%
  map_df(ReadDataset2)

ReadDataset3 <- function(file, delim = "\t"){
  location <- str_remove_all(file, "(.*BE_sat_|_r3_BNC2014.txt)")
  df <- read_delim(file, delim = delim, escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols()) %>%
    janitor::clean_names() %>%
    mutate(
      across(.cols = everything(), as.character),
      region3 = location
    )
}

bnc_data_region3_raw <- here("data_raw") %>%
  list.files(pattern = "r3_BNC", full.names = TRUE) %>%
  map_df(ReadDataset3)

# gender data
male <- here("data_raw", "BE_sat_Male_BNC2014.txt") %>%
  ReadDataset() %>%
  mutate(
    gender = "male"
  )

female <- here("data_raw", "BE_sat_Female_BNC2014.txt") %>%
  ReadDataset() %>%
  mutate(
    gender = "female"
  )

bnc_data_gender_raw <- bind_rows(male, female)

# combine the data
bnc_data_raw <- bnc_data_gender_raw %>%
  left_join(bnc_data_region3_raw,
            by = c("text_id", "context_before")) %>%
  select(!ends_with(".y")) %>%
  rename_with(~str_replace(.x, "\\.x$", "")) %>%
  left_join(bnc_data_region4_raw,
            by = c("text_id", "context_before")) %>%
  select(!ends_with(".y")) %>%
  rename_with(~str_replace(.x, "\\.x$", ""))

bnc_data_processed <- bnc_data_raw %>%
  StripBrackets() %>%
  mutate(
    variant = map_chr(query_item, GetVariant),
    verb = map_chr(query_item, GetVerb),
    subj_tagged = map2_chr(query_item, tagged_context_before, GetSubject),
    subj = str_remove(subj_tagged, "_.*"),
    subj_person = map_chr(subj, GetSubjectPerson),
    tense_aspect = map2_chr(tagged_query_item, tagged_context_before, GetTenseAspect),
    postmodifier_pp = map_chr(tagged_context_after, GetPostmodifierPP),
    postmodifier_vp = map_chr(tagged_context_after, GetPostmodifierVP),
    horror_aequi = map_chr(tagged_context_after, CheckHorroAequi),
    token_simple = pmap_chr(list(context_before, query_item, context_after), MakeContext)
  )

# check data
# bnc_data_processed %>%
#   glimpse()

# save as compact file
bnc_data_processed %>%
  rownames_to_column("token_id") %>%
  saveRDS(here("data_processed", "data_BE_sat_bnc.rds"))

rm(bnc_data_raw, bnc_data_processed, bnc_data_region3_raw, bnc_data_region4_raw,
   male, female, bnc_data_gender_raw)


# Bank of English data ----------------------------------------------------

# There are some parsing erros in the data, but we ignore these for now
bank_of_E_data_raw <- here("data_raw") %>%
  list.files(pattern = "bank_of_E", full.names = TRUE) %>%
  map_df(ReadDataset)

bank_of_E_data_raw %>%
  glimpse()

bank_of_E_data_processed <- bank_of_E_data_raw %>%
  StripBrackets() %>%
  mutate(
    variant = map_chr(query_item, GetVariant),
    verb = map_chr(query_item, GetVerb),
    subj_tagged = map2_chr(query_item, tagged_context_before, GetSubject),
    subj = str_remove(subj_tagged, "_.*"),
    subj_person = map_chr(subj, GetSubjectPerson),
    tense_aspect = map2_chr(tagged_query_item, tagged_context_before, GetTenseAspect),
    postmodifier_pp = map_chr(tagged_context_after, GetPostmodifierPP),
    postmodifier_vp = map_chr(tagged_context_after, GetPostmodifierVP),
    horror_aequi = map_chr(tagged_context_after, CheckHorroAequi),
    token_simple = pmap_chr(list(context_before, query_item, context_after), MakeContext)
  )


# check data
# bank_of_E_data_processed %>%
#   glimpse()

# Cases of passive "BE stood down" are common in AUS and NZ but are not reliable hits.
# Genuine uses of "BE stood down" are rare, so the false negative rate is likely
# to be low enough to exclude these here.
# Also there is some more parsing issues so we filter out these (~0.7%)
bank_of_E_data_processed <- bank_of_E_data_processed %>%
  dplyr::filter(!(verb == "stand" & grepl("down ", context_after))) %>%
  dplyr::filter(country %in% c("UK", "USA", "Canada", "Australia", "New Zealand", "South Africa", "India"))

# save as compact file
bank_of_E_data_processed %>%
  rownames_to_column("token_ID") %>%
  saveRDS(here("data_processed", "data_BE_sat_bank_of_E.rds"))

rm(bank_of_E_data_raw, bank_of_E_data_processed)

# CLMET data --------------------------------------------------------------

clmet_data <- here("data_raw", "BE_sat_CLMET.txt") %>%
  read.delim()

clmet_data %>%
  glimpse()

# the tagging is particularly bad here, so we need to filter out the false positives
clmet_data_clean <- clmet_data %>%
  dplyr::filter(grepl("^(was|were|is|are|am|be|been)", token_raw))

clmet_data_clean %>%
  glimpse()

clmet_data_clean %>%
  write_delim(here("data_raw", "BE_sat_CLMET_clean.txt"), delim = "\t")


