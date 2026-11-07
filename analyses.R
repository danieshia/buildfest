rm(list = ls())
ls()
library(dplyr); library(tidyr); library(ggplot2); library(janitor)
getwd()
list.files()
survey <- read_excel("buildfest_988_cleaned.xlsx", sheet = "analysis_ready")

# ---- Who is in the data ----
survey |> tabyl(role_primary)    |> adorn_pct_formatting()
survey |> tabyl(county)          |> adorn_pct_formatting()
survey |> tabyl(spanish_speaker) |> adorn_pct_formatting()

# Putting the agree/disagree answers in scale order ----
levels_likert <- c("Strongly disagree", "Disagree",
                   "Neither agree nor disagree", "Agree", "Strongly agree")

likert_cols <- c("post_identify", "post_describe", "post_when_use",
                 "pre_identify",  "pre_describe",  "pre_when_use",
                 "presenter_engaged", "presenter_knew", "well_organized")

survey <- survey |>
  mutate(across(all_of(likert_cols), ~ factor(.x, levels = levels_likert)))

# ---- Knowledge before training ----
survey |> tabyl(pre_identify)  |> adorn_pct_formatting()#I could identify..
survey |> tabyl(pre_describe)  |> adorn_pct_formatting()#I could describe 988..
survey |> tabyl(pre_when_use)  |> adorn_pct_formatting()#I knew when to use 988


# ---- Knowledge after training ----
survey |> tabyl(post_identify) |> adorn_pct_formatting() #I CAN identify mental health challenges and crises
survey |> tabyl(post_describe) |> adorn_pct_formatting() #I CAN describe 988 and how it works
survey |> tabyl(post_when_use) |> adorn_pct_formatting() #I KNOW when to use 988


# ---- Training experience ----
survey |> tabyl(presenter_engaged) |> adorn_pct_formatting()
survey |> tabyl(presenter_knew)    |> adorn_pct_formatting()
survey |> tabyl(well_organized)    |> adorn_pct_formatting()

# ---- 0-10 questions (valid_percent excludes blanks) ----
survey |> tabyl(recommend_0_10)   |> adorn_pct_formatting()
survey |> tabyl(likely_call_0_10) |> adorn_pct_formatting()

# ---- Language and other fields ----
survey |> tabyl(form_language)     |> adorn_pct_formatting()
survey |> tabyl(text_language)     |> adorn_pct_formatting()
survey |> tabyl(asked_likely_call) |> adorn_pct_formatting()

library(dplyr)

survey |>
  group_by(role_primary) |> #splits the 1,289 people into 5 groups by role
  summarise( #calculates each number separately inside each group
    n = n(),
    pre_identify  = round(100 * mean(pre_identify_num  >= 4, na.rm = TRUE)),
    post_identify = round(100 * mean(post_identify_num >= 4, na.rm = TRUE)),
    pre_describe  = round(100 * mean(pre_describe_num  >= 4, na.rm = TRUE)),
    post_describe = round(100 * mean(post_describe_num >= 4, na.rm = TRUE)),
    pre_when_use  = round(100 * mean(pre_when_use_num  >= 4, na.rm = TRUE)),
    post_when_use = round(100 * mean(post_when_use_num >= 4, na.rm = TRUE))
  ) |>
  mutate(
    identify_change = post_identify - pre_identify,
    describe_change = post_describe - pre_describe,
    when_use_change = post_when_use - pre_when_use
  )
