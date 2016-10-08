# Title:       Munging
#
# Description: Import surveymonkey data as CSV, change categorical data to OHE,
#              change ordinal data to int, get rid of bad rows.
#
# Authors:     Jason Signolet, Florence Gabriel
#
# Revision history:
#   - 2016-09-16 Initial
#

library(data.table)
library(magrittr)

# functions

ohe <- function(dt, col, max_ohe = 20, clear_col = T) {
  if (mode(dt[[col]]) != "character")
    set(dt, j = col, value = as.character(dt[[col]]))

  col_vals <- dt[[col]] %>%
    unique %>%
    sort

  if (length(col_vals) > max_ohe) stop("Too many columns to assign")

  for (col_val in col_vals) {
    this_col <- paste0(col, ".",
                       col_val  %>% gsub("[^[:alnum:]]", "", .) %>% tolower)
    set(dt, j = this_col, value = 0)
    set(dt, j = this_col, i = which(dt[[col]] == col_val), value = 1)
  }

  if (clear_col) set(dt, j = col, value = NULL)

  return(dt)
}




# call in data and rename columns
dt <- fread("data/export_from_surveymonkey.csv", colClasses = list(character = 1))
new_col_names <- fread("data/new_col_names.csv")
setnames(dt, new_col_names$newname)
str(dt)

# Drop state, as all but one are SA
# Drop all free-respsonse cols
###### May need to come back to this once the free response cols are re-coded
dt[, `:=`(
  state = NULL,
  subject.other_free = NULL,
  school_type_free = NULL,
  define_creativity_free = NULL,
  define_creative_student_1_free = NULL,
  define_creative_student_2_free = NULL,
  define_creative_student_3_free = NULL,
  barriers_to_creativity_free = NULL,
  courses_on_creativity_free = NULL,
  comments_free = NULL
  )]


# change applicable columns into boolean

cols_to_bool <-
  c("gender", "yeargroup.0", "yeargroup.1", "yeargroup.2", "yeargroup.3", "yeargroup.4",
    "yeargroup.5", "yeargroup.6", "yeargroup.7", "yeargroup.8", "yeargroup.9", "yeargroup.10",
    "yeargroup.11", "yeargroup.12", "school.primary", "school.secondary", "subject.maths",
    "subject.english", "subject.history", "subject.geography", "subject.biology",
    "subject.chemistry", "subject.physics", "subject.music", "subject.art",
    "subject.foreign_language", "subject.social_studies", "subject.practical_vocational",
    "subject.pe", "subject.other_bool", "barrier.time", "barrier.curriculum",
    "barrier.hard_to_plan", "barrier.standardised_tests", "barrier.school_culture",
    "barrier.resources", "barrier.student_disposition", "barrier.no_extracurricular",
    "barrier.lack_of_training", "barrier.parents", "courses_on_creativity")

for(col in cols_to_bool) {
  set(dt, j = col, value = dt[[col]] %>% factor %>% as.integer %>% `-`(1))
}


# change categoricals to OHE

cols_to_ohe <- c("school_type", "school_region", "school_gender")

for(col in cols_to_ohe) {
  ohe(dt, col)
}


# recode likert items

regularity <- data.table(
  code = c("Never", "Occasionally", "About half the time", "Regularly", "All the time"),
  score = 1:5,
  key = "code")

likelihood <- data.table(
  code = c("Not at all", "Unlikely", "Neutral", "Likely", "Extremely likely"),
  score = 1:5,
  key = "code")

agreement <- data.table(
  code = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
  score = 1:5,
  key = "code")

creativity_rating <- data.table(
  code = c("Not creative at all", "Somewhat creative", "Average creativity", "Quite creative", "Highly creative"),
  score = 1:5,
  key = "code")

cols_with_regularity <- names(dt)[c(grep("methods.", names(dt)),
                                    grep("plan.", names(dt)),
                                    grep("risk.", names(dt)),
                                    grep("ideas.", names(dt)))]

cols_with_likelihood <- names(dt)[grep("creativity.", names(dt))]

cols_with_agreement <- names(dt)[c(grep("statement.", names(dt)),
                                   grep("conceptionofcreativity.", names(dt)),
                                   grep("difficulty.", names(dt)),
                                   grep("explicitmethods.", names(dt)),
                                   grep("knowledge.", names(dt)))]

cols_with_creative_rating <- "self_rating_creativity"

for(col in cols_with_regularity) {
  for(i in 1:5) {
    dt[eval(as.name(col)) == regularity[score == i, code],
       eval(as.name(col)) := i]
  }
}

for(col in cols_with_agreement) {
  for(i in 1:5) {
    dt[eval(as.name(col)) == agreement[score == i, code],
       eval(as.name(col)) := i]
  }
}

for(col in cols_with_likelihood) {
  for(i in 1:5) {
    dt[eval(as.name(col)) == likelihood[score == i, code],
       eval(as.name(col)) := i]
  }
}

for(col in cols_with_creative_rating) {
  for(i in 1:5) {
    dt[eval(as.name(col)) == creativity_rating[score == i, code],
       eval(as.name(col)) := i]
  }
}

for(col in c(cols_with_regularity, cols_with_agreement, cols_with_likelihood, cols_with_creative_rating)) {
  set(dt, j = col, value = dt[[col]] %>% as.numeric)
}


# drop any zero-variance columns

zero_var_cols <- names(dt)[sapply(dt, function(a) length(levels(factor(a, exclude = NULL))) == 1) %>% which]
print(zero_var_cols)

for(col in zero_var_cols) {
  set(dt, j = col, value = NULL)
}

# write output to csv

write.csv(dt, "data/clean_data.csv", row.names = F)
