# Characteristics of creativity
#
# Author: Jason and Flo
#
# Revision history
# - 2016-09-17 Initial

library(data.table)
library(ggplot2)
library(magrittr)

characteristics <- fread("data/characteristics_of_creativity.csv", colClasses = list(character = "RespondentID"))

# we're replacing all the NA values with 0. This is specifically for data tables.It loops through all the columns of the data table as defined by names(characteristics). is.na() is a function that returns a boolean (TRUE or FALSE). which() returns the index of TRUE.
for (col in names(characteristics)) {
  set(characteristics,
      i = characteristics[[col]] %>% is.na %>% which,
      j = col,
      value = 0)
}

#We're going to get the column sums.

characteristics[, `:=`(RespondentID = NULL,
                      `List the top three characteristics that you feel best describe a creative student. - 1.` = NULL,
                      `List the top three characteristics that you feel best describe a creative student. - 2.` = NULL,
                      `List the top three characteristics that you feel best describe a creative student. - 3.` = NULL)]

char_sums <- colSums(characteristics)

sort(char_sums, decreasing = TRUE)

plot_characteristics <- char_sums %>%
  as.data.frame %>%
  setDT(keep.rownames = TRUE) %>%
  setnames(c("characteristic", "frequency"))

ggplot(plot_characteristics, aes(reorder(characteristic, frequency), frequency)) + geom_bar(stat = "identity") + coord_flip()
