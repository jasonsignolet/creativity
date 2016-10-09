## Graphs for extended summary
##
## Revision history:
## - 2016-10-09 Initial

library(data.table)
library(magrittr)
library(ggplot2)
library(GGally)


dt <- fread("data/clean_data.csv")
dt <- dt[rowSums(is.na(dt)) < 20]

# Q1: What's the distribution of the teacher's thoughts on the opportunities that students have
# to express their creativity in different topics?

dt.m1 <- melt(dt[, c(1, grep("^creativity.", names(dt))), with = F], id.vars = "id")

p_thoughts_on_creativity <- ggplot(dt.m1[!is.na(value)], aes(variable, fill = factor(value))) + geom_bar(position = "dodge") + coord_flip()
# pdf("output/thoughts_on_creativity_by_subject.pdf", 6, 8)
# print(p_thoughts_on_creativity)
# dev.off()

# Q2: How do the teacher's indices of creativity relate to how they rate their own creativity
# make index measures
#
#
# first make inverse responses negative
# then sum responses for each category
# then turn the sums into Z-scores
categories <- c("methods", "plan", "risk", "ideas", "knowledge", "conceptionofcreativity",
                "difficulty", "explicitmethod")

new_col_names <- fread("data/new_col_names.csv")

inverse_items <- new_col_names[invert == 0, newname]

dt2 <- copy(dt)


for(col in inverse_items) {
  set(dt2, j = col, value = 6 - dt2[[col]])
}

for(i in categories) {
  search_string <- paste0("^", i, ".")

  print(search_string)
  dt2[, new_col := rowSums(.SD, na.rm = T), .SDcols = grep(search_string, colnames(dt2))]
  dt2[, new_col := new_col - mean(new_col)]
  dt2[, new_col := new_col / sd(new_col)]
  setnames(dt2, "new_col", paste0("index.", i))
}

p_index_scattermatrix <- ggpairs(dt2[, c("self_rating_creativity", names(dt2)[grep("^index.", names(dt2))]), with = F])
# pdf("output/index_scattermatrix.pdf", 10, 10)
# print(p_index_scattermatrix)
# dev.off()


# Q3: What do teachers think about creativity?

dt.m3 <- melt(dt[, grep("^conceptionofcreativity.", names(dt)), with = F])

ggplot(dt.m3[!is.na(value)], aes(variable, fill = factor(value))) + geom_bar(position = "dodge") + coord_flip()

for(i in categories) {
  .dt.m3 <- melt(dt[, grep(paste0("^", i, "."), names(dt)), with = F])
  #pdf(paste0("output/", i, "_items.pdf"), 8, 6)
  print(ggplot(.dt.m3[!is.na(value)], aes(variable, fill = factor(value))) + geom_bar(position = "dodge") + coord_flip())
  #dev.off()
}




# Q4: What do teachers think are the biggest barriers to promoting creativity in the classroom?

dt.m4 <- melt(dt[, grep("^barrier.", names(dt)), with = F])

p_barrier_percentages <- ggplot(dt.m4[, sum(value) / length(value), by = variable], aes(reorder(variable, V1), V1 * 100)) + geom_bar(stat = "identity") + coord_flip()
# pdf("output/barrier_percentages.pdf", 6, 8)
# print(p_barrier_percentages)
# dev.off()




