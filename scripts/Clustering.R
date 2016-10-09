# Title:       Clustering
#
# Description: Do randomForest clustering
#
#
# Authors:     Jason Signolet, Florence Gabriel
#
# Revision history:
#  - 2016-10-08 Started looking at how responses vary across creativity-space
#  - 2016-09-17 Initial
#

library(data.table)
library(magrittr)
library(foreach)
library(doParallel)
library(randomForest)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(cluster)
library(scales)
library(GGally)

set.seed(20160917)


# functions
majority <- function(x, max_levels = 100) {
  # returns a single majority level (the first it encounters)
  factorised <- factor(x)
  x_levels <- levels(factorised)
  level_number <- length(x_levels)
  if(level_number > max_levels) stop("Too many levels")

  biggest_level <- 0
  output <- NA
  for(i in seq(level_number)) {
    current_size <- sum(x == x_levels[i], na.rm = T)
    if(current_size > biggest_level) {
      biggest_level <- current_size
      output <- x_levels[i]
    }
  }
  return(output)
}

# load in cleaned data

dt <- fread("data/clean_data.csv", colClasses = list(character = "id"))

# only keep rows that have fewer than 20 missing responses
dt <- dt[which(rowSums(is.na(dt)) < 20)]



# define the features
#
#features <- names(dt)[-1]
#features <- features[-c(grep("yeargroup.", features), grep("school.", features))]

categories <- c("methods", "plan", "risk", "ideas", "knowledge", "conceptionofcreativity", "difficulty", "explicitmethod")

features <- foreach(i = categories, .combine = c) %do% {

  search_string <- paste0("^", i, ".")

  colnames(dt)[grep(search_string, colnames(dt))]
}

features <- c(features, "self_rating_creativity")





# assign all NAs to the majority class


for(col in features) {
  if(any(is.na(dt[[col]]))) {
    set(dt, i = which(is.na(dt[[col]])), j = col, value = majority(dt[[col]]))
  }
}


# remove zero-variance columns
for(col in names(dt)){
  if(length(levels(factor(dt[[col]]))) <= 1) {
    dt[, col := NULL, with = F]
  }
}


# make synthetic data

synth_length <- nrow(dt) * 5

synth <-
  foreach(col = features, .combine = cbind) %do% {
    .output <- data.table("this_col" = sample(dt[[col]], synth_length, replace = TRUE))
    setnames(.output, col)
  }

# check to see if cols have been decoupled
# p_age_exp_r <- ggplot(dt, aes(age, years_experience)) + geom_point() + ggtitle("Real")# strong correlation
# p_age_exp_s <- ggplot(synth, aes(age, years_experience)) + geom_point() + ggtitle("Synthetic") # no correlation
# p1 <- grid.arrange(p_age_exp_r, p_age_exp_s, ncol = 2)

# make training table

x <- rbind(dt[, features, with = F], synth[, features, with = F])
y <- factor(c(rep(1, nrow(dt)), rep(0, nrow(synth))))
real_wt  <- 1 / (2 * nrow(dt))
synth_wt <- 1 / (2 * nrow(synth))

# train randomForest classifier

registerDoParallel(8)
rf <-
  foreach(i = 1:8,
          .combine = randomForest::combine,
          .multicombine = TRUE,
          .packages = 'randomForest') %dopar% {
            randomForest(x, y,
                         na.action = na.omit,
                         ntree = 1000,
                         classwt = c("0" = synth_wt, "1" = real_wt),
                         importance = TRUE,
                         proximity = TRUE,
                         confusion = TRUE)
          }
registerDoSEQ()


# get distance matrix
rf_diss <- 1 - sqrt(rf$proximity[seq(nrow(dt)), seq(nrow(dt))] %>% as.matrix)
colnames(rf_diss) <- rownames(rf_diss) <- dt$id
rf_dist <- as.dist(rf_diss)

# perform multidimensional scaling. Reduce to 2 dimensions
rf_mds <- cmdscale(rf_dist, k = 2) %>%
  as.data.frame %>%
  setDT(keep.rownames = TRUE) %>%
  setnames(c("id", "mds1", "mds2")) %>%
  setkey(id)

rf_clusters <- pam(rf_dist, k = 2, cluster.only = TRUE) %>%
  as.data.frame %>%
  setDT(keep.rownames = T) %>%
  setnames(c("id", "cluster")) %>%
  setkey(id)

plot_pam_clusters <- rf_clusters[rf_mds]

# plot mds
pmds1 <- ggplot(plot_pam_clusters, aes(mds1, mds2)) +
  geom_point() +
  ggtitle("No clusters")
pmds2 <- ggplot(plot_pam_clusters, aes(mds1, mds2, colour = factor(cluster))) +
  geom_point() +
  ggtitle("PAM: 2 clusters") +
  theme(legend.position="none")
p2 <- quote(grid.arrange(pmds1, pmds2, ncol = 2))


# get feature importance

rf_imp <- rf$importance[, "MeanDecreaseAccuracy"] %>%
  as.data.frame %>%
  setDT(keep.rownames = TRUE) %>%
  setnames(c("variable", "importance")) %>%
  setkey(importance)

variable_order <- rf_imp[order(importance, decreasing = TRUE), variable]


# prepare data for heatmap

dt <- plot_pam_clusters[dt]
for(col in features) {
  set(dt, j = col, value = rescale(dt[[col]]))
}
dt.m1 <- melt(dt, measure.vars = features)

dt.m1[, variable := factor(variable, levels = variable_order)]

# plot heatmap

p3 <- ggplot(dt.m1, aes(variable, reorder(id, mds2))) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "black", high = "white") +
  theme(axis.text.x  = element_text(angle=90, hjust = 1, vjust = 0.5))

#p1
eval(p2)
p3


ggplot(dt.m1, aes(mds1, mds2, colour = factor(school.secondary))) + geom_point()


# colour plots

setkey(dt, id)
#with R data.tables, you can treat the session as if it is a database (i.e. you can do things like joining tables based on an index).
#The setkey() allows you to set the index. merge() lets you join the tables.
#In this case, we only wanted to join age, years of experience and the school type.







# make index measures
#
# first make inverse responses negative
# then sum responses for each category
# then turn the sums into Z-scores
new_col_names <- fread("data/new_col_names.csv")

inverse_items <- new_col_names[invert == 0, newname]

for(col in inverse_items) {
  set(dt, j = col, value = 6 - dt[[col]])
}

for(i in categories) {
  search_string <- paste0("^", i, ".")

  print(search_string)
  dt[, new_col := rowSums(.SD), .SDcols = grep(search_string, colnames(dt))]
  dt[, new_col := new_col - mean(new_col)]
  dt[, new_col := new_col / sd(new_col)]
  setnames(dt, "new_col", paste0("index.", i))
}



dt.m2 <- melt(dt[, c("mds1", "mds2", paste0("index.", categories), "cluster"), with = F], id.vars = c("mds1", "mds2", "cluster"))

p_clustering_with_colour_gradients <- ggplot(dt.m2, aes(mds1, mds2, colour = value)) + geom_point(size = 3) + facet_wrap(~variable) + scale_colour_continuous(low = "black", high = "cyan")
# pdf("output/clustering_with_colour_gradients.pdf", 10, 10)
# print(p_clustering_with_colour_gradients)
# dev.off()



dt.m3 <- melt(dt[, c("mds1", "mds2", names(dt)[grep("^subject.", names(dt))], "school.primary"), with = F], id.vars = c("mds1", "mds2"))




p_clustering_with_topic_markers <- ggplot(dt.m3, aes(mds1, mds2, colour = factor(value))) + geom_point() + facet_wrap(~variable) + scale_colour_manual(values = c("grey", "red"))

# pdf("output/clustering_with_topic_markers.pdf", 10, 10)
# print(p_clustering_with_topic_markers)
# dev.off()




#ggplot(dt, aes(mds1, mds2, colour = self_rating_creativity)) + geom_point(size = 4)

#ggpairs(dt[, grep("^index.", names(dt)), with = F])


# boxplots by cluster
dt.m2[, median(value), by = .(cluster, variable)]
for(v in unique(dt.m2$variable)) {
  .mean_diff <- dt.m2[variable == v & cluster == 1, mean(value)] - dt.m2[variable == v & cluster == 2, mean(value)]
  dt.m2[variable == v, mean_diff := .mean_diff]
}

p_boxplots_by_cluster <- ggplot(dt.m2, aes(reorder(variable, -mean_diff), value, fill = factor(cluster))) + geom_boxplot()
# pdf("output/boxplots_by_cluster.pdf", 8, 6)
# print(p_boxplots_by_cluster)
# dev.off()
