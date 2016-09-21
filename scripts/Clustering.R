# Title:       Clustering
#
# Description: Do randomForest clustering
#
#
# Authors:     Jason Signolet, Florence Gabriel
#
# Revision history:
#   - 2016-09-17 Initial
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

dt <- fread("data/clean_data_(rows_removed).csv", colClasses = list(character = "id"))
features <- names(dt)[-1]
features <- features[-c(grep("yeargroup.", features), grep("school.", features))]

# assign all NAs to the majority class

for(col in features[-c(1, 2, 3)]) {
  if(any(is.na(dt[[col]]))) {
    set(dt, i = which(is.na(dt[[col]])), j = col, value = majority(dt[[col]]))
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
p_age_exp_r <- ggplot(dt, aes(age, years_experience)) + geom_point() + ggtitle("Real")# strong correlation
p_age_exp_s <- ggplot(synth, aes(age, years_experience)) + geom_point() + ggtitle("Synthetic") # no correlation
p1 <- grid.arrange(p_age_exp_r, p_age_exp_s, ncol = 2)

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
p2 <- grid.arrange(pmds1, pmds2, ncol = 2)


# get feature importance

rf_imp <- rf$importance[, "MeanDecreaseAccuracy"] %>%
  as.data.frame %>%
  setDT(keep.rownames = TRUE) %>%
  setnames(c("variable", "importance")) %>%
  setkey(importance)

variable_order <- rf_imp[order(importance, decreasing = TRUE), variable]


# prepare data for heatmap

dt_with_clusters <- plot_pam_clusters[dt]
for(col in features) {
  set(dt_with_clusters, j = col, value = rescale(dt_with_clusters[[col]]))
}
dt_with_clusters.m <- melt(dt_with_clusters, measure.vars = features)

dt_with_clusters.m[, variable := factor(variable, levels = variable_order)]

# plot heatmap

p3 <- ggplot(dt_with_clusters.m, aes(variable, reorder(id, mds2))) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "black", high = "white") +
  theme(axis.text.x  = element_text(angle=90, hjust = 1, vjust = 0.5))

p1
p2
p3


ggplot(dt_with_clusters.m, aes(mds1, mds2, colour = factor(school.secondary))) + geom_point()
ggplot(dt_with_clusters.m[variable == "courses_on_creativity"], aes(mds1, mds2, colour = value)) + geom_point()
