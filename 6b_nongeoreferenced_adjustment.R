## 6b- adjustment to non-georeferenced points

wd <- "XC_vigna"
setwd(wd)

nonref <- read.csv("table/non_georeferenced_points.csv")
nonref <- nonref[nonref$Var2 == "FALSE",]
nonref <- nonref[, c("Var1", "Freq")]
colnames(nonref) <- c("species", "nonref")

nonref <- nonref[!grepl("unguiculata ssp", nonref$species), ]

table <- read.csv("table/descriptive_table.csv")

mobs_all = loess(XC_scores ~ all_obs, data=table)
mobs_seed = loess(XC_scores ~ nseed, data=table)

p_seed <- pmax(table$XC_scores, 1/2*(predict(mobs_seed, nonref$nonref)))

p_seed <- as.data.frame(p_seed)
colnames(p_seed) <- "XC_nongeo_adjusted"
table <- cbind(table, p_seed)

table$rank_XC <- rank(table$XC_nongeo_adjusted)

write.csv(table, "table/descriptive_table_adjusted.csv")
