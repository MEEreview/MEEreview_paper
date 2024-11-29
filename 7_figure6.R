## 7- figure 6
### sample size Vs. XC score

wd <- "XC_vigna"
setwd(wd)

table <- read.csv("table/descriptive_table_adjusted.csv")

png(paste0("plots/", "fig6_samplesize.png"), 1200, 1200, pointsize=36)
par(mar = c(4.5, 4.5, 2, 2))

plot(table$nseed, table$XC_nongeo_adjusted, xlab="Number of seedbank samples", ylab="XC Score", las=1, col="red", log="x")

dev.off()
