## 7- figure 5
### adjusted vs unadjusted range size

wd <- "XC_vigna"
setwd(wd)

table <- read.csv("table/descriptive_table_adjusted.csv")
table$adjusted_range_size <- table$adjusted_range_size / 1000000
table$sdm_range_size <- table$sdm_range_size / 1000000

table <- table[table$sdm_range_size <= 30, ] 

png(paste0("plots/", "fig5"), 1200, 1200, pointsize=36)
par(mar = c(4.5, 4.5, 2, 2))

plot(table$sdm_range_size, table$adjusted_range_size, xlab=expression("Unadjusted range size (10"^6*" km"^2*")"), ylab=expression("Adjusted range size (10"^6*" km"^2*")"), col="red", las=1, cex=1, pch=20)
abline(0,1, lwd=2, col="gray")
text(5, 4, substitute(italic("y=x")), col="dark gray", srt=38)

dev.off()