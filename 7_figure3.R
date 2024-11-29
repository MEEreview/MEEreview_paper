## 7- figure 3
### XC scores

wd <- "XC_vigna"
setwd(wd)

s <- read.csv("table/descriptive_table_adjusted.csv")
s <- s[order(s$XC_nongeo_adjusted), ]

png(paste0("plots/", "fig3.png"), 1100, 1300, pointsize=36)
par(mar=c(4,1,1,3))

plot(s$XC_nongeo_adjusted, 1:nrow(s), cex=.5, pch=20, axes=FALSE, xlab="XC scores", ylab="", xlim=c(0, 1))
text(s$XC_nongeo_adjusted, 1:nrow(s), s$species, cex=.4, pos=4, xpd=T)
axis(1)

dev.off()