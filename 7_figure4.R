## 7- figure 4
### geo-environmental distances

wd <- "XC_vigna"
setwd(wd)

dxs <- read.csv("data/final/XC.csv")

png(paste0("plots/", "fig4.png"), 1200, 1200, pointsize=30)

plot(dxs$geodst, dxs$envdst, xlab = "Geographic distance (km)", ylab = "Environmental distance (km)")
m <- lm(dxs$envdst ~ dxs$geodst)
cf <- round(coefficients(m), 2)
txt <- paste0("y = ", round(cf[1],1), " + ", cf[2], " 	x")
abline(m, col="red", lty=2, lwd=1.5)
text(580, 700, substitute(italic(txt), list(txt=txt)), col="red", cex=.8, xpd=TRUE, srt=13.5)
abline(0,1, lwd=2, col="gray")
text(600, 500, substitute(italic("y=x")), col="dark gray", srt=15)

#define species to spot on the figure
spsp <- c("microsperma", "membranacea", "debanensis", "monantha", "longifolia", "venulosa", "richardsiae", "nuda", "juncea")
spdxs <-  dxs[dxs$species %in% spsp, ]
text(spdxs$geodst, spdxs$envdst, spdxs$species, pos=4, cex=0.7)

dev.off()

