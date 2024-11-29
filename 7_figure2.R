## 7- figure 2
### number of observations per species
## packages used: terra

wd <- "XC_vigna"
setwd(wd)

dir.create("data/plots/", FALSE, TRUE)

vignas <- MEEreview_data::vigna_occ()
sv <- terra::vect(vignas, crs="+proj=longlat")

afr <- terra::readRDS("data/intermediate/africa.rds")

tab1 <- as.data.frame(table(vignas$species))
colnames(tab1) <- c("spp", "n")

tab2 <- as.data.frame(table(vignas$species[vignas$gh=="seed"]))
colnames(tab2) <- c("spp", "seed")

d <- merge(tab1, tab2, by="spp", all.x=TRUE)
d$seed[is.na(d$seed)] <- 0
d <- d[order(d$n), ]

png(paste0("plots/", "fig2.png"), 2600, 1400, pointsize=32)

par(mfrow=c(1, 2), mar=c(3, 1, 1, 3))

terra::plot(afr, col="gray95", border="gray", las=1, ylim=c(-35,25), mar=c(1,1.5,0,0))
terra::plot(sv[sv$gh == "non-seed",], add=TRUE, legend="bottomleft", col=rgb(1, 0, 0, 0.2), pch=20, cex=.5)
terra::plot(sv[sv$gh == "seed",], add=TRUE, legend="bottomleft", col="blue", pch=20, cex=.5)
lines(afr, col="gray", lwd=2)
# Add scale
terra::sbar(1000, c(40, -34), cex=.8, below="km")
##Add arrow
terra::north(xy=c(-15, -15), type=1, angle=180, label="S")
text(45, 17, "(A)", cex=1.25, xpd=TRUE)
add_legend("bottomleft", legend=c("seed", "other"), col=c("blue", "red"), title="Sample type", pch = 20, pt.bg = NA, bty="n", cex=1.5, lty=0.5)

sq <- seq(0,2650,250)
plot(d$n, 1:nrow(d), las=1, cex=1, col="blue", axes=F, xlab="Number of observations/genebank observations per species", pch=20, ylab="")
points(d$seed, 1:nrow(d), pch=20, col=rgb(1, 0, 0, 0.75))
x <- sapply(1:nrow(d), \(i) lines(rbind(cbind(d[i, "seed"], i), cbind(d[i, "n"], i))))

axis(1, at=sq, cex.axis=.7, labels=F)
text(sq, -4, sq, cex=.6, xpd=T)
text(d$n, 1:nrow(d), d$spp, cex=.6, pos=4, xpd=T, col="black")
text(1200, 51, "(B)", cex=1.25, xpd=TRUE)

dev.off()
