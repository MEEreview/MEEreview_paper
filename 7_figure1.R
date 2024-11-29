## 7- figure 1 (diagram)
### same code for the supplementary diagram
## packages used: terra

wd <- "XC_vigna"
setwd(wd)

dir.create("data/diagram/", FALSE, TRUE)

vignas <- MEEreview_data::vigna_occ()
sv <- terra::vect(sv, c("lon", "lat"), crs="+proj=longlat")
usp <- sort(unique(sv$species))

r <- terra::rast(paste0("data/intermediate/sdm/adj/adj_", spp[1], ".tif"))
xy <- c(terra::init(r, "x"), terra::init(r, "y"))

deval <- read.csv("data/intermediate/sdm/evaluation.csv")
afr <- terra::readRDS("data/intermediate/africa.rds")

fun <- \(A, m=1/40) pmax(1, round(m * sqrt(A/pi))) #equation1

tercols <- rev(terrain.colors(25))
set.seed(3388)

for (sp in usp) {
  
  print(sp); flush.console()
  
  th <- deval$equal_sens_spec[deval$X == sp] |> as.numeric()
  svsp <- sv[sv$species==sp, ]
  seed <- svsp[svsp$gh=="seed", ]
  ca100 <- terra::aggregate(terra::buffer(svsp, 100000)) |> terra::crop(afr)
  ca250 <- terra::aggregate(terra::buffer(svsp, 250000)) |> terra::crop(afr)
  r_sdm <- terra::rast(paste0("data/intermediate/sdm/raw/raw_", sp, ".tif")) > th
  
  r_100 <- terra::rasterize(ca100, r_sdm, background=0) * 3
  m_sdm <- sum(c(r_sdm, terra::mask(r_sdm, ca250)), na.rm=TRUE)
  sdm <- terra::mask(terra::ifel(m_sdm == 0, r_100, m_sdm), m_sdm)
  
  adj_range <- terra::rast(paste0("data/intermediate/sdm/adj/adj_", sp, ".tif"))
  
  k <- MEEreview_exsitu::get_samplesize(adj_range, fun=fun)
  
  zones <- MEEreview_exsitu:::make_zones(xy, k$range, k$n, spread=TRUE)
  
  
  x <- terra::centroids(zones, inside=TRUE)
  
  e <- terra::ext(ca250) + .5
  
  # adjust to square to fill up the available space   
  p = as.vector(e)
  a = c(diff(p[3:4]), diff(p[1:2]))
  e = e * rep((a / min(a)), each=2)
  
  # network
  network <- MEEreview_exsitu::XS_net(zones, seed, maxlink=1500, return_network=TRUE)
  
  
  png(paste0("plots/diagram/", sp, "_diagram.png"), 1200, 1200, pointsize=28)
  
  par(mfrow=c(2,2), mar=rep(.5, 4))
  
  ## A
  plot(sdm, legend=FALSE, mar=.5, axes=FALSE, col=tercols, ylim=c(-35,25))
  lines(afr, col="light gray", lwd=.1)
  lines(ca250, col="blue", lwd=1.5)
  lines(e, lty=2)
  points(svsp, cex=.5)	
  
  add_legend(-20, -17, legend = c("not suitable", "suitable", "unsuitable within 100 km", "suitable within 250 km"), pch = 15, col=c("#F1F2E6", "#E3BB56", "#10A82C", "#84D63C"), cex=0.8, bty="n", y.intersp = 0.9)
  add_legend(-20, -11, legend = "observations", pch=20, bty="n", cex=.8)
  add_legend(-20, -5, legend = "250 km buffer", lty=1, lwd=1.5, col="blue", cex=.8, bty="n")
  add_legend("topleft", legend = "(A)", cex=1, bty="n")
  add_legend("topright", legend = paste0("V. ", sp), bty="n")
  
  ## B
  plot(sdm, legend=FALSE, ext=e, axes=FALSE, mar=.5, background="azure", box=FALSE, col=tercols)
  lines(afr, col="light gray", lwd=2)
  lines(ca250, col="blue", lwd=1.5)
  if (nrow(seed) > 0) {
    plot(seed,  add=TRUE, pch=3, col="black", )
  }
  plot(svsp[svsp$gh=="non-seed",],  add=TRUE, pch=1, col="black", )
  if (nrow(seed) > 0) {
    add_legend("bottomleft", legend=c("yes", "no"), pch=c(3, 1), title="seed", bty="o", bg="white")
  } else {
    add_legend("bottomleft", legend="no", pch=1, title="seed", bty="o", bg="white")
  }
  add_legend("topleft", legend = "(B)", cex=1, bty="n")
  add_box()
  
  ## C
  i <- extract(zones, seed)[,2] |> unique() 
  zones$seed <- (1:nrow(zones) %in% i) + 1
  
  plot(afr, col="gray95", border="gray95", mar=.5, axes=FALSE, ext=e, background="azure", box=FALSE)
  lines(afr, col="light gray", lwd=2)
  plot(zones, col=c("orange", "light blue")[zones$seed], add=TRUE, border=gray(.5), lwd=2, alpha=.6)
  add_legend("bottomleft", legend=c("yes", "no"), title="seed", fill=c("light blue", "orange"), bty="o", bg="white")
  points(seed, pch=3, col="black", cex=1,)
  add_legend("topleft", legend = "(C)", cex=1, bty="n")
  add_box()
  
  ## D
  plot(afr, col="gray95", border="gray95", mar=.5, axes=FALSE, ext=e, background="azure", box=FALSE)
  lines(afr, col="light gray", lwd=2)
  plot(zones, col=c("orange", "light blue")[zones$seed], add=TRUE, border=gray(.5), lwd=2, alpha=.6)
  lines(network, lwd=2, col="red")
  add_legend("bottomleft", legend=c("yes", "no"), title="seed", fill=c("light blue", "orange"), bty="o", bg="white")
  points(x, cex=2.5, col="white")
  text(x, cex=.8)
  legend("topleft", legend = "(D)", cex=1, bty="n")
  add_box()
  
  dev.off()
}