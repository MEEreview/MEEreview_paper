## 4- computing XC score
## packages used: terra, XSitu

wd <- "XC_vigna"

setwd(wd)
dir.create("data/final/XC_dist/", FALSE, TRUE)

# get species data
sv <- MEEreview_data::vigna_occ()

sv <- terra::vect(sv, c("lon", "lat"), crs="+proj=longlat")
spp <- sort(unique(sv$species) )

# get range data
r <- terra::rast(paste0("data/intermediate/sdm/adj/adj_", spp[1], ".tif"))
xy <- c(terra::init(r, "x"), terra::init(r, "y"))

# get environmental data
env <- terra::rast("data/intermediate/wc.tif")[[c("bio_1", "bio_12")]]
names(env) <- c("tmp", "prc")

# create envdist function
envdist <- function(x) {
  x$tmp[x$tmp > 13] <- 13 
  x$prc[x$prc > 2000] <- 2000
  p_tmp <- predict(mtmp, x)
  p_pr <- predict(mprc, x)
  rowMeans(cbind(p_tmp, p_pr))
}

# import fitted regression models
mprc <- readRDS("data/intermediate/m_prc.rds")
mtmp <- readRDS("data/intermediate/m_tmp.rds")

# equation 1
fun <- \(A, m=1/40) pmax(1, round(m * sqrt(A/pi)))

# create data to export in it
out <- data.frame(matrix(NA, ncol=6, nrow=length(spp)))
colnames(out) <- c("species", "zones", "XC", "dst", "envdst", "geodst")
out$species <- spp


set.seed(3388)

for (i in 1:length(spp)) {
  
  sp <- spp[i]
  
  # select G points
  seed <- sv[sv$species==sp & sv$gh=="seed", ]
  
  # get range
  r <- terra::rast(paste0("data/intermediate/sdm/adj/adj_", sp, ".tif"))
  k <- MEEreview_exsitu::get_samplesize(r, fun=fun)
  
  zones <- MEEreview_exsitu:::make_zones(xy, k$range, k$n, spread=TRUE)
  
  # get conservation score
  exs <- MEEreview_exsitu::XS_net(zones, seed, env, envdist, maxlink=1500)
  write.csv(exs$dist, paste0("data/final/XC_dist/", sp, "_dist.csv"), row.names=FALSE)
  out[i, 2:3] <- c(k$n, exs$score)
  
  if (!is.null(exs$dist)) {
    out[i,4:6] <- colMeans(exs$dist[, c("dst", "envdst", "geodst")]) 
  }
  
  print(paste(sp, k$n, round(exs$score, 3))); flush.console()
}

# export data with XC score
write.csv(out, "data/final/XC.csv", row.names=FALSE)
