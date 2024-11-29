## 6a- descriptive table preparation
## packages used: terra

wd <- "XC_vigna"
setwd(wd)

w1 <- terra::readRDS("data/intermediate/africa.rds")

vignas <- MEEreview_data::vigna_occ()
sv <- terra::vect(vignas, c("lon", "lat"), crs="+proj=lonlat")
spp <- sort(unique(sv$species))

deval <- read.csv("data/intermediate/sdm/evaluation.csv")

## number of all observations and ranks of scores
XC <- read.csv("data/final/XC.csv")
FCS1 <- read.csv("data/output/FCSex_original.csv")[, c("sp", "nseed", "nherb", "FCex")]
FCS2 <- read.csv("data/output/FCSex_fixed.csv")[, c("sp", "FCex")]
colnames(FCS2)[2] <- "FCex_fixed"
x <- merge(XC, FCS1, by=1, all.x=TRUE)
x <- merge(x, FCS2, by=1, all.x=TRUE)

x$rank_fcs <- rank(x$FCex)
x$All_observations <- (x$nseed + x$nherb)

## compute area size of adjusted and unadjusted range

sdm_range <- list()

for(i in seq_along(spp)){
  th <- deval$equal_sens_spec[deval$X == spp[i]] |> as.numeric()
  r_sdm <- terra::rast(paste0("data/intermediate/sdm/raw/raw_", spp[i], ".tif")) > th
  r_sdm <- ifel(r_sdm==0, NA, r_sdm)
  a_sdm <- expanse(r_sdm, unit="km")
  print(a_sdm)
  sdm_range[[i]] <- a_sdm$area
}
range_sdm <- as.data.frame(unlist(sdm_range))
colnames(range_sdm) <- "sdm_range"

adj <- matrix(NA, ncol=2, nrow=length(spp))
for(i in 1:length(spp)){
  sp <- spp[i]
  adj_range <- terra::rast(paste0("data/intermediate/sdm/adj/adj_", spp[i], ".tif"))
  a_adj <- expanse(adj_range, unit="km")
  print(a_adj)
  adj[i,] <- a_adj$area
  print(paste(sp, a_adj$area)); flush.console()
}

range_adjusted <- data.frame(species=spp, adj)
range_adjusted <- range_adjusted["X1"]
colnames(range_adjusted) <- "adj_range"

## merge with XC, FCS, and distance data 
###(ranks of XC will be added after adjustment - see 5-b)
descriptive_table <- cbind(x[,c(1, 7, 8, 12, 4:6)], range_sdm, range_adjusted, x[,c(9, 11, 3)])

colnames(descriptive_table) <- c("species", "nseed", "nherb", "all_obs", "dst", "envdst", "geodst", "sdm_range_size",
                                 "adjusted_range_size", "FCS_scores", "rank_FCS", "XC_scores")

write.csv(descriptive_table, "table/descriptive_table.csv")
