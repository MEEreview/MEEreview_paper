## 5- computing FCS score
## packages used: terra, GapAnalysis

wd <- "XC_vigna"
setwd(wd)

dir.create("data/output/", FALSE, TRUE)

vignas <- MEEreview_data::vigna_occ()
spp <- sort(unique(vignas$species))
sv <- terra::vect(vignas, c("lon", "lat"), crs="+proj=lonlat")

f <- paste0("data/intermediate/sdm/range/range_", spp[1], ".tif")
ecoreg <- terra::vect(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp", package = "GapAnalysis")) 
ecoreg <- terra::rasterize(ecoreg, terra::rast(f), 1:nrow(ecoreg))

fcs <- function(inrange) {
  out <- lapply(spp, \(sp) {
    print(sp); flush.console()
    svsp <- sv[sv$species==sp, ]
    seed <- svsp[svsp$gh=="seed", ]
    herb <- svsp[svsp$gh=="non-seed", ]
    r <- terra::rast(paste0("data/intermediate/sdm/range/range_", sp, ".tif"))
    MEEreview_exsitu::FCex(seed, herb, r, ecoreg, bsize=50000, inrange=inrange)
  })
  round(do.call(rbind, out), 3)
}

out1 <- data.frame(sp=spp, fcs(TRUE))
write.csv(out1, "data/output/FCSex_fixed.csv", row.names=FALSE)

out2 <- data.frame(sp=spp, fcs(FALSE))
write.csv(out2, "data/output/FCSex_original.csv", row.names=FALSE)