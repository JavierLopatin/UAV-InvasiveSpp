

################################################################################
## R-Script - 3_Shadow_simulation.R                                           ##
## author: Javier Lopatin                                                     ##
## mail: javierlopatin@gmail.com                                              ##  
##                                                                            ##
##                                                                            ##
## description: Simulation of the amount of shadows on the study areas        ##
##              depending on the time of the day and the season               ## 
##                                                                            ##
################################################################################

require(suncalc)
require(insol)
require(raster)
require(rgdal)

setwd("D:/invasive_spp/DSM")

# load rasters
acacia = raster("//IFGG-VG-NSMS//VgA2//samovar_all2//biobio_f1_DEM005_19s.tif")
ulex = raster("//IFGG-VG-NSMS//VgA2//samovar_all2//chiloe_f3_DEM005_19s.tif")
pinus = raster("dsm_pinus_f8.tif")

# shapefiles
shp_acacia <- readOGR("D:/invasive_spp/1_reference", layer = "acacia_f1_canopy")
shp_ulex <- readOGR("D:/invasive_spp/1_reference", layer = "ulex_f3_canopy")
shp_pinus <- readOGR("D:/invasive_spp/1_reference", layer = "pinus_f8_canopy")

# crop external areas
plot(acacia); plot(shp_acacia, add=T)
img_acacia <- crop(acacia, drawExtent())
plot(ulex); plot(shp_ulex, add=T)
img_ulex <- crop(ulex, drawExtent())
plot(pinus); plot(shp_pinus, add=T)
img_pinus <- crop(pinus, drawExtent())

# check rasters
plot(img_acacia); plot(shp_acacia, add=T)
plot(img_ulex); plot(shp_ulex, add=T)
plot(img_pinus); plot(shp_pinus, add=T)

# To have only positive values. Negative DSM values are obtained in surface areas below the altitude
# of the launch point
img_acacia = img_acacia+10
img_ulex = img_ulex+12
img_pinus = img_pinus+27

######################
#### Functions to use

# store altitude for varying hours between 10-18 hrs for beginning, middle and end of summer
getSunAngles <- function(lat, lon){ 
  # store results
  ele1 = c() # elevation; beggining of summer 
  ele2 = c() # elevation; end of summer
  ele3 = c() # elevation; middle summer
  azi1 = c() # azimuth; beggining of summer 
  azi2 = c() # azimuth; end of summer 
  azi3 = c() # azimuth; middle of summer 
  for(i in 9:18){
    if (i < 10){ 
      ele1[i] = getSunlightPosition(date = paste0("2017-12-21 0",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[4]
      ele2[i] = getSunlightPosition(date = paste0("2018-03-20 0",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[4]
      ele3[i] = getSunlightPosition(date = paste0("2018-02-04 0",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[4]
      azi1[i] = getSunlightPosition(date = paste0("2017-12-21 0",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[5]
      azi2[i] = getSunlightPosition(date = paste0("2018-03-20 0",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[5]
      azi3[i] = getSunlightPosition(date = paste0("2018-02-04 0",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[5]
    } else {
      ele1[i] = getSunlightPosition(date = paste0("2017-12-21 ",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[4]
      ele2[i] = getSunlightPosition(date = paste0("2018-03-20 ",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[4]
      ele3[i] = getSunlightPosition(date = paste0("2018-02-04 ",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[4]
      azi1[i] = getSunlightPosition(date = paste0("2017-12-21 ",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[5]
      azi2[i] = getSunlightPosition(date = paste0("2018-03-20 ",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[5]
      azi3[i] = getSunlightPosition(date = paste0("2018-02-04 ",i+4,":00:00"), lat = lat, lon = lon,
                                    keep = c("altitude", "azimuth"))[5]
    }
  }
  
  # store altitudes
  var_ele1 = rad2deg( unlist(ele1) )
  var_ele2 = rad2deg( unlist(ele2) )
  var_ele3 = rad2deg( unlist(ele3) )
  var_azi1 = rad2deg( unlist(azi1) )
  var_azi2 = rad2deg( unlist(azi2) )
  var_azi3 = rad2deg( unlist(azi3) )
  
  out <- list( list(var_ele1, var_ele2, var_ele3), list(var_azi1, var_azi2, var_azi3) )
  names(out) <- c("elevation", "zenith")
  
}

# convert radoans to degrees
rad2deg <- function(rad) {(rad * 180) / (pi)}

# estimate an average of azimuth orientations 
shadow_fraction <- function(dtm, elevation, azimuth, shp, outname, saveRaster=TRUE){ 
  # store shadows
  for(i in 1:length(elevation)){
    sv = normalvector(90-elevation[i], 360-azimuth[i])
    sh = doshade(dtm, sv)
    if (i == 1)
      img <- sh
    else 
      img <- addLayer(img, sh)
  }
  # mask out non-invasive canopies
  raster <- mask(img, shp)
  # save raster
  if (saveRaster == TRUE)
    writeRaster(raster, filename = outname, format="GTiff", overwrite=T)
  # obtain shadow fraction
  fraction = c()
  for (j in 1:length(elevation)){
    shadows = length(which(getValues(raster[[j]]) == 0))
    sunny = length(which(getValues(raster[[j]]) == 1))
    if(shadows > 1)
      fraction[j] = (shadows*100)/(shadows+sunny) 
    if(shadows == 0)
      fraction[j] = 0
  }
  fraction
}

# estimate shadow fraction at the beginning and end of the summer period 
shadow_season <- function(dtm, sunAngles, shp, outname){ 
  shadow1 = shadow_fraction(dtm, sunAngles$elevation[[1]], sunAngles$zenith[[1]], shp, outname, saveRaster=FALSE)
  shadow2 = shadow_fraction(dtm, sunAngles$elevation[[2]], sunAngles$zenith[[2]], shp, outname, saveRaster=FALSE)
  shadow3 = shadow_fraction(dtm, sunAngles$elevation[[3]], sunAngles$zenith[[3]], shp, outname, saveRaster=TRUE)
  data <- data.frame(shadow1, shadow2, shadow3)
  colnames(data) <- c("beginning", "end", "mid")
  data
  }


######################
#### Process

# get sun angles
ang_acacia <- getSunAngles(-36.783008, -72.811956)
ang_ulex <- getSunAngles(-35.33321, -72.41156)
ang_pinus <- getSunAngles(-41.886727, -73.897909)

# calculate shadow fraction per specie
sh_acacia <- shadow_season(img_acacia, ang_acacia, shp_acacia, "acacia_shadows.tif")
sh_ulex <- shadow_season(img_ulex, ang_ulex, shp_ulex, "ulex_shadows.tif")
sh_pinus <- shadow_season(img_pinus, ang_pinus, shp_pinus, "pinus_shadows.tif")


##########################
#### Count shaded pixels

shadow_acacia <- stack("acacia_shadows.tif")
shadow_ulex <- stack("ulex_shadows.tif")
shadow_pinus <- stack("pinus_shadows.tif")

count_acacia <- calc(shadow_acacia, function(x) sum(x == 0))
count_ulex <- calc(shadow_ulex, function(x) sum(x == 0))
count_pinus <- calc(shadow_pinus, function(x) sum(x == 0))

writeRaster(count_acacia, filename = "count_acacia.tif", format="GTiff", overwrite=T)
writeRaster(count_ulex, filename = "count_ulex.tif", format="GTiff", overwrite=T)
writeRaster(count_pinus, filename = "count_pinus.tif", format="GTiff", overwrite=T)

################
### plot

svg(file = "shadow_fractions.svg", width=9, height=3.5)
par(mfrow=c(1,3), mai = c(0.8, 0.6, 0.5, 0.1))
# acacia
plot(sh_acacia$mid, type="l", ylim=c(0,100), ylab="Shadow fraction [%]", xlab="Time of the day", las=1,xaxt = "n")
polygon( c(seq(1,length(var_ele1),1), rev(seq(1,length(var_ele1),1))), c(sh_acacia$beginning, rev(sh_acacia$end)), col=rgb(1,0,0,alpha = 0.4), border=NA )
lines(sh_acacia$mid, lwd=2)
axis(1, at=seq(1,10,2), labels=paste0(seq(9,18,2),rep(":00",5)))
grid()
# ulex
plot(sh_ulex$mid, type="l", ylim=c(0,100), ylab="Shadow fraction [%]", xlab="Time of the day", las=1,xaxt = "n")
polygon( c(seq(1,length(var_ele1),1), rev(seq(1,length(var_ele1),1))), c(sh_ulex$beginning, rev(sh_ulex$end)), col=rgb(1,0,0,alpha = 0.4), border=NA )
lines(sh_ulex$mid, lwd=2)
axis(1, at=seq(1,10,2), labels=paste0(seq(9,18,2),rep(":00",5)))
grid()
# pinus
plot(sh_pinus2$mid, type="l", ylim=c(0,100), ylab="Shadow fraction [%]", xlab="Time of the day", las=1,xaxt = "n")
polygon( c(seq(1,length(var_ele1),1), rev(seq(1,length(var_ele1),1))), c(sh_pinus2$beginning, rev(sh_pinus2$end)), col=rgb(1,0,0,alpha = 0.4), border=NA )
lines(sh_pinus2$mid, lwd=2)
axis(1, at=seq(1,10,2), labels=paste0(seq(9,18,2),rep(":00",5)))
grid()
dev.off()
