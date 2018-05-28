
################################################################################
## R-Script - 1_Modeling.R                                                    ##
## author: Javier Lopatin                                                     ##
## mail: javierlopatin@gmail.com                                              ##  
##                                                                            ##
## Manuscript: Example scripts of the modeling approach for the Acacia flight ##
##                                                                            ##
## description: MaxEnt classification storing of bootstrap distributions      ## 
##                                                                            ##
################################################################################

# set java number of cores to use
options(java.parameters = "-Xmx6g" )

# load require libraries
require(maptools)
require(raster)
require(rJava)
require(sp)
require(dismo)
require(rgdal)
require(rgeos)
require(doParallel)

# environment
setwd("E:/Somedirection")

#################################################
### Functions

loadAll <- function(prefix){
  ## load require shapefiles and rasters
  # load training areas (polygons) 
  studyarea = readOGR(paste0("1_reference/", prefix, "_AOI.shp"))
  studyarea = gBuffer(studyarea, byid=TRUE, width=0)
  treeallpoly = readOGR(paste0("1_reference/", prefix, "_canopy.shp"))
  treeallpoly = gBuffer(treeallpoly, byid=TRUE, width=0)
  shadows = readOGR(paste0("1_reference/", prefix, "_shadows.shp"))

  # load raster data 
  texture = stack(paste0("raw/", prefix, "/texture.tif"))
  hyper = stack(paste0("raw/", prefix, "/hyperspectral.tif"))
  rgb = stack(paste0("raw/", prefix, "/rgb.tif"))[[1:3]]
  structure = stack(paste0("raw/", prefix, "/structure.tif"))
  #stack(hyper, structure)

  # predict raster
  texture_pred = stack(paste0("raw/", prefix, "/texture_clip.tif"))
  hyper_pred = stack(paste0("raw/", prefix, "/hyperspectral_clip.tif"))
  rgb_pred = stack(paste0("raw/", prefix, "/rgb_clip.tif"))[[1:3]]
  structure_pred = stack(paste0("raw/", prefix, "/structure_clip.tif"))
  texture_pred <- resample(texture_pred, rgb_pred)
 
 }
 
runMaxent <- function(raster, pred_raster, outname){ 
  # Run maxent classifier with and without resuction of variance in the presence data
  # Evaluations are carried using all, only sunny and only shadowed presence data
  # Model results are saved as RData in a "Results" folder, while raster predictions are
  # returned as function outputs
  
  names(pred_raster) = names(raster)
  # unique loop name
  
  ### fit overall model
  fit <- maxent(x=raster, p = train_pos, a = train_back, removeDuplicates=F)
  # eveluate results
  eval <- evaluate(p=test_pos, a=test_back, model = fit, x=raster)
  eval_sunny <- evaluate(p=test_pos[-which(over(test_pos, shadows) == 0)], a=test_back, model = fit, x=raster)
  eval_shadow <- evaluate(p=test_pos[which(over(test_pos, shadows) == 0)], a=test_back, model = fit, x=raster)
  save(fit, file = paste0("results/models/all/", outname, "_", i, ".RData")) 
  save(eval, file = paste0("results/eval/all/", outname, "_", i, ".RData")) 
  save(eval_sunny, file = paste0("results/eval/all/", outname, "_sunny_", i, ".RData"))
  save(eval_shadow, file = paste0("results/eval/all/", outname, "_shadows_", i, ".RData"))
  beginCluster(6)
  pred <- clusterR(pred_raster, raster::predict, args = list(model = fit))
  endCluster()
  
  ### fit sunny model
  # select only sunny possitive points
  fit2 <- maxent(x=raster, p = train_pos[-which(over(train_pos, shadows) == 0)], a = train_back, removeDuplicates=F)
  # eveluate results
  eval2 <- evaluate(p=test_pos, a=test_back, model = fit2, x=raster)
  eval_sunny2 <- evaluate(p=test_pos[-which(over(test_pos, shadows) == 0)], a=test_back, model = fit2, x=raster)
  eval_shadow2 <- evaluate(p=test_pos[which(over(test_pos, shadows) == 0)], a=test_back, model = fit2, x=raster)
  save(fit2, file = paste0("results/models/sunny/", outname, "_", i, ".RData"))
  save(eval2, file = paste0("results/eval/sunny/", outname, "_", i, ".RData"))
  save(eval_sunny2, file = paste0("results/eval/sunny/", outname, "_sunny_", i, ".RData"))
  save(eval_shadow2, file = paste0("results/eval/sunny/", outname, "_shadows_", i, ".RData"))
  beginCluster(6)
  pred2 <- clusterR(pred_raster, raster::predict, args = list(model = fit2))
  endCluster()
  
  out <- list(pred, pred2)
  out
}
 
#############################################
### Modeling

##############
### Acacia ###
##############

prefix = "acacia_f1"
# load require data
loadAll(prefix)

# Create possitive and background random points
background = spsample(studyarea, 2000, type="random")
positive = spsample(treeallpoly, 500, type="random")

# variable selection
# run models using all variables
# RGB
rgb_model <- maxent(x=rgb, p = positive, a = background, removeDuplicates=F)
rgb_model@results[ grep("permutation", row.names(as.data.frame(rgb_model@results))), ]
cor(extract(rgb, positive))
rgb_best = c(2,3)
# hyperspectral
hyper_model <-  maxent(x=hyper, p = positive, a = background, removeDuplicates=F)
imp = hyper_model@results[ grep("permutation", row.names(as.data.frame(hyper_model@results))), ]
imp[which(imp > 2)]
cor(extract(hyper[[c(1,2,10,11,24,31)]], positive))
hyper_best = c(1,24)
hyper_model2 <-  maxent(x=hyper[[hyper_best]], p = positive, a = background, removeDuplicates=F)
# texture
text_model <-  maxent(x=texture, p = positive, a = background, removeDuplicates=F)
imp = text_model@results[ grep("permutation", row.names(as.data.frame(text_model@results))), ]
imp[which(imp > 3)]
cor(extract(texture[[c(30,36,37,38,39)]], positive))
text_best = c(30,36,37,38,39)
textr_model2 <-  maxent(x=texture[[text_best]], p = positive, a = background, removeDuplicates=F)
# structure
structure_model <- maxent(x=structure, p = positive, a = background, removeDuplicates=F)
imp = structure_model@results[ grep("permutation", row.names(as.data.frame(structure_model@results))), ]
imp[which(imp > 3)]
cor(extract(structure[[c(1,4,6,7)]], positive))
struct_best = c(1,4,6,7)
struct_model2 <-  maxent(x=structure[[struct_best]], p = positive, a = background, removeDuplicates=F)

save.image("acacia.RData")

# Fit models
registerDoParallel(6)

# prepare storing lists for predicted rasters
pred <- list()
pred2 <- list()

# Bootstrapping
for (i in 1:100){
  print(i)
  # create random training points and split them according to the tree/nontree areas/polygons 
  idx_pos = sample(500, 500, replace = T)
  idx_back = sample(2000, 2000, replace = T)
  # sample points
  train_pos <- positive[idx_pos]
  train_back <- background[idx_back]
  test_pos <- positive[-idx_pos]
  test_back <- background[-idx_back]
  
  # loop throught datasets sharing the same training/validation samples
  for (j in 1:length(raster_list)){ 
    # unique loop name
    outname = paste0(names(raster_list[j]), "_", prefix)
    pred_rgb <- runMaxent(raster_list[[j]], raster_pred[[j]], outname)
    pred[[i]] <- pred_rgb[[1]]
    pred2[[i]] <- pred_rgb[[2]]
    pred_out <- list(pred, pred2)
    save(pred_out, file = paste0("results/preds/", outname, ".RData"))
    print(paste0("Done ", outname, "!!!"))
  }
}

stopImplicitCluster()
