# library(photomoss)
# setwd("wd")
# ################################################################################
# 
# 
# # seleccionamos los colores en la carta de color para hacer la calibración
# tif.path <- "path"
# chart <- chart.from.tif(tif.path)
# 
# ################################################################################
# 
# # Importamos el área de interes que creamos con Image J
# # y lo convertimos en un objeto del tipo SpatialPolygons
# # roi.path <- paste0(tif.path, "/roi")
# # roi.paths <- Sys.glob(paste0(roi.path, "*.roi"))[1:4]
# roi.paths<- list.files(path = "./roi",pattern=".roi$",full.names = T)[1:4]
# polys <- lapply(roi.paths, roi2polygon, tif.path)
# 
# # # # solo para comprobar, ploteamos:
# # lapply(polys, plot, add=T, col="red")
# # names(polys) <- substring(gsub(".roi", "", roi.paths), 10)
# # centroids <- lapply(polys, function(poly){rgeos::gCentroid(poly)})
# # mapply(text, centroids, gsub("o/", "", names(polys)), cex=0.55)
# 
# # seleccionamos los pixeles de interés que caen en nuestro polygono de interés
# obs.areas <- lapply(polys, function(x){extractPIX.from.Poly(tif.path, x)})
# names(obs.areas) <- paste0("roi", c(1:length(obs.areas)))
# # lapply(obs.dfs, head)
# 
# ################################################################################
# 
# # change names
# change.labels.order("pruebas", "raw.names.csv")
# 
# ################################################################################
# 
# 
# # FOUR obs.areas per picture
# # ccSpectral.multiareas(tif.path, chart = chart, obs.areas = obs.dfs, ml = F, pdf=T)
# 
# ccSpectral.multiareas(tif.path, chart = chart, obs.areas = obs.areas, ml = F, pdf=T)
