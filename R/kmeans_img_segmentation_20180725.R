alveolo <- lr

 # lr is a raster brick, we need to get an array of the raster values

# v <- getValues(musgo)
alveolo <- array(getValues(alveolo), dim=dim(lr))
dim(alveolo)



# musgo.R <- array(getValues(musgo[[1]]))
# musgo.G <- array(getValues(musgo[[2]]))
# musgo.B <- array(getValues(musgo[[3]]))

# musgo.R <- musgo[[1]]
# musgo.G <- musgo[[2]]
# musgo.B <- musgo[[3]]


# 
# # musgo.R <- array(getValues(musgo[[1]]))
# class(musgo.R)
# # class(musgo.G)
# dim(musgo.G)
# 
# moss <- rgb(musgo.R, musgo.G, musgo.B)
# class(moss)
# dim(moss) <- dim(musgo.R)

# # install.packages("grid")
# library("grid")
# # install.packages("gridExtra")
# library("gridExtra")
# 
# # build the image grid
# img1 = rasterGrob(musgo.R)
# img2 = rasterGrob(musgo.G)
# img3 = rasterGrob(musgo.B)
# grid.arrange(img1, img2, img3, nrow=1)


df <-  data.frame(
  red <-  matrix(alveolo[,,1], ncol=1),
  green <-  matrix(alveolo[,,2], ncol=1),
  blue <-  matrix(alveolo[,,3], ncol=1)

  )
colnames(df) <- c("red", "green", "blue")

### compute the k-means clustering
K <-  kmeans(df,2)
df$label <-  K$cluster
# df[df$label==2,] <- 0




# ### Replace the color of each pixel in the image with the mean
# ### R,G, and B values of the cluster in which the pixel resides
 # dimnames(K$centers) = list( c(1, 2),        # row names
                             # c("red", "green", "blue")) # column names

# los clores no son necesarios
# # get the coloring
# colors = data.frame(
#   label = 1:nrow(K$centers),
#   R = K$centers[,"red"],
#   G = K$centers[,"green"],
#   B = K$centers[,"blue"]
# )
# colors
# merge color codes on to df
# df$order = 1:nrow(df)
# df = merge(df, colors)
# IMPORTANT: we must maintain the original order of the df after the merge!
# df = df[order(df$order),]
# df$order = NULL

#Finally, we have to reshape our data frame back into an image:

# # get mean color channel values for each row of the df.
# R = matrix(df$R, nrow=dim(alveolo)[1])
# G = matrix(df$G, nrow=dim(alveolo)[1])
# B = matrix(df$B, nrow=dim(alveolo)[1])

# #solo necesitamos la columnna "label"

#con trasposición de la matriz

musgo_guata.segmented <- t(matrix(df$label, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))
 

#con trasposición de la matriz y asignación del valor 0 a la guata.

# musgo.segmented2 <- replace(t(matrix(df$label, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2])), musgo.segmented == 2, 0)

# reconstitute the segmented image in the same shape as the input image
# musgo.segmented = array(dim=dim(alveolo))
# musgo.segmented[,,1] = R
# musgo.segmented[,,2] = G
# musgo.segmented[,,3] = B
# 
# musgo.segmented = array(dim=dim(alveolo))
# musgo.segmented[,,1] = label
# musgo.segmented[,,2] = label
# musgo.segmented[,,3] = label


musgo_guata.segmented.raster <- raster(musgo_guata.segmented)
# musgo.segmented.raster2 <- raster(musgo.segmented2)


par(mfrow=c(1,1), mar=c(2,2,2,5))
plot(musgo_guata.segmented.raster, main="Segmentación kmeans Hartigan-Wong1")






# head(df)
# 
# range(df$green)
# 
# hist(df$green, main ="distrib_green all", xlim=c(range(df$green)), xlab = "")
# 
# 
# # by(df$green, label, hist)
# 
# df0 <- subset(df, df$label==1)
# 
# hist(subset(df, df$label==1)$green, main ="distrib_green", xlim=c(range(df$green)), xlab = "")
# 
# hist(df0$green, main ="distrib_green guata", xlim=c(range(df$green)), xlab = "")
# 
# df1 <- subset(df, df$label==2)
# 
# hist(df1$green, main ="distrib_green musgo", xlim=c(range(df$green)), xlab = "")
# 
# nrow(subset(df, df$label==1))

#función ¿cuántos valores distinos hay en un vector?
different.values.vector <- function(v){
  n.dif.values <- length(unique(v))
  return(n.dif.values)
  }
# different.values.vector(subset(df, df$label==1)$green)

# length(unique(subset(df, df$label==1)$green))
# length(unique(subset(df, df$label==2)$green))


#miramos las frecuencias de las intensidades de color para cada uno de los canales del RGB en los do grupos obtenidos a partir de kmeans

hist( subset(df, df$label==1)$green, col=rgb(1,0,1,1/4), breaks=different.values.vector(subset(df, df$label==1)$green), xlim=c(0,0.6), ylim=c(0, 20000), main ="distrib_green musgo y guata", xlab="color intensity")  # first histogram
hist( subset(df, df$label==2)$green, col=rgb(0,1,0,1/4), breaks=different.values.vector(subset(df, df$label==2)$green), xlim=c(0,0.6),ylim=c(0, 20000), add=T)  # second

hist( subset(df, df$label==1)$red, col=rgb(1,0,1,1/4), breaks=different.values.vector(subset(df, df$label==1)$red), xlim=c(0,0.6), ylim=c(0, 20000), main ="distrib_red musgo y guata", xlab="color intensity")  # first histogram
hist( subset(df, df$label==2)$red, col=rgb(1,0,0,1/4), breaks=different.values.vector(subset(df, df$label==2)$red), xlim=c(0,0.6),ylim=c(0, 20000), add=T)  # second

hist( subset(df, df$label==1)$blue, col=rgb(1,0,1,1/4), breaks=different.values.vector(subset(df, df$label==1)$blue), xlim=c(0,0.6), ylim=c(0, 20000), main ="distrib_blue musgo y guata", xlab="color intensity")  # first histogram
hist( subset(df, df$label==2)$blue, col=rgb(0,0,1,1/4), breaks=different.values.vector(subset(df, df$label==2)$blue), xlim=c(0,0.6),ylim=c(0, 20000), add=T)  # second

#reclasificamos con nueva etiqueta zonas de coincidencia
## a) valores de coincidencia en el canal verde entre ambos grupos
coincidence.green <- Reduce(intersect, list(subset(df, df$label==1)$green, subset(df, df$label==2)$green))
coincidence.red <- Reduce(intersect, list(subset(df, df$label==1)$red, subset(df, df$label==2)$red))
coincidence.blue <- Reduce(intersect, list(subset(df, df$label==1)$blue, subset(df, df$label==2)$blue))

## b)creamos nuevas columnas para poder etiquetar la nueva clase de celda de coincidencia entre Guata y musgo dentro de cada clasep para cada canal: R, G y B
df[,c("label.c.green")] <- df$label
df[,c("label.c.red")] <- df$label
df[,c("label.c.blue")] <- df$label

## c) posiciones de las celdas de concidencia entre los dos grupos para las intensidades de verde
c.green <- which(df$green %in% coincidence.green) 
c.red <- which(df$red %in% coincidence.red)
c.blue <- which(df$blue %in% coincidence.blue)

# d) localizadas las posiciones, reetiquetamos las celdas de coincidencia en el canal verde con un 0
df$label.c.green[c.green] <- 0
df$label.c.red[c.red] <- 0
df$label.c.blue[c.blue] <- 0

#ahora volvemos a rehacer nuestra imagen.
musgo_guata.segmented.c.green <- t(matrix(df$label.c.green, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))
musgo_guata.segmented.c.red   <- t(matrix(df$label.c.red, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))
musgo_guata.segmented.c.blue  <- t(matrix(df$label.c.blue, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))


musgo_guata.segmented.raster.c.green <- raster(musgo_guata.segmented.c.green)
musgo_guata.segmented.raster.c.red <- raster(musgo_guata.segmented.c.red)
musgo_guata.segmented.raster.c.blue <- raster(musgo_guata.segmented.c.blue)


plot(musgo_guata.segmented.raster.c.green, main="musgo_guata segmentado coincidencia verde")
plot(musgo_guata.segmented.raster.c.red, main="musgo_guata segmentado coincidencia rojo")
plot(musgo_guata.segmented.raster.c.blue, main="musgo_guata segmentado coincidencia azul")

plot(musgo_guata.segmented.raster, main="Segmentación kmeans Hartigan-Wong1")

# df[df$label==1,]
# df[df$label==2,]

# reclasificamos el área de guata
#creamos columna para las nuevas etiquetas
df[df$label==1, c("label.guata")] <- NA
K1 <-  kmeans(df[df$label==1,c("red", "green", "blue")],2)
df[df$label==1, c("label.guata")] <-  K1$cluster

guata.segmented1 <- t(matrix(df$label.guata, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))

guata.segmented1.raster <- raster(guata.segmented1)


plot(guata.segmented1.raster, main="Segmentación guata", colNA="black")

# reclasificamos el área de musgo
#creamos columna para las nuevas etiquetas

df[df$label==2, c("label.musgo")] <- NA
K2 <-  kmeans(df[df$label==2,c("red", "green", "blue")],2)
df[df$label==2, c("label.musgo")] <-  K2$cluster

musgo.segmented1 <- t(matrix(df$label.musgo, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))

musgo.segmented1.raster <- raster(musgo.segmented1)


plot(musgo.segmented1.raster, main="Segmentación musgo", colNA="black")



# different.values.vector(df$label.c)


# which(df$label %in% c(1)) # 2 3 5
# 
# which(df$label %in% c(2))
# 
# 
# dplyr::filter(df, green==coincidence)
# 
# df$label.c <- function(hours, pph=40){
#   df$label.c <- df$label
#   if(df$green <- coincidencehours > 100) {
#     net.price <- net.price * 0.9
#   }
#   round(net.price)
# }
# 
# 
# y[match(x, x.lvl)]
# 
# 
# # reclasificamos las zonas de guata
# K <-  kmeans(df0,2)
# df0$label0 <-  K$cluster
# 
# musgo.segmented0 <- t(matrix(df0$label0, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2]))
# 
# 
# #con trasposición de la matriz y asignación del valor 0 a la guata.
# 
# # musgo.segmented2 <- replace(t(matrix(df$label, nrow = dim(alveolo)[1], ncol = dim(alveolo)[2])), musgo.segmented == 2, 0)
# 
# # reconstitute the segmented image in the same shape as the input image
# # musgo.segmented = array(dim=dim(alveolo))
# # musgo.segmented[,,1] = R
# # musgo.segmented[,,2] = G
# # musgo.segmented[,,3] = B
# # 
# # musgo.segmented = array(dim=dim(alveolo))
# # musgo.segmented[,,1] = label
# # musgo.segmented[,,2] = label
# # musgo.segmented[,,3] = label
# 
# 
# musgo.segmented.raster1 <- raster(musgo.segmented1)
# 
# 
# 
# summary(df$green....matrix.musgo.....2...ncol...1.)
# 
# grid.raster(musgo)
# 
# grid.raster(musgo.segmented)
# 
# # plotRGB(lr, 
# #         scale=1,
# #         asp = nrow(sps)/ncol(sps)  
# #         )
# raster::plo
# freq(musgo.segmented.raster)
# 
# 
# 
# # vis.red <- raster(musgo.segmented[,,1])
# # vis.green <- raster(musgo.segmented[,,2])
# # vis.blue <- raster(musgo.segmented[,,3])
# 
# # musgo.raster <- brick(vis.red, vis.green, vis.blue)
# # plot(musgo.raster)
# # View the result
# # grid.raster(musgo.segmented)
# # grid.raster(musgo)
# 
# # # install.packages("rgl")
# # library("rgl")
# # # color space plot of musgo
# # open3d()
# # 
# # plot3d(df$red, df$green, df$blue,
# #        col=rgb(df$red, df$green, df$blue),
# #        xlab="R", ylab="G", zlab="B",
# #        size=3, box=FALSE, axes=TRUE)
# # play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# # 
# # # color space plot of segmented musgo
# # # color space plot of segmented musgo
# # open3d()
# # plot3d(df$red, df$green, df$blue,
# #        col=rgb(df$R, df$G, df$B),
# #        xlab="R", ylab="G", zlab="B",
# #        size=3, box=FALSE)
# # # movie3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# # play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# # 
# # # Use
# # # movie3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# # # instead of play3d to generate GIFs (requires imagemagick).
# # 
# # install.packages("magick")
# # 
# # # To visualize color space in two dimensions, we can use principle
# # # components analysis. Principle components transforms the original
# # # RGB coordinate system into a new coordinate system UVW. In this system,
# # # the U coordinate captures as much of the variance in the original data
# # # as possible and the V coordinate captures as much of the variance as
# # # possible after factoring out U. So after performing PCA, most of the
# # # variation in the data should be visible by plotting in the UV plane.
# # # Here is the color space projection for the musgo:
# # 
# # require("ggplot2")
# # 
# # # perform PCA on the mandril data and add the uv coordinates to the dataframe
# # PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
# # df$u = PCA$x[,1]
# # df$v = PCA$x[,2]
# # 
# # # Inspect the PCA
# # # most of the cumulative proportion of variance in PC2 should be close to 1.
# # summary(PCA)
# # 
# # #Importance of components:
# # #                          PC1    PC2     PC3
# # #Standard deviation     1.3903 0.9536 0.39695
# # #Proportion of Variance 0.6443 0.3031 0.05252
# # #Cumulative Proportion  0.6443 0.9475 1.00000
# # 
# # # musgo
# # ggplot(df, aes(x=u, y=v, col=rgb(red,green,blue))) +
# #   geom_point(size=2) + scale_color_identity()
# # 
# # # segmented musgo
# # ggplot(df, aes(x=u, y=v, col=rgb(R,G,B))) +
# #   geom_point(size=2) + scale_color_identity()
# # 
