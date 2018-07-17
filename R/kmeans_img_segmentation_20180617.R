musgo <- lr

 # lr is a raster brick, we need to get an array of the raster values

# v <- getValues(musgo)
musgo <- array(getValues(musgo), dim=dim(lr))
dim(musgo)

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
  red <-  matrix(musgo[,,1], ncol=1),
  green <-  matrix(musgo[,,2], ncol=1),
  blue <-  matrix(musgo[,,3], ncol=1)
)
### compute the k-means clustering
K <-  kmeans(df,2)
df$label <-  K$cluster

# ### Replace the color of each pixel in the image with the mean
# ### R,G, and B values of the cluster in which the pixel resides
#  dimnames(K$centers) = list( c(1, 2),        # row names 
#                              c("red", "green", "blue")) # column names 

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
df$order = 1:nrow(df)
df = merge(df, colors)
# IMPORTANT: we must maintain the original order of the df after the merge!
df = df[order(df$order),]
df$order = NULL

#Finally, we have to reshape our data frame back into an image:

# # get mean color channel values for each row of the df.
# R = matrix(df$R, nrow=dim(musgo)[1])
# G = matrix(df$G, nrow=dim(musgo)[1])
# B = matrix(df$B, nrow=dim(musgo)[1])

# #solo necesitamos la columnna "label"
musgo.segmented <- t(matrix(df$label, nrow = dim(musgo)[1], ncol = dim(musgo)[2]))
# reconstitute the segmented image in the same shape as the input image
# musgo.segmented = array(dim=dim(musgo))
# musgo.segmented[,,1] = R
# musgo.segmented[,,2] = G
# musgo.segmented[,,3] = B

musgo.segmented.raster <- raster(musgo.segmented)



plot(musgo.segmented.raster)
freq(musgo.segmented.raster)



# vis.red <- raster(musgo.segmented[,,1])
# vis.green <- raster(musgo.segmented[,,2])
# vis.blue <- raster(musgo.segmented[,,3])

# musgo.raster <- brick(vis.red, vis.green, vis.blue)
# plot(musgo.raster)
# View the result
# grid.raster(musgo.segmented)
# grid.raster(musgo)

# # install.packages("rgl")
# library("rgl")
# # color space plot of musgo
# open3d()
# 
# plot3d(df$red, df$green, df$blue,
#        col=rgb(df$red, df$green, df$blue),
#        xlab="R", ylab="G", zlab="B",
#        size=3, box=FALSE, axes=TRUE)
# play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# 
# # color space plot of segmented musgo
# # color space plot of segmented musgo
# open3d()
# plot3d(df$red, df$green, df$blue,
#        col=rgb(df$R, df$G, df$B),
#        xlab="R", ylab="G", zlab="B",
#        size=3, box=FALSE)
# # movie3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# 
# # Use
# # movie3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )
# # instead of play3d to generate GIFs (requires imagemagick).
# 
# install.packages("magick")
# 
# # To visualize color space in two dimensions, we can use principle
# # components analysis. Principle components transforms the original
# # RGB coordinate system into a new coordinate system UVW. In this system,
# # the U coordinate captures as much of the variance in the original data
# # as possible and the V coordinate captures as much of the variance as
# # possible after factoring out U. So after performing PCA, most of the
# # variation in the data should be visible by plotting in the UV plane.
# # Here is the color space projection for the musgo:
# 
# require("ggplot2")
# 
# # perform PCA on the mandril data and add the uv coordinates to the dataframe
# PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
# df$u = PCA$x[,1]
# df$v = PCA$x[,2]
# 
# # Inspect the PCA
# # most of the cumulative proportion of variance in PC2 should be close to 1.
# summary(PCA)
# 
# #Importance of components:
# #                          PC1    PC2     PC3
# #Standard deviation     1.3903 0.9536 0.39695
# #Proportion of Variance 0.6443 0.3031 0.05252
# #Cumulative Proportion  0.6443 0.9475 1.00000
# 
# # musgo
# ggplot(df, aes(x=u, y=v, col=rgb(red,green,blue))) +
#   geom_point(size=2) + scale_color_identity()
# 
# # segmented musgo
# ggplot(df, aes(x=u, y=v, col=rgb(R,G,B))) +
#   geom_point(size=2) + scale_color_identity()
# 
