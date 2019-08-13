## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
require(jpeg)
require(RCurl)
#url <-"https://alumni.utdallas.edu/image/-wallpaper/BSB-Ext-1920.jpg"
url <-"https://www.utdallas.edu/~axa174032/R/datasets/Clustering/BSB-Ext-1920.jpg"
#url <- paste(url, "master/Blog/LloydsBuilding.jpg", sep="")
readImage <- readJPEG(getURLContent(url, binary=TRUE))
readImage
dm <- dim(readImage)
dm
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main="UTD",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")




## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 3
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 3 colours")

## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 5
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD Alumni",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours")
## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 7 
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD Alumni",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 7 colours")

## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 10 
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD Alumni",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 10 colours")

## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 20 
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD Alumni",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 20 colours")
## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 30 
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD Alumni",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 30 colors")
## ---- message= FALSE, echo=FALSE, eval=TRUE, message = FALSE, warning=FALSE----
kColors <- 25 
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="UTD Alumni",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 25 colors")




#PCA

ncol(readImage)
nrow(readImage)
img.r.pca <- prcomp(rgbImage$r.value, center = FALSE)
img.g.pca <- prcomp(rgbImage$g.value, center = FALSE)
img.b.pca <- prcomp(rgbImage$b.value, center = FALSE)
rgb.pca <- list(img.r.pca, img.g.pca, img.b.pca)
for (i in seq.int(3, round(nrow(readImage) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed/img_compressed_', round(i,0), '_components.jpg', sep = ''))
}



original <- dim / 1000
imgs <- dir('compressed/')

for (i in imgs) {
  full.path <- paste('compressed/', i, sep='')
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}