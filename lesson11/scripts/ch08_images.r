# Load libraries
library(jpeg)
library(abind)
options(width=70)

OUTDIR = "../img/ch08/"
dir.create(OUTDIR,FALSE,TRUE)

#####################
# Simple manipulation examples
vanGogh = readJPEG("../data/ch08/vanGogh_selfPortrait.jpg")

dim(vanGogh)
range(vanGogh)

vanGoghCrop = vanGogh[100:400,100:400,]
writeJPEG(vanGoghCrop,paste0(OUTDIR,"vanGoghCrop.jpg"))

vanGoghRotate = aperm(vanGogh,c(2,1,3))
vanGoghRotate = vanGoghRotate[dim(vanGoghRotate)[1]:1,,]
writeJPEG(vanGoghRotate,paste0(OUTDIR,"vanGoghRotate.jpg"))

vanGoghRed = vanGogh
vanGoghRed[,,2] = 0
vanGoghRed[,,3] = 0

vanGoghGreen = vanGogh
vanGoghGreen[,,1] = 0
vanGoghGreen[,,3] = 0

vanGoghBlue = vanGogh
vanGoghBlue[,,1] = 0
vanGoghBlue[,,2] = 0

vanGoghBW = vanGogh
vanGoghBW[,,1] = (vanGogh[,,1] + vanGogh[,,2] + vanGogh[,,3]) / 3
vanGoghBW[,,2] = (vanGogh[,,1] + vanGogh[,,2] + vanGogh[,,3]) / 3
vanGoghBW[,,3] = (vanGogh[,,1] + vanGogh[,,2] + vanGogh[,,3]) / 3

vanGoghAll = abind(vanGoghRed,vanGoghGreen,vanGoghBlue,vanGoghBW,along=2)
writeJPEG(vanGoghAll,paste0(OUTDIR,"vanGoghAll.jpg"))

#####################
# Indoor/Outdoor Corpus
files = dir("../data/ch08/columbiaImages", full.names=TRUE)
meta = read.csv("../data/ch08/photoMetaData.csv", as.is=TRUE)

files = files[meta$category %in% c("outdoor-night","outdoor-day")]
meta = meta[meta$category %in% c("outdoor-night","outdoor-day"),]

pchSymb = rep(19,length(files))
pchSymb[meta$category == "outdoor-day"] = 3
colType = rep(rgb(1,0,0,0.2),length(files))
colType[meta$category == "outdoor-day"] = rgb(0,0,1,0.2)

output = matrix(0,nrow=length(files),ncol=3)
for (j in 1:length(files)) {
  z = readJPEG(files[j])
  output[j,1] = median(z[,,1])
  output[j,2] = median(z[,,2])
  output[j,3] = median(z[,,3])
}

round(cor(output),3)

pdf(paste0(OUTDIR, "rgbScatter.pdf"), 8, 4.5)
par(mfrow=c(1,2))
plot(output[,1],output[,2],pch=pchSymb,cex=0.7,col=colType,xlab="red",ylab="green")
plot(output[,1],output[,3],pch=pchSymb,cex=0.7,col=colType,xlab="red",ylab="blue")
dev.off()

vanGoghHSV = rgb2hsv(r=as.numeric(vanGogh[,,1]),
                     g=as.numeric(vanGogh[,,2]),
                     b=as.numeric(vanGogh[,,3]),
                     maxColorValue=1)

temp = vanGoghHSV
temp[2,] = 1
temp[3,] = 1
vanGoghRGBmat = col2rgb(
                hsv(h=temp[1,],
                    s=temp[2,],
                    v=temp[3,])) / 256
vanGoghHue = array(t(vanGoghRGBmat),dim=dim(vanGogh))

temp = vanGoghHSV
temp[1,] = temp[2,]
temp[3,] = temp[2,]
vanGoghSat = array(t(temp),dim=dim(vanGogh))

temp = vanGoghHSV
temp[1,] = temp[3,]
temp[2,] = temp[3,]
vanGoghVal = array(t(temp),dim=dim(vanGogh))

vanGoghAllHSV = abind(vanGoghHue,vanGoghSat,vanGoghVal,along=2)
writeJPEG(vanGoghAllHSV,paste0(OUTDIR,"vanGoghAllHSV.jpg"))

#

output = matrix(0,nrow=length(files),ncol=3)
for (j in 1:length(files)) {
  z = readJPEG(files[j])
  mat = rgb2hsv(as.numeric(z[,,1]),as.numeric(z[,,2]),
                as.numeric(z[,,3]),maxColorValue=1)

  output[j,1] = atan2(mean(sin(mat[1,]*2*pi)), mean(cos(mat[1,]*2*pi))) / (2*pi)
  output[j,2] = median(mat[2,])
  output[j,3] = median(mat[3,])
}

output[,1] = output[,1] %% 1

round(cor(output),3)

pdf(paste0(OUTDIR, "hsvScatter.pdf"), 8, 4.5)
par(mfrow=c(1,2))
plot(output[,1],output[,2],pch=pchSymb,cex=0.7,col=colType,xlab="hue",ylab="saturation",axes=FALSE)
box()
axis(2)
axis(1,at=rgb2hsv(col2rgb(c("red","orange","yellow","green","cyan","blue","violet","purple")))[1,],
       label=c("red","orange","yellow","green","cyan","blue","violet","purple"),las=2)
plot(output[,3],output[,2],pch=pchSymb,cex=0.7,col=colType,xlab="hue",ylab="value",axes=FALSE)
box()
axis(2)
axis(1,at=rgb2hsv(col2rgb(c("red","orange","yellow","green","cyan","blue","violet","purple")))[1,],
       label=c("red","orange","yellow","green","cyan","blue","violet","purple"),las=2)
dev.off()

#####################
# PCA
outputRGB = matrix(0,nrow=length(files),ncol=3)
for (j in 1:length(files)) {
  z = readJPEG(files[j])
  outputRGB[j,1] = median(z[,,1])
  outputRGB[j,2] = median(z[,,2])
  outputRGB[j,3] = median(z[,,3])
}

pc = prcomp(outputRGB,center=FALSE,scale.=FALSE)
pc

outputPC = predict(pc)

out2 = matrix(0,ncol=3,nrow=300)
out2[1:100,  1] = seq(0,1,length.out=100)
out2[101:200,2] = seq(0,1,length.out=100)
out2[201:300,3] = seq(0,1,length.out=100)
out2Scale = scale(out2,center=pc$center,scale=pc$scale)
out2PC = out2Scale %*% pc$rotation

round(pc$rotation[,1],2)

pdf(paste0(OUTDIR, "pcaScatter3.pdf"), 8, 4.5)
par(mfrow=c(1,2))
plot(outputPC[,1],outputPC[,2],pch=pchSymb,cex=0.7,col=colType,xlab="PC1",ylab="PC2",
    xlim=range(c(out2PC[,1],outputPC[,1])),
    ylim=range(c(out2PC[,2],outputPC[,2])))
lines(out2PC[1:100,  1],out2PC[1:100,  2])
lines(out2PC[101:200,1],out2PC[101:200,2])
lines(out2PC[201:300,1],out2PC[201:300,2])
text(out2PC[100,  1],out2PC[100,  2], "red", col="red")
text(out2PC[200,  1],out2PC[200,  2], "green", col="green")
text(out2PC[300,  1],out2PC[300,  2], "blue", col="blue")

plot(outputPC[,1],outputPC[,3],pch=pchSymb,cex=0.7,col=colType,xlab="PC1",ylab="PC3",
    xlim=range(c(out2PC[,1],outputPC[,1])),
    ylim=range(c(out2PC[,3],outputPC[,3])))
lines(out2PC[1:100,  1],out2PC[1:100,  3])
lines(out2PC[101:200,1],out2PC[101:200,3])
lines(out2PC[201:300,1],out2PC[201:300,3])
text(out2PC[100,  1],out2PC[100,  3], "red", col="red")
text(out2PC[200,  1],out2PC[200,  3], "green", col="green")
text(out2PC[300,  1],out2PC[300,  3], "blue", col="blue")
dev.off()

output9 = matrix(0,nrow=length(files),ncol=9)
for (j in 1:length(files)) {
    z = readJPEG(files[j])
    mat = rgb2hsv(as.numeric(z[,,1]),as.numeric(z[,,2]),
                  as.numeric(z[,,3]),maxColorValue=1)

    output9[j,1:3] = quantile(mat[1,],probs=c(0.25,0.5,0.75))
    output9[j,4:6] = quantile(mat[2,],probs=c(0.25,0.5,0.75))
    output9[j,7:9] = quantile(mat[3,],probs=c(0.25,0.5,0.75))
}

pc9 = prcomp(output9,center=TRUE,scale.=TRUE)
round(pc9$rotation,3)

outputPC9 = predict(pc9)

pdf(paste0(OUTDIR, "pcaScatter9.pdf"), 8, 4.5)
par(mfrow=c(1,2))
plot(outputPC9[,1],outputPC9[,2],pch=pchSymb,cex=0.7,col=colType,
      xlab="PC1",ylab="PC2", xlim=range(outputPC9[,1]))
plot(outputPC9[,1],outputPC9[,6],pch=pchSymb,cex=0.7,col=colType,xlab="PC1",ylab="PC3")
dev.off()

pdf(paste0(OUTDIR, "pcaScatter9.pdf"), 8, 8)
ran = range(outputPC9)
par(mfrow=c(9,9))
par(mar=c(0,0,0,0))
for (i in 1:9) {
  for (j in 1:9) {
    if (i != j) {
      plot(outputPC9[,j],outputPC9[,i],pch=pchSymb,cex=0.7,col=colType,
        axes=FALSE, xlim=ran, ylim=ran)
    } else {
      plot(0,0,xlim=ran, ylim=ran, type="n", axes=FALSE)
      text(mean(ran),mean(ran),colnames(outputPC9)[j], cex=2)
    }

    box()
  }
}
dev.off()

#####################
# Kmeans
set.seed(1)
outputPC9 = predict(pc9)
cluster = kmeans(outputPC9,centers=6)
cluster$cluster

table(cluster$cluster,meta$category)

index = which(cluster$cluster == 4 & meta$category =="outdoor-day")

centers = predict(pc9, cluster$centers)

pdf(paste0(OUTDIR, "clusterScatter.pdf"), 5, 5)
plot(outputPC9[,1],outputPC9[,2],pch=pchSymb,cex=0.7,col=colType,xlab="PC1",ylab="PC2")
points(cluster$centers[,1],cluster$centers[,2], pch=19,cex=5,col=grey(0.2))
text(cluster$centers[,1],cluster$centers[,2], label=1:6, cex=2, col="white")
dev.off()

output = NULL
for (i in index) {
  z = readJPEG(files[i])
  if (dim(z)[1] > dim(z)[2]) {
    z = aperm(z,c(2,1,3))
    z = z[dim(z)[1]:1,,]
  }
  z = (z[1:480,1:722,1] + z[1:480,1:722,2] + z[1:480,1:722,3]) / 3
  output = abind(output,z,along=1)
}
writeJPEG(output,paste0(OUTDIR,"outlierOutdoorImgs.jpg"))

#####################
# Raster Scatter plot

x = outputPC9[,1]
x = (x - min(x)) / (max(x) - min(x))
y = outputPC9[,2]
y = (y - min(y)) / (max(y) - min(y))


jpeg(paste0(OUTDIR, "outdoorScatterRaster.jpg"), 1200, 800)
par(mar=c(0,0,0,0))
rho = 0.1
plot(0,0,type="n", xlim=c(0,1+rho), ylim=c(0,1+rho))

set.seed(1)
for (j in sample(1:length(files))) {
  z = readJPEG(files[j])

  rat = dim(z)[1] / dim(z)[2]
  delta_x = ifelse(rat > 1, 1, rat) * rho
  delta_y = ifelse(rat > 1, rat, 1) * rho
  rasterImage(z, xleft=x[j], ybottom=y[j], xright=x[j]+delta_x, ytop=y[j]+delta_y)
  rect(x[j],y[j],x[j]+delta_x,y[j]+delta_y,lwd=2)
}
dev.off()

