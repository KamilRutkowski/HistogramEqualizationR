nonZeroMin <- function(v) {
    for (i in 1:length(v))
        if (v[i] > 0) {
            return(v[i])
        }
    return(1)
}

saveHistogram <- function(v, path, title) {
    jpeg(path)
    xvec <- integer(256)
    for (i in 2:256)
        xvec[i] <- xvec[i-1] + 1
    plot(xvec, v, "h", main = title)
    dev.off()
}

colorTable = c("Red", "Green", "Blue")

libpath <- paste(.libPaths()[1], '/libs/', sep = "")
#install.packages("jpeg", libpath) ## if necessary
.libPaths(c(.libPaths(), libpath))
library("jpeg")
im <- readJPEG("G:/Studia/Wyrownanie_histogramu/Wyrownanie_histogramu/img/nied1wyj.jpg")
dimensions <- dim(im)
yDim <- dimensions[1]
xDim <- dimensions[2]
for (color in 1:3) {
    colorVec <- integer(256)
    for (y in 1:yDim) {
        for (x in 1:xDim) {
            value <- round(im[y,x,color] * 255) + 1
            colorVec[value] <- colorVec[value] + 1
        }
    }
    #Draw histogram for input pixel values
    saveHistogram(colorVec, paste("G:/Studia/Wyrownanie_histogramu/Wyrownanie_histogramu/img/histogramIn", colorTable[color], ".jpeg", sep = ""), paste("In", colorTable[color], sep = ""))

    #Calculate distribution
    Dist <- integer(256)
    Dist[1] <- colorVec[1]
    for (i in 2:256)
        Dist[i] <- colorVec[i] + Dist[i - 1]
    minDist <- nonZeroMin(Dist)

    #calculating new values
    for (y in 1:yDim) {
        for (x in 1:xDim) {
            tmp <- (Dist[round(im[y, x, color] * 255) + 1] - minDist) / ((yDim * xDim) - minDist)
            im[y, x, color] <- tmp
        }
    }
    
    #Draw histogram for new pixel values
    colorVec <- integer(256)
    for (y in 1:yDim) {
        for (x in 1:xDim) {
            value <- round(im[y, x, color] * 255) + 1
            colorVec[value] <- colorVec[value] + 1
        }
    }
    saveHistogram(colorVec, paste("G:/Studia/Wyrownanie_histogramu/Wyrownanie_histogramu/img/histogramOut", colorTable[color], ".jpeg", sep = ""), paste("Out", colorTable[color], sep = ""))
}
writeJPEG(im, "G:/Studia/Wyrownanie_histogramu/Wyrownanie_histogramu/img/nied1wyjwyj.jpg", 1)


