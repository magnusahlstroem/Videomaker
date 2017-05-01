atomizeImage <- function(path, width = 1, height = 1, filetype = "png") {
        
        atomizeImgMat2String <- function(x, width = width, height = height) {
                a_letter <- list(Letter = name.of.image, 
                                 RelPos_x = 0,
                                 RelPos_y = 0,
                                 Home = get(x)[,1:2],
                                 StartPos = get(x)[,1:2],
                                 Destination = get(x)[,1:2],
                                 Position =  get(x)[,1:2],
                                 atDest = rep(T, nrow(get(x)[,1:2])),
                                 HomeCol = t(col2rgb(get(x)[,3])/256),
                                 StartCol = t(col2rgb(get(x)[,3])/256),
                                 PositionCol = t(col2rgb(get(x)[,3])/256),
                                 DestinationCol = t(col2rgb(get(x)[,3])/256),
                                 MoveDuration = rep(0, nrow(get(x)[,1:2])),
                                 Suspend = rep(0, nrow(get(x)[,1:2])), 
                                 MovementVectorName = "c(0,0)")
                
                class(a_letter) <- append(class(a_letter), "a_letter")
                a_string <- list(a_letter)
                class(a_string) <- append(class(a_string), "a_string")
                a_string
        }
        name.of.image <- gsub(".*/", "", path)
        
        img <- readPNG(path)
        img.r <- as.raster(img,interpolate=F)
        
        rows <- nrow(img.r)
        cols <- ncol(img.r)
        
        colInColumns <- lapply(1:ncol(img.r), function(t) {
                list(x = rep(t, length(img.r[,t])), y = seq_along(img.r[,t]), col = img.r[,t])
        })
        columnsToMatrix <- lapply(colInColumns, function(x) cbind(x$x, x$y, x$col))
        
        combinedMatrices <- data.frame(do.call(rbind, columnsToMatrix), stringsAsFactors = F) %>%
                mutate_each(funs(as.numeric), X1, X2) %>%
                filter(X3 != "#FFFFFF") %>%
                mutate(X1 = (X1/cols - 0.5) * width,
                       X2 = (X2/-rows + 0.5) * height)
        
        out <- atomizeImgMat2String("combinedMatrices")
        out
}