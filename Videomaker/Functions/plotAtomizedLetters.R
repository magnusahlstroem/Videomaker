atomizeString <- function(string) {
        n <- 1:nchar(string)
        a_string <- sapply(n, function(x) substr(string,x,x))
        n_start <- which(a_string == "[")
        n_stop <- which(a_string == "]")
        for (i in seq_along(n_start)) {
                a_string[n_start[i]:n_stop[i]] <- do.call(paste0, as.list(a_string[(n_start[i] + 1):(n_stop[i] - 1)]))
        }
        minus.rem <- c(0, (n_stop - n_start))
        for (i in seq_along(n_start)) {
                a_string <- a_string[-c((n_start[i] + 1 - sum(minus.rem[1:i])):(n_stop[i]-sum(minus.rem[1:i])))]
        }
        
        a_string <- lapply(a_string, atomizeLetter)
        class(a_string) <- append(class(a_string), "a_string")
        a_string <- updateRelPos(a_string)
        a_string
}
        



addPointsFrame <- function(p1, p2) {
        if(p1[3] == "cir") {
                newPoints <- circlePoints(
                        p1 = as.numeric(c(p1[1],p1[2])), 
                        p2 = as.numeric(c(p2[1], p2[2])), 
                        p0 = as.numeric(c(p1[4],p1[5])))
        } else {
        x1 = as.numeric(p1[1])
        y1 = as.numeric(p1[2])
        x2 = as.numeric(p2[1])
        y2 = as.numeric(p2[2])
        if(x1 == x2) {
                diffy <- y2 - y1
                a <- NA
                b <- NA
                N_new <- 1:abs(diffy/0.01)
                new_Ys <- min(c(y1,y2)) + 0.01*c(0, N_new)
                new_Xs <- rep(x1, max(N_new))
                newPoints <- lapply(N_new, function(x) c(new_Xs[x], new_Ys[x]))
        } else {
                if(y1 == y2) {
                        diffx <- x2 - x1
                        a <- NA
                        b <- NA
                        N_new <- 1:abs(diffx/0.01)
                        new_Xs <- min(c(x1,x2)) + 0.01*c(0, N_new)
                        new_Ys <- rep(y1, max(N_new))
                        newPoints <- lapply(N_new, function(x) c(new_Xs[x], new_Ys[x]))
                } else {
                        diffy <- y2 - y1
                        diffx <- x2 - x1
                        N_new <- 1:abs(diffx/0.01)
                        a <- diffy/diffx
                        b <- y2 - a * x2
                        new_Xs <- min(c(x1,x2)) + 0.01*c(0, N_new)
                        new_Ys <- round((a*new_Xs + b)*100,0)/100
                        newPoints <- lapply(N_new, function(x) c(new_Xs[x], new_Ys[x]))
                }
        }
        }
        do.call(rbind, newPoints)
}

drawFrame <- function(pointFrame) {
        N_points <- 1:length(pointFrame)
        pair_vector <- lapply(N_points, function(x) c(x, x+1))[-max(N_points)]
        pointPairs <- lapply(pair_vector, function(x) list(pointFrame[[x[1]]], pointFrame[[x[2]]]))
        points_mat <- do.call(rbind,lapply(pointPairs, function(x) addPointsFrame(x[[1]], x[[2]])))
        points_mat <- points_mat[order(points_mat[,1]),]
        round(points_mat, 2)
}

fillFrame <- function(frame) {
        x <- frame[,1]
        y <- frame[,2]
        unique_Xs <- unique(x)
        ylimits <- lapply(unique_Xs, function(x) c(min(frame[frame[,1] == x,2]), max(frame[frame[,1] == x,2])))
        new_Ys <- lapply(ylimits, function(y) {
                diffy <- y[2] - y[1]
                N_new <- 1:abs(diffy/0.01)
                new_Ys <- min(c(y[2],y[1])) + 0.01*c(0, N_new)
                new_Ys
        }
        )
        l_unique_Xs <- 1:length(unique_Xs)
        new_Xs <- lapply(l_unique_Xs, function(t) rep(unique_Xs[t], length(new_Ys[[t]])))
        newPoints <- matrix(c(unlist(new_Xs), unlist(new_Ys)), ncol = 2)
        newPoints
}

fillFrame2 <- function(pointFrame) {
        filledFrame <- lapply(lapply(pointFrame, drawFrame), fillFrame)
        filledFrame <- do.call(rbind, filledFrame)
        filledFrame <- filledFrame[!duplicated(filledFrame, MARGIN = c(0,1)),]
        filledFrame
}
