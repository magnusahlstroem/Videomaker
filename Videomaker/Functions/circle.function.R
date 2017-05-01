circlePoints <- function(p1, p2, p0) {
        p1 <- as.numeric(p1)
        p0 <- as.numeric(p0)
        x0 <- p0[1]
        y0 <- p0[2]
        x1 <- p1[1]
        y1 <- p1[2]
        x2 <- p2[1]
        y2 <- p2[2]
        #radius <- dist(p1 = p0, p2 = p2)
        #dist1 <- dist(p0, p1)
        #dist2 <- dist(p0, p2)
        
        #dist1 == dist2
        
        diffx <- x2 - x1 
        diffy <- y2 - y1 
        drawDirectionPos <-  x2 > x1
        N_new <- 0:(diffx/0.01)
        
        if(drawDirectionPos) {
                new_Xs <- min(c(x1,x2)) + 0.01*N_new
        } else {
                new_Xs <- max(c(x1,x2)) + 0.01*N_new
        }
        
        a <- 1
        b <- - 2 * y0
        c <- y0^2 + (new_Xs-x0)^2 - (x1 - x0)^2 - (y1 - y0)^2
        d <- round((b^2 - 4 * a * c), 4)
        
        if(drawDirectionPos) {
                if(x1 < x0) {
                        if(y2 < y0) { 
                                new_Ys <- round((-b - sqrt(d))/(2*a),2) 
                        } else {
                                new_Ys <- round((-b + sqrt(d))/(2*a),2)
                        }
                } else {
                        if(y1 < y0) {
                                new_Ys <- round((-b - sqrt(d))/(2*a),2)
                        } else {
                                new_Ys <- round((-b + sqrt(d))/(2*a),2)
                        }
                }
        } else {
                if(x1 > x0) {
                        if(y2 < y0) { 
                                new_Ys <- round((-b - sqrt(d))/(2*a),2) 
                        } else {
                                new_Ys <- round((-b + sqrt(d))/(2*a),2)
                        }
                } else {
                        if(y1 < y0) {
                                new_Ys <- round((-b - sqrt(d))/(2*a),2)
                        } else {
                                new_Ys <- round((-b + sqrt(d))/(2*a),2)
                        }
                }
        }
        
        newPoints <- mapply(FUN = function(x,y) c(x,y), x = new_Xs, y = new_Ys, SIMPLIFY = F)
        newPoints
}