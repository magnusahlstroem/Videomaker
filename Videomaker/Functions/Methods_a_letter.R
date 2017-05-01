atomizeLetter <- function(Letter,...) {
        if(Letter %in% c("space", "newline")) {
                a_letter <- list(Letter = Letter, 
                                 RelPos_x = 0,
                                 RelPos_y = 0,
                                 StartPos = NULL,
                                 Destination = NULL,
                                 Position = NULL,
                                 Home = NULL,
                                 atDest = NULL,
                                 HomeCol = NULL,
                                 StartCol = NULL,
                                 PositionCol = NULL,
                                 DestinationCol = NULL,
                                 MoveDuration = NULL,
                                 MoveSpeed = NULL,
                                 Suspend = NULL, 
                                 MovementVectorName = "c(0,0)")
        } else {
                cornerdots <- cornerDots(Letter)
                filledFrame <- fillFrame2(cornerdots)
                a_letter <- list(Letter = Letter, 
                                 RelPos_x = 0,
                                 RelPos_y = 0,
                                 Home = filledFrame,
                                 StartPos = filledFrame,
                                 Destination = filledFrame,
                                 Position = filledFrame,
                                 atDest = rep(T, nrow(filledFrame)),
                                 HomeCol = t(col2rgb(rep("#000000", nrow(filledFrame)))),
                                 StartCol = t(col2rgb(rep("#000000", nrow(filledFrame)))),
                                 PositionCol = t(col2rgb(rep("#000000", nrow(filledFrame)))),
                                 DestinationCol = t(col2rgb(rep("#000000", nrow(filledFrame)))),
                                 MoveDuration = rep(0, nrow(filledFrame)),
                                 Suspend = rep(0, nrow(filledFrame)), 
                                 MovementVectorName = "c(0,0)")    
        }
        class(a_letter) <- append(class(a_letter), "a_letter")
        a_letter
}

plot.a_letter <- function(a_letter, pch = 46, xlim = c(-1,1), ylim = c(-0.5,1.5),...) {
        plot(a_letter[["Position"]], 
             axes = 0, 
             pch = pch, 
             xlim = eval(xlim), 
             ylim = ylim,
             xlab = "",
             ylab = "",...)
}

updateDist <- function(x) {
        UseMethod("updateDist", x)
}

updateDist.a_letter <- function(a_letter) {
        a_letter[["Dist"]] <- a_letter[["Destination"]] - a_letter[["Position"]]
        class(a_letter) <- append(class(a_letter), "a_letter")
        a_letter
}

print.a_letter <- function(a_letter,...) {
        print(a_letter[["Letter"]],...)
        cat("class: a_letter", "\n", "Assigned movement vector: ", a_letter[["MovementVectorName"]], sep = "")
}

atDestination <- function(a_letter) {
        UseMethod("atDestination", a_letter)
}

atDestination.a_letter <- function(a_letter) {
        sum(!a_letter[["atDest"]]) == 0
}

moveVectorAssign <- function(x, fun,...) {
        UseMethod("moveVectorAssign", x)
}

moveVectorAssign.a_letter <- function(a_letter, fun = "rnorm", sd = sd,...) {
        n <- nrow(a_letter[["Position"]])
        a_letter[["MovementVectorName"]] <- fun
        a_letter[["MovementVector"]][,1] <- rnorm(n = n, sd = sd,...)
        a_letter[["MovementVector"]][,2] <- rnorm(n = n, sd = sd,...)
        a_letter
}

move <- function(a_letter) {
        UseMethod("move", a_letter)
}

move.a_letter <- function(a_letter) {
        a_letter[["Position"]] <- a_letter[["Position"]] + a_letter[["MovementVector"]]
        a_letter
}

morph <- function(x, new_letter,...) {
        UseMethod("morph", x)
}

morph.a_letter <- function(x, new_letter,...) {
        new.a_letter <- atomizeLetter(new_letter)
        n_new <- nrow(new.a_letter[["Position"]])
        n_old <- nrow(x[["Position"]])
        new.a_letter <- if(n_new > n_old) {
                times.old <- floor(n_new/n_old)
                full.number <- times.old * n_old
                n_extra <- nrow(new.a_letter[["Position"]]) %% full.number
                which.exstra.from.old <- sample(1:n_old, n_extra)
                new.a_letter[["Position"]][,1] <- c(rep(x[["Position"]][,1],times.old), 
                                                    x[["Position"]][which.exstra.from.old,1])
                new.a_letter[["Position"]][,2] <- c(rep(x[["Position"]][,2],times.old), 
                                                    x[["Position"]][which.exstra.from.old,2])
                new.a_letter[["Position"]] <- new.a_letter[["Position"]][order(sample(1:n_new, n_new)),]
                #class(new.a_letter) <- append(class(new.a_letter), "a_letter")
                new.a_letter
        } else { if(n_new < n_old) {
                times.new <- floor(n_old/n_new)
                full.number <- times.new * n_new
                n_extra <- n_old %% full.number
                which.exstra.from.new <- sample(1:n_new, n_extra)
                new.letter.temp <- x
                new.letter.temp[["Home"]] <- do.call(rbind, lapply(1:(times.new + 1), function(n) {
                        if(n < (times.new + 1)) {
                                new.letter.temp[["Home"]][((n-1)*n_new + 1):(n*n_new),] <- new.a_letter[["Home"]]
                        } else {
                                new.letter.temp[["Home"]][((n-1)*n_new + 1):((n-1)*n_new + n_extra),] <- new.a_letter[["Home"]][which.exstra.from.new,]
                        }
                }
                ))
                new.letter.temp[["Position"]] <- new.letter.temp[["Position"]][order(sample(1:n_old, n_old)),]
                new.letter.temp[["Letter"]] <- new_letter
                #class(new.letter.temp) <- append(class(new.letter.temp), "a_letter")
                new.letter.temp
        } else {
                new.a_letter[["Position"]] <- x[["Position"]]
                new.a_letter[["Position"]] <- new.a_letter[["Position"]][order(sample(1:n_new, n_new)),]
                #new.a_letter[["Letter"]] <- new_letter
                new.a_letter
        }
        }
        class(new.a_letter) <- append(class(new.a_letter), "a_letter")
        new.a_letter
}
