melt <- function(a_letter, duration, destination, wait,...) {
        UseMethod("melt", a_letter)
}

melt.a_string <- function(a_string, 
                          Suspend = list(abs(round(runif(nrow(a_string[[1]][["Position"]]), min = 1, max = 20) + rnorm(nrow(a_string[[1]][["Position"]]), mean = 4, sd = 2))),
                                         abs(round(runif(nrow(a_string[[2]][["Position"]]), min = 1, max = 20) + rnorm(nrow(a_string[[2]][["Position"]]), mean = 4, sd = 2))),
                                         abs(round(runif(nrow(a_string[[3]][["Position"]]), min = 1, max = 20) + rnorm(nrow(a_string[[3]][["Position"]]), mean = 4, sd = 2))),
                                         abs(round(runif(nrow(a_string[[4]][["Position"]]), min = 1, max = 20) + rnorm(nrow(a_string[[4]][["Position"]]), mean = 4, sd = 2))),
                                         abs(round(runif(nrow(a_string[[5]][["Position"]]), min = 1, max = 20) + rnorm(nrow(a_string[[5]][["Position"]]), mean = 4, sd = 2))))
                          ,...) {
        n <- sum(sapply(a_string, function(a_letter) nrow(a_letter[["Position"]])))
        a_string <- lapply(a_string, function(a_letter) {
                n_dur <- which(lapply(a_string, function(x) x[["RelPos_x"]]) == a_letter[["RelPos_x"]])
                a_letter[["Destination"]] <- matrix(c(a_letter[["Position"]][,1], 
                                                      sample(seq(-0.5,-0.45,0.001), 
                                                             nrow(a_letter[["Position"]]), 
                                                             replace = T)), 
                                                    ncol = 2)
                a_letter[["atDest"]] <- rep(F, nrow(a_letter[["Position"]]))
                a_letter[["Dist"]] <- a_letter[["Destination"]] - a_letter[["Position"]]
                a_letter[["MoveDuration"]] <- round(sqrt(rowSums((a_letter[["Dist"]])^2))/a_letter[["MoveSpeed"]])
                a_letter[["Suspend"]] <- Suspend[[n_dur]]
                #a_letter[["MovementVector"]] <- dist * a_letter[["MoveSpeed"]]
                class(a_letter) <- append(class(a_letter), "a_letter")
                a_letter
        })
        class(a_string) <- append(class(a_string), "a_string")
        a_string
        iteration <- 0
        
        while(!atDestination(a_string) & iteration < 50) 
        {
                a_string <- lapply(a_string, function(a_letter) {
                        one_to_n <- 1:nrow(a_letter[["Position"]])
                        a_letter[["atDest"]] <- sapply(one_to_n, function(i) {
                                if(a_letter[["Position"]][i,1] < a_letter[["Destination"]][i,1] + 0.00001 & 
                                   a_letter[["Position"]][i,1] > a_letter[["Destination"]][i,1] - 0.00001 &
                                   a_letter[["Position"]][i,2] < a_letter[["Destination"]][i,2] + 0.00001 &
                                   a_letter[["Position"]][i,2] > a_letter[["Destination"]][i,2] - 0.00001) { 
                                        a_letter[["atDest"]][i] <- T 
                                } else { 
                                        a_letter[["atDest"]][i] <- F
                                }
                        })
                        a_letter[["MovementVector"]] <- t(sapply(one_to_n, function(i) {
                                if(a_letter[["Suspend"]][i] < iteration) {
                                        if(!a_letter[["atDest"]][i]) {
                                                a_letter[["MovementVector"]][i,] <- a_letter[["Dist"]][i,] * (1/a_letter[["MoveDuration"]][i])
                                        } else {
                                                a_letter[["MovementVector"]][i,] <- c(0,0)
                                        }
                                } else {
                                        a_letter[["MovementVector"]][i,] <- c(0,0)
                                }
                        }))
                        class(a_letter) <- append(class(a_letter), "a_letter")
                        a_letter
                })
                class(a_string) <- append(class(a_string), "a_string")
                a_string <- move(a_string)
                dev.hold()
                plot(a_string)
                ani.pause()
                iteration <- iteration + 1
        }
        a_string
}