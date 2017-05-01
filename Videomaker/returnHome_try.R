

returnHome.a_string <- function(a_string, 
                                Suspend = list(abs(round(rnorm(nrow(a_string[[1]][["Position"]]), mean = 4, sd = 0.05)*100)-380),
                                               abs(round(rnorm(nrow(a_string[[2]][["Position"]]), mean = 4, sd = 0.05)*100)-380),
                                               abs(round(rnorm(nrow(a_string[[3]][["Position"]]), mean = 4, sd = 0.05)*100)-380),
                                               abs(round(rnorm(nrow(a_string[[4]][["Position"]]), mean = 4, sd = 0.05)*100)-380),
                                               abs(round(rnorm(nrow(a_string[[5]][["Position"]]), mean = 4, sd = 0.05)*100)-380)),
                                duration = list(sample(2:8, nrow(a_string[[1]][["Position"]]), replace = T),
                                                sample(4:10, nrow(a_string[[2]][["Position"]]), replace = T),
                                                sample(6:12, nrow(a_string[[3]][["Position"]]), replace = T),
                                                sample(8:14, nrow(a_string[[4]][["Position"]]), replace = T),
                                                sample(10:16, nrow(a_string[[5]][["Position"]]), replace = T)),...) {
        n <- sum(sapply(a_string, function(a_letter) nrow(a_letter[["Position"]])))
        a_string <- lapply(a_string, function(a_letter) {
                n_dur <- which(lapply(a_string, function(x) x[["RelPos_x"]]) == a_letter[["RelPos_x"]])
                a_letter[["Destination"]] <- a_letter[["Home"]]
                a_letter[["atDest"]] <- rep(F, nrow(a_letter[["Position"]]))
                a_letter[["Dist"]] <- a_letter[["Destination"]] - a_letter[["Position"]]
                a_letter[["MoveSpeed"]] <- 1/duration[[n_dur]]
                a_letter[["Suspend"]] <- Suspend[[n_dur]]
                #a_letter[["MovementVector"]] <- dist * a_letter[["MoveSpeed"]]
                class(a_letter) <- append(class(a_letter), "a_letter")
                a_letter
        })
        class(a_string) <- append(class(a_string), "a_string")
        iteration <- 0
        
        while(!atDestination(a_string))
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
                                                a_letter[["MovementVector"]][i,] <- a_letter[["Dist"]][i,]*a_letter[["MoveSpeed"]][i]
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
                plot(a_string)
                iteration <- iteration + 1
        }
        a_string
}


HIMIN[[1]][["Position"]][1:10,]
HIMIN[[1]][["Destination"]][1:10,]


HIMIN <- atomizeString("HIMIN")
HIMIN <- moveVectorAssign(HIMIN, fun = "rnorm", sd = 0.2)
HIMIN <- move(HIMIN)
plot(HIMIN)
returnHome(HIMIN, duration = list(sample(2:8, nrow(HIMIN[[1]][["Position"]]), replace = T),
                                  sample(4:10, nrow(HIMIN[[2]][["Position"]]), replace = T),
                                  sample(6:12, nrow(HIMIN[[3]][["Position"]]), replace = T),
                                  sample(8:14, nrow(HIMIN[[4]][["Position"]]), replace = T),
                                  sample(10:16, nrow(HIMIN[[5]][["Position"]]), replace = T)))

melt(HIMIN, duration = list(abs(rnorm(nrow(HIMIN[[1]][["Position"]], mean = 4, sd = 0.01)*100),
                                  sample(4:10, nrow(HIMIN[[2]][["Position"]]), replace = T),
                                  sample(6:12, nrow(HIMIN[[3]][["Position"]]), replace = T),
                                  sample(8:14, nrow(HIMIN[[4]][["Position"]]), replace = T),
                                  sample(10:16, nrow(HIMIN[[5]][["Position"]]), replace = T))))




     
HIMIN <- lapply(HIMIN, function(a_letter) {
        one_to_n <- 1:nrow(a_letter[["Position"]])
        a_letter[["MovementVector"]] <- t(sapply(one_to_n, function(i) {
                if(a_letter[["Position"]][i,1] < a_letter[["Destination"]][i,1] + 0.00001 & 
                   a_letter[["Position"]][i,1] > a_letter[["Destination"]][i,1] - 0.00001 &
                   a_letter[["Position"]][i,2] < a_letter[["Destination"]][i,2] + 0.00001 &
                   a_letter[["Position"]][i,2] > a_letter[["Destination"]][i,2] - 0.00001) { 
                        a_letter[["atDest"]][i] <- T 
                } else { 
                        a_letter[["atDest"]][i] <- F
                }
                if(a_letter[["Suspend"]][i] > iteration) {
                        if(!a_letter[["atDest"]][i]) {
                                a_letter[["MovementVector"]][i] <- (a_letter[["Destination"]] - a_letter[["Position"]])/a_letter[["MoveSpeed"]][i]
                        } else {
                                a_letter[["MovementVector"]][i] <- c(0,0)
                        }
                } else {
                        a_letter[["MovementVector"]] <- c(0,0)
                }
        }))
        class(a_letter) <- append(class(a_letter), "a_letter")
        a_letter
})
class(HIMIN) <- append(class(HIMIN), "a_string")
HIMIN <- move(HIMIN)
plot(HIMIN)
iteration <- iteration + 1
