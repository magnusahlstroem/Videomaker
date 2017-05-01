returnHome <- function(a_letter, duration,...) {
        UseMethod("returnHome", a_letter)
}

returnHome.a_letter <- function(a_letter, duration = 10) {
        a_letter[["Destination"]] <- a_letter[["Home"]]
        dist <- a_letter[["Destination"]] - a_letter[["Position"]]
        a_letter[["MoveSpeed"]] <- 1/duration
        a_letter[["MovementVector"]] <- dist * a_letter[["MoveSpeed"]]
        n <- nrow(a_letter[["Position"]])
        one_to_n <- 1:n
        iteration <- 0
        
        while(sum(a_letter[["MovementVector"]][,1] == 0) != n  
              & sum(a_letter[["MovementVector"]][,2] == 0) != n) {
                a_letter <- move(a_letter)
                a_letter[["MovementVector"]] <- t(sapply(one_to_n, function(i) {
                        if(a_letter[["Position"]][i,1] < a_letter[["Destination"]][i,1] + 0.00001 & 
                           a_letter[["Position"]][i,1] > a_letter[["Destination"]][i,1] - 0.00001 &
                           a_letter[["Position"]][i,2] < a_letter[["Destination"]][i,2] + 0.00001 &
                           a_letter[["Position"]][i,2] > a_letter[["Destination"]][i,2] - 0.00001) { 
                                a_letter[["MovementVector"]][i,] <- c(0,0) 
                        } else { 
                                a_letter[["MovementVector"]][i,] <- a_letter[["MovementVector"]][i,]
                        }
                }
                ))
                plot(a_letter)
                iteration <- iteration + 1
        }
        a_letter
}

returnHome.a_string <- function(a_string, 
                                easing = easings(),
                                Suspend = NA,
                                Duration = Durations(),...) {
        n <- sum(unlist(sapply(a_string, function(a_letter) nrow(a_letter[["Position"]]))))
        Duration <- match.arg(Duration)
        Duration <- eval(call(Duration, a_string = a_string))
        easingFun <- match.arg(easing)
        
        a_string <- lapply(a_string, function(a_letter) {
                rel.pos.string <- lapply(a_string, function(x) c(x[["RelPos_x"]], x[["RelPos_y"]]))
                rel.pos.letter <- c(a_letter[["RelPos_x"]], a_letter[["RelPos_y"]])
                list(rel.pos.letter, rel.pos.string)
                n_dur <- which(sapply(rel.pos.string, function(x) x[1] == rel.pos.letter[1] & x[2] == rel.pos.letter[2]))
                a_letter[["Destination"]] <- a_letter[["Home"]]
                a_letter[["DestinationCol"]] <- a_letter[["HomeCol"]]
                a_letter[["atDest"]] <- if(!is.null(nrow(a_letter[["Position"]]))) rep(F, nrow(a_letter[["Position"]])) else T
                a_letter[["StartPos"]] <- a_letter[["Position"]]
                a_letter[["StartCol"]] <- a_letter[["PositionCol"]]
                a_letter[["MoveDuration"]] <- Duration[[n_dur]]
                #a_letter[["Suspend"]] <- Suspend[[n_dur]]
                #a_letter[["MovementVector"]] <- dist * a_letter[["MoveSpeed"]]
                class(a_letter) <- append(class(a_letter), "a_letter")
                a_letter
        })
        
        class(a_string) <- append(class(a_string), "a_string")
        i <- 0
        
        while(!atDestination(a_string)) 
        {
                a_string <- lapply(a_string, function(a_letter) {
                        if(!(a_letter[["Letter"]] %in% c("newline", "space"))) {
                                one_to_n <- 1:nrow(a_letter[["Position"]])
                                a_letter[["atDest"]] <- i >= a_letter[["MoveDuration"]]
                                a_letter[["Position"]] <- (a_letter[["atDest"]] * a_letter[["Destination"]]) +  
                                        (eval(call(easingFun, 
                                                   t = i, 
                                                   b = a_letter[["StartPos"]], 
                                                   c = a_letter[["Destination"]] - a_letter[["StartPos"]], 
                                                   d = a_letter[["MoveDuration"]])) * !a_letter[["atDest"]])
                                a_letter[["PositionCol"]] <- (a_letter[["atDest"]] * a_letter[["DestinationCol"]]) + 
                                        (eval(call(easingFun,
                                                   t = i,
                                                   b = a_letter[["StartCol"]],
                                                   c = a_letter[["DestinationCol"]] - a_letter[["StartCol"]],
                                                   d = a_letter[["MoveDuration"]])) * !a_letter[["atDest"]])
                                class(a_letter) <- append(class(a_letter), "a_letter")
                                a_letter
                        } else {
                                return(a_letter)
                        }
                })
                
                
                class(a_string) <- append(class(a_string), "a_string")
                #a_string <- move(a_string)
                dev.hold()
                plot(a_string,...)
                ani.pause()
                i <- i + 1
        }
        a_string <- lapply(a_string, function(a_letter) {
                dupl <- duplicated(a_letter[["Home"]])
                a_letter <- lapply(a_letter, function(element) {
                        if(!is.null(nrow(element))) {
                                if(is.matrix(element)) {
                                        element <- element[!dupl,]
                                } else {
                                        element <- element[!dupl]
                                }        
                        } else {
                                element
                        }
                })
                class(a_letter) <- append(class(a_letter), "a_letter")
                a_letter
        })
        class(a_string) <- append(class(a_string), "a_string")
        a_string
}




