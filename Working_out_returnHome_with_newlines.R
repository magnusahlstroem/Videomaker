returnHome.a_string <- function(a_string, 
                                easing = c("linear", "easeInQuad"),
                                Suspend = NA,
                                Duration = c("simpleDurationAssign"),...) {
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
                a_letter[["atDest"]] <- if(!is.null(nrow(a_letter[["Position"]]))) rep(F, nrow(a_letter[["Position"]])) else T
                a_letter[["StartPos"]] <- a_letter[["Position"]]
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
        a_string
}


linear <- function (t, b, c, d) {
        return(c*t/d + b)
}