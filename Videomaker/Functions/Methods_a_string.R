plot.a_string <- function(a_string, pch = 46, 
                          xlim = quote(c(((1-n)+(n-1)/2)-0.5,((n-1)/2)+0.5)), 
                          ylim = c(-1.5,2),
                          useCol = F,...) {
        n <- length(a_string)
        to.plot.pos <- do.call(rbind, lapply(a_string, function(x) x[["Position"]]))
        
        if(useCol) {
                to.plot.col <- rgb(do.call(rbind, lapply(a_string, function(x) x[["PositionCol"]])))
        } else {
                to.plot.col <- "#000000"
        }
        
        plot(to.plot.pos, 
             col = to.plot.col,
             axes = 0, 
             pch = pch, 
             xlim = eval(xlim), 
             ylim = ylim,
             xlab = "",
             ylab = "",...)
}

print.a_string <- function(a_string,...) {
        Letters <- sapply(a_string, function(x) x[["Letter"]])
        print(Letters,...)
        cat("class: a_string", "\n", "Elements of class a_letter in string: ", length(a_string), sep = "")
}

moveVectorAssign.a_string <- function(a_string, fun = "rnorm", sd = 0.001,...) {
        a_string <- lapply(a_string, moveVectorAssign, sd = sd,...)
        class(a_string) <- append(class(a_string), "a_string")
        a_string
}

move.a_string <- function(a_string,...) {
        a_string <- lapply(a_string, move)
        class(a_string) <- append(class(a_string), "a_string")
        a_string
}

updateDist.a_string <- function(a_string) {
        a_string <- lapply(a_string, updateDist)
        class(a_string) <- append(class(a_string), "a_string")
        a_string
}

addAtomizedString <- function(a_string, ...) {
        UseMethod("addAtomizedString", a_string)
}

addAtomizedStrings.a_string <- function(a_string,...) {
        new.a_string <- c(a_string,...)
        class(new.a_string) <- append(class(new.a_string), "a_string")
        new.a_string
}

concatenateAtomizedStrings <- function(...) {
        new.a_string <-  c(...)
        class(new.a_string) <- append(class(new.a_string), "a_string")
        new.a_string
}

updateRelPos <- function(a_string) {
        UseMethod("updateRelPos", a_string)
}

updateRelPos.a_string <- function(a_string) {
        border.at <-c(0, which(sapply(a_string, function(a_letter) which(a_letter[["Letter"]] == "newline")) == 1), length(a_string))
        n_lines <- length(border.at)
        number_of_lines <- n_lines - 1
        temp.string <- lapply(1:n_lines, function(n) {
                if(border.at[n] != 0) {
                        temp.substring <- a_string[(border.at[n-1]+1):(border.at[n])]
                        temp.substring <- concatenateAtomizedStrings(temp.substring)
                        n_string <- length(temp.substring)
                        one_to_ns <- 1:n_string
                        if(c("newline") %in% sapply(temp.substring, function(x) x[["Letter"]])) {
                                temp.substring <- lapply(one_to_ns, function(t) {
                                        temp.substring[[t]][["RelPos_x"]] <- (t+0.5) - n_string+(n_string-1)/2
                                        temp.substring[[t]][["Position"]][,1] <- atomizeLetter(temp.substring[[t]][["Letter"]])[["Position"]][,1] + temp.substring[[t]][["RelPos_x"]]
                                        temp.substring[[t]][["Home"]][,1] <- atomizeLetter(temp.substring[[t]][["Letter"]])[["Home"]][,1] + temp.substring[[t]][["RelPos_x"]]
                                        temp.substring[[t]][["RelPos_y"]] <- ((n - 1) - (number_of_lines) + (number_of_lines - 1)/2) * -2                                        
                                        temp.substring[[t]][["Position"]][,2] <- temp.substring[[t]][["Position"]][,2] + temp.substring[[t]][["RelPos_y"]]
                                        temp.substring[[t]][["Home"]][,2] <- temp.substring[[t]][["Home"]][,2] + temp.substring[[t]][["RelPos_y"]]
                                        temp.substring[[t]]
                                }) 
                        } else {
                                temp.substring <- lapply(one_to_ns, function(t) {
                                        temp.substring[[t]][["RelPos_x"]] <- t-n_string+(n_string-1)/2
                                        temp.substring[[t]][["Position"]][,1] <- atomizeLetter(temp.substring[[t]][["Letter"]])[["Position"]][,1] + temp.substring[[t]][["RelPos_x"]]
                                        temp.substring[[t]][["Home"]][,1] <- atomizeLetter(temp.substring[[t]][["Letter"]])[["Home"]][,1] + temp.substring[[t]][["RelPos_x"]]
                                        temp.substring[[t]][["RelPos_y"]] <- ((n - 1) - (number_of_lines) + (number_of_lines - 1)/2) * -2                                        
                                        temp.substring[[t]][["Position"]][,2] <- temp.substring[[t]][["Position"]][,2] + temp.substring[[t]][["RelPos_y"]]
                                        temp.substring[[t]][["Home"]][,2] <- temp.substring[[t]][["Home"]][,2] + temp.substring[[t]][["RelPos_y"]]
                                        temp.substring[[t]]
                                }) 
                        }
                        class(temp.substring) <- append(class(temp.substring), "a_string")
                        temp.substring
                }
        })
        do.call(concatenateAtomizedStrings, temp.string[-1])
}



atDestination <- function(a_letter) {
        UseMethod("atDestination", a_letter)
}

atDestination.a_string <- function(a_string) {
        sum(sapply(a_string, atDestination)) == length(a_string)
}