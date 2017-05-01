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
                new.a_letter[["StartPos"]] <- rbind(do.call(rbind, 
                                                            rep(x["Destination"], 
                                                                2)), 
                                                    x[["Destination"]][which.exstra.from.old,])
                new.a_letter[["StartCol"]] <- rbind(do.call(rbind, 
                                                            rep(x["DestinationCol"], 
                                                                2)), 
                                                    x[["DestinationCol"]][which.exstra.from.old,])
                theSamp <- sample(1:n_new, n_new)
                new.a_letter[["StartCol"]] <- new.a_letter[["StartCol"]][order(theSamp),]
                new.a_letter[["StartPos"]] <- new.a_letter[["StartPos"]][order(theSamp),]
                
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

morph.a_string <- function(x, new_string,...) {
        if(!("a_string" %in% class(new_string))) {
                new.a_string <- atomizeString(new_string)
        } else {
                new.a_string <- new_string
        }
        pos_new <- do.call(rbind, lapply(new.a_string, function(a_letter) a_letter[["Position"]]))
        pos_old <- do.call(rbind, lapply(x, function(a_letter) a_letter[["Position"]]))
        n_new <- nrow(pos_new)
        n_old <- nrow(pos_old)
        
        new.a_string <- if(n_new > n_old) {
                times.old <- floor(n_new/n_old)
                full.number <- times.old * n_old
                n_extra <- n_new %% full.number
                which.exstra.from.old <- sample(1:n_old, n_extra)
                pos_new[,1] <- c(rep(pos_old[,1],times.old), 
                                 pos_old[which.exstra.from.old,1])
                pos_new[,2] <- c(rep(pos_old[,2],times.old), 
                                 pos_old[which.exstra.from.old,2])
                pos_new <- pos_new[order(sample(1:n_new, n_new)),]
                n_indi_new <- sapply(new.a_string, function(x) if(!is.null(x[["Position"]])) nrow(x[["Position"]]) else 0)
                Increments <- sapply(seq_along(n_indi_new),  function(i) sum(n_indi_new[1:i]))
                
                new.postitions <- lapply(seq_along(Increments), function(i) {
                        
                        if(i == 1) {
                                pos_new[1:(Increments[i]),]
                        } else { 
                                if(Increments[i] != Increments[i-1]) {
                                        pos_new[(Increments[i-1] + 1):Increments[i],]
                                } else {
                                        NULL
                                }
                                
                        }
                })
                lapply(seq_along(new.a_string), function(n) {
                        new.a_string[[n]][["Position"]] <- new.postitions[[n]]
                        class(new.a_string[[n]]) <- append(class(new.a_string[[n]]), "a_letter")
                        new.a_string[[n]]
                })
        } else { if(n_new < n_old) {
                new.pos.numbers <- lapply(new.a_string, function(x) nrow(x[["Position"]]))
                new.pos.numbers <- lapply(new.pos.numbers, function(x) if(is.null(x)) 0 else x)
                new.pos.numbers <- unlist(new.pos.numbers)
                
                new.pos.distribution <- new.pos.numbers/sum(new.pos.numbers)
                
                distribution.from.old <- floor(n_old * new.pos.distribution)
                rest <- n_old - sum(distribution.from.old)
                #times.new <- floor(distribution.from.old/new.pos.numbers)
                times.new <- mapply(function(x, y) if(x == 0) 0 else floor(y/x), 
                                    x = new.pos.numbers, 
                                    y = distribution.from.old)
                
                full.numbers <- times.new * new.pos.numbers
                n_extras <- mapply(function(x, y) if(x == 0) 0 else y %% x, 
                                   x = full.numbers, 
                                   y = distribution.from.old)
                
                n_extras[1] <- n_extras[1] + rest
                
                which.extras.from.new <- lapply(seq_along(n_extras), function(n) sample(1:new.pos.numbers[n], n_extras[n]))
                
                #which.extras.from.new <- mapply(function(x, y) if(x == 0) 0 else sample(1:x, y), x = new.pos.numbers, y = n_extras)
                
                new.temp.letters.home <- lapply(seq_along(distribution.from.old),
                                                function(x) {
                                                        do.call(rbind, lapply(1:(times.new[x] + 1), function(n) {
                                                                new.home.mat <- matrix(nrow = full.numbers[x] + n_extras[x], ncol = 2)
                                                                if(n < (times.new[x] + 1)) {
                                                                        new.home.mat[((n-1)*new.pos.numbers[x] + 1):(n*new.pos.numbers[x]),] <- new.a_string[[x]][["Home"]]
                                                                } else {
                                                                        new.home.mat[((n-1)*new.pos.numbers[x] + 1):((n-1)*new.pos.numbers[x] + n_extras[x]),] <- new.a_string[[x]][["Home"]][which.extras.from.new[[x]],]
                                                                }
                                                        }
                                                        )) 
                                                })
                
                pos_old <- pos_old[order(sample(1:n_old, n_old)),]
                total = full.numbers + n_extras
                Increments <- sapply(seq_along(total),  function(i) sum(total[1:i]))
                
                new.temp.letters.pos <- lapply(seq_along(Increments), function(i) {
                        
                        if(i == 1) {
                                pos_old[1:(Increments[i]),]
                        } else { 
                                if(Increments[i] != Increments[i-1]) {
                                        pos_old[(Increments[i-1] + 1):Increments[i],]
                                } else {
                                        NULL
                                }
                                
                        }
                })
                
                
                new.temp.string <- lapply(seq_along(new.temp.letters.home), function(x) {
                        a_letter.temp <- list(Letter = new.a_string[[x]][["Letter"]], 
                                              RelPos_x = new.a_string[[x]][["RelPos_x"]],
                                              RelPos_y = new.a_string[[x]][["RelPos_y"]],
                                              Position = new.temp.letters.pos[[x]],
                                              Destination = matrix(rep(c(0,0), (full.numbers[x] + n_extras[x])), ncol = 2),
                                              Dist = matrix(rep(c(0,0), (full.numbers[x] + n_extras[x])), ncol = 2),
                                              atDest = rep(T, (full.numbers[x] + n_extras[x])),
                                              MovementVector = matrix(rep(c(0,0), (full.numbers[x] + n_extras[x])), ncol = 2),
                                              MoveDuration = rep(0, (full.numbers[x] + n_extras[x])),
                                              MoveSpeed = rep(0.05, (full.numbers[x] + n_extras[x])),
                                              Suspend = rep(0, (full.numbers[x] + n_extras[x])), 
                                              Home = new.temp.letters.home[[x]],
                                              MovementVectorName = "c(0,0)")
                        class(a_letter.temp) <- append(class(a_letter.temp), "a_letter")
                        a_letter.temp
                })                                        
                #class(new.letter.temp) <- append(class(new.letter.temp), "a_letter")
                new.temp.string
        }
        }
        class(new.a_string) <- append(class(new.a_string), "a_string")
        new.a_string
}
