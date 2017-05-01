M_atomized <- atomizeLetter("M")
M_atomized
plot(M_atomized)
M_atomized <- moveVectorAssign(M_atomized, sd = 0.1)
plot(M_atomized)
M_atomized <- move(M_atomized)
plot(M_atomized)
M_atomized <- move(M_atomized)
plot(M_atomized)
returnHome(M_atomized, duration = sample(5:50, nrow(M_atomized[["Position"]]), replace = TRUE))

HIMIN <- atomizeString("HIMIN")
HIMIN[[1]][["Position"]][1:10,]
HIMIN[[1]][["Destination"]][1:10,]
HIMIN[[1]][["Dist"]][1:10,]
HIMIN[[1]][["MoveSpeed"]][1:10]
HIMIN[[1]][["MoveDuration"]][1:10]
HIMIN[[1]][["Suspend"]][1:10]
HIMIN[[1]][["atDest"]][1:10]

class(HIMIN) <- append(class(HIMIN), "a_string")
#HIMIN <- moveVectorAssign(HIMIN, sd = 0.1)
HIMIN <- move(HIMIN)
HIMIN <- updateDist(HIMIN)
plot(HIMIN)
test2[[1]][["Position"]][1:10,]
test2[[1]][["Destination"]][1:10,]
test2[[1]][["Dist"]][1:10,]
test2[[1]][["MoveSpeed"]][1:10]
test2[[1]][["MoveDuration"]][1:10]
test2[[1]][["Suspend"]][1:10]
test2[[1]][["atDest"]][1:10]
test2[[1]][["MovementVector"]][1:10,]
test2[[1]][["Home"]][1:10,]

#HIMIN <- returnHome(HIMIN)

HIMIN <- melt(HIMIN)
# <- move(HIMIN)
plot(HIMIN)
HIMIN <- updateDist(HIMIN)


round(HIMIN[[1]][["Dist"]]/0.05)[1:10,]
round(sqrt(rowSums((HIMIN[[1]][["Dist"]][1:10,])^2))/0.05)

I_atomized <- reassemble(atomizeLetter("M"), "I")
I_atomized[["Position"]][1:10,] 
I_atomized[["Destination"]][1:10,]
I_atomized[["Dist"]][1:10,]
I_atomized[["MoveSpeed"]][1:10]
I_atomized[["Suspend"]][1:10]
I_atomized[["atDest"]][1:10]
I_atomized[["MovementVector"]][1:10,]
I_atomized[["Home"]][1:10]

plot(HIMIN)


HIMIN <- melt(HIMIN)

HIMIN <- returnHome(HIMIN)

HIMIN[[1]][["Position"]][1:10,]
HIMIN[[1]][["Destination"]][1:10,]
HIMIN[[1]][["Dist"]][1:10,]
HIMIN[[1]][["MoveSpeed"]][1:10]
HIMIN[[1]][["Suspend"]][1:100]
atDestination(HIMIN)
class(HIMIN) <- append(class(HIMIN), "a_string")

setwd("MeltDownDemo")



old_letter <- atomizeLetter("I")

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
        } else { 
                if(n_new < n_old) {
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
                }
        }
        class(new.a_letter) <- append(class(new.a_letter), "a_letter")
        new.a_letter
}   

class(new.letter.temp) <- append(class(new.letter.temp), "a_letter")
new.letter.temp

new.letter.temp[["Position"]][1:10,] 
new.letter.temp[["Destination"]][1:10,]
new.letter.temp[["Dist"]][1:10,]
new.letter.temp[["MoveSpeed"]][1:10]
new.letter.temp[["Suspend"]][1:10]
new.letter.temp[["atDest"]][1:10]
new.letter.temp[["MovementVector"]][1:10,]
new.letter.temp[["Home"]][1:10,]