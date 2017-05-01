test <- c("HIM[Space]MM[Smiley]")
test <- c("[AHM][S][N][K][L][K][J][L][K]")
n <- 1:nchar(string)
a_string <- sapply(n, function(x) substr(string,x,x))

n_start <- which(a_string == "[")
n_stop <- which(a_string == "]")

#replace.with <- mapply(function(x,y) do.call(paste0, as.list(a_string[(x + 1):(y - 1)])), x = n_start, y = n_stop)

for (i in seq_along(n_start)) {
        a_string[n_start[i]:n_stop[i]] <- do.call(paste0, as.list(a_string[(n_start[i] + 1):(n_stop[i] - 1)]))
}

minus.rem <- c(0, (n_stop - n_start))

for (i in seq_along(n_start)) {
        a_string <- a_string[-c((n_start[i] + 1 - sum(minus.rem[1:i])):(n_stop[i]-sum(minus.rem[1:i])))]
}
        
        
        mapply(function(x, y, with) 
        {
        a_test[x:y] <- with
        a_test
        }, 
        x = n_start, y = n_stop, with = replace.with)

        do.call(paste0, as.list(a_test[(x + 1):(y - 1)])), x = n_start, y = n_stop)


a_test <- 
a_test2 <- vector()
test2 <- test
for (i in seq_along(n_start)) {
        test2 <- sub("\\[.*?\\]", paste("\\", i, sep = ""), test2)        
}


number <- n_stop[1] - (n_start - 1)[1]

rem <- mapply(function(x, y) paste(x, y, sep =":"), n_start, n_stop) 

n <- 1:nchar(test)




chars_s_s <- mapply(function(x, y) paste(x+1, y-1, sep =":"), n_start, n_stop) 
