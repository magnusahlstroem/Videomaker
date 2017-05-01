Durations <- function() {
        c("sameExtremelyShort", "bigVarExtremelyShort", "bigVarLong",
          "bigVarMedium", "bigVarShort", "bigVarVeryShort", "bigVarVeryLong",
          "medVarExtremelyLong", "medVarExtremelyShort", "medVarLong",
          "medVarMedium", "medVarShort", "medVarVeryShort", "medVarVeryLong",
          "sameExtremelyLong", "bigVarExtremelyLong", "sameLong", 
          "sameMedium", "sameShort", "sameVeryShort", "sameVeryLong",
          "smallVarExtremelyLong", "smallVarExtremelyShort", "smallVarLong",
          "smallVarMedium", "smallVarShort", "smallVarVeryShort", "smallVarVeryLong")
}

##Same value to all


sameExtremelyShort <- function(a_string, SampleVect = 5) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

sameVeryShort <- function(a_string, SampleVect = 10) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

sameShort <- function(a_string, SampleVect = 20) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

sameMedium <- function(a_string, SampleVect = 30) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

sameLong <- function(a_string, SampleVect = 50) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

sameVeryLong <- function(a_string, SampleVect = 80) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

sameExtremelyLong <- function(a_string, SampleVect = 150) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

### Small variation

smallVarExtremelyShort <- function(a_string, SampleVect = list(3:7)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

smallVarVeryShort <- function(a_string, SampleVect = list(8:12)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

smallVarShort <- function(a_string, SampleVect = list(17:23)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

smallVarMedium <- function(a_string, SampleVect = list(27:33)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

smallVarLong <- function(a_string, SampleVect = list(46:54)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

smallVarVeryLong <- function(a_string, SampleVect = list(75:85)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

smallVarExtremelyLong <- function(a_string, SampleVect = list(142:158)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}


## medium variation
## medium variation

medVarExtremelyShort <- function(a_string, SampleVect = list(2:8)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

medVarVeryShort <- function(a_string, SampleVect = list(7:13)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

medVarShort <- function(a_string, SampleVect = list(16:24)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

medVarMedium <- function(a_string, SampleVect = list(23:37)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

medVarLong <- function(a_string, SampleVect = list(41:59)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

medVarVeryLong <- function(a_string, SampleVect = list(69:91)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

medVarExtremelyLong <- function(a_string, SampleVect = list(130:170)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}


## Big variation
## Big Variation

bigVarExtremelyShort <- function(a_string, SampleVect = list(1:9)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

bigVarShort <- function(a_string, SampleVect = list(5:15)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

bigVarShort <- function(a_string, SampleVect = list(10:30)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

bigVarMedium <- function(a_string, SampleVect = list(15:45)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

bigVarLong <- function(a_string, SampleVect = list(20:60)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

bigVarVeryLong <- function(a_string, SampleVect = list(30:110)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}

bigVarExtremelyLong <- function(a_string, SampleVect = list(100:200)) {
        x = lapply(SampleVect, enquote)
        size = lapply(a_string, function(x) nrow(x[["Position"]]))
        mapply(FUN = function(x, size, replace = T) { if(!is.null(size)) sample(x = eval(x), size = size, replace = replace) else (NULL)},
               x = x,
               size = size, 
               SIMPLIFY = F)
}