library(animation)
library(png)
library(dplyr)
setwd("H:/OOP_R/Videomaker")
setwd("Functions")
functions <- list.files()
lapply(functions, function(x) source(x))
setwd("..")
dir.create("easingDemo5")
setwd("easingDemo5")

IMG <- atomizeImage("H:/Dokumenter/Forskning/Studier/HIV_GFR_Creatinine/Submission_nephron/Til_Resubmission/Figures_and_Tables/Figure1.png", width = 10, height = 4)

jazz <- morph(IMG, atomizeString("THIS[space]WAS[space]AN[newline]IMAGE"))
returnHome(jazz, xlim = c(-7,7), ylim = c(-2,2), 
           Duration = "medVarLong", 
           easing = "easeInOutQuint",
           useCol = T)

new.png <- function(filename = "Rplot%03d.png",
                    width = 900, height = 600, units = "px", pointsize = 12,
                    bg = "white", res = NA, family = "", restoreConsole = TRUE,
                    type = c("windows", "cairo", "cairo-png"), antialias) {
        png(filename = filename,
            width = width, height = height, units = units, pointsize = pointsize,
            bg = bg, res = res, family = family, restoreConsole = restoreConsole,
            type = type, antialias)
}

        
        
saveHTML({
        par(mar = c(1, 1, 1, 1))
        ani.options(interval = 0.02, nmax = 200, ani.width = 900, ani.height = 600)
        returnHome(jazz, xlim = c(-7,7), ylim = c(-2,2), 
                   Duration = "medVarLong", 
                   easing = "easeInOutQuint",
                   useCol = T)
        }, img.name = "easingDemo", htmlfile = "index.html", navigator = FALSE)