saveHTML({
        ani.options(interval = 0.05, nmax = 50)
        par(mar = c(3, 3, 2, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, 
            cex.lab = 0.8, cex.main = 1)
        melt(HIMIN, main = "MELTING HIMIN")}, 
        img.name = "Melt_Plots", 
        title = "Demonstration the power of Dots", 
        description = c("Lets Melt some letters")
)