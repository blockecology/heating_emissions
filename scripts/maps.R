library(terra)
library(here)

gemi <- read.csv(here("processed_data/gem_emissions.csv"), 
                 stringsAsFactors = FALSE)

gemeinde <- vect(here("data/vg250_ebenen_1231/VG250_GEM.shp"))

gemeinde$id <- as.numeric(gemeinde)


gemeinde <- merge(gemeinde, gemi, by = "id", all.x = TRUE)

get_gcols <- function(x){
  
  bottom <- quantile(x, 1/3, na.rm = T)
  top <- quantile(x, 2/3, na.rm = T)
  gcols <- "lightgray"
  gcols[x < bottom] <- "#FFCC00"
  gcols[x < top & x >= bottom] <- "#DD0000"
  gcols[x >= top] <- "black"
  return(gcols)
  
}


gcols <- get_gcols(gemeinde$emissions_per_capita)


par(mar = c(0, 0, 0, 3))
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)

threshold <- quantile(gemeinde$emissions_per_capita, 0.9, na.rm = T)
gcols <- ifelse(gemeinde$emissions_per_capita > threshold, "darkred", "white")
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)


threshold <- 2
gcols <- ifelse(gemeinde$emissions_per_capita > threshold, "darkred", "white")
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)



threshold <- 1
gcols <- ifelse(gemeinde$emissions_per_capita < threshold, "turquoise4", "white")
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)



threshold <- quantile(gemeinde$emissions_per_capita, 0.01, na.rm = T)
gcols <- ifelse(gemeinde$emissions_per_capita < threshold, "turquoise4", "white")
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)
text(8, 49,  "1 % of municipalities with lowest emissions per capita")


png(here("maps/emissions_per_capita.png"), width = 9, height = 9, units = "in", 
    res = 300) #, bg = NA)

gcols <- get_gcols(gemeinde$emissions_per_capita)

par(mar = c(0, 0, 0, 3))
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)
add_legend(8e5, 5595000, 
       legend = c("Top",
                  "Middle",
                  "Bottom ",
                  "Missing data"), 
       pch = 15, pt.cex = 1.5, cex = 0.6, bty = "n", y.intersp = 1.6,
       col = c("black", "#DD0000", "#FFCC00", "lightgray"), xpd = T)
dev.off()

