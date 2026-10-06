library(terra)
library(here)

gemi <- read.csv(here("processed_data/gem_emissions.csv"), 
                 stringsAsFactors = FALSE)

gemeinde <- vect(here("data/vg250_ebenen_1231/VG250_GEM.shp"))

gemeinde$id <- as.numeric(gemeinde$ARS)


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

# Draw flag bottom, middle, top figure

e <- ext(gemeinde)
x <- gemeinde$emissions_per_capita

draw_flag_fig <- function(variable) {
  
  x <- gemeinde[[variable]]
  
  # Labels
  if (variable == "emissions_per_capita") {
    var_label <- "Residential heating emissions per person"
    var_units <- "Tonnes of carbon dioxide per year"
  } 
  
  if (variable == "emissions") {
    var_label <- "Residential heating emissions"
    var_units <- "Thousand tonnes of carbon dioxide per year"
    x <- x / 1e3
  } 
  
  if (variable == "avg_residence_area") {
    var_label <- "Average residence area"
    var_units <- "Square meters"
  } 
  
  if (variable == "avg_ef") {
    var_label <- "Average emission factor"
    var_units <- "kg of carbon dioxide per kWh"
  } 
  
  if (variable == "avg_consumption") {
    var_label <- "Average heat consumption rate"
    var_units <- "kWh per square meter"
  } 
  
  bottom <- quantile(x, 1/3, na.rm = TRUE)
  top    <- quantile(x, 2/3, na.rm = TRUE)
  
  # Display range for the histogram
  lo <- min(x, na.rm = TRUE)
  hi <- quantile(x, 0.99, na.rm = TRUE)
  
  # Breaks: regular bins PLUS the two tercile cutoffs, so no bin straddles a color change.
  # Because bin widths end up slightly unequal, we plot density rather than counts.
  breaks <- sort(unique(c(seq(lo, hi, length.out = 101), bottom, top)))
  h <- hist(x[x <= hi], breaks = breaks, plot = FALSE)
  
  # Color each bin by its midpoint, using the same scheme as the map
  bin_cols <- ifelse(h$mids < bottom, "#FFCC00",
                     ifelse(h$mids < top, "#DD0000", "black"))
  
  file_name <- here(paste0("maps/", variable, ".png")) 
  
  png(file_name, width = 12, height = 9, units = "in",  res = 300)
  
  layout(matrix(1:2, nrow = 1), widths = c(2, 1))
  
  ## ---- Panel 1: map ----
  gcols <- get_gcols(x)
  
  plot(gemeinde, col = gcols, box = FALSE, lwd = 0.05, axes = FALSE,
       mar = c(0, 0, 3, 0))
  
  text(x = e$xmin, y = e$ymax * 1.009,
       labels = var_label,
       adj = c(0, 0), cex = 1.6, font = 2, xpd = NA)
  
  text(x = e$xmin, y = e$ymax * 1.0035,
       labels = "Top, mid, and bottom thirds of all German municipalities",
       adj = c(0, 0), cex = 1.2, col = "gray", xpd = NA)
  
  
  ## ---- Panel 2: vertical histogram ----
  par(mar = c(5, 5, 2, 3))
  
  plot(NA, xlim = c(0, max(h$density)), ylim = c(lo, hi),
       xlab = "Density", 
       ylab = var_units,
       las = 1, xaxs = "i", yaxs = "i", frame.plot = F, 
       cex.axis = 1.2, cex.lab = 1.5)
  
  rect(xleft = 0, ybottom = h$breaks[-length(h$breaks)],
       xright = h$density, ytop = h$breaks[-1],
       col = bin_cols, border = "white", lwd = 0.3)
  
  
  dev.off()
  
}



draw_flag_fig("emissions_per_capita")
draw_flag_fig("emissions")
draw_flag_fig("avg_residence_area")
draw_flag_fig("avg_ef")
draw_flag_fig("avg_consumption")




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

