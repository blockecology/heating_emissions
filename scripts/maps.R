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


# Drawing flag figs
draw_flag_fig("emissions_per_capita")
draw_flag_fig("emissions")
draw_flag_fig("avg_residence_area")
draw_flag_fig("avg_ef")
draw_flag_fig("avg_consumption")



# Hot spot maps

draw_hotspot_maps <- function(variable, high_bar, low_bar) {
  
  x <- gemeinde[[variable]]
  high_bar <- round(high_bar, 2)
  low_bar <- round(low_bar, 2)
  
  # Labels
  if (variable == "emissions_per_capita") {
    var_label <- "Average carbon dioxide emissions per person"
    var_high   <- paste0("> ", high_bar, " tonnes")
    var_low    <- paste0("< ", low_bar, " tonnes")
  } 
  
  if (variable == "emissions") {
    x <- x / 1e3
    high_bar <- round(high_bar / 1e3, 2)
    low_bar <- round(low_bar / 1e3, 2)
    
    var_label <- "Carbon dioxide emissions"
    var_units <- " thousand tonnes"
    var_high   <- paste0("> ", high_bar, var_units)
    var_low    <- paste0("< ", low_bar, var_units)
  } 
  
  if (variable == "avg_residence_area") {
    var_label <- "Average residence area"
    var_units <- " square meters"
    var_high   <- paste0("> ", high_bar, var_units)
    var_low    <- paste0("< ", low_bar, var_units)
    
  } 
  
  if (variable == "avg_ef") {
    var_label <- "Average emission factor"
    var_units <- " kg of carbon dioxide per kWh"
    var_high   <- paste0("> ", high_bar, var_units)
    var_low    <- paste0("< ", low_bar, var_units)
  } 
  
  if (variable == "avg_consumption") {
    var_label <- "Average heat consumption rate"
    var_units <- " kWh per square meter"
    var_high   <- paste0("> ", high_bar, var_units)
    var_low    <- paste0("< ", low_bar, var_units)
  } 
  
  file_name <- here(paste0("maps/", variable, "_hotspots.png")) 
  
  png(file_name, width = 12, height = 9, units = "in",  res = 300)
  
  layout(matrix(1:2, nrow = 1), widths = c(1, 1))
  
  ## ---- Panel 1: map ----
  gcols <- ifelse(x >= high_bar, "darkred", "white")
  
  plot(gemeinde, col = gcols, box = FALSE, lwd = 0.025, axes = FALSE,
       mar = c(0, 0, 3, 0))
  
  text(x = e$xmax * 1.01, y = e$ymax * 1.012,
       labels = var_label, cex = 1.5, col = "black", xpd = NA, font = 2)
  
  x_mid_point <- (e$xmin + (e$xmax - e$xmin)/2)
  text(x = x_mid_point, y = e$ymax * 1.004,
       labels = var_high, cex = 1.25, col = "black", xpd = NA, font = 1)
  
  
  ## ---- Panel 2: vertical histogram ----
  threshold <- 2
  gcols <- ifelse(x < low_bar, "turquoise4", "white")
  
  plot(gemeinde, col = gcols, box = FALSE, lwd = 0.025, axes = FALSE,
       mar = c(0, 0, 3, 0))
  
  text(x = x_mid_point, y = e$ymax * 1.004,
       labels = var_low, cex = 1.3, col = "black", xpd = NA, font = 1)
  
  
  dev.off()
  
}

draw_hotspot_maps("emissions_per_capita", 2, 0.5)

draw_hotspot_maps("emissions", 
                  quantile(gemeinde$emissions, 0.95, na.rm = T), 
                  quantile(gemeinde$emissions, 0.05, na.rm = T))

draw_hotspot_maps("avg_ef", 
                  quantile(gemeinde$avg_ef, 0.95, na.rm = T), 
                  quantile(gemeinde$avg_ef, 0.05, na.rm = T))

draw_hotspot_maps("avg_consumption", 
                  quantile(gemeinde$avg_consumption, 0.95, na.rm = T), 
                  quantile(gemeinde$avg_consumption, 0.05, na.rm = T))


