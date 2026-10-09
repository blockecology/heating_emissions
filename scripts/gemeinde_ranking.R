# Analysis of regional correlates of residential heating emissions in Germany
library(here)


gemi <- read.csv(here("processed_data/gem_emissions.csv"), as.is = TRUE)

# Divide municipalities into pop. size categories before ranking
large_cities  <- gemi[gemi$inhabitats >= 1e5, ]
medium_cities <- gemi[gemi$inhabitats < 1e5 & gemi$inhabitats >= 2e4, ]
small_cities  <- gemi[gemi$inhabitats < 2e4 & gemi$inhabitats >= 5e3, ]
villages      <- gemi[gemi$inhabitats < 5e3, ]


rank_cities   <- function(x, variable) {
  
  x <- x[order(x[, variable]), ]
  
  message("Top ten (lowest values):")
  
  for (city in head(x$name, 10)) {
    print(city)
  }
  
  message("Vottom ten (highest values):")
  
  for (city in tail(x$name, 10)) {
    print(city)
  }
  
}

rank_cities(large_cities, "emissions_per_capita")
rank_cities(large_cities, "avg_residence_area")
rank_cities(large_cities, "avg_consumption")
rank_cities(large_cities, "avg_ef")



