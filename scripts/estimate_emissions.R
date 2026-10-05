
library(readxl)
library(here)
source(here("scripts/utils.R"))

DATA_PATH <- here("data/census_tables/")

read_census_table <- function(file_name, s) {
  x <- read_xlsx(paste0(DATA_PATH, file_name), 
                          sheet = s, na = c("", "–", "."))
  x <- x[x$Regionalebene == "Gemeinde", ]
  names(x)[1:4] <- c("date", "id", "name", "admin_level")
  
  return(x)
}

population <- read_census_table("Regionaltabelle_Bevoelkerung.xlsx", 7)
demography <- read_census_table("Regionaltabelle_Demografie.xlsx", 6)
buildings <- read_census_table("Regionaltabelle_Gebaeude_Wohnungen.xlsx", 8)


# Calculate average energy consumption per Gemeinde
avg_consumption <- numeric(nrow(buildings))

for (i in 1:nrow(buildings)) {
  weights <- as.numeric(buildings[i, grep("BAUJAHR", names(buildings))])
  weights <- ifelse(is.na(weights), 0, weights)
  avg_consumption[i] <- weighted.mean(energy_consumption$consumption, weights,
                                      na.rm = TRUE)
  
}

# Calculate average emission factor per Gemeinde
avg_ef <- numeric(nrow(buildings))

for (i in 1:nrow(buildings)) {
  weights <- as.numeric(buildings[i, grep("NERGIETRAEGER", names(buildings))])
  weights <- ifelse(is.na(weights), 0, weights)
  avg_ef[i] <- weighted.mean(emission_factors$ef, weights, na.rm = F)
  
}


# drivers
number_of_residences <- buildings$GEBAEUDEART_SYS_1
avg_residence_area <- buildings$FLAECHE
inhabitats <- population$EWZ


# Calculate emissions
total_heated_area <- number_of_residences * avg_residence_area 

emissions <- total_heated_area * avg_consumption * avg_ef / 1e3

emissions_per_capita <- (emissions / inhabitats) 


# Put results together
gemi <- cbind(population[, 2:3], 
              inhabitats,
              total_heated_area,
              number_of_residences,
              avg_residence_area,
              avg_consumption,
              avg_ef,
              emissions_per_capita,
              emissions)

write.csv(gemi, here("processed_data/gem_emissions.csv"), row.names = FALSE)


# correlates
rent_per_m2 <- as.numeric(buildings$QMMIETE)
owners <- as.numeric(buildings$ETQ)
vacants <- as.numeric(buildings$LEQ)
vacants[is.na(vacants)] <- 0
occupancy <- 1 - vacants/100

seniors <- (demography$Alter_infr__10 + demography$Alter_infr__11) / inhabitats
seniors <- seniors * 100 # convert from fraction to percentage


