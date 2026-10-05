
energy_consumption <- data.frame(
  building_age = c("BAUJAHR_10JA_01",
                   "BAUJAHR_10JA_02",
                   "BAUJAHR_10JA_03",
                   "BAUJAHR_10JA_04",
                   "BAUJAHR_10JA_05",
                   "BAUJAHR_10JA_06",
                   "BAUJAHR_10JA_07",
                   "BAUJAHR_10JA_08",
                   "BAUJAHR_10JA_09",
                   "BAUJAHR_10JA_10"),
  consumption = c(134.6, 134.6, 135.7, 135.7, 135.7, 
                  126.2, 93.3, 78.5, 74.1, 74.1)
)


# Scope 1 emission factors
emission_factors <- data.frame(
  energy_carrier = c("ENERGIETRAEGER__1", # gas
                     "ENERGIETRAEGER__2", # oil
                     "ENERGIETRAEGER__3", # wood
                     "ENERGIETRAEGER__4", # biomass
                     "ENERGIETRAEGER__5", # heat pumps
                     "ENERGIETRAEGER__6", # electricity
                     "ENERGIETRAEGER__7", # coal
                     "ENERGIETRAEGER__8", # district heating
                     "ENERGIETRAEGER__9"), # no heat
  ef = c(0.2, 0.27, 0.34, 0.2, 0, 0, 0.34, 0, 0)
)
