library(terra)
library(here)

gemi <- read.csv(here("processed_data/gem_emissions.csv"), 
                 stringsAsFactors = FALSE)

gemeinde <- vect(here("data/vg250_ebenen_1231/VG250_GEM.shp"))

gemeinde$id <- as.numeric(gemeinde$ARS)


gemeinde <- merge(gemeinde, gemi, by = "id", all.x = TRUE)


# Map Gemeinde that potentially still lack a heating plan

## Exclude

# 35 municipalities in SH had a 31.12.2024 deadline
# https://www.landtag.ltsh.de/infothek/wahl20/drucks/02000/drucksache-20-02053.pdf

# Relevant legislation: https://www.gesetze-rechtsprechung.sh.juris.de/bssh/document/jlr-NNLSH00002A8DNN00000000002 

sh_ober_mittel <- c("Flensburg",
                    "Kiel",
                    "Lübeck",
                    "Neumünster",
                    "Bad Oldesloe",
                    "Bad Segeberg", 
                    "Wahlstedt",
                    "Brunsbüttel",
                    "Eckernförde",
                    "Elmshorn",
                    "Eutin", 
                    "Heide",
                    "Husum", 
                    "Itzehoe", 
                    "Kaltenkirchen",
                    "Mölln",
                    "Rendsburg",
                    "Schleswig",
                    "Ahrensburg",
                    "Geesthacht",
                    "Glinde",
                    "Norderstedt",
                    "Pinneberg",
                    "Reinbeck",
                    "Wedel",
                    "Wentorf",
                    "Neustadt in Holstein",
                    "Ratzeburg",
                    "Sylt",
                    "Niebüll",
                    "Oldenburg in Holstein",
                    "Plön",
                    "Kappeln",
                    "Meldorf",
                    "Tönning")

# Federal rules
gcols <- ifelse(gemeinde$inhabitats < 1e5, "lightblue4", "lightgreen")
gcols[gemeinde$inhabitats < 1e4] <- "blue4"

# State rules
gcols[gemeinde$inhabitats > 2e4 & gemeinde$LKZ == "BW"] <- "lightgreen"
gcols[gemeinde$GEN %in% sh_ober_mittel & gemeinde$LKZ == "SH"] <- "lightgreen"



par(mar = c(0, 0, 0, 3))
plot(gemeinde, col = gcols, box = F, lwd = 0.05, axes = F)
