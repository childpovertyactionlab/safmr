library(tidyverse)
library(sf)
library(tigris)
library(googlesheets4)

safmr <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1VEjpNV2AyGVHYpJRkfUfrLycQPYbCTFhHPtMDkJ00pQ/") %>%
  distinct()

county <- tigris::counties(state = "TX")

countyntx <- county %>%
  filter(NAME %in% c("Bonham", "Collin", "Cooke", "Dallas", "Denton", "Ellis", "Grayson", "Hunt", "Johnson", "Kaufman", "Parker", "Rockwall", "Tarrant", "Wise")) %>%
  st_transform(crs = 4269)

zcta <- tigris::zctas(state = "TX",
                      year = 2010) %>%
  transmute(ZCTA = ZCTA5CE10) %>%
  st_transform(crs = 4269)

zctantx <- zcta[countyntx, ]

mfntx <- rio::import("data/Multi-Family Apartments (September 2023).csv") %>%
  st_as_sf(coords = c(x = "longitude", y = "latitude"),
           crs = 4269) %>%
  mutate(lat = st_coordinates(.)[,1],
         lon = st_coordinates(.)[,2]) %>%
  st_join(., zcta)

mfzcta <- unique(mfntx$ZCTA) %>%
  as.data.frame()

mfzcta <- mfntx %>%
  st_drop_geometry(.) %>%
  group_by(ZCTA) %>%
  summarize(TotUnits = sum(number_of_units, na.rm = TRUE),
            TotMF = n(),
            AvgSF = round(mean(avg_unit_sf, na.rm = TRUE), digits = 0),
            medRent1BR = median(one_bedroom_effective_rent_unit, na.rm = TRUE),
            medRent2BR = median(two_bedroom_effective_rent_unit, na.rm = TRUE),
            medRent3BR = median(three_bedroom_effective_rent_unit, na.rm = TRUE),
            medRent4BR = median(four_bedroom_effective_rent_unit, na.rm = TRUE)
            ) %>%
  left_join(safmr, .) %>%
  left_join(zctantx, .) %>%
  filter(!is.na(TotUnits)) %>%
  mutate(RentDiff = medRent2BR - fairMrkt2BR)

plot(mfzcta["RentDiff"])

st_write(mfzcta, "data/safmr_zcta.geojson", delete_dsn = TRUE)
st_write(countyntx, "data/ntx_county.geojson", delete_dsn = TRUE)
