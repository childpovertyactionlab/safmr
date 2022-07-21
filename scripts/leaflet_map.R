#### ACS Data #####
acs_var <- c(
  tot_pop = "B01003_001", #total population
  pop_u18 = "S0101_C01_022", #population under 18
  his_pop = "B03002_012", #hispanic population
  wh_pop = "B03002_003", #white population
  bl_pop = "B03002_004", #black population
  as_pop = "B03002_006", #asian population
  #  oohh = "B25106_002", #owner-occupid households
  #  rohh = "B25106_024", #renter-occupied households
  #  thh = "B25106_001", #total households
  pop_bp = "S1701_C02_001", #population below poverty
  bp_u18 = "S1701_C02_002" #population under 18 below poverty
)

counties <- c("Dallas County",
              "Collin County",
              "Denton County")

tract_demographic_data <- get_acs(
  geography = "tract",
  variables = acs_var,
  year = 2019,
  state = "TX",
  county = counties,
  output = "wide",
  geometry = TRUE) %>%
  mutate(TractArea = as.numeric(st_area(.)))

tract_dallas <- tract_demographic_data[DallasBoundary, ] %>%
  mutate(vgc_count = lengths(st_intersects(., vgc12months)),
         vgc_rate = round(vgc_count/tot_popE*10000, digits = 1))


# Incidents Across Geography
The reported U.S. violent crime rate includes murder, rape and sexual assault, robbery, and assault.

## Violent Crime Rate {-}

This map displays the violent crime rate (per 10,000 residents) for the past 12 months by census tract. 

```{r, Violent Crime Rate Map, echo = FALSE, message=FALSE, warning=FALSE, fig.width=10,fig.height=6}
cpal_style <- "https://api.mapbox.com/styles/v1/owencpal/ckecb71jp22ct19qc1id28jku/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1Ijoib3dlbmNwYWwiLCJhIjoiY2tlYnR3emdxMGNhZzMwb2EzZWR4ajloNCJ9.P7Mujz8F3Rssq5-Q6dcvMw"

map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='https://childpovertyactionlab.org/'>Child Poverty Action Lab</a>"

bins <- BAMMtools::getJenksBreaks(tract_dallas$vgc_rate, k = 5)
pal <- colorBin("BuPu", domain = tract_dallas$vgc_rate, bins = bins)

leaflet() %>%
  setView(lng = -96.7970, lat = 32.7767, zoom = 10) %>%
  addTiles(urlTemplate = cpal_style, attribution = map_attr) %>%
  addPolygons(data = tract_dallas,
              fillColor = ~pal(vgc_rate),
              color = FALSE,
              fillOpacity = 0.7,
              opacity = 1,
              weight = 1,
              label = ~paste("VGC Rate:", vgc_rate)) %>%
  addPolygons(data = dpd_divisions,
              fill = FALSE,
              color = "#008097",
              opacity = 1,
              weight = 2) %>%
  addLegend(data = tract_dallas, "topleft", pal = pal, values = ~vgc_rate,
            title = "Violent Crime Rate",
            opacity = 1)
```

## Change in Violent Crime {-}

This map displays the average percent change in violent crimes for the past 12 months in comparison to an average of incidents over the last 5 years. All incidents are aggregated to a tenth mile width grid.

```{r, fig.height=7, fig.width=7, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10,fig.height=6}
cpal_style <- "https://api.mapbox.com/styles/v1/owencpal/ckecb71jp22ct19qc1id28jku/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1Ijoib3dlbmNwYWwiLCJhIjoiY2tlYnR3emdxMGNhZzMwb2EzZWR4ajloNCJ9.P7Mujz8F3Rssq5-Q6dcvMw"

map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='https://childpovertyactionlab.org/'>Child Poverty Action Lab</a>"

vgc_grid <- grid_incidents %>%
  filter(!is.na(vgc_perch))

absVal <- max(abs(vgc_grid$vgc_perch))
domain <- c((absVal*-1),absVal)

colorPal <- c(colorRampPalette(colors = c("#008097", "white"), space = "Lab")(abs(absVal)),
              colorRampPalette(colors = c("white", "#E98816"), space = "Lab")(absVal))

leaflet() %>%
  setView(lng = -96.7970, lat = 32.7767, zoom = 10) %>%
  addTiles(urlTemplate = cpal_style, attribution = map_attr) %>%
  addPolygons(data = vgc_grid,
              fillColor = ~get('colorBin')(colorPal,
                                           domain)(vgc_perch),
              label = ~paste0(round(vgc_perch*100, digits = 2), "%"),
              color = FALSE,
              fillOpacity = 0.7,
              opacity = 1,
              weight = 1,
              group = "Violent Gun Crimes")  %>%
  addPolygons(data = dpd_divisions,
              fill = FALSE,
              color = "#008097",
              opacity = 1,
              weight = 2) %>%
  addLegend(data = vgc_grid, 
            "topleft", 
            pal = colorBin(colorPal, domain = domain),
            values = ~domain,
            title = "VGC Percent Change (5 Year)",
            opacity = 0.9)
```
