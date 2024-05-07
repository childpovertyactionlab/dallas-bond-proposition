library(tidyverse)
library(sf)
library(leaflet)

libDB <- "C:/Users/Michael/CPAL Dropbox/"

bonds_sf <- st_read("data/City of Dallas Proposed 2024 Bond Projects.geojson") %>%
  mutate(Council = ifelse(str_detect(Council, ","), "Multiple Districts", Council),
         Council = ifelse(Council %in% c("CW", "Multiple Districts"), Council, paste("CD", Council)))

# Generate the map
leaflet(data = bonds_sf) %>%
  addTiles(urlTemplate = cpaltemplates::cpal_mapbox_color) %>%
  addCircleMarkers(
    fillColor = ~colorFactor(palette = "viridis", domain = bonds_sf$Council)(Council),
    fillOpacity = 0.8,
    color = "#000000",  # Black border for each point
    weight = 1,  # Border thickness
    radius = 5,  # Size of the markers
    popup = ~paste("Name:", Name, "<br>", "Description:", Description)
  ) %>%
  addPolygons(data = smu_deserts,
              fillColor = cpaltemplates::palette_cpal_main[1],
              fillOpacity = 0.2,
              color = "#000000",
              weight = 1
               ) %>%
  addLegend("bottomright", 
            pal = colorFactor(palette = "viridis", domain = bonds_sf$Council), 
            values = ~Council, 
            title = "Council District",
            opacity = 1
  )

##### Import SMU Infrastructure Data #####
smu_infra <- st_read(paste0(libDB, "Data Library/SMU/Infrastructure Desert Analysis/Feb 2022 Data/Infrastructure_assessment_Dallas.shp"))

unique(smu_infra$IF_5)

smu_deserts <- smu_infra %>%
  filter(IF_5 == "Highly Deficient") %>%
  st_transform(crs = 4326) %>%
  select(BLOCKGROUP, IF_5)

bonds_inf <- st_join(bonds_sf, smu_deserts) %>%
  mutate(INFDES_FLAG = ifelse(is.na(IF_5), FALSE, TRUE)) %>%
  select(-BLOCKGROUP, -IF_5)

bonds_tbl <- bonds_inf %>%
  st_drop_geometry() %>%
  group_by(Council, INFDES_FLAG) %>%
  summarize(count = n(),
            amount = sum(BondAmount2024)) %>%
  ungroup() %>%
  pivot_wider(names_from = INFDES_FLAG,
              values_from = c(count, amount),
              values_fill = 0) %>%
  mutate(total_projects = count_FALSE+count_TRUE,
         total_amount = amount_FALSE+amount_TRUE,
         per_desert = count_TRUE/total_projects,
         amt_desert = amount_TRUE/total_amount) %>%
  select(Council,
         count_TRUE,
         total_projects,
         per_desert,
         amount_TRUE,
         total_amount,
         amt_desert
  ) %>%
  rename(desert_projects = 2,
         desert_amount = 5)

sum(bonds_tbl$total_amount) # amount for geocoded projects
sum(bonds_tbl$desert_amount) # amount for geocoded projects

sum(bonds_tbl$desert_amount)/sum(bonds_tbl$total_amount)
