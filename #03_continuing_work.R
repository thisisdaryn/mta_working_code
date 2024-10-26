library(feather)
library(sf)
library(tidyverse)

hourly24 <- read_feather("working_data/hourly24.feather") |>
    select(station_complex_id, station_complex, borough, latitude, longitude) |>
  unique()

write_feather(hourly24, "working_data/StationInfo.feather")

check <- hourly24 |>
  group_by(station_complex_id, station_complex) |>
  summarise(lon = mean(longitude), lat = mean(latitude),
            .groups = "drop") 

ggplot(check, aes(x = lon, y = lat)) + 
  geom_point()

df <- read_sf("mta_data/SubwayLines.geojson")

ggplot(df) + geom_sf()

map_min_lon <- min(check$lon)
map_min_lat <- min(check$lat)

sintheta <- sin(0.94/2)
costheta <- cos(0.94/2)

check <- check |>
  mutate(mod_lon = lon - map_min_lon,
         mod_lat = lat - map_min_lat,
         station_num = as.numeric(station_complex_id)) |>
  mutate(new_lon = costheta*mod_lon - sintheta*mod_lat,
         new_lat = sintheta*mod_lon - costheta*mod_lat)

check_t <- check |>
  filter(station_num >= 154, station_num <= 157)


ggplot(check, aes(x = mod_lon, y = mod_lat, label = station_complex_id)) + 
  geom_text()
