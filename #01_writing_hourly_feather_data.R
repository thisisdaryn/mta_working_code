library(tidyverse)
library(feather)

hourly22 <- read_csv("mta_data/download2/MTA_Subway_Hourly_Ridership__Beginning_July_2020_20241025_2022.csv",
                     show_col_types = FALSE)
write_feather(hourly22, "working_data/hourly22_updated.feather")

hourly23 <- read_csv("mta_data/download2/MTA_Subway_Hourly_Ridership__Beginning_July_2020_20241025_2023.csv",
                     show_col_types = FALSE)
write_feather(hourly23, "working_data/hourly23_updated.feather")

hourly24 <- read_csv("mta_data/download2/MTA_Subway_Hourly_Ridership__Beginning_July_2020_20241025_2024.csv",
                     show_col_types = FALSE)
write_feather(hourly24, "working_data/hourly24_updated.feather")


### everything above here updated on October 25th 2024

bt <- read_csv("mta_data/MTA_Bridges___Tunnels_Hourly_Traffic_Rates__Beginning_2018_to_October_2024.csv",
               show_col_types = FALSE)

write_feather(bt, "working_data/BridgesAndTunnelsTraffic.feather")

bus_ridership_df <- read_csv("mta_data/download2/MTA_Bus_Hourly_Ridership__Beginning_February_2022_20241025_2024.csv", show_col_types = FALSE)

bus_ridership_df_up <- read_csv("mta_data/download2/")
write_feather(bus_ridership_df, "working_data/HourlyBusRiderShip_2024.feather")

daily_df <- read_csv("mta_data/MTA_Daily_Ridership_Data__Beginning_2020_20240929.csv", show_col_types = FALSE)

daily_df_updated <- read_csv("mta_data/download2/MTA_Daily_Ridership_Data__Beginning_2020_20241025_all_data.csv",
                             show_col_types = FALSE)

write_feather(daily_df, "working_data/DailyRidership.feather")

write_feather(daily_df_updated, "working_data/DailyRidership_updated.feather")

bus_hourly_df <- read_csv("mta_data/MTA_Bus_Hourly_Ridership__Beginning_February_2022_20241004.csv",
               show_col_types = FALSE)
write_feather(bus_hourly_df, "working_data/HourlyBusRidership.feather")


bus_capacity_hourly_df <- read_csv("mta_data/MTA_Express_Bus_Capacity__April_2023_-_September_2023_20241006.csv",
                                   show_col_types = FALSE)
write_feather(bus_capacity_hourly_df, "working_data/BusCapacity.feather")
