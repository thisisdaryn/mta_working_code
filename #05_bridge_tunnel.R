library(tidyverse)
library(feather)
library(readxl)


plaza <- read_excel("working_data/PlazaLookup.xlsx")
bt <- read_feather("working_data/BridgesAndTunnelsTraffic.feather") |> 
  left_join(plaza, by = c("Plaza ID")) |>
  rename(ez =  `# Vehicles - E-ZPass`, toll = `# Vehicles - VToll`) |>
  mutate(traffic = ez + toll,
         date = mdy(Date),
         year = year(date))


bt23 <- bt |>
  filter(year == 2023)

wday_levels <-  c("Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday", "Saturday", "Sunday") 

inout_df <- bt |>
  group_by(date, Hour, Direction, `Plaza ID`) |>
  summarise(traffic = sum(traffic), .groups = "drop") |>
  mutate(wday = weekdays(date)) |>
  group_by(wday, Hour, Direction, `Plaza ID`) |>
  summarise(traffic = median(traffic), .groups = "drop") |>
  mutate(plt_traffic = if_else(Direction == "I", traffic, -1*traffic),
         wday = factor(wday, levels = wday_levels),
         plt_dir = if_else(Direction == "I", "Inbound", "Outbound"))


ggplot(data = inout_df, 
       aes(x = Hour, y = plt_traffic, fill = plt_dir)) +
  geom_col() + facet_grid(wday~`Plaza ID`, scales = "free") + 
  theme_bw() + 
  theme(legend.position = "top", legend.title = element_blank()) 
