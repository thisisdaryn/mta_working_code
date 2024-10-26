library(tidyverse)
library(feather)

source("lib01_levels.R")

df <- read_feather("working_data/HourlyBusRiderShip_2024.feather")


assign_boro <- function(in_df){
  
  out_df <- in_df |>
    mutate(boro = case_when(str_detect(bus_route, "BR") ~ "Brooklyn-Manhattan",
                            str_detect(bus_route, "BxM") ~ "Bronx-Manhattan",
                            str_detect(bus_route, "QM") ~ "Queens-Manhattan",
                            str_detect(bus_route, "SIM") ~ "Staten Island Express",
                            str_detect(bus_route, "BX") ~ "Bronx",
                            str_detect(bus_route, "B") ~ "Brooklyn",
                            str_detect(bus_route, "Q") ~ "Queens",
                            str_detect(bus_route, "M") ~ "Manhattan",
                            str_detect(bus_route, "S") ~ "Staten Island"))
           
  return(out_df)
}


df24 <- df |>
  #filter(str_detect(transit_timestamp, "/2024")) |>
  assign_boro()


route_totals <- df24 |>
  group_by(bus_route) |>
  summarise(ridership = sum(ridership, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(ridership)) |>
  assign_boro()

#|>
  #mutate(br2 = str_replace_all(bus_route, "\\+", "")) |>
  #group_by(br2) |>
  #summarise(ridership = sum(ridership)) |>
  #arrange(desc(ridership)) 

top_routes <- route_totals$bus_route[1:50]

weekdays_vec <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekends_vec <- c("Saturday", "Sunday")


m90 <- df24 |>
  filter(bus_route == "M90")

man24 <- df24 |>
  filter(boro == "Manhattan") |>
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge") 

b24 <- df24 |>
  filter(boro == "Brooklyn") |>
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge")
q20B <- df24 |>
  filter(bus_route == "Q20B")
q24 <- df24 |>
  filter(boro == "Queens") |>
  bind_rows(q20B) |>
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge")

bx24 <- df24 |>
  filter(boro == "Bronx") |>
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge")

man_top24_hourly_totals <- man24 |> 
  group_by(date, hour, bus_route) |>
  summarise(ridership = sum(ridership), .groups = "drop")  |>
  mutate(date = mdy(date),
         wday = weekdays(date)) |> 
  mutate(daytype = case_when(wday %in% weekdays_vec ~ "Weekday",
                             wday %in% weekends_vec ~ "Weekend")) |>
  mutate(rep_hour = factor(hour, levels = rev(hour_levels))) |>
  filter(!(bus_route == "M90"))


btop24_hr <- b24 |> 
  group_by(date, hour, bus_route) |>
  summarise(ridership = sum(ridership), .groups = "drop")  |>
  mutate(date = mdy(date),
         wday = weekdays(date)) |> 
  mutate(daytype = case_when(wday %in% weekdays_vec ~ "Weekday",
                             wday %in% weekends_vec ~ "Weekend")) |>
  mutate(rep_hour = factor(hour, levels = rev(hour_levels))) |>
  filter(!(bus_route == "M90"))


qtop24_hr <- q24 |> 
  group_by(date, hour, bus_route) |>
  summarise(ridership = sum(ridership), .groups = "drop")  |>
  mutate(date = mdy(date),
         wday = weekdays(date)) |> 
  mutate(daytype = case_when(wday %in% weekdays_vec ~ "Weekday",
                             wday %in% weekends_vec ~ "Weekend")) |>
  mutate(rep_hour = factor(hour, levels = rev(hour_levels))) |>
  filter(!(bus_route == "M90"))

bxtop24_hr <- bx24 |>
  group_by(date, hour, bus_route) |>
  summarise(ridership = sum(ridership), .groups = "drop")  |>
  mutate(date = mdy(date),
         wday = weekdays(date)) |> 
  mutate(daytype = case_when(wday %in% weekdays_vec ~ "Weekday",
                             wday %in% weekends_vec ~ "Weekend")) |>
  mutate(rep_hour = factor(hour, levels = rev(hour_levels))) |>
  filter(!(bus_route == "M90"))

ride_cat_levels <- c("0", "1 - 249", "250 - 499", "500 - 749", "750 - 999", 
                     "1,000 - 1,499", "1,500+")

man_top_24_median <- man_top24_hourly_totals |>
  group_by(bus_route, daytype, rep_hour) |>
  summarise(med_ridership = median(ridership, na.rm = TRUE), .groups = "drop") |>
  mutate(ride_cat = case_when(med_ridership < 1 ~ "0",
                              med_ridership < 250 ~ "1 - 249", 
                              med_ridership < 500 ~ "250 - 499",
                              med_ridership < 750 ~ "500 - 749",
                              med_ridership < 1000 ~ "750 - 999",
                              med_ridership < 1500 ~ "1,000 - 1,499",
                              med_ridership >= 1500 ~ "1,500+")) |>
  mutate(`Median ridership` = factor(ride_cat, levels = ride_cat_levels)) |>
  filter(daytype == "Weekday") |>
  mutate(bus_route = factor(bus_route, levels = man_bus_levels))


man_top_24_median_wknd <- man_top24_hourly_totals |>
  group_by(bus_route, daytype, rep_hour) |>
  summarise(med_ridership = median(ridership, na.rm = TRUE), .groups = "drop") |>
  mutate(ride_cat = case_when(med_ridership < 1 ~ "0",
                              med_ridership < 250 ~ "1 - 249", 
                              med_ridership < 500 ~ "250 - 499",
                              med_ridership < 750 ~ "500 - 749",
                              med_ridership < 1000 ~ "750 - 999",
                              med_ridership >= 1000 ~ "1000+")) |>
  mutate(`Median ridership` = factor(ride_cat, levels = ride_cat_levels)) |>
  filter(daytype == "Weekend") |>
  mutate(bus_route = factor(bus_route, levels = man_bus_levels))

scale_fill_levels <- c("0" = "#EFF9F0", 
                       "1 - 249" = "#EAC8C8",
                       "250 - 499" = "#E5969F", 
                       "500 - 749" = "#DB324D", 
                       "750 - 999" = "#96273B", 
                       "1,000 - 1,499" = "#511E39",
                       "1,500+" = "black")


### manhattan weekday 

man_bus_sub <- "Median hourly bus ridership for Manhattan Bus Routes for the period January 1st 2024 to September 30th 2024."
ggplot(data = man_top_24_median,
                  mapping = aes(y = rep_hour, x = bus_route, fill = `Median ridership`)) +
  geom_tile(colour = "white", na.rm = TRUE) + 
  scale_fill_manual(values = scale_fill_levels) +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(breaks = hour_levels, labels = disp_hour_levels) +
  labs(x = "", y = "", title = "Manhattan Weekday Bus Ridership",
       caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Bus Hourly Ridership: Beginning February 2022",
       subtitle = man_bus_sub) + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 12, face = "bold"),
        legend.title = element_text()) + 
  theme(plot.title = element_text(family = "Arial Narrow", size = 18, margin=margin(b=10)),
plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
panel.grid.major.x = element_blank(),
axis.title = element_text(family = "Arial Narrow"),
axis.text = element_text(family = "Arial Narrow", size = 8))


b_remove <- c("B90", "B94", "B96", "B98", "Q20B",  "BM1", "BM2", 
              "BM3", "BM4",  "BM5")

bk_bus_levels <- c("B1","B2", "B3",  "B4", "B6", "B7" , "B8",  "B9",   
"B11", "B12", "B13", "B14", "B15", "B16", "B17", "B20", 
"B24",  "B25",  "B26",  "B31",  "B32",  "B35",  "B36",  "B37",  "B38",  "B39",    
"B41", "B42", "B43", "B44", "B44+", "B45", "B46",  "B46+", "B47",  "B48",  "B49", "B52", 
"B54",  "B57",   "B60",  "B61", "B62", "B63", "B64", "B65", "B67", "B68", "B69", 
"B70",  "B74",  "B82",  "B82+", "B83",  "B84",  "B93", "B100", "B103") 

b_med <- btop24_hr  |>
  group_by(bus_route, daytype, rep_hour) |>
  summarise(med_ridership = median(ridership, na.rm = TRUE), .groups = "drop") |>
  mutate(ride_cat = case_when(med_ridership < 1 ~ "0",
                              med_ridership < 250 ~ "1 - 249", 
                              med_ridership < 500 ~ "250 - 499",
                              med_ridership < 750 ~ "500 - 749",
                              med_ridership < 1000 ~ "750 - 999",
                              med_ridership < 1500 ~ "1,000 - 1,499",
                              med_ridership >= 1500 ~ "1,500+")) |>
  mutate(`Median ridership` = factor(ride_cat, levels = ride_cat_levels)) |>
  filter(daytype == "Weekday") |>
  filter(!(bus_route %in% b_remove)) |>
  mutate(bus_route = factor(bus_route, levels = bk_bus_levels))

q_remove <- c("Q107", "Q108", "Q90", "Q92", "Q93", "Q94")

  
q_bus_levels <- c( "Q1", "Q2", "Q3",  "Q4", "Q5", "Q06",  "Q07",  "Q08",  "Q09",
                   "Q10",  "Q11", "Q12", "Q13",  "Q15",  "Q15A", "Q16",  "Q17",  
                   "Q18",  "Q19",   "Q20A", "Q20B", "Q21",  "Q22",  "Q23",  "Q24",  
                   "Q25",  "Q26", "Q27",  "Q28",  "Q29",  "Q30",  "Q31",  "Q32",
                   "Q33",  "Q34",  "Q35",  "Q36",  "Q37",  "Q38",  "Q39", "Q40",  
                   "Q41", "Q42",  "Q43", "Q44+", "Q46",  "Q47", "Q48", "Q49", 
                   "Q50","Q52+", "Q53+", "Q54",  "Q55",  "Q56",  "Q58",  "Q59",
                   "Q60", "Q64",  "Q65",  "Q66",  "Q67",  "Q69",  "Q70+", "Q72",
                   "Q76",  "Q77",  "Q83",  "Q84",  "Q85",  "Q88",  "Q95",  "Q96",
                   "Q100", "Q101", "Q102", "Q103", "Q104", "Q110", "Q111", "Q112", 
                   "Q113", "Q114")

q_med <- qtop24_hr |> 
  group_by(bus_route, daytype, rep_hour) |>
  summarise(med_ridership = median(ridership, na.rm = TRUE), .groups = "drop") |>
  mutate(ride_cat = case_when(med_ridership < 1 ~ "0",
                              med_ridership < 250 ~ "1 - 249", 
                              med_ridership < 500 ~ "250 - 499",
                              med_ridership < 750 ~ "500 - 749",
                              med_ridership < 1000 ~ "750 - 999",
                              med_ridership < 1500 ~ "1,000 - 1,499",
                              med_ridership >= 1500 ~ "1,500+")) |>
  mutate(`Median ridership` = factor(ride_cat, levels = ride_cat_levels)) |>
  filter(daytype == "Weekday") |>
  filter(!(bus_route %in% q_remove))  |>
mutate(bus_route = factor(bus_route, levels = q_bus_levels))
queens_bus_sub <- "Median hourly bus ridership for Queens Bus Routes for the period January 1st 2024 to September 30th 2024."
ggplot(data = q_med,
         mapping = aes(y = rep_hour, x = bus_route, fill = `Median ridership`)) +
    geom_tile(colour = "white", na.rm = TRUE) + 
    scale_fill_manual(values = scale_fill_levels) +
    scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
    scale_y_discrete(breaks = hour_levels, labels = disp_hour_levels) +
    labs(x = "", y = "", title = "Queens Weekday Bus Ridership",
         caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Bus Hourly Ridership: Beginning February 2022",
         subtitle = queens_bus_sub) + 
    theme_minimal() + 
    theme(legend.position = "bottom", axis.text.x = element_text(size = 12, face = "bold"),
          legend.title = element_text()) + 
    theme(plot.title = element_text(family = "Arial Narrow", size = 18, margin=margin(b=10)),
          plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
          panel.grid.major.x = element_blank(),
          axis.title = element_text(family = "Arial Narrow"),
          axis.text = element_text(family = "Arial Narrow", size = 6))


bx_bus_sub <- "Median hourly bus ridership for Bronx Bus Routes for the period January 1st 2024 to September 30th 2024."


bxtop24_hr <- bx24 |>
  group_by(date, hour, bus_route) |>
  summarise(ridership = sum(ridership), .groups = "drop")  |>
  mutate(date = mdy(date),
         wday = weekdays(date)) |> 
  mutate(daytype = case_when(wday %in% weekdays_vec ~ "Weekday",
                             wday %in% weekends_vec ~ "Weekend")) |>
  mutate(rep_hour = factor(hour, levels = rev(hour_levels)))

bx_remove <- c("BX18A", "BX18B", "BX90",  "BXM1",  "BXM10", "BXM11", "BXM18", 
               "BXM2",  "BXM3", "BXM4",  "BXM6",  "BXM7",  "BXM8",  "BXM9" )

bx_bus_levels <-  c("BX1", "BX2", "BX3", "BX4", "BX4A",  "BX5",   "BX6" ,  
  "BX6+",  "BX7",   "BX8",   "BX9", "BX10",  "BX11",  "BX12",  "BX12+", "BX13",  
  "BX15",  "BX16",  "BX17",  "BX19", "BX20", "BX21",  "BX22",  "BX23",  "BX24",
  "BX25",  "BX26",  "BX27",  "BX28", "BX29", "BX30",  "BX31",  "BX32",  "BX33",
  "BX34",  "BX35",  "BX36",  "BX38", "BX39",  "BX40",  "BX41",  "BX41+", 
  "BX42",  "BX46")    

 

bx_med <- bxtop24_hr |> 
  group_by(bus_route, daytype, rep_hour) |>
  summarise(med_ridership = median(ridership, na.rm = TRUE), .groups = "drop") |>
  mutate(ride_cat = case_when(med_ridership < 1 ~ "0",
                              med_ridership < 250 ~ "1 - 249", 
                              med_ridership < 500 ~ "250 - 499",
                              med_ridership < 750 ~ "500 - 749",
                              med_ridership < 1000 ~ "750 - 999",
                              med_ridership < 1500 ~ "1,000 - 1,499",
                              med_ridership >= 1500 ~ "1,500+")) |>
  mutate(`Median ridership` = factor(ride_cat, levels = ride_cat_levels)) |>
  filter(daytype == "Weekday") |>
  filter(!(bus_route %in% bx_remove))   |>
  mutate(bus_route = factor(bus_route, levels = bx_bus_levels))

ggplot(data = bx_med,
       mapping = aes(y = rep_hour, x = bus_route, fill = `Median ridership`)) +
  geom_tile(colour = "white", na.rm = TRUE) + 
  scale_fill_manual(values = scale_fill_levels) +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(breaks = hour_levels, labels = disp_hour_levels) +
  labs(x = "", y = "", title = "Bronx Weekday Bus Ridership",
       caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Bus Hourly Ridership: Beginning February 2022",
       subtitle = bx_bus_sub) + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 12, face = "bold"),
        legend.title = element_text()) + 
  theme(plot.title = element_text(family = "Arial Narrow", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow", size = 8))

sort(unique(bx_med$bus_route))

bk_bus_sub <- "Median hourly bus ridership for Brooklyn Bus Routes for the period January 1st 2024 to September 30th 2024."

ggplot(data = b_med,
       mapping = aes(y = rep_hour, x = bus_route, fill = `Median ridership`)) +
  geom_tile(colour = "white", na.rm = TRUE) + 
  scale_fill_manual(values = scale_fill_levels) +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(breaks = hour_levels, labels = disp_hour_levels) +
  labs(x = "", y = "", title = "Brooklyn Weekday Bus Ridership",
       caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Bus Hourly Ridership: Beginning February 2022",
       subtitle = bk_bus_sub) + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 12, face = "bold"),
        legend.title = element_text()) + 
  theme(plot.title = element_text(family = "Arial Narrow", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow", size = 8))


ggplot(data = q_med,
       mapping = aes(y = rep_hour, x = bus_route, fill = `Median ridership`)) +
  geom_tile(colour = "white", na.rm = TRUE) + 
  scale_fill_manual(values = scale_fill_levels) +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(breaks = hour_levels, labels = disp_hour_levels) +
  labs(x = "", y = "", title = "Brooklyn Weekday Bus Ridership",
       caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Bus Hourly Ridership: Beginning February 2022",
       subtitle = bk_bus_sub) + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 12, face = "bold"),
        legend.title = element_text()) + 
  theme(plot.title = element_text(family = "Arial Narrow", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow", size = 8))




        

