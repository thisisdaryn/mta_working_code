library(feather)
library(tidyverse)
library(scales)
library(hrbrthemes)

h22 <- read_feather("working_data/hourly22_updated.feather")
h23 <- read_feather("working_data/hourly23_updated.feather")
h24 <- read_feather("working_data/hourly24_updated.feather")

rows23 <- dim(h23)[1]

set.seed(1729)
sample_rows <- sample(rows23, 1234560)

test <- h23[sample_rows, ]


hour_levels <- c("12:00:00 AM", "01:00:00 AM", "02:00:00 AM", "03:00:00 AM",
                 "04:00:00 AM", "05:00:00 AM", "06:00:00 AM", "07:00:00 AM",
                 "08:00:00 AM", "09:00:00 AM", "10:00:00 AM", "11:00:00 AM", 
                 "12:00:00 PM", "01:00:00 PM", "02:00:00 PM", "03:00:00 PM", 
                 "04:00:00 PM", "05:00:00 PM", "06:00:00 PM", "07:00:00 PM",
                 "08:00:00 PM", "09:00:00 PM", "10:00:00 PM", "11:00:00 PM")

disp_hour_levels <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
                 "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", 
                 "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", 
                 "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")

disp_3hour_vals <- c("12 AM", "3 AM", "6 AM", "9 AM", "12 PM", "3 PM",  
                      "6 PM", "9 PM")
disp_3hour_labels <- c("12am", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")

disp_hour_labels <- c("12", "1", "2", "3", "4", "5", 
                      "6", "7", "8", "9", "10", "11", 
                      "12", "1", "2", "3", "4", "5", 
                      "6", "7", "8", "9", "10", "11")

wday_levels <-  c("Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday", "Saturday", "Sunday") 

month_levels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")

payment_cols <- c("MetroCard" = "#F0AD25", "OMNY" = "#004C9F")

#F0AD25", 

"#F0AD25"

"#9F361C"

hourly_ridership <- h24 |> 
  select(transit_timestamp, transit_mode, station_complex_id,  station_complex, 
         ridership) |>
  #separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge") |>
  group_by(transit_timestamp) |>
  summarise(ridership = sum(ridership), .groups = "drop") |>
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge") |>
  mutate(date = mdy(date),
         wday = weekdays(date)) |> 
  mutate(rep_hour = factor(hour, levels = hour_levels))

check24 <- h24 |>
  select(transit_timestamp) |>
  unique() |> 
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge") |>
  arrange(desc(date)) 


ggplot(hourly_ridership,
       aes(x = rep_hour, y = ridership)) +
  geom_boxplot() + facet_wrap(~wday)

wday_median_ridership <- hourly_ridership |>
  group_by(wday, hour, rep_hour) |>
  summarise(ridership = median(ridership), .groups = "drop") |>
  separate(hour, into = c("hr", "min", "sec", "ampm"), remove = FALSE) |>
  mutate(disp_hour = paste(as.integer(hr), ampm)) |>
  mutate(disp_hour = factor(disp_hour, levels = disp_hour_levels),
         disp_wday = factor(wday, levels = wday_levels))


ggplot(wday_median_ridership,
       aes(x = disp_hour, y = ridership)) +
  geom_col(fill = "#0F61A9") + facet_wrap(~disp_wday, scales = "free_x") +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 3))
  #scale_x_discrete(breaks = disp_3hour_vals, labels = disp_hour) +
  scale_x_discrete(breaks = disp_3hour_vals) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + #theme_ipsum() +
  labs(y = "Median ridership", x = "",
       title = "MTA Hourly Ridership 2024",
       subtitle = "Median hourly ridership (inclusive of Subway, Long Island Railroad and Trams) for the period Jan 1st 2024 to September 30th 2024.",
       caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Subway Hourly Ridership: Beginning July 2020") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8,  hjust = .5, vjust = .5, angle = 90, face = "plain")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(family = "Arial Narrow", face = "bold", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow"),
        strip.background =element_rect(fill="white"))

st23_totals <- h23 |>
  group_by(station_complex) |>
  summarise(`2023 ridership` = sum(ridership, na.rm = TRUE))

metroomny22 <- h22 |> 
  select(transit_timestamp, transit_mode, station_complex_id, station_complex,
         borough, payment_method, ridership)

metroomny23 <- h23 |>
  select(transit_timestamp, transit_mode, station_complex_id, station_complex,
         borough, payment_method, ridership)
metroomny24 <- h24  |>
  select(transit_timestamp, transit_mode, station_complex_id, station_complex,
         borough, payment_method, ridership)

metroomny <- bind_rows(metroomny22, metroomny23, metroomny24)

metroomny_hourly_totals <- metroomny |>
  group_by(transit_timestamp, payment_method) |>
  summarise(ridership = sum(ridership), .groups = "drop") |>
  separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge") |>
  mutate(date = mdy(date)) |>
  mutate(wday = weekdays(date),
         month = months(date),
         year = year(date)) |> 
  mutate(rep_hour = factor(hour, levels = hour_levels)) |>
  group_by(year, month, payment_method) |>
  summarise(ridership = sum(ridership), .groups = "drop") |>
  mutate(month = factor(month, levels = month_levels)) |>
  arrange(year, month) |>
  mutate(plt_date = paste0(substr(month, 1,3), " '", substr(year, 3, 4))) |>
  mutate(plt_date = fct_inorder(plt_date)) |>
  mutate(plt_payment = if_else(payment_method == "metrocard", "MetroCard", "OMNY"))

library(extrafont)
#font_import()
loadfonts(device = "win")
ggplot(metroomny_hourly_totals,
       aes(x = plt_date, y = ridership, fill = plt_payment)) +
  geom_col(position = position_fill(), alpha = 0.9) + 
  geom_hline(yintercept = 0.5, linetype = "solid", color = "#9F361C", linewidth = 1.5) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 3)) + 
  theme_minimal() + 
  scale_fill_manual(values = payment_cols) + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(family = "Arial Narrow", face = "bold", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow")) +
  labs(x = "", y = "Ridership", title = "Usage of OMNY vs MetroCard on Subway 2022 to 2024",
       subtitle = "Total monthly subway ridership using the OMNY tap-to-pay system exceeded that of the MetroCard for the first time in January 2024.", 
       caption = "Daryn Ramsden @thisisdaryn
       Source Data Set: MTA Subway Hourly Ridership: Beginning July 2020")

check <- metroomny_hourly_totals |>
  arrange(year, month)
