library(tidyverse)
library(feather)

daily_df <- read_feather("working_data/DailyRidership_updated.feather") |>
  rename(subway = 2, bus = 4) |>
  mutate(date = mdy(Date)) |>
  select(date, subway, bus) |>
  mutate(wday = weekdays(date),
         month_day = day(date),
         month = month(date),
         week_increment = ifelse(month_day == 1 | wday == "Sunday", 1, 0))  |>
  group_by(month) %>% 
  mutate(week = cumsum(week_increment),
         text_month = months(date)) %>% 
  ungroup() |>
  mutate(wday = str_sub(wday, 1, 3)) |>
  mutate(year = year(date)) |>
  filter(year == 2023)


wday_vec <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
daily_df$wday <- factor(daily_df$wday, levels = wday_vec)
month_vec <- c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December") 
daily_df$text_month <- factor(daily_df$text_month, 
                                  levels =  month_vec)

weekday_days <-  c("Mon", "Tue", "Wed", "Thu", "Fri")
weekend_days <-  c("Sun", "Sat")
week2_df <- daily_df |>
  filter(wday %in% weekday_days)
weekend2_df <- daily_df |>
  filter(wday %in% weekend_days)

median(week2_df$subway)
median(weekend2_df$subway)

week2_df$more4 <- ifelse(week2_df$subway >= 4000000, TRUE, FALSE)
sum(week2_df$more4)



sub_str <- "In 2023, median week day subway ridership was 3,629,722 and median weekend ridership was 2,075,651. Minimum daily ridership was observed on December 25th (1,273,729 estimated riders) and maximum daily ridership was observed on September 20th (4,179,861 estimated riders). Daily subway ridership exceeded 4 million on 39 occasions."
#subway_cal <- 
  
ggplot(daily_df, aes(x = wday, y = week)) + 
  geom_tile(aes(fill = subway), colour = "white") +
  facet_wrap(~text_month, scales = "free") + 
  scale_y_reverse() + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "D", 
                       labels = label_number(scale = 1e-6, suffix = " million")) + 
  scale_x_discrete(position = "top") + 
  ylab("") + xlab("") + labs(fill = "Ridership") + 
  ggtitle("MTA Subway Ridership 2023") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"), 
        #plot.title = element_text(size = 14, hjust = 0.5),
        strip.placement = "outside",
        plot.title = element_text(family = "Arial Narrow", face = "bold", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow", size = 8)) + 
  labs(subtitle = str_wrap(sub_str, 145), caption = "Daryn Ramsden @thisisdaryn
        Source Data Set: MTA Daily Ridership Data: Beginning 2020")


median(week2_df$bus)
median(weekend2_df$bus)

bus_sub_str <- "In 2023, median week day bus ridership was 1,368,402 and median weekend bus ridership was 715,608. Minimum daily ridership was observed on December 25th (381,879 estimated riders) and maximum daily ridership was observed on May 11th (1,535,264 estimated riders). Daily bus ridership exceeded 1 million on 248 occasions."

ggplot(daily_df, aes(x = wday, y = week)) + 
  geom_tile(aes(fill = bus), colour = "white") +
  facet_wrap(~text_month, scales = "free") + 
  scale_y_reverse() + 
  theme_minimal() + 
  scale_fill_viridis_c(option = "D", 
                       labels = label_number(scale = 1e-6, suffix = " million")) + 
  scale_x_discrete(position = "top") + 
  ylab("") + xlab("") + labs(fill = "Ridership") + 
  ggtitle("MTA Bus Ridership 2023") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"), 
        #plot.title = element_text(size = 14, hjust = 0.5),
        strip.placement = "outside",
        plot.title = element_text(family = "Arial Narrow", face = "bold", size = 18, margin=margin(b=10)),
        plot.subtitle = element_text(family = "Arial Narrow", size = 12), 
        panel.grid.major.x = element_blank(),
        axis.title = element_text(family = "Arial Narrow"),
        axis.text = element_text(family = "Arial Narrow", size = 8)) + 
  labs(subtitle = str_wrap(bus_sub_str, 145), caption = "Daryn Ramsden @thisisdaryn
        Source Data Set: MTA Daily Ridership Data: Beginning 2020")





bus_cal <- ggplot(daily_df, aes(x = wday, y = week)) + 
  geom_tile(aes(fill = bus), colour = "white") +
  facet_wrap(~text_month, scales = "free") + 
  scale_y_reverse() + 
  theme_minimal() + 
  scale_fill_viridis_c() + 
  scale_x_discrete(position = "top") + 
  ylab("") + xlab("") + labs(fill = "Ridership") + 
  ggtitle("MTA Bus Ridership (2023)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"), 
        plot.title = element_text(size = 14, hjust = 0.5),
        strip.placement = "outside")

bus_cal
