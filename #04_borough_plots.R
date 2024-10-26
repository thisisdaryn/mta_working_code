library(feather)
library(tidyverse)
library(scales)
library(hrbrthemes)

get_ridership_borough_cols <- function(in_df){
  out_df <- in_df |> 
    select(transit_timestamp, borough, ridership) |>
    mutate(date = str_sub(transit_timestamp, 1, 10)) |>
    
    #separate(transit_timestamp, into = c("date", "hour"), sep = " ", extra = "merge") #|>
    #mutate(date = mdy(date)) |>
    group_by(date, borough) |> 
    summarise(ridership = sum(ridership), .groups = "drop") |>
    mutate(date = mdy(date), 
           wday = weekdays(date)) |>
    group_by(borough, wday) |>
    summarise(med_ridership = median(ridership), .groups = "drop")
  
  return(out_df)
}


h22 <- read_feather("working_data/hourly22.feather") |> get_ridership_borough_cols()
h23 <- read_feather("working_data/hourly23.feather") |> get_ridership_borough_cols()
h24 <- read_feather("working_data/hourly24.feather") |> get_ridership_borough_cols()


rows24 <- dim(h24)[1]



test22 <- h24[sample(rows24, 12345), ] |> get_ridership_borough_cols()




plot_weekly_ridership_boro <- function(in_df){
  

  
  out_plt <- ggplot(in_df, aes(x = wday, y = med_ridership)) + 
    geom_col() + facet_wrap(~borough)
  
  return(out_plt)
  
}

plot24 <- plot_weekly_ridership_boro(h24)

plot24
