# Load libraries
library(rvest)
library(dplyr)
library(purrr)
library(xml2)
library(stringr)
library(lubridate)
library(ggplot2)

x <- 20* 1:11
HIA_link <- paste0("https://flightaware.com/live/airport/ZSSH/departures?;offset=", 
                   x, ";order=actualdeparturetime;sort=DESC") %>% as.list()

HIA <- map(HIA_link, function(x){read_html(x)}) 

flight_ID <- 
  map(HIA, ~
  html_nodes(., css = ".prettyTable td:nth-child(1)") %>% 
    html_text() %>% stringr::str_remove_all(" ")) %>% unlist

destination <- 
  map(HIA, ~
  html_nodes(., css = ".hint span") %>% html_text()) %>% unlist
departure_time <- 
  map(HIA, ~
  html_nodes(., css = "td:nth-child(4)") %>% html_text()) %>% unlist()
arrival_time <- 
  map(HIA, ~
  html_nodes(., css = "td:nth-child(5)") %>% html_text()) %>% unlist()

HIA_df <- dplyr::tibble(flight_ID, destination, departure_time, arrival_time)

adjust_pm <- function(x){
  hour <- x %>% str_extract("^\\d{2}") %>% as.numeric()
  hour_12 <- ifelse(hour == 12, 12, hour + 12)
  min <- x %>% str_extract("\\d{2}$")
  paste0(hour_12, ":", min)
}

adjust_am_12 <- adjust_pm <- function(x){
  hour <- x %>% str_extract("^\\d{2}") %>% as.numeric()
  min <- x %>% str_extract("\\d{2}$")
  paste0(hour - 12, ":", min)
}

convert_to_min <- function(x){
  hour <- x %>% str_extract("^\\d{2}") %>% as.numeric()
  min <- x %>% str_extract("\\d{2}$") %>% as.numeric()
  hour * 60 + min
}

adjust_zero <- function(x){
  min <- x %>% str_extract("\\d{2}$")
  paste0("00", ":", min)
}

HIA_df_clean <- 
HIA_df %>% 
  # left_join(readr::read_csv("data/HIA_airport.csv"), by = c("destination" = "EN")) %>% mutate(destination = CN) %>% select(-CN) %>%
   mutate(day_departure = departure_time %>% str_extract("Mon|Tue|Wed|Thu|Fri|Sat|Sun"),
                                                      day_arrival = arrival_time %>% str_extract("Mon|Tue|Wed|Thu|Fri|Sat|Sun"),
                                                      departure_am_pm = departure_time %>% str_extract("AM|PM"),
                                                      arrival_am_pm = arrival_time %>% str_extract("AM|PM"),
                                                      departure_time = departure_time %>% str_extract("\\d{2}:\\d{2}"),
                                                      arrival_time = arrival_time %>% str_extract("\\d{2}:\\d{2}")) %>% 
  mutate(arrival_time = ifelse(arrival_am_pm == "PM", arrival_time %>% adjust_pm(), arrival_time),
         departure_time = ifelse(departure_am_pm == "PM", departure_time %>% adjust_pm(), departure_time)) %>% 
  mutate(arrival_h = arrival_time %>% str_extract("^\\d{2}") %>% as.numeric(),
         departure_h = departure_time %>% str_extract("^\\d{2}") %>% as.numeric()) %>% 
  mutate(arrival_h = ifelse(arrival_h == "12" & arrival_am_pm == "AM", 0, arrival_h)) %>% 
  mutate(arrival_time = ifelse(arrival_h == "0", arrival_time %>% adjust_zero, arrival_time)) %>% 
  mutate(duration = convert_to_min(.$arrival_time) - convert_to_min(.$departure_time)) %>% 
  mutate(duration = ifelse(duration < 0, 
                           convert_to_min(.$arrival_time) + 24*60 - convert_to_min(.$departure_time), 
                           duration)) %>% 
  mutate(day_departure = factor(day_departure, levels = c("Mon","Tue","Wed","Thu","Fri","Sat", "Sun"))) %>% 
  arrange(day_departure, departure_time, duration) %>% 
  distinct(flight_ID, day_departure, .keep_all = TRUE) %>% 
  filter(flight_ID != "CES7170") 

HIA_per_week_summary <- HIA_df_clean %>% group_by(destination) %>% summarise(per_week = n()) %>% arrange(-per_week)
HIA_df_clean_sum <- 
  HIA_df_clean %>% left_join(HIA_per_week_summary) %>% 
  mutate(destination = factor(destination))

saveRDS(HIA_df_clean_sum , "data/HIA_airport.csv")


HIA_airport <- readr::read_rds("data/HIA_airport.csv") %>% 
  mutate(destination = factor(destination, levels = HIA_per_week_summary %>% pull(destination))) %>% 
  mutate(carrier = flight_ID %>% stringr::str_extract("[:alpha:]{1,}"))


HIA_airport %>% 
  ggplot(aes(departure_h, destination)) + 
  geom_point(aes(size = duration, color = carrier)) + facet_grid( ~ day_departure) +
  ggthemes::theme_economist() + 
  ggtitle("HIA Flight in a Week")


