# Load libraries
library(rvest)
library(dplyr)
HIA <- read_html("https://flightaware.com/live/airport/ZSSH/scheduled/all")
flight_ID <- html_nodes(HIA, css = ".prettyTable td:nth-child(1)") %>% html_text() %>% stringr::str_remove_all(" ")
destination <- html_nodes(HIA, css = ".hint span") %>% html_text()
departure_time <- html_nodes(HIA, css = "td:nth-child(4)") %>% html_text()
arrival_time <- html_nodes(HIA, css = "td:nth-child(5)") %>% html_text()

dplyr::tibble(flight_ID, destination, departure_time, arrival_time)
