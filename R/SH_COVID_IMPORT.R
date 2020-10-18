# shanghai
library(dplyr)
library(ggplot2)

page <- 1:23
html <- paste0("http://wsjkw.sh.gov.cn/yqtb/index_", page ,".html")
html[1] <- "http://wsjkw.sh.gov.cn/yqtb/index.html"

html_l <- html %>% as.list()

SH_COVID <- purrr::map(html_l, purrr::possibly(~xml2::read_html(.),NA)) %>% purrr::discard(is.na)

SH_COVID_title <- purrr::map(SH_COVID, function(x){html_nodes(x,css = ".list-date a") %>% html_text()}) %>% unlist
SH_COVID_title %>% str_extract("\\d{1,2}月\\d{1,2}日")
SH_COVID_title %>% str_extract("境外输入\\d{1,}例") %>% str_extract("\\d{1,}")

SH_COVID_df <- 
data.frame(
  date = SH_COVID_title %>% str_extract("\\d{1,2}月\\d{1,2}日") %>% str_replace_all("月","/") %>% str_remove_all("日") %>% as.Date(format = "%m/%d"),
  import_cases =SH_COVID_title %>% str_extract("境外输入\\d{1,}例") %>% str_extract("\\d{1,}")) %>% 
  filter(! is.na(date), date >= as.Date("2020-03-17")) %>% 
  mutate(import_cases = ifelse(is.na(import_cases), 0, import_cases) %>% as.numeric()) %>% 
  mutate(weekday = weekdays(date),
         week = lubridate::isoweek(date))

SH_COVID_import_daily <- 
SH_COVID_df %>% 
  ggplot(aes(date, import_cases)) + 
  geom_point(aes(color = weekday, size = import_cases)) + 
  ggthemes::theme_economist_white() + 
  ggtitle("Shanghai COVID-19 Imported Cases (Daily)") + geom_line()
  
plotly::ggplotly(SH_COVID_import_daily)

SH_COVID_import_weekly <-
SH_COVID_df %>% group_by(week) %>% 
  summarise(week_mean = sum(import_cases))  %>% ggplot(aes(week, week_mean)) + 
  geom_bar(stat="identity") + geom_point() + geom_line()  + 
  ggtitle("Shanghai COVID-19 Imported Cases (Weekly Cumulative)") +ggthemes::theme_economist_white()  
plotly::ggplotly(SH_COVID_import_weekly)

