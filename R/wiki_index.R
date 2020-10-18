# wiki
library(pageviews)

huaian_pageviews <- article_pageviews(project = "en.wikipedia", 
                                      c("Huai'an", "Beijing", "Copenhagen"),
                                      start = "2015100100",
                                      end = "2020100200")

huaian_pageviews <- article_pageviews(project = "en.wikipedia", "Copenhagen")
huaian_pageviews <- article_pageviews(project = "en.wikipedia", "Denmark")