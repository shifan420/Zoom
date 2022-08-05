library(tm)
library(twitteR)
library(tidytext)
library(stopwords)
library(ggplot2)
library(syuzhet)
library(lubridate)
library(dplyr)
library(wordcloud2)
library("openssl")
library("httpuv")
library(tidyquant)
library(magrittr)
library(Hmisc)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
zoom <- tq_get("ZM",
             from = "2020-03-19",
             to = "2020-03-25", #not included
             get = "stock.prices")
zoom = as.data.frame(zoom)

ms <- tq_get("MSFT",
               from = "2020-03-19",
               to = "2020-03-25", #not included
               get = "stock.prices")
ms = as.data.frame(ms)

google <- tq_get("GOOG",
             from = "2020-03-19",
             to = "2020-03-25", #not included
             get = "stock.prices")
google = as.data.frame(google)

cisco <- tq_get("CSCO",
             from = "2020-03-19",
             to = "2020-03-25", #not included
             get = "stock.prices")
cisco = as.data.frame(cisco)

zoom$return = (zoom$close - lag(zoom$close))/lag(zoom$close)
ms$return = (ms$close - lag(ms$close))/lag(ms$close)
google$return = (google$close - lag(google$close))/lag(google$close)
cisco$return = (cisco$close - lag(cisco$close))/lag(cisco$close)
zoom = zoom[-1,c(1,2,9)]
ms = ms[-1,c(1,2,9)]
google = google[-1,c(1,2,9)]
cisco = cisco[-1,c(1,2,9)]
data = data.frame(zoom$date,zoom$return,google$return,ms$return,cisco$return)
colnames(data) = c('date','zoom','google','ms','cisco')
ggplot(data, aes(x=date)) + 
  geom_line(aes(x=date,y = zoom, color = 'red')) + geom_line(aes(x=date,y = google))+geom_line(aes(x=date,y = ms))+geom_line(aes(x=date,y = cisco))

abr = zoom$return-(google$return+cisco$return+ms$return)/3
CAR = sum(abr)
