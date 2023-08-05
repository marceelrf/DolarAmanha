library(quantmod)
library(httr)
library(jsonlite)
library(tidyverse)
library(glue)


lookback <- 360
# API
url <- glue("https://economia.awesomeapi.com.br/json/daily/USD-BRL/{lookback}")

#Collect last 360 values
data_from <- GET(url)
Dolar_360 <- (fromJSON(content(data_from, "text")))

Dates <- seq(from = Sys.Date(), to = (Sys.Date() - lookback + 1), by = -1)

tibble(value = as.numeric(Dolar_360$bid),
       Date = Dates) %>% 
  ggplot(aes(x = Date, y = value)) +
  geom_point(color = "black",size =2,alpha = .6) +
  geom_line(color = "black") +
  labs(title = glue("{Sys.Date() - 360 + 1} - {Sys.Date()}"))
