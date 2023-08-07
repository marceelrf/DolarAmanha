library(tidymodels)
library(modeltime)
library(timetk)


BRLUSD <- tibble(value = as.numeric(Dolar_360$bid),
       Date = Dates) %>% 
  arrange(Date)

#Dolar truth values 

# dolar_data <- list()
#update base

dolar_data[[length(dolar_data)+1]] <- BRLUSD

names(dolar_data)[length(dolar_data)] <- paste("Dolar series",Sys.Date())

names(dolar_data)
# Tidymodels --------------------------------------------------------------

BRLUSD %>% 
  plot_time_series(
    .date_var    = Date, 
    .value       = value, 
    #.facet_var   = id, 
    .smooth      = F, 
    .interactive = F
  )

# Data Preparation
FORECAST_HORIZON <- 7

BRLUSD_extended <- BRLUSD %>%
  #group_by(id) %>%
  future_frame(
    .length_out = FORECAST_HORIZON,
    .bind_data  = TRUE
  ) #%>% ungroup()

#Transform Function

lag_roll_transformer <- function(data){
  data %>%
    tk_augment_lags(value, .lags = 1:FORECAST_HORIZON) %>%
    tk_augment_slidify(
      contains("lag7"),
      .f = ~mean(.x, na.rm = T),
      .period  = 7,
      .partial = TRUE
    ) 
}

BRLUSD_rolling <- BRLUSD_extended %>%
  lag_roll_transformer()

# Split into Training and Future Data
train_data <- BRLUSD_rolling %>%
  drop_na()

future_data <- BRLUSD_rolling %>% 
  filter(is.na(value))


# Modeling
model_fit_lm_recursive <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ ., data = train_data) %>%
  # One additional step - use recursive()
  recursive(
    transform  = lag_roll_transformer,
    train_tail = tail(train_data, FORECAST_HORIZON)
  )


model_tbl <- modeltime_table(
  #model_fit_lm,
  model_fit_lm_recursive
) 

# Forecast using future data
forecast <- model_tbl %>%
  modeltime_forecast(
    new_data    = future_data,
    actual_data = BRLUSD
  )
# Visualize the forecast
forecast %>%
  plot_modeltime_forecast(
    .interactive        = FALSE,
    .conf_interval_show = FALSE
  )

# save the forecast
# forecast_list <- list()

forecast_list[[length(forecast_list) + 1]] <- forecast %>%
  select(value = .value,
         Dates = .index) %>% 
  slice_max(Dates,n = 7)
names(forecast_list)[length(forecast_list)] <- paste("Dolar series",Sys.Date())
