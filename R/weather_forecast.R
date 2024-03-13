library(renv)
library(httr2)
library(dplyr)
library(censusxy)
library(glue)
library(purrr)
library(ggplot2)
library(lubridate)

renv::status()
renv::snapshot()

rm(list=ls());cat('\f')
gc()

# source: https://3mw.albert-rapp.de/p/weather-api


NWS_base_url <- "https://api.weather.gov"
NWS_response <- request(base_url = NWS_base_url) %>%
  req_url_path_append(
    'points', 
    '38.8894,-77.0352'
  ) %>%
  # <httr2_request>
  # GET https://api.weather.gov/points/38.8894,-77.0352
  # Body: empty
  req_perform() 
# <httr2_response>
# GET https://api.weather.gov/points/38.8894,-77.0352
# Status: 200 OK
# Content-Type: application/geo+json
# Body: In memory (3091 bytes)

NWS_response %>%
  resp_body_json() %>%
  glimpse()


forecast_url <- NWS_response %>%
  resp_body_json() %>%
  pluck('properties', 'forecastHourly')


forecast_response <- request(forecast_url) %>%
  req_perform()

forecast_response %>%
  resp_body_json() %>%
  glimpse()

extracted_data <- forecast_response %>%
  resp_body_json() |>
  pluck('properties', 'periods') |>
  map_dfr( #iterates over each list and binds rows to a tibble
    \(x) {
      tibble(
        time = x |> pluck('startTime'), 
        temp_F = x |> pluck('temperature'), 
        rain_prob = x |> pluck('probabilityOfPrecipitation', 'value'), 
        forecast = x |> pluck('shortForecast')
      )
    }
  )

extracted_data %>% plot

ggplot() + 
  geom_boxplot(data = extracted_data, 
               aes(x = hour(ymd_hms(time, 
                                    tz = "America/New_York")), 
                   group = hour(ymd_hms(time, 
                                        tz = "America/New_York")),
                   y = rain_prob))


