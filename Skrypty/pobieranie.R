library(ecmwfr)
library(tidync)
library(dplyr)
library(lubridate)
library(sf)               # analizy przestrzenne
library(rnaturalearth)    # granice Polski
library(keyring)

options(keyring_backend = "env")
# Dane logowania do Copernicus CDS są w pliku .Renviron.
mail  <- Sys.getenv("CDS_USER")
klucz <- Sys.getenv("CDS_KEY")
if (mail == "" || klucz == "") {
  stop("Brak CDS_USER lub CDS_KEY. Utwórz plik .Renviron na wzór .Renviron.example i zrestartuj R.")
}

wf_set_key(user = mail, key = klucz)
zapisany_klucz <- wf_get_key(user = mail)

if(zapisany_klucz == klucz) {
  print("Wysyłanie 12 zgłoszeń do chmury")
  
  obszar_polski_prostokat <- c(55.0, 14.0, 49.0, 24.2) 
  
  for (m in 1:12) {
    miesiac_str <- sprintf("%02d", m)
    dni_w_miesiacu <- as.numeric(days_in_month(as.Date(paste0("2025-", miesiac_str, "-01"))))
    
    zapytanie_era5 <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type   = "reanalysis",
      variable       = c("100m_u_component_of_wind", "100m_v_component_of_wind", 
                         "10m_u_component_of_wind", "10m_v_component_of_wind",
                         "surface_pressure", "2m_temperature", "geopotential", 
                         "forecast_surface_roughness",
                         "10m_wind_gust_since_previous_post_processing",       
                         "2m_dewpoint_temperature",
                         "surface_solar_radiation_downwards", "total_cloud_cover", "friction_velocity"),                        
      year           = "2025",
      month          = miesiac_str,
      day            = sprintf("%02d", 1:dni_w_miesiacu),
      time           = sprintf("%02d:00", 0:23),
      area           = obszar_polski_prostokat,
      format         = "netcdf",
      target         = paste0("polska_2025_", miesiac_str, ".nc") 
    )
    
    # Wysyłamy i zapominamy (transfer = FALSE)
    wf_request(user = mail, request = zapytanie_era5, transfer = FALSE, path = getwd())
    print(paste("-> Wysłano zamówienie na miesiąc:", miesiac_str))
  }
  
  print("Wszystkie 12 miesięcy zostało przekazanych do kolejki.")
  
} else {
  print("Niepoprawny klucz")
}

