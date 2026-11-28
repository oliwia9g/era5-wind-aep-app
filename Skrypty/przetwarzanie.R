library(tidync)
library(dplyr)
library(lubridate)
library(sf)                
library(rnaturalearth)    
library(ggplot2)

print("Szukanie plików ZIP ...")

polska_granice <- ne_countries(scale = "medium", country = "Poland", returnclass = "sf")
dane_z_calego_roku <- list() 

for (m in 1:12) {
  miesiac_str <- sprintf("%02d", m)
  nazwa_pobierana_zip <- paste0("polska_2025_", miesiac_str, ".zip")
  
  print(paste("=================================================="))
  print(paste("PRZETWARZAM MIESIĄC:", miesiac_str))
  
  folder_temp <- paste0("temp_folder_", miesiac_str)
  
  if (file.exists(nazwa_pobierana_zip)) {
    dir.create(folder_temp, showWarnings = FALSE)
    print("   -> Znaleziono plik ZIP ...")
    
    # wypakowanie i zapisanie ich ścieżek do zmiennej
    pliki_wypakowane <- unzip(nazwa_pobierana_zip, exdir = folder_temp)
  
    plik_instant <- grep("instant\\.nc$", pliki_wypakowane, value = TRUE)
    plik_max <- grep("max\\.nc$", pliki_wypakowane, value = TRUE)
    plik_accum <- grep("accum\\.nc$", pliki_wypakowane, value = TRUE)
    
    if(length(plik_instant) == 0 | length(plik_max) == 0 | length(plik_accum) == 0) {
      print("W pliku ZIP brakuje kompletu plików (instant/max/accum). Pomijam miesiąc.")
      next
    }
    
    print("   -> Wczytywanie i scalanie plików ...")
    
    # Wczytanie trzech plików do pamięci RAM
    dane_instant <- tidync::tidync(plik_instant) |> tidync::hyper_tibble()
    dane_max <- tidync::tidync(plik_max) |> tidync::hyper_tibble()
    dane_accum <- tidync::tidync(plik_accum) |> tidync::hyper_tibble()
    
    dane_surowe <- dane_instant |> 
      left_join(dane_max, by = c("longitude", "latitude", "valid_time")) |>
      left_join(dane_accum, by = c("longitude", "latitude", "valid_time"))
    
  } else {
    print(paste("Brakuje pliku", nazwa_pobierana_zip))
    next 
  }
  
  print("   -> Obliczanie parametrów fizycznych i wycinanie mapy Polski...")
  dane_przetworzone <- dane_surowe |> 
    mutate(
      longitude = as.numeric(longitude), 
      latitude = as.numeric(latitude),
      wind_speed_100m = sqrt(u100^2 + v100^2), 
      wind_speed_10m = sqrt(u10^2 + v10^2),
      wind_gust_10m = fg10,
      friction_velocity = zust, # w m/s
      temp_celsius = t2m - 273.15,             
      dewpoint_celsius = d2m - 273.15,         
      pressure_hpa = sp / 100,                 
      geopotential = z / 9.80665, #m n.p.m           
      surf_roughness = fsr, #z0 [m] to teoretyczna wysokość nad poziomem gruntu, na której prędkość wiatru spada dokładnie do zera (0 m/s) na skutek tarcia powietrza o chropowatą powierzchnię ziemi.
      cloud_cover = tcc, #wartosci 0-1
      solar_radiation_wm2 = ssrd / 3600, #waty/m^2
      date_1 = as.Date(as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC"))
    )
  
  # Wycinanie punktów
  punkty_siatki <- dane_przetworzone |> distinct(longitude, latitude)
  punkty_sf <- st_as_sf(punkty_siatki, coords = c("longitude", "latitude"), crs = 4326)
  punkty_w_polsce_maska <- st_within(punkty_sf, st_make_valid(polska_granice), sparse = FALSE)
  punkty_w_polsce_sf <- punkty_sf[as.vector(punkty_w_polsce_maska), ]
  
  # Rysowanie mapy poglądowej przy pierwszym przejściu pętli
  if (length(dane_z_calego_roku) == 0) {
    print("   -> Generuję i zapisuję mapę podglądową punktów...")
    
    liczba_p <- nrow(punkty_w_polsce_sf)
    
    mapa_podglad <- ggplot() +
      geom_sf(data = polska_granice, fill = "white", color = "black", linewidth = 0.5) +
      geom_sf(data = punkty_w_polsce_sf, color = "darkred", size = 1.0, alpha = 0.8) +
      coord_sf(xlim = c(14.0, 24.2), ylim = c(49.0, 55.0)) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Siatka punktów ERA5 nad Polską",
        subtitle = paste("Liczba punktów wewnątrz granic:", liczba_p),
        x = "Długość geograficzna",
        y = "Szerokość geograficzna"
      ) 
    print(mapa_podglad)
    ggsave("mapa_punktow_finalna.png", plot = mapa_podglad, width = 8, height = 6, dpi = 300)
    print("   -> Mapa zapisana jako")}
  
  wspolrzedne_polska <- st_coordinates(punkty_w_polsce_sf) |> 
    as.data.frame() |> rename(longitude = X, latitude = Y)
  
  dane_finalne_miesiac <- dane_przetworzone |> 
    inner_join(wspolrzedne_polska, by = c("longitude", "latitude")) |>
    group_by(longitude, latitude) |> 
    arrange(valid_time, .by_group = TRUE) |> 
    mutate(
      hour = rep(0:23, length.out = n()),
      date = as.POSIXct(paste(date_1, sprintf("%02d:00:00", hour)), tz = "UTC"),
      day = day(date),      
      month = month(date)
    ) |> ungroup() |>
    select(longitude, latitude, date, day, month, hour,
           u100, v100, u10, v10,
           wind_speed_100m, wind_speed_10m, wind_gust_10m, friction_velocity,
           temp_celsius, dewpoint_celsius, pressure_hpa, 
           geopotential, surf_roughness, cloud_cover, solar_radiation_wm2)
  
  dane_z_calego_roku[[m]] <- dane_finalne_miesiac
  
  # Usuwanie folderu tymczasowego po miesiącu
  unlink(folder_temp, recursive = TRUE)
}

if(length(dane_z_calego_roku) > 0) {
  print("==================================================")
  print("Zakończono czytanie plików! Łączenie danych w całość...")
  dane_caly_rok <- bind_rows(dane_z_calego_roku)
  
  nazwa_folderu <- "dane_punkty_2025"
  dir.create(nazwa_folderu, showWarnings = FALSE)
  print(paste("Dzielenie danych na osobne lokalizacje (zapisuję do folderu:", nazwa_folderu, ")..."))
  
  unikalne_lokalizacje <- unique(dane_caly_rok[, c("longitude", "latitude")])
  
  for(i in 1:nrow(unikalne_lokalizacje)) {
    lon <- unikalne_lokalizacje$longitude[i]
    lat <- unikalne_lokalizacje$latitude[i]
    dane_pojedynczego_punktu <- dane_caly_rok |> filter(longitude == lon, latitude == lat)
    nazwa_pliku_punktu <- sprintf("%s/dane_lat_%.2f_lon_%.2f.csv", nazwa_folderu, lat, lon)
    write.csv(dane_pojedynczego_punktu, nazwa_pliku_punktu, row.names = FALSE)
  }
  
  print("BAZA JEST GOTOWA")
} else {
  print("Nie wczytano ani jednego miesiąca. Upewnij się, że pliki .zip są w folderze projektu.")
}
