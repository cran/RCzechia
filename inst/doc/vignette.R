## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
library(httr)
library(roxygen2)


## ----census, echo = T, eval = T, message = F-----------------------------
library(RCzechia)
library(dplyr)
library(readxl)
library(httr)
library(tmap)
library(sf)

GET("https://raw.githubusercontent.com/jlacko/RCzechia/master/data-raw/zvcr034.xls", 
    write_disk(tf <- tempfile(fileext = ".xls")))
      # original (ugly!!) format from the Czech Statistical Office

src <- read_excel(tf, sheet = 1, col_names = F) %>% # read in with fake column names
  select(NAZ_LAU1 = X__1, # key for the shapefile
         obyvatel = X__2) %>% # population (as text)
  mutate(obyvatel = suppressWarnings(as.double(obyvatel))) %>% 
    # convert from text to number; ignore errors
  mutate(NAZ_LAU1 = ifelse(NAZ_LAU1 == "Hlavní město Praha", "Praha", NAZ_LAU1)) 
    # rename Prague (from The Capital to a regular city)
  
okresni_data <- okresy("low") %>% # data shapefile
  inner_join(src, by = "NAZ_LAU1") 
    # key for data connection - note the use of inner (i.e. filtering) join

vystup <- tm_shape(okresni_data) + tm_fill(col = "obyvatel", title = "Population", 
                                           palette = "Blues", style = "quantile", n = 5) +
  tm_shape(okresni_data) + tm_borders("grey40", lwd = 0.5) + # thin edges of districts
  tm_shape(republika("low")) + tm_borders("grey30", lwd = 1.5) # thick national borders

print(vystup)


## ----geocode, echo = T, eval = T, message = F----------------------------
library(RCzechia)
library(dplyr)
library(tmap)
library(sf)

rivers <- reky()

rivers <- rivers[rivers$Major == T, ]

mista <- data.frame(misto = c('kramarova vila', 'arcibiskupske zahrady v kromerizi', 'becov nad teplou'),
                    lon = c(14.41030, 17.39353, 12.83833),
                    lat = c(50.09380, 49.30048, 50.08346))

# to geocode a list of locations consider ggmap::geocode()

POI <- mista %>% # or geocode using ggmap
  st_as_sf(coords = c("lon", "lat"), crs = 4326) # convert plain data to spatial CRS = WGS84, used by Google

tm_plot <- tm_shape(republika("low")) + tm_borders("grey30", lwd = 1) +
  tm_shape(POI) + tm_symbols(col = "firebrick3", shape = 20, size = 0.5) +
  tm_shape(rivers) + tm_lines(col = 'steelblue', lwd = 1.5)
  

print(tm_plot)


## ----unempl, echo = T, eval = T, message = F-----------------------------
library(dplyr)
library(RCzechia)
library(tmap)
library(sf)

src <- read.csv(url("https://raw.githubusercontent.com/jlacko/RCzechia/master/data-raw/unempl.csv"), stringsAsFactors = F) 
# open data on unemployment from Czech Statistical Office - https://www.czso.cz/csu/czso/otevrena_data
# lightly edited for size (rows filtered)

src <- src %>%
  mutate(KOD_OBEC = as.character(uzemi_kod))  # keys in RCzechia are of type character

podklad <- obce_polygony() %>% # obce_polygony = municipalities in RCzechia package
  inner_join(src, by = "KOD_OBEC") # linking by key


vystup <- tm_shape(republika()) + tm_borders(col = "grey40") +
  tm_shape(podklad) + tm_fill(col = "hodnota", title = "Unemployment", palette = "YlOrRd") +
  tm_legend(position = c("RIGHT", "top"),
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), " %")))

print(vystup)
 

## ----distance, echo = T, eval = T, message = F---------------------------
library(dplyr)
library(RCzechia)
library(sf)
library(units)

obce <- obce_polygony()

praha <- obce[obce$NAZ_OBEC == "Praha", ]
brno <- obce[obce$NAZ_OBEC == "Brno", ]

vzdalenost <- st_distance(praha, brno) %>%
  set_units("kilometers") # easier to interpret than meters, miles or decimal degrees..

print(vzdalenost)


## ----interactive, echo = T, eval = T, message = F------------------------
library(dplyr)
library(RCzechia)
library(tmap)
library(sf)

src <- read.csv(url("https://raw.githubusercontent.com/jlacko/RCzechia/master/data-raw/unempl.csv"), stringsAsFactors = F) 
# open data on unemployment from Czech Statistical Office - https://www.czso.cz/csu/czso/otevrena_data
# lightly edited for size (rows filtered)


src <- src %>%
  mutate(KOD_OBEC = as.character(uzemi_kod))  # keys in RCzechia are of type character

podklad <- obce_polygony() %>% # obce_polygony = municipalities in RCzechia package
  inner_join(src, by = "KOD_OBEC") %>% # linking by key
  filter(KOD_CZNUTS3 == "CZ071") # Olomoucký kraj

vystup <- tm_shape(republika()) + tm_borders(col = "grey40") +
  tm_shape(podklad) + tm_fill(col = "hodnota", title = "Unemployment", palette = "YlOrRd", id = "NAZ_OBEC") +
  tm_legend(position = c("RIGHT", "top"),
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), " %"))) +
  tm_view(basemaps = "Stamen.Toner")

save_tmap(vystup, filename = "vystup.html")
 

