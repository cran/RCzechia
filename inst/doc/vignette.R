## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = T, fig.width = 8, fig.height = 6)
library(httr)
library(roxygen2)


## ----census, echo = T, eval = T, message = F, fig.align="center", dpi = 100, out.width = "100%"----
library(RCzechia)
library(dplyr)
library(readxl)
library(httr)
library(tmap)
library(sf)

GET("https://raw.githubusercontent.com/jlacko/RCzechia/master/data-raw/zvcr034.xls", 
    write_disk(tf <- tempfile(fileext = ".xls")))

src <- read_excel(tf, range = "Data!B5:C97") # read in with original column names

colnames(src) <- c("NAZ_LAU1", "obyvatel") # meaningful names instead of the original ones

src <- src %>%
  mutate(obyvatel = as.double(obyvatel)) %>% 
    # convert from text to number
  mutate(NAZ_LAU1 = ifelse(NAZ_LAU1 == "Hlavní město Praha", "Praha", NAZ_LAU1)) 
    # rename Prague (from The Capital to a regular city)
  
okresni_data <- RCzechia::okresy("low") %>% # data shapefile
  inner_join(src, by = "NAZ_LAU1") 
    # key for data connection - note the use of inner (i.e. filtering) join

vystup <- tm_shape(okresni_data) + tm_fill(col = "obyvatel", title = "Population", 
                                           palette = "Blues", style = "quantile", n = 5) +
  tm_shape(okresni_data) + tm_borders("grey40", lwd = 0.5) + # thin edges of districts
  tm_shape(republika("low")) + tm_borders("grey30", lwd = 1.5) + # thick national borders
  tm_layout(frame = F) # clean does it

print(vystup)


## ----geocode, echo = T, eval = T, message = F, fig.align="center", dpi = 100, out.width = "100%"----
library(RCzechia)
library(tmap)
library(sf)

borders <- RCzechia::republika("low")

rivers <- subset(RCzechia::reky(), Major == T)

mista <- data.frame(misto =  c("Kramářova vila", 
                               "Arcibiskupské zahrady v Kromeříži", 
                               "Hrad Bečov nad Teplou"),
                    adresa = c("Gogolova 1, Praha 1",
                               "Sněmovní náměstí 1, Kroměříž",
                               "nám. 5. května 1, Bečov nad Teplou"))

# from a string vector to sf spatial points object
POI <- RCzechia::geocode(mista$adresa) 


tm_plot <- tm_shape(borders) + tm_borders("grey30", lwd = 1) +
  tm_shape(POI) + tm_symbols(col = "firebrick3", shape = 20, size = 0.5) +
  tm_shape(rivers) + tm_lines(col = "steelblue", lwd = 1.5, alpha = 0.5) +
  tm_legend(title = "Very Special Places") + # ... or whatever :)
  tm_layout(frame = F)
  

print(tm_plot)


## ----unempl,  echo = T, eval = T, message = F, out.width = "100%", fig.align="center", dpi = 300----
library(dplyr)
library(RCzechia)
library(tmap)
library(sf)

src <- read.csv(url("https://raw.githubusercontent.com/jlacko/RCzechia/master/data-raw/unempl.csv"), stringsAsFactors = F) 
# open data on unemployment from Czech Statistical Office - https://www.czso.cz/csu/czso/otevrena_data
# lightly edited for size (rows filtered)

src <- src %>%
  mutate(KOD_OBEC = as.character(uzemi_kod))  # keys in RCzechia are of type character

podklad <- RCzechia::obce_polygony() %>% # obce_polygony = municipalities in RCzechia package
  inner_join(src, by = "KOD_OBEC") # linking by key


vystup <- tm_shape(republika()) + tm_borders(col = "grey40") +
  tm_shape(podklad) + tm_fill(col = "hodnota", title = "Unemployment", palette = "YlOrRd") +
  tm_legend(legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), " %"))) +
  tm_layout(frame = F)

print(vystup)
 

## ----distance, echo = T, eval = T, message = F, fig.align="center", dpi = 100, out.width = "100%"----
library(dplyr)
library(RCzechia)
library(sf)
library(units)

obce <- RCzechia::obce_polygony()

praha <- subset(obce, NAZ_OBEC == "Praha")

brno <- subset(obce, NAZ_OBEC == "Brno")

vzdalenost <- sf::st_distance(praha, brno) %>%
  units::set_units("kilometers") # easier to interpret than meters, miles or decimal degrees..

print(vzdalenost)


## ----brno-center, echo = T, eval = T, message = F, fig.align="center", dpi = 100, out.width = "100%"----
library(dplyr)
library(RCzechia)
library(tmap)
library(sf)

brno <- subset(RCzechia::obce_polygony(), NAZ_OBEC == "Brno")

pupek_brna <- brno %>%
  st_transform(5514) %>% # planar CRS (eastings & northings)
  sf::st_centroid(brno) # calculate central point of a polygon

# the revgeo() function takes a sf points data frame and returns it back
# with address data in "revgeocoded"" column
adresa_pupku <- RCzechia::revgeo(pupek_brna)$revgeocoded

tm_plot <- tm_shape(brno) + tm_borders(col = "grey40") +
  tm_shape(pupek_brna) + tm_dots(size = 1/3, col = "red", shape = 4) +
  tm_legend(title = "Center of Brno") + 
  tm_layout(frame = F)
  
print(adresa_pupku)

print(tm_plot)




## ----interactive, echo = T, eval = F-------------------------------------
#  library(dplyr)
#  library(RCzechia)
#  library(tmap)
#  library(sf)
#  
#  src <- read.csv(url("https://raw.githubusercontent.com/jlacko/RCzechia/master/data-raw/unempl.csv"), stringsAsFactors = F)
#  # open data on unemployment from Czech Statistical Office - https://www.czso.cz/csu/czso/otevrena_data
#  # lightly edited for size (rows filtered)
#  
#  
#  src <- src %>%
#    mutate(KOD_OBEC = as.character(uzemi_kod))  # keys in RCzechia are of type character
#  
#  podklad <- RCzechia::obce_polygony() %>% # obce_polygony = municipalities in RCzechia package
#    inner_join(src, by = "KOD_OBEC") %>% # linking by key
#    filter(KOD_CZNUTS3 == "CZ071") # Olomoucký kraj
#  
#  tmap_mode("view")
#  
#  vystup <- tm_shape(podklad) + tm_fill(col = "hodnota", title = "Unemployment", palette = "YlOrRd", id = "NAZ_OBEC") +
#    tm_legend(legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), " %"))) +
#    tm_view(basemaps = "Stamen.Toner")
#  
#  print(vystup)
#  

## ----union,  echo = T, eval = T, message = F, out.width = "100%", fig.asp = 0.7, dpi = 100----
library(RCzechia)
library(dplyr)
library(sf)


poly <- RCzechia::okresy("low") %>% # Czech LAU1 regions as sf data frame
  mutate(oddeven = ifelse(nchar(NAZ_LAU1) %% 2 == 1, "odd", "even" )) %>% # odd or even?
  RCzechia::union_sf("oddeven") # ... et facta est lux

plot(poly, key.pos = 1)

## ----ctverce, echo = T, eval = T, message = F, warning = F, out.width = "100%",fig.asp = 0.7, dpi = 100----
library(RCzechia)
library(ggplot2)
library(dplyr)
library(sf)

obec <- "Humpolec" # a Czech location

# geolocate centroid of a place
place <- RCzechia::geocode(obec) %>% 
  filter(typ == "Obec") 

# ID of the KFME square containg place geocoded
ctverec_id <- sf::st_intersection(RCzechia::KFME_grid(), place)$ctverec

print(paste0("Location found in grid cell number ", ctverec_id, "."))

# a single KFME square to be highlighted
highlighted_cell <- KFME_grid() %>% 
  filter(ctverec == ctverec_id) 

# a summary plot
ggplot() +
  geom_sf(data = RCzechia::republika(), size = .85) + # Czech borders
  geom_sf(data = highlighted_cell, # a specific KFME cell ...
          fill = "limegreen", alpha = .5) +  # ... highlighted in lime green
  geom_sf(data = KFME_grid(), size = .33, # all KFME grid cells, thin
          color = "gray80", fill = NA) + # in gray and without fill
  geom_sf(data = place,  color = "red", pch = 4) +  # X marks the spot!
  ggtitle(paste("Location", obec, "in grid cell number", ctverec_id))


