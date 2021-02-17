## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = T, fig.align="center", dpi = 150, out.width = "100%", fig.width = 8, fig.height = 4)
library(httr)
library(roxygen2)

## ----census, echo = T, eval = T, message = F----------------------------------
library(RCzechia)
library(ggplot2)
library(dplyr)
library(czso)

src <- czso::czso_get_table("SLDB-VYBER") %>% 
   select(uzkod, obyvatel = vse1111) %>% 
   mutate(obyvatel = as.numeric(obyvatel)) 

okresni_data <- RCzechia::okresy("low") %>% # data shapefile
  inner_join(src, by = c("KOD_OKRES" = "uzkod")) 
    # key for data connection - note the use of inner (i.e. filtering) join

# report results
ggplot(data = okresni_data) +
  geom_sf(aes(fill = obyvatel), colour = NA) +
  geom_sf(data = republika("low"), color = "gray30", fill = NA) +
  scale_fill_viridis_c(trans = "log", labels = scales::comma) +
  labs(title = "Czech population",
       fill = "population\n(log scale)") +
  theme_bw() +
  theme(legend.text.align = 1,
        legend.title.align = 0.5)

## ----geocode, echo = T, eval = T, message = F, warning = F,fig.width = 8, fig.height = 5----
library(RCzechia)
library(ggplot2)
library(sf)

borders <- RCzechia::republika("low")

rivers <- subset(RCzechia::reky(), Major == T)

mista <- data.frame(misto =  c("Kramářova vila", 
                               "Arcibiskupské zahrady v Kromeříži", 
                               "Hrad Bečov nad Teplou"),
                    adresa = c("Gogolova 212, Praha 1",
                               "Sněmovní náměstí 1, Kroměříž",
                               "nám. 5. května 1, Bečov nad Teplou"))

# from a string vector to sf spatial points object
POI <- RCzechia::geocode(mista$adresa) 

class(POI) # in {sf} package format = spatial and data frame

# report results
ggplot() +
  geom_sf(data = POI, color = "red", shape = 4, size = 2) +
  geom_sf(data = rivers, color = "steelblue", alpha = 0.5) +
  geom_sf(data = borders, color = "grey30", fill = NA) +
  labs(title = "Very Special Places") +
  theme_bw()


## ----distance, echo = T, eval = T, message = F--------------------------------
library(dplyr)
library(RCzechia)
library(sf)
library(units)

obce <- RCzechia::obce_polygony()

praha <- subset(obce, NAZ_OBEC == "Praha")

brno <- subset(obce, NAZ_OBEC == "Brno")

vzdalenost <- sf::st_distance(praha, brno) %>%
  units::set_units("kilometers") # easier to interpret than meters, miles or decimal degrees..

# report results
print(vzdalenost[1])


## ----brno-center, echo = T, eval = T, message = F, warning = F, fig.width = 6, fig.height = 6----
library(dplyr)
library(RCzechia)
library(ggplot2)
library(sf)

# all districts
brno <- RCzechia::okresy() %>% 
  dplyr::filter(KOD_LAU1 == "CZ0642")

# calculate centroid
pupek_brna <- brno %>%
  sf::st_transform(5514) %>% # planar CRS (eastings & northings)
  sf::st_centroid(brno) # calculate central point of a polygon

# the revgeo() function takes a sf points data frame and returns it back
# with address data in "revgeocoded"" column
adresa_pupku <- RCzechia::revgeo(pupek_brna) %>% 
  pull(revgeocoded)

# report results
print(adresa_pupku)

ggplot() +
  geom_sf(data = pupek_brna, col = "red", shape = 4) +
  geom_sf(data = reky("Brno"), color = "skyblue3") +
  geom_sf(data = brno, color = "grey50", fill = NA) +
  labs(title = "Geographical Center of Brno") +
  theme_bw()




## ----interactive, echo = T, eval = F------------------------------------------
#  library(dplyr)
#  library(RCzechia)
#  library(leaflet)
#  library(czso)
#  
#  # metrika pro mapování - uchazeči za říjen
#  metrika <- czso::czso_get_table("250169r20") %>%
#     filter(obdobi == "20201031" & vuk == "NEZ0004")
#  
#  podklad <- RCzechia::obce_polygony() %>% # obce_polygony = municipalities in RCzechia package
#    inner_join(metrika, by = c("KOD_OBEC" = "uzemi_kod")) %>% # linking by key
#    filter(KOD_CZNUTS3 == "CZ071") # Olomoucký kraj
#  
#  pal <- colorNumeric(palette = "viridis",  domain = podklad$hodnota)
#  
#  leaflet() %>%
#    addProviderTiles("Stamen.Toner") %>%
#    addPolygons(data = podklad,
#                fillColor = ~pal(hodnota),
#                fillOpacity = 0.75,
#                color = NA)
#  

## ----ctverce, echo = T, eval = T, message = F, warning = F,fig.width = 8, fig.height = 5----
library(RCzechia)
library(ggplot2)
library(dplyr)
library(sf)

obec <- "Humpolec" # a Czech location, as a string

# geolocate the place
place <- RCzechia::geocode(obec) %>% 
  filter(typ == "Obec") 

class(place) # a spatial data frame

# ID of the KFME square containg place geocoded (via spatial join)
ctverec_id <- sf::st_join(RCzechia::KFME_grid(), 
                          place, left = FALSE) %>% # not left = inner (filtering) join 
  pull(ctverec)

print(paste0("Location found in grid cell number ", ctverec_id, "."))

# a single KFME square to be highlighted as a polygon
highlighted_cell <- KFME_grid() %>% 
  filter(ctverec == ctverec_id) 

# report results
ggplot() +
  geom_sf(data = RCzechia::republika(), size = .85) + # Czech borders
  geom_sf(data = highlighted_cell, # a specific KFME cell ...
          fill = "limegreen", alpha = .5) +  # ... highlighted in lime green
  geom_sf(data = KFME_grid(), size = .33, # all KFME grid cells, thin
          color = "gray80", fill = NA) + # in gray and without fill
  geom_sf(data = place,  color = "red", pch = 4) +  # X marks the spot!
  labs(title = paste("Location", obec, "in grid cell number", ctverec_id)) +
  theme_bw()


## ----relief, echo = T, eval = T, message = F, warning = F,fig.width = 8, fig.height = 5----
library(RCzechia)
library(ggplot2)
library(dplyr)
library(raster)

# ggplot does not play nice with {raster} package; a data frame is required
relief <- vyskopis("rayshaded") %>% 
  as("SpatialPixelsDataFrame") %>% # old style format - {sp}
  as_tibble()

# report results
ggplot() +
  geom_raster(data = relief, aes(x = x, y  = y, alpha = -raytraced), # relief
              fill = "gray30",  show.legend = F) + # no legend is necessary
  geom_sf(data = subset(RCzechia::reky(), Major == T), # major rivers
          color = "steelblue", alpha = .7) +
  labs(title = "Czech Rivers & Their Basins") +
  theme_bw() +
  theme(axis.title = element_blank())


## ----senat, echo = T, eval = T, message = F, warning = F----------------------

library(RCzechia)
library(ggplot2)
library(dplyr)
library(rvest)

# official result of elections from Czech Statistical Office
vysledky <- "https://www.volby.cz/pls/senat/se1111?xjazyk=CZ&xdatum=20201002&xv=7&xt=2" %>% 
  xml2::read_html() %>%  # because rvest::html is deprecated
  html_nodes(xpath = "//*[@id=\"se1111_t1\"]") %>%  # get the table by its xpath
  html_table(fill = T) %>% 
  .[[1]] %>% 
  dplyr::select(OBVOD = Obvod, strana = `Volebnístrana`) %>% 
  # pad OBVOD with zero to 2 places to align to RCzechia data format
  mutate(OBVOD = stringr::str_pad(OBVOD, 2, side = "left", pad = "0"))

podklad <- RCzechia::senat_obvody("low") %>% 
  # match by key; left to preserve geometry of off cycle districts (NAs)
  left_join(vysledky, by = "OBVOD") 

ggplot() +
  geom_sf(data = RCzechia::republika(), size = .85) + # Czech borders
  geom_sf(data = podklad, aes(fill = strana)) +
  labs(title = "Senate elections 2020") +
  theme_bw()



