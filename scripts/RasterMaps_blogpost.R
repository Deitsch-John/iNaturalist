# Raster data


# Packages ----------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(raster)
library(spDataLarge)
library(ggspatial)
library(cowplot)

# Functions ---------------------------------------------------------------

obs_to_coords <- function(df, coord_col, crs_add){
  
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)
  
  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)
  
}
rast_to_dfsf <- function(ras, crs_add){
  
  #takes a raster (terra) object and converts to an sf dataframe
  #requires terra and sf 
  
  dummy.coords <- terra::xyFromCell(ras, seq_len(ncell(ras)))
  dummy.values <- as.data.frame(terra::values(ras))
  dummy.df <- cbind(dummy.coords, dummy.values)%>%
    rowwise()%>%
    mutate(coords = list(sf::st_point(c(x,y))))
  
  coords.sfg <- dummy.df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)
  
  dfsf <- dummy.df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)
  
}

# Backgroup map items -----------------------------------------------------

georgia = dplyr::filter(spData::us_states, NAME == "Georgia")
setwd("C:/Users/jfdei/OneDrive/Desktop/Work/Joro")

# Spider Data -------------------------------------------------------------

#Loading spider data
Araneae <- read_tsv("All.csv")%>%
  dplyr::select(decimalLatitude, decimalLongitude)%>%
  rowwise()%>%
  mutate(coords = list(st_point(c(decimalLongitude, decimalLatitude))))

Araneae.sf <- obs_to_coords(Araneae, coords, 4269)
Aran.projected <- st_transform(Araneae.sf, 26917)

GeorgiaSpiders <- Araneae.sf[georgia, ]

Spiders <- read_csv("iNatData.csv")%>%
  dplyr::filter(Species=="Argiope aurantia")%>%
  dplyr::select(-geometry)%>%
  rowwise()%>%
  mutate(coords = list(st_point(c(Long, Lat))))

Spiders.sf <- obs_to_coords(Spiders, coords, 4269)
GeorgiaArgiope <- Spiders.sf[georgia, ]


# Initial Maps ------------------------------------------------------------

#initial maps
ArgPlot <- ggplot()+
  geom_sf(data = georgia)+
  geom_sf(data = GeorgiaArgiope)+
  labs(title = "Argiope aurantia")+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_nautical)

SpidPlot <- ggplot()+
  geom_sf(data = georgia)+
  geom_sf(data = GeorgiaSpiders)+
  labs(title = "All Spiders")+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_nautical)

plot_grid(ArgPlot, SpidPlot)


# Raster Templates --------------------------------------------------------

Aran.ext <- ext(GeorgiaSpiders)
raster_template <- terra::rast(Aran.ext, nrows = 100, ncols = 100,
                                crs = "EPSG:4269")

SpiderRast <- rasterize(vect(GeorgiaSpiders), raster_template, fun = "length")
ArgRast <- rasterize(vect(GeorgiaArgiope), raster_template, fun = "length")

df.rast_all <- rast_to_dfsf(SpiderRast, 4269)
df.rast_arg <- rast_to_dfsf(ArgRast, 4269)

SpidPlot2 <- ggplot()+
  geom_sf(data = georgia, fill = "white")+
  geom_sf(data = filter(df.rast_all, !is.na(lyr.1)), aes(color = scale(log10(lyr.1))),
          size = 1.1, shape = 15)+
  theme_void()+
  scale_fill_distiller(type = "seq", palette = "RdBu", 
                       guide = "colourbar", 
                       direction = -1, aesthetics = "color")+
  labs(title = "All Spiders", color = "# Observations")+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_nautical)

ArgPlot2 <- ggplot()+
  geom_sf(data = georgia, fill = "white")+
  geom_sf(data = filter(df.rast_arg, !is.na(lyr.1)), aes(color = scale(log10(lyr.1))),
          size = 1.1, shape = 15)+
  theme_void()+
  scale_fill_distiller(type = "seq", palette = "RdBu", 
                       guide = "colourbar", 
                       direction = -1, aesthetics = "color")+
  labs(title = "Argiope aurantia", , color = "# Observations")+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_nautical)

plot_grid(ArgPlot2, SpidPlot2)


# Raster Math -------------------------------------------------------------

ArgRast2 <- ArgRast/SpiderRast
df.rast_arg2 <- rast_to_dfsf(ArgRast2, 4269)

df.rast_arg2ed <- df.rast_arg2 %>%
  filter(!is.na(lyr.1))%>%
  rename(prop = lyr.1)%>%
  mutate(scaled = scale(prop))

attach(df.rast_arg2ed)

df.rast_arg2.sf <- st_as_sf(df.rast_arg2ed, geometry = geometry)

ArgPlot3 <- ggplot()+
  geom_sf(data = georgia, fill = "white")+
  geom_sf(data = df.rast_arg2.sf, aes(color = scale(log10(prop))),
          size = 1.1, shape = 15)+
  theme_void()+
  scale_fill_distiller(type = "seq", palette = "RdBu", 
                       guide = "colourbar", 
                       direction = -1, aesthetics = "color")+
  labs(title = "Argiope aurantia", color = "Scaled Abundance")+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_nautical)

plot_grid(ArgPlot2, ArgPlot3)

test <- df.rast_arg2 %>%
  rename(prop = lyr.1)%>%
  mutate(prop = ifelse(is.na(prop)==TRUE, 0, prop))

raster.test <- rasterize(vect(test),raster_template, field = "prop")
raster.test2 <- aggregate(raster.test, fact = 3, fun = mean)
plot(raster.test2)

# Saving Plots ------------------------------------------------------------

setwd("C:/Users/jfdei/OneDrive/Desktop/iNaturalist")

pdf("Arg1.pdf", width = 4.2, height = 5.38)
print(ArgPlot)
dev.off()

pdf("All1.pdf", width = 4.2, height = 5.38)
print(SpidPlot)
dev.off()

pdf("Arg2.pdf", width = 4.5, height = 5.38)
print(ArgPlot2)
dev.off()

pdf("All2.pdf", width = 4.5, height = 5.38)
print(SpidPlot2)
dev.off()

pdf("Arg3.pdf", width = 4.5, height = 5.38)
print(ArgPlot3)
dev.off()



## future wonderings
setwd("C:/Users/jfdei/OneDrive/Desktop/qGIS")
counties = read_sf("US_counties", dsn = "./shapefile.library")

gwinnett <- counties %>%
  filter(COUNTYNAME=="Gwinnett")

GwinnettSpiders <- Araneae.sf[gwinnett, ]
GwinnettArgiope <- Spiders.sf[gwinnett, ]
raster_gw <- terra::rast(ext(gwinnett), nrows = 20, ncols = 20,
                               crs = "EPSG:4269")
SpiderRast.gw <- rasterize(vect(GwinnettSpiders), raster_gw, fun = "length")
ArgRast.gw <- rasterize(vect(GwinnettArgiope), raster_gw, fun = "length")
ArgRast.gw2 <- ArgRast.gw/SpiderRast.gw

df.gwi <- rast_to_dfsf(ArgRast.gw2, 4269)


ggplot()+
  geom_sf(data = gwinnett, fill = "white")+
  geom_sf(data = filter(df.gwi, !is.na(lyr.1)), aes(color = scale(lyr.1)),
          size = 5, shape = 15)+
  theme_void()+
  scale_fill_distiller(type = "seq", palette = "RdBu", 
                       guide = "colourbar", 
                       direction = -1, aesthetics = "color")+
  labs(title = "Argiope aurantia", color = "Scaled Abundance")+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_nautical)

setwd("C:/Users/jfdei/OneDrive/Desktop/qGIS/raster.library")
NLCD <- rast("nlcd_2019_land_cover_l48_20210604.IMG")

Gwinnett <- st_transform(gwinnett, st_crs(NLCD))

gwinnett.landcover <- mask(crop(NLCD, vect(Gwinnett)), vect(Gwinnett))

gwinnett_agg = terra::aggregate(gwinnett.landcover, fact = 5, fun = modal)

plot(gwinnett_agg)

gw.points.simp <- as.points(gwinnett_agg)%>%
  st_as_sf()%>%
  mutate(Class = case_when(
    `NLCD Land Cover Class`%in% c("21", "22", "23", "24")~"Developed",
    `NLCD Land Cover Class`%in% c("41", "42", "43")~"Forest",
    `NLCD Land Cover Class`%in% c("81","82", "81")~"Field",
    `NLCD Land Cover Class`%in% c("52", "90", "95")~"Shrubs and Wetlands",
    `NLCD Land Cover Class`=="11"~"Water"
  ))

raster_templgw <- terra::rast(ext(Gwinnett), nrows = 304, ncols = 292,
                              crs = "EPSG:26917")
gwrast.simp <-rasterize(vect(gw.points.simp), raster_templgw, field = "Class")

gw.points.simp.sf <- st_as_sf(gw.points.simp, geometry = geometry)

gw.points.simp.sf <- st_transform(gw.points.simp.sf, st_crs(df.gwi))

# Plot <- ggplot()+
#   geom_sf(data = gwinnett, fill = "white")+
#   geom_sf(data = gw.points.simp.sf, aes(color = Class),
#           size = 0.3, shape = 1)+
#   scale_fill_discrete(palette = "Spectral")+
#   theme_void()+
#   labs(title = "Argiope aurantia")+
#   theme(
#     panel.grid.major = element_line(linetype = "blank"),
#     panel.background = element_rect(fill = "aliceblue"),
#     panel.border = element_rect(fill = NA, size = 1))+
#   annotation_scale()+
#   annotation_north_arrow(location = "tr", 
#                          which_north = "true",
#                          style = north_arrow_nautical)+
#   new_scale_colour()+
#   geom_sf(data = filter(df.gwi, !is.na(lyr.1)), aes(color = scale(lyr.1)),
#           size = 5)+
#   scale_fill_distiller(type = "seq", palette = "RdBu",
#                        direction = -1, aesthetics = "color")


