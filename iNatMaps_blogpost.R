#John Deitsch
#March 13

library(tidyverse) #data wrangling
library(sf) #working with spatial data
library(ggspatial) #adding spatial objects to maps
library(units)

#import observation data
Ischnura <- read_csv("./data/Ischnura.csv")

#import shapefiles
GreatLakes = read_sf(layer = "GreatLakes", dsn = "./shapefile.library")
NorthAmerica = read_sf("NorthAmerica", dsn = "./shapefile.library")

#simplifying Great Lakes shapefile
GreatLakes <- GreatLakes %>%
  filter(NAME %in% c("Lake Michigan", "Lake Erie", 
                     "Lake Superior", "Lake Huron", "Lake Ontario"))

#make observation data spatial
Ischnura.sfc <- Ischnura %>%
  rowwise()%>%
  mutate(coords = list(st_point(c(decimalLongitude, decimalLatitude))))%>% #list column
  pull(coords)%>%
  st_sfc(crs = 4269)

Ischnura.sf <- Ischnura %>%
  st_as_sf(geometry = Ischnura.sfc)

#making sure all data has the same crs
st_crs(NorthAmerica) = "EPSG:4269"
st_crs(GreatLakes) = "EPSG:4269"
st_transform(NorthAmerica, crs = 4269)
st_transform(GreatLakes, crs = 4269)

#define funciton to make map of species range
rangemap <- function(df, speciescol, speciestomap) {#input is string in quotes
  
  sf::sf_use_s2(FALSE)
  
  Species <- df %>%
    filter(speciescol == speciestomap)%>% #subsetting observations
    filter(institutionCode=="iNaturalist")%>%
    summarise()%>% #we are creating a new variable
    st_convex_hull() #creates a 'range polygon' for species
  
  Species.Land <- st_intersection(Species, st_union(NorthAmerica)) 
  #damselflies don't live in the oceans
  Species.Land2 <- st_difference(Species.Land, st_union(GreatLakes)) 
  #damselflies don't live in the great lakes
  
  box <- st_bbox(Species.Land2) #returns boundaries of range
  
  Map <- ggplot()+
    geom_sf(data = NorthAmerica, #North American political boundaries
            fill = "white", 
            color = "grey41", 
            size = 0.10)+
    geom_sf(data = GreatLakes, #Great Lakes boundaries
            fill = "lightskyblue", 
            color = NA)+
    geom_sf(data = Species.Land2, #Species range polygon
            fill = "forestgreen", 
            alpha = 0.5, 
            color = "NA")+
    coord_sf(xlim = c(box[1], box[3]),
             ylim = c(box[2], box[4]))+
    theme(
      panel.grid.major = element_line(linetype = "blank"),
      panel.background = element_rect(fill = "lightskyblue"),
      panel.border = element_rect(fill = NA, size = 1))+
    annotation_scale()+
    annotation_north_arrow(location = "br", 
                           which_north = "true",
                           style = north_arrow_nautical)
  
  return(Map)
}

#define function to map two species ranges
rangemap_2 <- function(df, speciescol, species1, species2) {#input is string in quotes
  
  sf::sf_use_s2(FALSE)
  
  Spec1 <- df %>%
    filter(speciescol == species1)%>% 
    filter(institutionCode=="iNaturalist")%>%
    summarise()%>% 
    st_convex_hull() 
  
  Spec2 <- df %>%
    filter(speciescol == species2)%>% 
    filter(institutionCode=="iNaturalist")%>%
    summarise()%>% 
    st_convex_hull() 
  
  Spec1.land <- st_intersection(Spec1, st_union(NorthAmerica))
  Spec2.land <- st_intersection(Spec2, st_union(NorthAmerica))
  
  Both <- st_intersection(Spec1.land, Spec2.land)
  Only1 <- st_difference(Spec1.land, Spec2.land)
  Only2 <- st_difference(Spec2.land, Spec1.land)
  
  box <- st_bbox(st_union(Spec1.land, Spec2.land)) #returns boundaries of range
  
  Map <- ggplot()+
    geom_sf(data = NorthAmerica, #North American political boundaries
            fill = "white", 
            color = "grey41", 
            size = 0.10)+
    geom_sf(data = Both, #Species range polygon
            fill = "forestgreen", 
            alpha = 0.8, 
            color = "NA")+
    geom_sf(data = Only1, #Species range polygon
            fill = "blue", 
            alpha = 0.2, 
            color = "NA")+
    geom_sf(data = Only2, #Species range polygon
            fill = "red", 
            alpha = 0.2, 
            color = "NA")+
    geom_sf(data = GreatLakes, #Great Lakes boundaries
            fill = "lightskyblue", 
            color = NA)+
    # coord_sf(xlim = c(box[1], box[3]),
    #          ylim = c(box[2], box[4]))+
    coord_sf(xlim = c(-130, -50),
             ylim = c(15, 55))+
    theme(
      panel.grid.major = element_line(linetype = "blank"),
      panel.background = element_rect(fill = "lightskyblue"),
      panel.border = element_rect(fill = NA, size = 1))+
    annotation_scale()+
    annotation_north_arrow(location = "br", 
                           which_north = "true",
                           style = north_arrow_nautical)
  
  return(Map)
}

#Maps
map1 <- ggplot()+
  geom_sf(data = NorthAmerica,
          fill = "white",
          color = "grey41",
          size = 0.10)+
  geom_sf(data = GreatLakes,
          fill = "lightskyblue",
          color = NA)+
  geom_sf(data = filter(Ischnura.sf,
                        verbatimScientificName=="Ischnura ramburii"),
          size = 1.1,
          color = "forestgreen")+
  coord_sf(xlim = c(-130, -50),
           ylim = c(15, 55))+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "lightskyblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "br", which_north = "true")

map2 <- rangemap(Ischnura.sf, verbatimScientificName, "Ischnura ramburii")

map3 <- ggplot()+
  geom_sf(data = NorthAmerica,
          fill = "white",
          color = "grey41",
          size = 0.10)+
  geom_sf(data = GreatLakes,
          fill = "lightskyblue",
          color = NA)+
  geom_sf(data = filter(Ischnura.sf,
                        verbatimScientificName%in%c("Ischnura verticalis", "Ischnura posita")),
          size = 1.1,
          aes(color = verbatimScientificName))+
  coord_sf(xlim = c(-130, -50),
           ylim = c(15, 55))+
  theme(
    panel.grid.major = element_line(linetype = "blank"),
    panel.background = element_rect(fill = "lightskyblue"),
    panel.border = element_rect(fill = NA, size = 1))+
  annotation_scale()+
  annotation_north_arrow(location = "br", which_north = "true")

map4 <- rangemap_2(Ischnura.sf, verbatimScientificName,
                   "Ischnura posita", "Ischnura verticalis")

#saving plots
pdf("map1.pdf")
print(map1)
dev.off()
pdf("map2.pdf")
print(map2)
dev.off()
pdf("map3.pdf")
print(map3)
dev.off()
pdf("map4.pdf")
print(map4)
dev.off()

