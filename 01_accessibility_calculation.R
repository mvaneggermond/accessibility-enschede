# First  we needto give JAVA more space
options(java.parameters = '-Xmx8G')
# Then we can load the libraries
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(akima)
library(dplyr)
library(classInt) 
library(viridis)
library(nngeo)
library(tidyverse)
library(tmap)
library(ggspatial)



# Accessibility!



# TODO Add stops and reli
# 

# Routing! ------
# We will use the package r5r
# This requires JAVA JDK 11 
# And some memory
# I pre-built the network and uploaded it
path <- "_data"
# You can see the data sets that we have below
list.files(path)
r5r_core <- r5r::setup_r5(data_path = path, verbose = TRUE)
# This packages comes with several functions
# First let's look at itineraries
?detailed_itineraries
# Test data
df_test_destinations <- data.frame(id= numeric(), lon= numeric(), lat = numeric())


df_test_destinations[1, ] <- c( 1, 6.861370223097654,52.23795055331917 ) # Horst, University Twente
df_test_destinations[2, ] <- c( 2, 6.890440424300697,52.222347842475365) # Station Enschede
df_test_destinations[3, ] <- c( 3,  5.97345010775397,52.20968914105861) # Saxion Apeldoorn
# Convert to spatial frame
sf_test_destinations<- st_as_sf(x = df_test_destinations,
                                coords = c("lon", "lat"),
                                crs = 4326)
# Plot interactively with tmap
tmap_mode("view") # interactive mode
tmap_mode("view") # Change to view for interactive map
# Add the dots to the map
map <- tm_shape(sf_test_destinations) +
  tm_dots("id")
# View the map
map
# Now we need a depature datetime
departure_datetime <- as.POSIXct("16-06-2022 16:30:43",
                                 format = "%d-%m-%Y %H:%M:%S",
                                 tz = "CET")
# Calculate an itinerary
det <- r5r::detailed_itineraries(r5r_core,
                                 origins = sf_test_destinations[1,],
                                 destinations = sf_test_destinations[2,],
                                 mode = c('WALK','TRANSIT'),
                                 mode_egress = c('WALK'),
                                 departure_datetime = departure_datetime,
                                 max_walk_dist = 800,
                                 max_trip_duration=90,
                                 max_rides = 4,
                                 max_lts = 1,
                                 shortest_path = FALSE)
# View the results
det

# Plot the results
tmap_mode("view") # Change to view for interactive map
map <- tm_shape(det) +
  tm_lines("option", col = 'black')
# Have a look
map


# Origins and destinations ------
# Which layers are in our geopackage
# You can use the function st_layers from the package sf as follows:
sf::st_layers("_data/cbs_vk100_2020_v2.gpkg")
# Note that there is one layers in the geopackage
sf_population <- st_read("_data/cbs_vk100_2020_v2.gpkg",layer='vierkant_100m_2020') %>% st_centroid()
# Let's read the employment
# We need a distinct data set with the reli, coordinates and the figures. 
# Something like this reli, e, n, population, employment
# reli, e, n, pop, emp
# 1122, 1, 1, 100, 1000

# We use a function to add the columns
# See this exchange on Github: https://github.com/r-spatial/sf/issues/231
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

sf_population_4326 <- sf_population %>% st_transform(4326) 

sf_population_coords <- sfc_as_cols(sf_population_4326, names = c("lon","lat"))
df_population <- sf_population_coords %>% st_drop_geometry() %>% as_tibble() %>% 
  mutate(across(everything(), ~replace(., . ==  -99997 , 0)))%>%
  mutate(id=crs28992res100m)
df_population %>% relocate(lon,lat,.after="crs28992res100m")

tail(df_population %>% relocate(lon,lat,.after="crs28992res100m"))
# Isochrone ------
# Example inspired by https://ipeagit.github.io/r5r/articles/calculating_isochrones.html
# routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 500 # in meters
travel_time_cutoff <- 120 # in minutes
time_window <- 60 # in minutes
percentiles <- 50
# calculate travel time matrix
ttm <- travel_time_matrix(r5r_core,
                          origins = sf_test_destinations[1,],
                          destinations = df_population,
                          mode = c('WALK','TRANSIT'),
                          departure_datetime = departure_datetime,
                          max_walk_dist = 800,
                          max_trip_duration = 120,
                          time_window = time_window,
                          percentiles = percentiles,
                          progress = FALSE)
# View the results
ttm
sf_output <- st_read("_data/cbs_vk100_2020_v2.gpkg",layer='vierkant_100m_2020')

sf_output <- sf_output %>% mutate(id=crs28992res100m) %>%  inner_join(ttm ,by=c("id"="toId"))
write_sf(sf_output,"travel_time_matrix.gpkg")

# add coordinates of destinations to travel time matrix
ttm <- df_population %>% 
  mutate(id=as.character(id)) %>%
  inner_join(ttm ,by=c("id"="toId"))


  

write_csv(ttm,"ttm_enschede_transit.csv")

# interpolate estimates to get spatially smooth result
travel_times.interp <- with(na.omit(ttm), interp(lon, lat, travel_time,duplicate=T)) %>%
  with(cbind(travel_time=as.vector(z),  # Column-major order
             x=rep(x, times=length(y)),
             y=rep(y, each=length(x)))) %>%
  as.data.frame() %>% na.omit()


# find isochrone's bounding box to crop the map below
bb_x <- c(min(travel_times.interp$x), max(travel_times.interp$x))
bb_y <- c(min(travel_times.interp$y), max(travel_times.interp$y))
# Plot
site <- data.frame(longitude = 7.642380067887121, latitude = 47.534662861574446) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


ggplot(site) +
  annotation_map_tile("cartolight") +
  geom_sf(size = 5)+
  annotation_map_tile("osm") +
  geom_contour_filled(data=travel_times.interp,aes(x=x, y=y, z=travel_time), alpha=.5) +
  scale_fill_viridis_d(direction = -1, option = 'B') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim = bb_x, ylim = bb_y) +
  labs(fill = "travel time (minutes)", color='') +
  theme_minimal() +
  theme(axis.title = element_blank())

#plot(sf_population)
# Accessibilitly ------
# routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 1000 # in meters
travel_time_cutoff <- 45 # in minutes
time_window <- 60 # in minutes
percentiles <- 50
departure_datetime <- as.POSIXct("16-06-2022 07:00:43",
                                 format = "%d-%m-%Y %H:%M:%S",
                                 tz = "CET")
summary(df_population$aantal_inwoners_25_tot_45_jaar)
sum(df_population$aantal_inwoners_25_tot_45_jaar)
?accessibility
# From selected origins
access_car <- accessibility(r5r_core,
                           origins = sf_test_destinations,
                           destinations = df_population,
                           mode = 'CAR',
                           opportunities_colname = "aantal_inwoners_25_tot_45_jaar",
                           decay_function = "step",
                           cutoffs = travel_time_cutoff,
                           departure_datetime = departure_datetime,
                           max_walk_dist = max_walk_dist,
                           time_window = time_window,
                           percentiles = percentiles,
                           verbose = FALSE)


access_car


access_pt <- accessibility(r5r_core,
                        origins = sf_test_destinations,
                        destinations = df_population,
                        mode = mode,
                        opportunities_colname = "aantal_inwoners_25_tot_45_jaar",
                        decay_function = "step",
                        cutoffs = travel_time_cutoff,
                        departure_datetime = departure_datetime,
                        max_walk_dist = max_walk_dist,
                        time_window = time_window,
                        percentiles = percentiles,
                        verbose = FALSE)


access_pt
access_bicycle <- accessibility(r5r_core,
                        origins = sf_test_destinations,
                        destinations = df_population,
                        mode = c('BICYCLE'),
                        opportunities_colname = "aantal_inwoners_25_tot_45_jaar",
                        decay_function = "step",
                        cutoffs = travel_time_cutoff,
                        departure_datetime = departure_datetime,
                        max_walk_dist = max_walk_dist,
                        time_window = time_window,
                        bike_speed=14,
                        percentiles = percentiles,
                        verbose = FALSE)


access_bicycle


access_ebike <- accessibility(r5r_core,
                                origins = sf_test_destinations,
                                destinations = df_population,
                                mode = c('BICYCLE'),
                                opportunities_colname = "aantal_inwoners_25_tot_45_jaar",
                                decay_function = "step",
                                cutoffs = travel_time_cutoff,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                time_window = time_window,
                                bike_speed=25,
                                percentiles = percentiles,
                                verbose = FALSE)


access_ebike

# Compare the results
access_bicycle <-access_bicycle %>% mutate(mode='Bicycle [12 km/h]')
access_ebike <-access_ebike %>% mutate(mode='E-Bike [25 km/h]')

access_car <- access_car %>% mutate(mode='Car')
access_pt <- access_pt %>% mutate(mode='Public transport')
# Bind the rows
levels_sites <- c(1,2,3)
labels_sites <- c('University of Twente', 'ITC', 'Saxion Apeldoorn')
# Modes
levels_mode <- rev(c('Car','Public transport', 'E-Bike [25 km/h]','Bicycle [12 km/h]'))
labels_mode <- rev(c('Car','Public transport', 'E-Bike [25 km/h]','Bicycle [12 km/h]'))


access_all <- bind_rows(access_bicycle,access_ebike,access_car,access_pt) %>% 
  mutate(site=factor(from_id,levels_sites,labels_sites,ordered=T),
         mode_f=factor(mode,levels_mode,labels_mode))%>%
  filter(mode!='Car')
  


  #inner_join(sf_sites %>%
  #             st_drop_geometry(), by=c("from_id"="id"))%>%
  #as.data.frame() %>% as_tibble() %>% rename(site=name)

ggplot(access_all,aes(x=site,y=accessibility,group=mode_f))+
  geom_bar(stat="identity",aes(fill=mode_f), position = "dodge")+
  ylab("Accessibilty to population [25 - 45 years old]")+
  xlab( "Site")


