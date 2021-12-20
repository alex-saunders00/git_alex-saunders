#-------------------------------------------------------------------------------
# Alex Saunders
# 08/11/2021
# gis_in_r_practice.R
# some informal practice at spatial data and creating plots in R
# from Nick Eubank "Making maps in R", building off excellent tutorials by Claudia Engel
# download the RGIS1, 2, 3 etc. datasets from https://www.nickeubank.com/gis-in-r/
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Install packages and set input and output paths
# ------------------------------------------------------------------------------

rm(list=ls())
dev.off()
packages = c("sp","rgdal", "raster", "rgeos","plyr", "RColorBrewer", "maptools")

for(package in packages){
  print(paste0('** Loading package: ',package))
  if(!require(package,character.only = TRUE)) {
    print("*** Package not found... installing")
    install.packages(package)
    if(!require(package,character.only = TRUE)) {
      print("*** ERROR: Package not found")
    }else{
      print('*** Package installed')
    }
  }
  library(package,character.only = TRUE)
  print(paste0('*** Package loaded: ', package))
}
print('* END: Loading packages')

# set input path
setwd("C://Users/alexa/Documents/02_work/00_general/data/maps_in_r_nick_eubank")
input_path <- getwd()
output_path <- paste0(input_path, "/output")
list.files(input_path)


# ------------------------------------------------------------------------------
# 1. spatial data in R
# intro to vector and raster data types in R
# ------------------------------------------------------------------------------

# create some sp spatial data from scratch

# make some points
practice_coords <- rbind(c(1.5, 2), c(2.5, 2), c(0.5, 0.5), c(1, 0.25), c(1.5, 0), c(2, 0), c(2.5, 0), c(3, 0.25), c(3.5, 0.5))
practice_coords

practice_points <- SpatialPoints(practice_coords)
plot(practice_points)

summary(practice_points)
coordinates(practice_points)

# add a coord ref system to the points - crs library available at www.spatialreference.org
proj_crs <- CRS("+init=EPSG:32633")
proj4string(practice_points) <- proj_crs

# subset of points data, performed same as for a vector - first two features
practice_points[1:2]

# add some attributes to move from a spatialpoints to spatialpointsdataframe
df <- data.frame(attr1 = c("a", "b", "z", "d", "e", "q", "w", "r", "z"), attr2 = c(101:109))
df
practice_spdf <- SpatialPointsDataFrame(practice_points, df) 
summary(practice_spdf)

# subset of spdf data, performed same as for dataframe
practice_spdf[c(1:2), "attr1"]
plot(practice_spdf[which(practice_spdf$attr2 > 105), ])


# create spatial points from a lon/lat table
list.files(paste0(input_path,"/RGIS1_Data/"))
restaurant_inspections <- read.csv(paste0(input_path,"/RGIS1_Data/","sf_restaurant_inspections.csv"))
head(restaurant_inspections)
class(restaurant_inspections)
# turn the dataframe into spatial object
coordinates(restaurant_inspections) <- c("longitude", "latitude")
class(restaurant_inspections)
scores_q1 <- c(summary(restaurant_inspections$Score)[2]) # min 54 out of 100, Q1 = 90
# plot all restaurants with a score in the first quartile (i.e. the 25% of worst scores)
plot(restaurant_inspections)
plot(restaurant_inspections[which(restaurant_inspections$Score <= scores_q1), ], col = "red", add = TRUE)


# create spatial polygons from coordinates - each starts as a Polygon
house1.building <- Polygon(rbind(c(1, 1), c(2, 1), c(2, 0), c(1, 0)))
house1.roof <- Polygon(rbind(c(1, 1), c(1.5, 2), c(2, 1)))
house2.building <- Polygon(rbind(c(3, 1), c(4, 1), c(4, 0), c(3, 0)))
house2.roof <- Polygon(rbind(c(3, 1), c(3.5, 2), c(4, 1)))
house2.door <- Polygon(rbind(c(3.25, 0.75), c(3.75, 0.75), c(3.75, 0), c(3.25, 0)), hole = TRUE) # use hole = true for gaps

# create list of polygon objects with Polygons
house1 <- Polygons(list(house1.building, house1.roof), "house1")
house2 <- Polygons(list(house2.building, house2.door, house2.roof), "house2")

# create spatialpolygons object from the lists
houses <- SpatialPolygons(list(house1, house2))
plot(houses)

# add attributes to the spatialpolygons
attr <- data.frame(attr1 = 1:2, attr2 = 6:5, row.names = c("house2","house1"))
houses_df <- SpatialPolygonsDataFrame(houses, attr)
as.data.frame(houses_df)
spplot(houses_df)
proj4string(houses_df) <- proj_crs

# create a very rough spatialpoygon representing south africa based on points
sa_pts <- Polygon(rbind(c(-21,33), c(-29,33), c(-34,26), c(-35,17), c(-29,16)))
lt_pts <- Polygon(rbind(c(-28,29), c(-31,28), c(-30,27)), hole = TRUE)

sa <- Polygons(list(sa_pts), "sa")
lt <- Polygons(list(lt_pts), "lt")

countries <- SpatialPolygons(list(sa, lt))
plot(countries)
countries_attr <- data.frame(gdp = c(7000, 1000), row.names = c("sa", "lt"))
countries_df <- SpatialPolygonsDataFrame(countries, countries_attr)
spplot(countries_df)

# much more common to read in spatial data using rgdal
sf_incomes <- readOGR(dsn = paste0(input_path,"/RGIS1_Data/shapefiles"), layer = "sf_incomes")
class(sf_incomes) # sp
summary(sf_incomes)
names(sf_incomes)
head(as.data.frame(sf_incomes))
spplot(sf_incomes, "MdIncHH")


# raster data in R - more compact than vectors since each gridd cell does not require a coordinate to be recorded
# raster data has: a grid, values associated to each gridd cell, projection

# create some raster data from scratch
practice_raster <- raster(ncol = 5, nrow = 10, xmn = 0, xmx = 5, ymn = 0, ymx = 10)
practice_raster
hasValues(practice_raster) # no values yet

# give the raster some values - assigned in order of row 1 moves across all cols, then row 2 etc.
values(practice_raster) <- 1:50
plot(practice_raster)

projection(practice_raster) <- "+init=EPSG:4326"

# load some elevation data for san francisco
elevation_raster <- raster(paste0(input_path,"/RGIS1_Data/sanfrancisconorth.dem"))
plot(elevation_raster)

# interrogate the raster
res(practice_raster) #  resolution





# ------------------------------------------------------------------------------
# 2. combining multiple spatial data sources
# methods like spatial joins, distance calculations etc.
# ------------------------------------------------------------------------------

# rgeos is the library for geometric operations e.g. buffers, intersections, unions etc

# combining spatial and non-spatial data
worldCountries <- merge(wolrdCountries, countryData, by.x = "id", by.y = "countryid")

# load some data on votes per district
list.files(paste0(input_path, "/RGIS2_Data/shapefiles"))
vote_shares <- read.csv(paste0(input_path, "/RGIS2_Data","/district_vote_shares.csv"))
head(vote_shares)

districts <- readOGR(dsn = paste0(input_path, "/RGIS2_Data/shapefiles"), layer = "congressional_districts")
head(districts)

# combine the votes per district data with the spatial districts data
districts_votes <- merge(districts, vote_shares, by.x = "DISTRICT", by.y = "DISTRICT")
spplot(districts_votes, "dem_vote_share")


# combine spatial data with spatial data - join based on spatial relationship
# important to ensure in same projections, use spTransform
common.crs <- CRS(proj4string(file.a))
file.b.reprojected <- spTransform(file.b, common.crs)

# read federal grants shapefile - spatialpoints
grants <- readOGR(dsn = paste0(input_path, "/RGIS2_Data/shapefiles"), layer = "federal_grants")
proj_crs <- CRS(proj4string(districts))
grants_reproj <- spTransform(grants, proj_crs)
range(coordinates(grants))
range(coordinates(grants_reproj)) # confirms that crs has been updated
par(mfrow = c(1,2))
plot(grants, axes = TRUE)
plot(grants_reproj, axes = TRUE)


# perform a spatial join with over - for every item of first data, return info about items of second data that intersect
# for every grant, obtain its district
grants.districts <- over(grants_reproj, districts)
grants.districts
head(grants)
grants_reproj <- cbind(grants_reproj, grants.districts)
grants_reproj


# over takes the first item of the target that intersects an item in the source
# for multiple items, can use two approaches

# using returnList = TRUE
over.list <- over(districts, geometry(grants_reproj), returnList = TRUE)
over.list
num.grants <- sapply(over.list, length)
districts <- cbind(districts, num.grants)
districts
names(districts)[5] <- "num.grants"
names(districts)

# or using fn
# get the average value of grants in each district
grants_reproj$GrantBudge <- as.numeric(grants_reproj$GrantBudge)
over(districts, grants_reproj[, "GrantBudge"], fn = mean)

# or using aggregate - returns a spatial object
aggregate(grants_reproj[, "GrantBudge"], districts, mean)

# aggregate can often be better when area weighted
aggregate(polygonsA, polygonsB, mean, areaWeighted = TRUE)


# example for drone strikes in pakistan
pk_dist <- readOGR(dsn = paste0(input_path, "/RGIS2_Data/shapefiles"), layer = "pk_districts")
strikes <- readOGR(dsn = paste0(input_path, "/RGIS2_Data/shapefiles"), layer = "pk_drone_strikes")
proj_crs <- CRS(proj4string(pk_dist))
strikes <- spTransform(strikes, proj_crs)

plot(pk_dist)
par(new = T)
plot(strikes, type = ".", col = "magenta", add = T)
par(new = F)

# compute the no of strikes per district
head(strikes)
head(pk_dist)
strikes$count <- 1
pk_dist <- cbind(pk_dist, over(pk_dist, strikes[ , "count"], fn = sum))
pk_dist$count[is.na(pk_dist$count)] <- 0
names(pk_dist)[8] <- "no_strikes"
pk_dist[pk_dist$no_strikes > 0, ]

# compute the average fatality rate per district
pk_dist <- cbind(pk_dist, over(pk_dist, strikes["Killed"], fn = mean))
names(pk_dist)[9] <- "mean_killed_per_strike"
pk_dist <- cbind(pk_dist, over(pk_dist, strikes[ , "Killed"], fn = sum))
names(pk_dist)[10] <- "ttl_killed_per_strike"
pk_dist[!is.na(pk_dist$mean_killed_per_strike), ]


# answer
pk.dist <- readOGR(dsn = paste0(input_path, "/RGIS2_Data/shapefiles"), layer = "pk_districts")
over.list <- over(pk.dist, geometry(strikes), returnList = TRUE)
num.strikes <- sapply(over.list, length)
pk.dist <- cbind(pk.dist, num.strikes)
names(pk.dist)[8] <- "num.strikes"
pk.dist[pk.dist$num.strikes != 0, ]



# working with rasters and spatial polygons - best to convert everything to crs of the raster
pollution <- raster(paste0(input_path, "/RGIS2_Data/pollution.tif"))
raster.crs <- CRS(projection(pollution))
districts <- spTransform(districts, raster.crs)
pollution.values <- extract(pollution, districts)
sapply(pollution.values, mean)

# load the pakistan population data
pk.pop <- raster(paste0(input_path, "/RGIS2_Data/pakp00g"))
# estimatre the population in each distict
pk_dist$pop <- extract(pk.pop, pk_dist, fun = sum)
head(pk_dist)
# compute the number of drone strikes per capita for each district
pk_dist$no_strikes_percap <- pk_dist$no_strikes / pk_dist$pop




# ------------------------------------------------------------------------------
# 3. create some simple spatial maps
# mapping spatial objects with plot and ssplot from the sp library
# ------------------------------------------------------------------------------

# read palo alto data
list.files(paste0(input_path, "/RGIS3_Data/Los_Angeles"))
palo_alto <- readOGR(dsn = paste0(input_path, "/RGIS3_Data/palo_alto"), layer = "palo_alto")
plot(palo_alto, border = "red")
title(main = "Palo Alto", sub = "By Census Tracts")


# add multiple layers with plot
palo_alto_freeways <- readOGR(dsn = paste0(input_path, "/RGIS3_Data/palo_alto"), layer = "palo_alto_freeways")
stopifnot(proj4string(palo_alto) == proj4string(palo_alto_freeways))
plot(palo_alto)
plot(palo_alto_freeways, col = "green", lwd = 3, add = T)


# read Los Angeles RGIS3 data
los_angeles <- readOGR(dsn = paste0(input_path, "/RGIS3_Data/Los_Angeles"), layer = "los_angeles")
names(los_angeles)

# simple plot
plot(los_angeles)

# some simply plot modifiers
plot(los_angeles, border = "blue")
title(main = "Los Angeles", sub = "By Census Tracts")

# plot freeways
freeways <- readOGR(dsn = paste0(input_path, "/RGIS3_Data/Los_Angeles"), layer = "los_angeles_freeways")
par(mfrow = c(1, 2)) # plot two side-by-side
plot(freeways, col = "red", bg = "gray") # background colour
plot(freeways, lwd = 5, col = "green") #  change line width

# add multiple layers using add
stopifnot(proj4string(los_angeles) == proj4string(freeways)) # check in same projection
plot(los_angeles)
plot(freeways, col = "blue", add = T)

# using spplot for chloropleth style maps i.e. filling polygons
spplot(los_angeles, "PrCpInc", main = "Los Angeles Demographics", sub = "Average Per Capita Income", col = "transparent") # col controls borders

# control the map extent either using scale and shift parameters based on original shapefile extent
scale.parameter = 0.5 # < 1 is zoom in, > 1 is zoom out
xshift = -0.1 # shift to right (map units)
yshift = 0.2 # shift to left
original.bbox = los_angeles@bbox # pass bbox of spatial object i.e. xlim and ylim

edges = original.bbox
edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,]) + xshift # xlim
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,]) + yshift # ylim

spplot(los_angeles, "PrCpInc", main = "Los Angeles Demographics", sub = "Average Per Capita Income", col = "transparent",
       xlim = edges[1,],
        ylim = edges[2,])

# control colours using custom palettes for spplot
library(RColorBrewer)
display.brewer.all()
my.palette <- brewer.pal(n = 7, name = "OrRd")
spplot(los_angeles, "PrCpInc", col.regions = my.palette, cuts = 6, col = "transparent") # set cuts to 1 minus the number of colours

# control colour breaks using classInt to make custom cuts
install.packages("classInt")
library(classInt)
breaks.qt <- classIntervals(los_angeles$PrCpInc, n = 6, style = "quantile", intervalClosure = "right")
spplot(los_angeles, "PrCpInc", col = "transparent", col.regions = my.palette, at = breaks.qt$brks)

# plot average incomes using custom colour scheme
my.palette <- brewer.pal(n = 9, name = "PuRd") # n is number of different colours in the palette, min 3, max depends on palette
spplot(los_angeles, "PrCpInc", col.regions = my.palette, cuts = 8, col = "transparent")

# multiple layers with spplot using sp.layout - need to create a layer list then using sp.layout
freeway.layer <- list("sp.lines", freeways, col = "blue") # type of layer to add, actual object, plotting options
spplot(los_angeles, "PrCpInc", sp.layout = freeway.layer, col.regions = my.palette, cuts = 8, col = "transparent")

# simply combine layer lists for multiple layers - in order of layers
freeway.layer2 <- list("sp.lines", freeways, col = "gray", lwd = 3) 
spplot(los_angeles, "PrCpInc", sp.layout = list(freeway.layer2, freeway.layer), main = "Los Angeles Demographics", sub = "Average Per Capita Income", col.regions = my.palette, cuts = 8, col = "transparent")

# accoutrements e.g. north arrows, scales can be added through sp.layout
los_angeles_proj <- spTransform(los_angeles, CRS("+init=EPSG:32611"))
bbox <- los_angeles_proj@bbox
scale_bar <- round( (bbox[1, 2] - bbox[1, 1]) / 5, -4)
# create offset from bottom left corner for plotting the scale bar
offset_x <- bbox[1, 1] + ((bbox[1, 2] - bbox[1, 1]) / 10)
offset_y <- bbox[2, 1] + ((bbox[2, 2] - bbox[2, 1]) / 10)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = scale_bar, fill = c("transparent", "black"), offset = c(offset_x, offset_y)) # scale sets length of bar in map units
text1 <- list("sp.text", c(offset_x, offset_y + 4000), "0")
text2 <- list("sp.text", c(offset_x + scale_bar, offset_y + 4000), "10 km")

# create offset from top right corner for plotting the north arrow
offset_x2 <- bbox[1, 2] - ((bbox[1, 2] - bbox[1, 1]) / 10)
offset_y2 <- bbox[2, 2] - ((bbox[2, 2] - bbox[2, 1]) / 10)
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(offset_x2, offset_y2), scale = scale_bar/3)

# write out as png
tmp_plot <- spplot(los_angeles_proj, "PrCpInc", sp.layout = list(scale, text1, text2, arrow), main = "Los Angeles Demographics", sub = "Average Per Capita Income", col.regions = my.palette, cuts = 6, col = "transparent")
png(paste0(output_path,"/3.1_los_angeles_income.png"))
print(tmp_plot)
dev.off()


# basemaps - tend to by default use WGS84 Web Mercator (EPSG: 3857) so be careful for crs misalingment issues

# easiest for basic basemaps with gmap - creates a raster layer with correct projection, think gmap no longer works since needs a google API key
install.packages("dismo")
library(dismo)
basemap <- gmap(palo_alto, type = "terrain") # can use other types e.g. satellite, hybrid etc


# more cumbersome in spplot, use ggmap
install.packages("ggmap")
library(ggmap)
# REPROJECT YOUR DATA TO EPSG 3857
to.plot.web.merc <- spTransform(palo_alto, CRS("+init=EPSG:3857"))


# COPY AND PASTE SEGEMENT 1 Series of weird conversions to deal with
# inconsistencies in units for API.
box <- to.plot.web.merc@bbox

midpoint <- c(mean(box[1, ]), mean(box[2, ]))
left.bottom <- c(box[1, 1], box[2, 1])
top.right <- c(box[1, 2], box[2, 2])

boundaries <- SpatialPoints(rbind(left.bottom, top.right))
proj4string(boundaries) <- CRS("+init=EPSG:3857")
boundaries.latlong <- c(t(spTransform(boundaries, CRS("+init=EPSG:4326"))@coords))

# END COPY-PASTE SEGMENT 1


# SET MAP TYPE HERE, LEAVE OTHER PARAMETERS AS THEY ARE
gmap <- get_map(boundaries.latlong, maptype = "terrain", source = "stamen", 
                crop = TRUE)

# COPY-PASTE SEGMENT 2 Create object that sp.layout likes.
long.center <- midpoint[1]
lat.center <- midpoint[2]
height <- box[2, 2] - box[2, 1]
width <- box[1, 2] - box[1, 1]

sp.raster <- list("grid.raster", gmap, x = long.center, y = lat.center, width = width, 
                  height = height, default.units = "native", first = TRUE)
# END COPY-PASTE SEGMENT 2


# NORMAL PLOTTING TRICKS - HAVE FUN HERE!

# Housecleaning and set colors
to.plot.web.merc$ethnicity <- as.factor(to.plot.web.merc$ethnicity)

my.palette <- c("red", "blue")
point.size <- 0.5

# Plot!
tmp_plot <- spplot(to.plot.web.merc, "PrCpInc", sp.layout = sp.raster, col.regions = my.palette, cuts = 6, col = "transparent", main = "Demographic Distribution of Santa Clara County", sub = "Average Per Capita Income")
png(paste0(output_path,"/3.2_palo_alto_income_wbasemap.png"))
print(tmp_plot)
dev.off()
plot(to.plot.web.merc, add=T)


# edit legends


# ggplot2 is used for graphing but can also be used for mapping spatial data
# ggplot2 only accepts dataframes as inputs, so need to convert spatial data to dataframe using fortify

library(ggplot2)

# create a unique ID for the later join
palo_alto$id = rownames(as.data.frame(palo_alto))


# turn SpatialPolygonsDataframe into a data frame
palo_alto.pts <- fortify(palo_alto, region="id") #this only has the coordinates
palo_alto.df <- merge(palo_alto.pts, palo_alto, by="id", type='left') # add the attributes back 

# calculate even breaks breaks
palo_alto.df$qt <- cut(palo_alto.df$PrCpInc, 5)


# plot  
ggplot(palo_alto.df, aes(long,lat,group=group, fill=qt)) + # the data
  geom_polygon() + # make polygons
  scale_fill_brewer("Per Cap Income", palette = "OrRd") + # fill with brewer colors
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal()


# making maps with raster data
us.pop <- raster(paste0(input_path, "/RGIS3_Data/us_population_raster.asc"))
plot(us.pop)

# managing breaks with logs
us.pop.logged <- us.pop
values(us.pop.logged) <- log(values(us.pop.logged))
plot(us.pop.logged)

# managing breaks using classInt - first drop the zero values
value.vector <- values(us.pop)
value.vector <- value.vector[value.vector != 0]
breaks.qt <- classIntervals(value.vector, n = 15, style = "quantile", intervalClosure = "right")  
plot(us.pop, breaks = breaks.qt$brks)  

# managing colours
my.palette <- brewer.pal(n = 8, name = "OrRd")
plot(us.pop.logged, col = my.palette)

# managing extent
new.extent <- extent(c(-125, -100, 20, 50))
plot(us.pop.logged, ext = new.extent, col = my.palette)



# ------------------------------------------------------------------------------
# 4. google maps api and geocoding
# how apis work and how to convert addresses to coordinates via google maps api
# ------------------------------------------------------------------------------
library(ggmap)

addresses <- c("1600 Pennsylvania NW, Washington, DC", "denver, co")

locations <- geocode(addresses, source = "google", output = "more")
locations

# need to set up google api see ?register_google
# currently having issue with billing


# ------------------------------------------------------------------------------
# 6. networks
# components, types, adjacency matrices, edgelists etc.
# ------------------------------------------------------------------------------
install.packages("igraph")
library(igraph)

# create an edgelist
edge.list.df <- data.frame(list(col1 = c(1, 2, 3, 3, 4), col2 = c(4, 1, 2, 4, 2)))
edge.list.df

# convert to a graph
my.graph <- graph_from_data_frame(edge.list.df, directed = F)
V(my.graph) # vertices
E(my.graph) # edges
plot(my.graph)


# read airline data
airline_travel <- read.csv(paste0(input_path,"/RGIS6_Data/airline_travel.csv"))
head(airline_travel)
setNames(airline_travel, c("origin","destination","weight"))
# convert to graph
airline_graph <- graph_from_data_frame(airline_travel, directed = F)
V(airline_graph)
E(airline_graph)
summary(airline_graph)
plot(airline_graph)

# Louvain clustering community detection algorithm
louvain.result <- cluster_louvain(airline_graph)
memberships <- membership(louvain.result)
network.result <- as.data.frame(cbind(names(memberships), membership(louvain.result)))
names(network.result) <- c("state", "group_assignment")
unique(network.result$group_assignment)

# join the network result with spatial data for mapping
states <- readOGR(dsn = paste0(input_path,"/RGIS6_Data/state_shapefile"), layer = "states")
states <- spTransform(states, CRS("+init=EPSG:5070"))
states <- merge(states, network.result, by.x = "STUSPS", by.y = "state")
spplot(states, "group_assignment")

