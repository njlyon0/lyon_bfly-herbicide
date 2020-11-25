##  --------------------------------------------------------------------------------------  ##
                # Butterfly & Floral Resource Response to Spray and Seed ####
##  --------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE #### 
  ## Everybody likes a nice clean site map, so that's what this script will give 'em

# Necessary libraries
library(tidyverse); library(rgdal); library(maps); library(mapdata); library(mapproj); library(devEMF)
library(raster); library(broom); library(maptools)
library(ggmap)

# Set the working directory
setwd("~/Documents/_Publications/2020_Lyon_Butterfly SnS/Herbicide.WD")
                
# Clear the environment
rm(list = ls())

# Record the default graphing parameter ("par") environment
.pardefault <- par()

# Re-set the parameter to default (if changed downstream)
par(.pardefault)

## -------------------------------------------- ##
         # Shapefile Map Creation ####
## -------------------------------------------- ##
library(maps); library(mapdata); library(maptools); library(scales)

# Get shape files
grg.shape <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/GRG Borders/", layer = "GRG Boundary")
ltr.shape <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/LTR Files/", layer = "LTR")
gil.shape <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/GIL Files/", layer = "GIL")
pyw.shape <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/PYW Files/", layer = "PYW")

# Transform from UTM to lat/long
grg.geo <- spTransform(grg.shape, CRS("+proj=longlat +datum=WGS84"))
ltr.geo <- spTransform(ltr.shape, CRS("+proj=longlat +datum=WGS84"))
gil.geo <- spTransform(gil.shape, CRS("+proj=longlat +datum=WGS84"))
pyw.geo <- spTransform(pyw.shape, CRS("+proj=longlat +datum=WGS84"))

# Ditch the margins
#par(mar = c(0,0,0,0))

# Get a map of the US
us <- raster::getData('GADM', country = 'USA', level = 1)  ## Get the Province Shapefile for France

# Subset to just the states of interest
region.states <- subset(us, NAME_1 == "Iowa" | NAME_1 == "Missouri" | NAME_1 == "Nebraska" | 
                          NAME_1 == "Kansas" |  NAME_1 == "Oklahoma" |  NAME_1 == "Illinois" | 
                          NAME_1 == "Minnesota" |  NAME_1 == "Wisconsin" |  NAME_1 == "Arkansas" | 
                          NAME_1 == "South Dakota" |  NAME_1 == "Kentucky" |  NAME_1 == "Tennessee")

# Re-set the parameter to default (if changed downstream)
par(.pardefault)

# Make the plot!
  ## State borders
plot(region.states, col = "gray99", xlim = c(-94.16, -94.14), ylim = c(40.545, 40.608))
  ## Make a black box around the map
box(which = "plot", lty = "solid")
  ## Add the three sites' maps
plot(gil.geo, add = T, col = "#4575b4")
plot(ltr.geo, add = T, col = "#d73027")
plot(pyw.geo, add = T, col = "#fee090")
  ## Add text for the sites and states
text("GIL", cex = 0.75, x = -94.1255, y = 40.59)
text("LTR", cex = 0.75, x = -94.14, y = 40.5825)
text("PYW", cex = 0.75, x = -94.176, y = 40.5825)
text("IA", font = 3, cex = 1, x = -94.115, y = 40.5755)
text("MO", font = 3, cex = 1, x = -94.115, y = 40.569)
  ## Add scale bar
map.scale(ratio = F, relwidth = 0.2, x = -94.19, y = 40.56)
  ## Add lat/long axes
map.axes(cex.axis = 0.8)

# Make another plot of the larger state region
  ## Plot the larger region
plot(region.states, col = "gray99", xlim = c(-93.555, -93.55), ylim = c(38, 43.5))
  ## Make a black box around the map
box(which = "plot", lty = "solid")
  ## Add the outline of the larger GRG region
plot(grg.geo, add = T, col = "gray75")
  ## Add text to define some key bits
text("GRG", font = 3, cex = 0.7, x = -94.1, y = 41.02)
text("IA", cex = 1.4, x = -93.5, y = 42.1)
text("MO", cex = 1.4, x = -92.75, y = 38.5)
text("IL", cex = 1.4, x = -89.4, y = 40.2)

# Now make both plots and save them separately
  ## Site Map
jpeg(filename = "./Graphs/GRG_Site_Map.jpeg", width = 6, height = 6, units = "in", res = 720)
plot(region.states, col = "gray99", xlim = c(-94.16, -94.14), ylim = c(40.545, 40.608))
box(which = "plot", lty = "solid")
plot(gil.geo, add = T, col = "#4575b4")
plot(ltr.geo, add = T, col = "#d73027")
plot(pyw.geo, add = T, col = "#fee090")
text("GIL", cex = 0.75, x = -94.1255, y = 40.59)
text("LTR", cex = 0.75, x = -94.14, y = 40.5825)
text("PYW", cex = 0.75, x = -94.176, y = 40.5825)
text("IA", font = 3, cex = 1, x = -94.105, y = 40.5755)
text("MO", font = 3, cex = 1, x = -94.105, y = 40.569)
map.scale(ratio = F, relwidth = 0.2, x = -94.19, y = 40.56)
map.axes(cex.axis = 0.8)
dev.off()

  ## State Map
jpeg(filename = "./Graphs/GRG_State_Map.jpeg", width = 6, height = 6, units = "in", res = 720)
plot(region.states, col = "gray99", xlim = c(-93.555, -93.55), ylim = c(38, 43.5))
box(which = "plot", lty = "solid")
plot(grg.geo, add = T, col = "gray75")
text("GRG", font = 3, cex = 0.7, x = -94.1, y = 41.02)
text("IA", cex = 1.4, x = -93.5, y = 42.1)
text("MO", cex = 1.4, x = -92.75, y = 38.5)
text("IL", cex = 1.4, x = -89.4, y = 40.2)
dev.off()

## -------------------------------------------- ##
        # Fancy Inset Map Creation ####
## -------------------------------------------- ##
# Now we'll try making the larger (area) plot an inset of the local (site) map

# Re-set the parameter to default (if changed downstream)
par(.pardefault)

# Make the plot!
## State borders
plot(region.states, col = "gray99", xlim = c(-94.16, -94.14), ylim = c(40.545, 40.608))
## Make a black box around the map
box(which = "plot", lty = "solid")
## Add the three sites' maps
plot(gil.geo, add = T, col = "#4575b4")
plot(ltr.geo, add = T, col = "#d73027")
plot(pyw.geo, add = T, col = "#fee090")
## Add text for the sites and states
text("GIL", cex = 0.75, x = -94.1255, y = 40.59)
text("LTR", cex = 0.75, x = -94.14, y = 40.5825)
text("PYW", cex = 0.75, x = -94.176, y = 40.5825)
text("IA", font = 3, cex = 1, x = -94.115, y = 40.5755)
text("MO", font = 3, cex = 1, x = -94.115, y = 40.569)
## Add scale bar
map.scale(ratio = F, relwidth = 0.2, x = -94.19, y = 40.56)
## Add lat/long axes
map.axes(cex.axis = 0.8)

# Create inset dimensions
  ## Add new graph on old one = T
par(new = T)
  ## Eliminate margins
par(mar = c(0, 0, 0, 0))
  ## Set the size of the box (xmin, xmax, ymin, ymax)
par(fig = c(0.26, 0.48, 0.7, 0.85))

# Make another plot of the larger state region
## Plot the larger region
plot(region.states, col = "gray99", xlim = c(-93.555, -93.55), ylim = c(38, 43.5))
## Make a black box around the map
box(which = "plot", lty = "solid")
## Add the outline of the larger GRG region
plot(grg.geo, add = T, col = "gray75")
## Add text to define some key bits
text("GRG", font = 3, cex = 0.4, x = -94.1, y = 41.4)

# Looks great! Now re-run it to save it
jpeg(filename = "./Figures/GRG_Map.jpeg", width = 6, height = 6, units = "in", res = 720)
par(.pardefault)
plot(region.states, col = "gray99", xlim = c(-94.16, -94.14), ylim = c(40.545, 40.608))
box(which = "plot", lty = "solid")
plot(gil.geo, add = T, col = "#4575b4")
plot(ltr.geo, add = T, col = "#d73027")
plot(pyw.geo, add = T, col = "#fee090")
text("GIL", cex = 0.75, x = -94.1255, y = 40.59)
text("LTR", cex = 0.75, x = -94.14, y = 40.5825)
text("PYW", cex = 0.75, x = -94.176, y = 40.5825)
text("IA", font = 3, cex = 1, x = -94.115, y = 40.5755)
text("MO", font = 3, cex = 1, x = -94.115, y = 40.569)
map.scale(ratio = F, relwidth = 0.2, x = -94.19, y = 40.56)
map.axes(cex.axis = 0.8)
par(new = T)
par(mar = c(0, 0, 0, 0))
par(fig = c(0.26, 0.48, 0.7, 0.85))
plot(region.states, col = "gray99", xlim = c(-93.555, -93.55), ylim = c(38, 43.5))
box(which = "plot", lty = "solid")
plot(grg.geo, add = T, col = "gray75")
text("GRG", font = 3, cex = 0.4, x = -94.1, y = 41.4)
dev.off()
  
## -------------------------------------------- ##
          # Shapefile Map Version ####
## -------------------------------------------- ##
library(rgdal)
ltr.t <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/LTR Files/", layer = "LTR")
gil.t <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/GIL Files/", layer = "GIL")
pyw.t <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/PYW Files/" , layer = "PYW")
states.t <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/State Files/",
                    layer = "cb_2015_us_state_500k")
grg.bord.t <- readOGR(dsn = "./SnS GRG Map/GRG Shape Files/GRG Borders/",
                      layer = "GRG Boundary")

library(raster)
par(mar = c(0,0,0,0))
plot(grg.bord.t, xlim = c(400000, 401000), ylim = c(4490000, 4500000))
plot(ltr.t, col = "#80cdc1", add = T)
plot(gil.t, col = "#80cdc1", add = T)
plot(pyw.t, col = "#80cdc1", add = T)
plot(states.t, add = T)

par(mar = c(0,0,0,0))
plot(ltr.t, col = "#80cdc1", xlim = c(400000, 401000), ylim = c(4490000, 4500000))
plot(gil.t, col = "#80cdc1", add = T)
plot(pyw.t, col = "#80cdc1", add = T)
plot(states.t, add = T)

## -------------------------------------------- ##
            # Points Map Version ####
## -------------------------------------------- ##
# Pulled GPS point for transect start from one patch of each pasture
  ## Tried to go with most 'central' patch for each pasture
  ## Patch and whittaker are listed in the first dataframe
utms <- read.csv("./SnS GRG Map/mapdata_raw.csv")

# Ditch the sites I no longer use
utms.v2 <- filter(utms, Pasture == "LTR" |  Pasture == "GIL" |  Pasture == "PYW")

# Get R to see the utm coords as a spatial object
utmcoor <- SpatialPoints(cbind(utms.v2$UTM.X, utms.v2$UTM.Y), proj4string = CRS("+proj=utm +zone=15"))
  ## utms$X and utms$Y are corresponding to UTM Easting and Northing, respectively
  ## zone = UTM zone

# Get latitude and longitude
longlatcoor <- spTransform(utmcoor, CRS("+proj=longlat"))

# Get it into dataframe form again
longlat <- as.data.frame(longlatcoor)

# The shapefile is shifted slightly south
  ## SO need to ammend the points to be on the right side of the IA/MO border
longlat$coords.x2 <- longlat$coords.x2 + 0.03

# Check the bounding box it assigned
print(summary(longlatcoor))

# And manually set my box to be just a smidge bigger than that
y1 <- 40.4
y2 <- 40.8
x1 <- -94.3
x2 <- -93.9

# Add a column for pasture name and another for management
longlat$Site <- utms.v2$Pasture
longlat$Mgmt <- c("GB", "GB", "GB")

# Now this next bit is super manual and gross, but it doesn't have to be pretty
  ## Assign shapes and colors to sites of different treatments
shapes <- c(23, 23, 23)
  ## BO = 21 | PBG = 22 | GB = 23 | H+ = 24

colors <- c("#abd9e9", "#abd9e9", "#abd9e9")
  ## BO = "#d73027", # red | PBG = "#fdae61", # yellow |
  ## GB = "#abd9e9", # light blue

# Get a map of the counties in which we sample and the state boundaries
map("state", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, lwd = 2.5)
map("county", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, add = T)

map(ltr, add = T)

# Add the site points
points(longlat, pch = shapes, bg = colors, cex = 0.75)
#text(longlat, as.vector(longlat$Site), cex = 1)

# Maybe add county names?
#text(c(-94, 40.5), expression(italic("Ringgold Co.")))
#text(c(-94, 40.5), expression(italic("Harrison Co.")))

# add sites
plot(gil, col = "purple", add = T)

# Draw a box around the whole map you just created
box()

# Set the position of the map inset
par(plt = c(0.1, 0.375, 0.6, 0.9), new = T)

# And set the extent the inset will cover
inset.x <- c(-97, -89)
inset.y <- c(36, 45)
plot.window(xlim = inset.x, ylim = inset.y)

# Actually put bigger area in here
map('state', c('iowa', 'missouri', 'nebraska', 'kansas', 'oklahoma', 'illinois', 'minnesota',
               'wisconsin', 'arkansas', 'south dakota', 'kentucky', 'tennessee'), 
    xlim = inset.x, ylim = inset.y, interior = F, add = T, fill = T, col = 'gray98')
box()

# Add a box into the inset showing where the more zoomed-in one is from
polygon(x = c(x1, x2, x2, x1), y = c(y1, y1, y2, y2), col = "black")

# And re-add state boundaries so the iowa/missouri border is clear in the inset
map("state", regions = c('iowa', 'missouri'), xlim = inset.x, ylim = inset.y,
    col = NA, fill = T, res = 0, add = T)

##  ----------------------------------------------------------  ##
                    # Points Map Saving ####
##  ----------------------------------------------------------  ##
# NOTE
  ## For the plotting, because we have to call all that stuff again
  ## Let's just agree to do it without any comments (to increase efficiency)

# Stuff a low-res, but checkable, jpeg into the Graphs folder
jpeg(filename = "./Site Map/GRG_Map.jpeg", width = 6, height = 6, units = "in", res = 720)
map("state", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, lwd = 2.5)
map("county", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, add = T)
points(longlat, pch = shapes, bg = colors, cex = 1)
box()
par(plt = c(0.1, 0.375, 0.6, 0.9), new = T)
plot.window(xlim = inset.x, ylim = inset.y)
map('state', c('iowa', 'missouri', 'nebraska', 'kansas', 'oklahoma', 'illinois', 'minnesota',
               'wisconsin', 'arkansas', 'south dakota', 'kentucky', 'tennessee'), 
    xlim = inset.x, ylim = inset.y, interior = F, add = T, fill = T, col = 'gray98')
box()
polygon(x = c(x1, x2, x2, x1), y = c(y1, y1, y2, y2), col = "black")
dev.off()

# Get an enhanced metafile (EMF) for actual inclusion in any publication
emf(file = "./Site Map/GRG_Map.emf", bg = "white", width = 7, height = 7, family = "Calibri", coordDPI = 350)
map("state", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, lwd = 2.5)
map("county", regions = c('iowa', 'missouri'), ylim = c(y1, y2), xlim = c(x1, x2),
    col = NA, fill = T, res = 0, add = T)
points(longlat, pch = shapes, bg = colors, cex = 2)
box()
par(plt = c(0.1, 0.375, 0.6, 0.9), new = T)
plot.window(xlim = inset.x, ylim = inset.y)
map('state', c('iowa', 'missouri', 'nebraska', 'kansas', 'oklahoma', 'illinois', 'minnesota',
               'wisconsin', 'arkansas', 'south dakota', 'kentucky', 'tennessee'), 
    xlim = inset.x, ylim = inset.y, interior = F, add = T, fill = T, col = 'gray98')
box()
polygon(x = c(x1, x2, x2, x1), y = c(y1, y1, y2, y2), col = "black")
dev.off()

# END ####

