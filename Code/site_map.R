##  --------------------------------------------------------------------------------------  ##
                # Butterfly & Floral Resource Response to Spray and Seed ####
##  --------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE #### 
  ## Everybody likes a nice clean site map, so that's what this script will give 'em

# Necessary libraries
library(rgdal); library(sp); library(raster)
                
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
jpeg(filename = "./Figures/Figure 1.jpeg", width = 6, height = 6, units = "in", res = 720)
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

# END ####

