##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                    # Herbicide Side Project - Tables
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Want to include tables of each species' abundance in each of the three treatments with a table per year
  ## Easiest to make these in R, export to .csv, and paste into Word from Excel

# Required libraries
library(tidyr)

# Set working directory
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------------------------------------------------------  ##
                                 # Initial Data Summarization ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Read in data
bf <- read.csv("./Data/bf-long.csv")
flr <- read.csv("./Data/flr-long.csv")
str(bf); str(flr)

# Sum through each species within each year and treatment (get one value per treatment, species, and year)
bf.v2 <- aggregate(Number ~ BFLY.Common.Name + Herb.Trt + Year, data = bf, FUN = sum)
flr.v2 <- aggregate(TransectTotals ~ Nectar.Plant.Name + L48.Status + Herb.Trt + Year, data = flr, FUN = sum)

# Re-level the herbicide treatment factor column
bf.v2$Herb.Trt <- factor(as.character(bf.v2$Herb.Trt), levels = c("Con", "Spr", "SnS"))
flr.v2$Herb.Trt <- factor(as.character(flr.v2$Herb.Trt), levels = c("Con", "Spr", "SnS"))
levels(bf.v2$Herb.Trt); levels(flr.v2$Herb.Trt)

# Get them into wide format
bf.v3 <- spread(data = bf.v2, key = Herb.Trt, value = Number, fill = NA)
flr.v3 <- spread(data = flr.v2, key = Herb.Trt, value = TransectTotals, fill = NA)

##  ----------------------------------------------------------------------------------------------------------  ##
                      # Prep Species Information Index Files ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Read in species info files
bf.info <- read.csv("./Summary Info/bf-spp-info.csv")
flr.info <- read.csv("./Summary Info/flr-spp-info.csv")

# Eliminate all but the necessary columns (i.e. only Latin and English)
bf.info.v2 <- bf.info[,1:3]
flr.info.v2 <- flr.info[,1:3]

# Get a single column with genus and species
bf.info.v2$Latin <- paste0(bf.info.v2$Genus, " ", bf.info.v2$species)
flr.info.v2$Latin <- paste0(flr.info.v2$Genus, " ", flr.info.v2$species)

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Final Table Creation ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Make new dataframes in case something goes wrong
bf.v4 <- bf.v3
flr.v4 <- flr.v3

# Stuff in species information
bf.v4$Scientific <- bf.info.v2$Latin[match(bf.v4$BFLY.Common.Name, tolower(bf.info.v2$Common.Name))]
flr.v4$Scientific <- flr.info.v2$Latin[match(flr.v4$Nectar.Plant.Name, tolower(flr.info.v2$Common.Name))]

# Double check to make sure everything came across fine
sort(unique(bf.v4$BFLY.Common.Name[is.na(bf.v4$Scientific)]))
sort(unique(flr.v4$Nectar.Plant.Name[is.na(flr.v4$Scientific)]))
  ## "factor(0)" means that everything transferred as desired

# Re-order columns,
bf.v5 <- bf.v4[, c(2, 1, 6, 3:5)]
flr.v5 <- flr.v4[, c(3, 1, 7, 2, 4:6)]

# Standardize column names for the butterfly table
colnames(bf.v5)
colnames(bf.v5) <- c("Year", "Common Name", "Scientific Name", "Con", "Spr", "SnS")
colnames(bf.v5)

# Standardize them for the flower table
colnames(flr.v5)
colnames(flr.v5) <- c("Year", "Common Name", "Scientific Name", "Status", "Con", "Spr", "SnS")
colnames(flr.v5)

# Save them both
write.csv(bf.v5, "./Tables/bf-tables.csv", row.names = F)
write.csv(flr.v5, "./Tables/flr-tables.csv", row.names = F)

# END ####

