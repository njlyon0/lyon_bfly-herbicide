##  --------------------------------------------------------------------------------------  ##
                # Butterfly & Floral Resource Response to Spray and Seed ####
##  --------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Reviewers asked for a butterfly species checklist to go with the paper

# Required libraries
library(tidyverse)

# Set working directory
setwd("~/Documents/_Publications/2021_Lyon_Butterfly SnS/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

## -------------------------------------------- ##
            # Checklist Creation ####
## -------------------------------------------- ##
# Read in data
bf <- read.csv("./Data/bf-long.csv")
str(bf)

# Do several tidying steps all in one
bf.v2 <- bf %>%
    ## Sum through everything but year and butterfly species
  group_by(Year, Butterfly.Common.Name) %>%
  summarise(Number = sum(Number)) %>%
    ## Pivot to wide format
  pivot_wider(id_cols = Butterfly.Common.Name, names_from = Year,
              values_from = Number, values_fill = 0)
    
# Check the structure
str(bf.v2)

# Read in an earlier summary table with scientific names
bf.spp.info <- read.csv("./Tables/bf-tables.csv")

# Let's include Latin names in our checklist in case folks use different common names
bf.v2$Butterfly.Scientific.Name <- bf.spp.info$Scientific.Name[match(bf.v2$Butterfly.Common.Name,
                                                                     bf.spp.info$Common.Name)]

# Did they all transfer?
unique(is.na(bf.v2$Butterfly.Scientific.Name))
  ## Yep!

# Let's smush some columns around and we'll be done
bf.v3 <- select(bf.v2, Butterfly.Common.Name, Butterfly.Scientific.Name, `14`:`18`)

# Final structure and content check
str(bf.v3)
summary(bf.v3)

# Looks great, save it out!
write.csv(bf.v3, "./Tables/bf-sns-checklist.csv", row.names = F)

## -------------------------------------------- ##
        # Species Relative Abundances ####
## -------------------------------------------- ##
# Get the total abundance of each bfly species across the study years
bf.spp <- bf %>%
  ## Sum through everything but year and butterfly species
  group_by(Butterfly.Common.Name) %>%
  summarise(Number = sum(Number)) %>%
  ## Pivot to wide format
  as.data.frame()

str(bf.spp)


# END ####
