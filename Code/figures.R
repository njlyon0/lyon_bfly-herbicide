##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                    # Herbicide Side Project - Figures
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Required libraries
library(ggplot2); library(cowplot)

# Set working directory
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------------------------------------------------------  ##
                                 # Housekeeping ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Read in data
bf <- read.csv("./Data/bf-wide.csv")
flr <- read.csv("./Data/flr-wide.csv")
str(bf); str(flr)

# Make year a factor!
bf$Year <- as.factor(bf$Year)
flr$Year <- as.factor(flr$Year)
str(bf$Year); str(flr$Year)

# And get the treatment levels in the right order (alpha order doesn't really make sense for our purposes)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf$Herb.Trt); unique(flr$Herb.Trt)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
bf.colors <- c("Con" = "#2b8cbe", # shades of blue
               "Spr" = "#7bccc4",
               "SnS" = "#e0f3db")
flr.colors <- c("Con" = "#8c510a", # shades of brown
                "Spr" = "#bf812d",
                "SnS" = "#dfc27d")
yr.colors <- c("14" = "#ffffcc", # shades of reddish orange
               "15" = "#ffeda0",
               "16" = "#feb24c",
               "17" = "#fc4e2a",
               "18" = "#bd0026")
box.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.position = 'none')
sct.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.background = element_blank(), legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Butterfly Figure ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Abundance plot
bf.abun.plt <- ggplot(bf, aes(x = Year, y = Abundance, fill = Year)) +
  geom_boxplot(outlier.shape = 16) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_fill_manual(values = yr.colors) +
  geom_text(label = "AB", x = 0.7, y = 85) +
  geom_text(label = "AB", x = 1.7, y = 80) +
  geom_text(label = "A", x = 2.8, y = 120) +
  geom_text(label = "AB", x = 3.7, y = 87) +
  geom_text(label = "B", x = 4.8, y = 52) +
  box.theme; bf.abun.plt

# Species richness plot
bf.dens.plt <- ggplot(bf, aes(x = Herb.Trt, y = Species.Density, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 16) +
  labs(x = "Herbicide Treatment", y = "Butterfly Richness") + 
  scale_fill_manual(values = bf.colors) +
  box.theme; bf.dens.plt

# Diversity plot
bf.dive.plt <- ggplot(bf, aes(x = Year, y = Diversity, fill = Year)) +
  geom_boxplot(outlier.shape = 16) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_fill_manual(values = yr.colors) +
  geom_text(label = "A", x = 0.8, y = 1.87) +
  geom_text(label = "A", x = 1.8, y = 1.83) +
  geom_text(label = "AB", x = 2.7, y = 1.89) +
  geom_text(label = "AB", x = 3.7, y = 1.9) +
  geom_text(label = "B", x = 4.8, y = 2.04) +
  box.theme; bf.dive.plt

# Make three-panel butterfly figure
plot_grid(bf.abun.plt, bf.dens.plt, bf.dive.plt, labels = c("i", "ii", "iii"), ncol = 3, nrow = 1)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_1.pdf", 
                width = 8, height = 7, units = "in")

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Flower Figure ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Flower abundance plot
flr.abun.plt <- ggplot(flr, aes(x = Year, y = Abundance, fill = Year)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Year", y = "Floral Abundance") +
  scale_fill_manual(values = yr.colors) +
  geom_text(label = "A", x = 0.8, y = 2250) +
  geom_text(label = "A", x = 1.8, y = 2750) +
  geom_text(label = "AB", x = 2.7, y = 6300) +
  geom_text(label = "B", x = 3.8, y = 8500) +
  geom_text(label = "AB", x = 4.7, y = 4900) +
  box.theme; flr.abun.plt

# Flower richness
  ## Make plotting dataframe
flr.dens.pltdf <- summarySE(data = flr, measurevar = "Species.Density",
                        groupvars = c("Composite.Variable", "Herb.Trt", "Year"))
flr.dens.pltdf$Year <- as.numeric(as.character(flr.dens.pltdf$Year))

  ## Make plot
flr.dens.plt <- ggplot(flr.dens.pltdf, aes(x = Year, y = Species.Density, color = Herb.Trt, shape = Herb.Trt)) +
  geom_path(aes(group = Herb.Trt), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Species.Density + se, ymin = Species.Density - se),
                position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  labs(x = "Year", y = "Flower Richness") +
  scale_color_manual(values = flr.colors) +
  sct.theme + theme(legend.position = c(0.0, 0.8)); flr.dens.plt

# Flower diversity
flr.dive.plt <- ggplot(flr, aes(x = Herb.Trt, y = Diversity, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Flower Diversity") + 
  scale_fill_manual(values = flr.colors) +
  box.theme; flr.dive.plt

# Make three-panel flower figure
plot_grid(flr.abun.plt, flr.dens.plt, flr.dive.plt, labels = c("i", "ii", "iii"), ncol = 3, nrow = 1)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_2.pdf", 
                width = 8, height = 7, units = "in")

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Seedmix Figure ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Prep the dataframe
  ## Get the long format flower data
flr.lng <- read.csv("./Data/flr-long.csv")

  ## Make year a factor!
flr.lng$Year <- as.factor(flr.lng$Year)
str(flr.lng$Year)

  ## And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr.lng$Herb.Trt)
flr.lng$Herb.Trt <- factor(as.character(flr.lng$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr.lng$Herb.Trt)

  ## Get a subset for seed-mix species
sdmx.v0 <- subset(flr.lng, flr.lng$Seedmix == "X")

  ## Get aggregated values
sdmx.v1 <- aggregate(TransectTotals ~ Composite.Variable + Year + Site + Patch +
                       Herb.Trt + Nectar.Plant.Name, FUN = sum, data = sdmx.v0)

  ## Spread both to wide format
sdmx.v2 <- spread(sdmx.v1, key = "Nectar.Plant.Name", value = "TransectTotals", fill = 0)

  ## Calculate abundance and species density for seed-mix species and percent native for native/exotics
sdmx.v2$Abundance <- rowSums(sdmx.v2[,-c(1:5)])
sdmx.v2$Species.Density <- vegan::specnumber(sdmx.v2[,-c(1:5)])

  ## Save both as more easily called dataframes
sdmx <- sdmx.v2

# Plot abundance
sdmx.abun.plt <- ggplot(sdmx, aes(x = Year, y = Abundance, fill = Year)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Year", y = "Seedmix Abundance") +
  scale_fill_manual(values = yr.colors) +
  geom_text(label = "A", x = 0.8, y = 125) +
  geom_text(label = "AB", x = 1.7, y = 325) +
  geom_text(label = "B", x = 2.8, y = 950) +
  geom_text(label = "B", x = 3.8, y = 550) +
  geom_text(label = "B", x = 4.8, y = 450) +
  box.theme; sdmx.abun.plt

# Plot the species density information
  ## Get plotting dataframe
sdmx.dens.pltdf <- summarySE(data = sdmx, measurevar = "Species.Density",
                             groupvars = c("Composite.Variable", "Herb.Trt", "Year"))
sdmx.dens.pltdf$Year <- as.numeric(as.character(sdmx.dens.pltdf$Year))

# Plot
sdmx.dens.plt <- ggplot(sdmx.dens.pltdf, aes(x = Year, y = Species.Density, color = Herb.Trt, shape = Herb.Trt)) +
  geom_path(aes(group = Herb.Trt), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Species.Density + se, ymin = Species.Density - se),
                position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  labs(x = "Year", y = "Seedmix Richness") +
  scale_color_manual(values = flr.colors) +
  sct.theme + theme(legend.position = c(0, 0.8)); sdmx.dens.plt

# Make two-panel flower figure
plot_grid(sdmx.abun.plt, sdmx.dens.plt, labels = c("i", "ii"), ncol = 2, nrow = 1)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_3.pdf", 
                width = 8, height = 7, units = "in")

# END ####

