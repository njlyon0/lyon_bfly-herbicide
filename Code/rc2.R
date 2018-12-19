##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                         # Herbicide Side Project - Richardson 2 Observations
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Because RC2 is the only un-grazed and un-burned site, it is not representative of broader treatment response
  ## SO, we generate here figures that indicate the response of this site for observational purposes
  ## While this does not hold the robustness of frequentist statistical methods, it is informative

# Required libraries
library(ggplot2); library(cowplot)

# Set working directory
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

##  ----------------------------------------------------------------------------------------------------------  ##
                                    # Housekeeping ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# Pull in data
bf.rc2 <- read.csv("./Data/bf-rc2-wide.csv")
flr.rc2 <- read.csv("./Data/flr-rc2-wide.csv")

# Re-level herbicide treatment levels
unique(bf.rc2$Herb.Trt); unique(flr.rc2$Herb.Trt)
bf.rc2$Herb.Trt <- factor(as.character(bf.rc2$Herb.Trt), levels = c("Con", "Spr", "SnS"))
flr.rc2$Herb.Trt <- factor(as.character(flr.rc2$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf.rc2$Herb.Trt); unique(flr.rc2$Herb.Trt)

# Plotting shortcuts
bf.cols <- c("Con" = "#8e0152", "Spr" = "#c51b7d", "SnS" = "#de77ae")
flr.cols <- c("Con" = "#4d9221", "Spr" = "#7fbc41", "SnS" = "#b8e186")
shapes <- c("Con" = 21, "Spr" = 22, "SnS" = 24)
dodge <- position_dodge(width = 0.1)

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Butterfly Graphs ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Abundance
bf.rc2.abun <- ggplot(bf.rc2, aes(x = Year, y = Abundance, color = Herb.Trt, shape = Herb.Trt)) +
  geom_point(position = dodge) +
  geom_path(position = dodge) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_color_manual(values = bf.cols) +
  theme(legend.position = c(0.7, 0.8), legend.title = element_blank()); bf.rc2.abun

# Species density/richness
bf.rc2.dens <- ggplot(bf.rc2, aes(x = Year, y = Species.Density, color = Herb.Trt, shape = Herb.Trt)) +
  geom_point(position = dodge) +
  geom_path(position = dodge) +
  labs(x = "Year", y = "Butterfly Richness") +
  scale_color_manual(values = bf.cols) +
  theme(legend.position = "none"); bf.rc2.dens

# Diversity
bf.rc2.dive <- ggplot(bf.rc2, aes(x = Year, y = Diversity, color = Herb.Trt, shape = Herb.Trt)) +
  geom_point(position = dodge) +
  geom_path(position = dodge) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_color_manual(values = bf.cols) +
  theme(legend.position = "none"); bf.rc2.dive

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Floral Graphs ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Abundance
flr.rc2.abun <- ggplot(flr.rc2, aes(x = Year, y = Abundance, color = Herb.Trt, shape = Herb.Trt)) +
  geom_point(position = dodge) +
  geom_path(position = dodge) +
  labs(x = "Year", y = "Floral Abundance") +
  scale_color_manual(values = flr.cols) +
  theme(legend.position = c(0, 0.8), legend.title = element_blank()); flr.rc2.abun

# Species density/richness
flr.rc2.dens <- ggplot(flr.rc2, aes(x = Year, y = Species.Density, color = Herb.Trt, shape = Herb.Trt)) +
  geom_point(position = dodge) +
  geom_path(position = dodge) +
  labs(x = "Year", y = "Floral Richness") +
  scale_color_manual(values = flr.cols) +
  theme(legend.position = "none"); flr.rc2.dens

# Diversity
flr.rc2.dive <- ggplot(flr.rc2, aes(x = Year, y = Diversity, color = Herb.Trt, shape = Herb.Trt)) +
  geom_point(position = dodge) +
  geom_path(position = dodge) +
  labs(x = "Year", y = "Floral Diversity") +
  scale_color_manual(values = flr.cols) +
  theme(legend.position = "none"); flr.rc2.dive

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Figure Creation ####
##  ----------------------------------------------------------------------------------------------------------  ##
plot_grid(bf.rc2.abun, bf.rc2.dens, bf.rc2.dive, flr.rc2.abun, flr.rc2.dens, flr.rc2.dive, 
          labels = c("A", "B", "C", "D", "E", "F"), ncol = 3, nrow = 2)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Richardson2.pdf",
                width = 8, height = 7, units = "in")

# END ####



