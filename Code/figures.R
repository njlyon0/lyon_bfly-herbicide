##  ----------------------------------------------------------------------------------  ##
                            # Herbicide Side Project - Figures
##  ----------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Required libraries
library(tidyverse); library(Rmisc); library(ggplot2); library(cowplot)
library(gridExtra); library(egg)

# Set working directory
myWD <- getwd()
myWD
  ## Should end in the project's directory

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
                        # Housekeeping ####
##  ----------------------------------------------------------  ##
# Read in data
bf.v0 <- read.csv("./Data/bf-wide.csv")
flr.v0 <- read.csv("./Data/flr-wide.csv")
str(bf.v0); str(flr.v0)

# And get the treatment levels in the right order (alpha order doesn't really make sense for our purposes)
bf.v0$Herb.Trt <- factor(as.character(bf.v0$Herb.Trt), levels = c("Con", "Spr", "SnS"))
flr.v0$Herb.Trt <- factor(as.character(flr.v0$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf.v0$Herb.Trt); unique(flr.v0$Herb.Trt)

# Make the years be full year numbers
unique(bf.v0$Year); unique(flr.v0$Year)
bf.v0$Year <- as.numeric(paste0("20", bf.v0$Year))
flr.v0$Year <- as.numeric(paste0("20", flr.v0$Year))
unique(bf.v0$Year); unique(flr.v0$Year)

# Get separated 2014 and 15-18 datasets
bf.14 <- subset(bf.v0, Year == 2014)
bf <- subset(bf.v0, Year != 2014)
flr.14 <- subset(flr.v0, Year == 2014)
flr <- subset(flr.v0, Year != 2014)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
bf.colors <- c("Con" = "#003c30", #  shades of teal
               "Spr" = "#35978f",
               "SnS" = "#80cdc1")
flr.colors <- c("Con" = "#8c510a", # shades of brown
                "Spr" = "#bf812d",
                "SnS" = "#dfc27d")
sdmx.colors <- c("Con" = "#49006a", # shades of purples
                  "Spr" = "#ae017e",
                  "SnS" = "#dd3497")
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    axis.title = element_text(size = 14), axis.text = element_text(size = 12),
                    legend.title = element_blank(), legend.position = "none")
no.y.axis <- theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(), axis.line.y = element_blank())
pref.panel.bord <- panel_border(color = "black", size = 0.75)

##  ----------------------------------------------------------  ##
                     # Butterfly Figure ####
##  ----------------------------------------------------------  ##
# Abundance plots
  ## Pre-Treatment (2014)
bf.14.abun.pltdf <- summarySE(bf.14, measurevar = "Abundance", groupvars = "Herb.Trt")
bf.abun.pltdf <- summarySE(bf, measurevar = "Abundance", groupvars = c("Herb.Trt", "Year"))

bf.14.abun.plt <- ggplot(bf.14.abun.pltdf, aes(x = Herb.Trt, y = Abundance)) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = bf.colors) +
  scale_fill_manual(values = bf.colors) +
  ylim(0, 135) +
  labs(x = "Pre-treatment", y = "Butterfly abundance") +
  pref.theme + theme(axis.title.x = element_blank()) +
  pref.panel.bord; bf.14.abun.plt

  ## Post-Treatment (2015-18)
bf.abun.plt <- ggplot(bf.abun.pltdf, aes(x = Year, y = Abundance)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 135) +
  scale_fill_manual(values = bf.colors) +
  scale_color_manual(values = bf.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + theme(legend.position = c(0.85, 0.75), axis.title.x = element_blank()) +
  no.y.axis + pref.panel.bord; bf.abun.plt

  ## Make a two-panel figure
bf.abun.fig <- egg::ggarrange(bf.14.abun.plt, bf.abun.plt, nrow = 1, widths = c(1, 2.5))

# Richness plots
  ## Pre-Treatment (2014)
bf.14.rich.pltdf <- summarySE(bf.14, measurevar = "Richness", groupvars = "Herb.Trt")
bf.rich.pltdf <- summarySE(bf, measurevar = "Richness", groupvars = c("Herb.Trt", "Year"))

bf.14.rich.plt <- ggplot(bf.14.rich.pltdf, aes(x = Herb.Trt, y = Richness)) +
  geom_errorbar(aes(ymax = Richness + se, ymin = Richness - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = bf.colors) +
  scale_fill_manual(values = bf.colors) +
  ylim(0, 16) +
  labs(x = "Pre-treatment", y = "Butterfly richness") +
  pref.theme + theme(axis.title.x = element_blank()) + pref.panel.bord; bf.14.rich.plt

  ## Post-Treatment (2015-18)
bf.rich.plt <- ggplot(bf.rich.pltdf, aes(x = Year, y = Richness)) +
  #geom_smooth(method = "lm", se = F, color = "black") +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Richness + se, ymin = Richness - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 16) +
  scale_fill_manual(values = bf.colors) +
  scale_color_manual(values = bf.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + theme(axis.title.x = element_blank()) + no.y.axis + pref.panel.bord; bf.rich.plt

  ## Make a two-panel figure
bf.rich.fig <- egg::ggarrange(bf.14.rich.plt, bf.rich.plt, nrow = 1, widths = c(1, 2.5))

# Diversity plots
  ## Pre-Treatment (2014)
bf.14.dive.pltdf <- summarySE(bf.14, measurevar = "Diversity", groupvars = "Herb.Trt")
bf.dive.pltdf <- summarySE(bf, measurevar = "Diversity", groupvars = c("Herb.Trt", "Year"))

bf.14.dive.plt <- ggplot(bf.14.dive.pltdf, aes(x = Herb.Trt, y = Diversity)) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = bf.colors) +
  scale_fill_manual(values = bf.colors) +
  ylim(0, 2.75) +
  labs(x = "Pre-treatment", y = "Butterfly diversity") +
  pref.theme + pref.panel.bord; bf.14.dive.plt

  ## Post-Treatment (2015-18)
bf.dive.plt <- ggplot(bf.dive.pltdf, aes(x = Year, y = Diversity)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 2.75) +
  scale_fill_manual(values = bf.colors) +
  scale_color_manual(values = bf.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + no.y.axis + pref.panel.bord; bf.dive.plt

  ## Make a two-panel figure
bf.dive.fig <- egg::ggarrange(bf.14.dive.plt, bf.dive.plt, nrow = 1, widths = c(1, 2.5))

# Make three-panel butterfly figure
plot_grid(bf.abun.fig, bf.rich.fig, bf.dive.fig, labels = c("A", "B", "C"),
          align = 'v', ncol = 1, nrow = 3)

# Save it
ggsave(plot = last_plot(), filename = "./Figures/Figure 3.pdf", width = 7, height = 9, units = "in")

##  ----------------------------------------------------------  ##
                        # Flower Figure ####
##  ----------------------------------------------------------  ##
# Abundance plots
  ## Pre-Treatment (2014)
flr.14.abun.pltdf <- summarySE(flr.14, measurevar = "Abundance", groupvars = "Herb.Trt")
flr.abun.pltdf <- summarySE(flr, measurevar = "Abundance", groupvars = c("Herb.Trt", "Year"))

flr.14.abun.plt <- ggplot(flr.14.abun.pltdf, aes(x = Herb.Trt, y = Abundance)) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = flr.colors) +
  scale_fill_manual(values = flr.colors) +
  ylim(0, 7000) +
  labs(x = "Pre-treatment", y = "Floral abundance") +
  pref.theme + theme(axis.title.x = element_blank()) + pref.panel.bord; flr.14.abun.plt

  ## Post-Treatment (2015-18)
flr.abun.plt <- ggplot(flr.abun.pltdf, aes(x = Year, y = Abundance)) +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 7000) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + theme(legend.position = c(0.8, 0.7), axis.title.x = element_blank()) +
  no.y.axis + pref.panel.bord; flr.abun.plt

  ## Make a two-panel figure
flr.abun.fig <- egg::ggarrange(flr.14.abun.plt, flr.abun.plt, nrow = 1, widths = c(1, 2.5))

# Richness plots
  ## Pre-Treatment (2014)
flr.14.rich.pltdf <- summarySE(flr.14, measurevar = "Richness", groupvars = "Herb.Trt")
flr.rich.pltdf <- summarySE(flr, measurevar = "Richness", groupvars = c("Herb.Trt", "Year"))

flr.14.rich.plt <- ggplot(flr.14.rich.pltdf, aes(x = Herb.Trt, y = Richness)) +
  geom_errorbar(aes(ymax = Richness + se, ymin = Richness - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = flr.colors) +
  scale_fill_manual(values = flr.colors) +
  ylim(0, 25) +
  labs(x = "Pre-treatment", y = "Floral richness") +
  pref.theme + theme(axis.title.x = element_blank()) + pref.panel.bord; flr.14.rich.plt

  ## Post-Treatment (2015-18)
flr.rich.plt <- ggplot(flr.rich.pltdf, aes(x = Year, y = Richness)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Richness + se, ymin = Richness - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 25) +
  geom_text(label = "A", x = 15.5, y = 12) +
  geom_text(label = "AB", x = 14.7, y = 14.5) +
  geom_text(label = "B", x = 15.3, y = 18) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + theme(axis.title.x = element_blank()) + no.y.axis + pref.panel.bord; flr.rich.plt

  ## Make a two-panel figure
flr.rich.fig <- egg::ggarrange(flr.14.rich.plt, flr.rich.plt, nrow = 1, widths = c(1, 2.5))

# Diversity plots
  ## Pre-Treatment (2014)
flr.14.dive.pltdf <- summarySE(flr.14, measurevar = "Diversity", groupvars = "Herb.Trt")
flr.dive.pltdf <- summarySE(flr, measurevar = "Diversity", groupvars = c("Herb.Trt", "Year"))

flr.14.dive.plt <- ggplot(flr.14.dive.pltdf, aes(x = Herb.Trt, y = Diversity)) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = flr.colors) +
  scale_fill_manual(values = flr.colors) +
  ylim(0, 2.75) +
  labs(x = "Pre-treatment", y = "Floral diversity") +
  pref.theme + pref.panel.bord; flr.14.dive.plt

## Post-Treatment (2015-18)
flr.dive.plt <- ggplot(flr.dive.pltdf, aes(x = Year, y = Diversity)) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_smooth(method = "lm", se = F, color = "black") +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 2.75) +
  geom_text(label = "AB", x = 15.5, y = 1.1) + # Spr
  geom_text(label = "A", x = 14.7, y = 1.35) + # Con
  geom_text(label = "B", x = 15.3, y = 2) + # SnS
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + no.y.axis + pref.panel.bord; flr.dive.plt

  ## Make a two-panel figure
flr.dive.fig <- egg::ggarrange(flr.14.dive.plt, flr.dive.plt, nrow = 1, widths = c(1, 2.5))

# Make three-panel butterfly figure
plot_grid(flr.abun.fig, flr.rich.fig, flr.dive.fig, labels = c("A", "B", "C"),
          align = 'v', ncol = 1, nrow = 3)

# Save it
ggsave(plot = last_plot(), filename = "./Figures/Figure 2.pdf", width = 7, height = 9, units = "in")

##  ----------------------------------------------------------  ##
                      # Seedmix Figure ####
##  ----------------------------------------------------------  ##
# Get the long format flower data
flr.lng <- read.csv("./Data/flr-long.csv")

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr.lng$Herb.Trt)
flr.lng$Herb.Trt <- factor(as.character(flr.lng$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr.lng$Herb.Trt)

# Make a seedmix dataframe too
sdmx.ver0 <- flr.lng %>%
  filter(Seedmix == "X") %>%
  dplyr::select(Year:Patch, Herb.Trt, Seedmix, Number) %>%
  group_by(Year, Site, Patch, Herb.Trt) %>%
  dplyr::summarise(Abundance = sum(Number),
            Richness = vegan::specnumber(Number)) %>%
  as.data.frame()
str(sdmx.ver0)

# Fix the year column as above
unique(sdmx.ver0$Year)
sdmx.ver0$Year <- as.numeric(paste0("20", sdmx.ver0$Year))
unique(sdmx.ver0$Year)

# Split 2014 from the rest
sdmx.14 <- subset(sdmx.ver0, Year == 2014)
sdmx <- subset(sdmx.ver0, Year != 2014)

# Seedmix abundance stuff
  ## Pre-Treatment (2014)
sdmx.14.abun.pltdf <- summarySE(sdmx.14, measurevar = "Abundance", groupvars = "Herb.Trt")
sdmx.abun.pltdf <- summarySE(sdmx, measurevar = "Abundance", groupvars = c("Herb.Trt", "Year"))

sdmx.14.abun.plt <- ggplot(sdmx.14.abun.pltdf, aes(x = Herb.Trt, y = Abundance)) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = sdmx.colors) +
  scale_fill_manual(values = sdmx.colors) +
  ylim(0, 1500) +
  labs(x = "Pre-treatment", y = "Seeded species abundance") +
  pref.theme + theme(axis.title.x = element_blank()) + pref.panel.bord; sdmx.14.abun.plt

  ## Post-Treatment (2015-18)
sdmx.abun.plt <- ggplot(sdmx.abun.pltdf, aes(x = Year, y = Abundance)) +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 1500) +
  scale_fill_manual(values = sdmx.colors) +
  scale_color_manual(values = sdmx.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + theme(legend.position = c(0.85, 0.7)) +
  theme(axis.title.x = element_blank()) + no.y.axis + pref.panel.bord; sdmx.abun.plt

  ## Make a two-panel figure
sdmx.abun.fig <- egg::ggarrange(sdmx.14.abun.plt, sdmx.abun.plt, nrow = 1, widths = c(1, 2.5))

# Richness plots
  ## Pre-Treatment (2014)
sdmx.14.rich.pltdf <- summarySE(sdmx.14, measurevar = "Richness", groupvars = "Herb.Trt")
sdmx.rich.pltdf <- summarySE(sdmx, measurevar = "Richness", groupvars = c("Herb.Trt", "Year"))

sdmx.14.rich.plt <- ggplot(sdmx.14.rich.pltdf, aes(x = Herb.Trt, y = Richness)) +
  geom_errorbar(aes(ymax = Richness + se, ymin = Richness - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = sdmx.colors) +
  scale_fill_manual(values = sdmx.colors) +
  ylim(0, 8.5) +
  labs(x = "Pre-treatment", y = "Seeded species richness") +
  pref.theme + pref.panel.bord; sdmx.14.rich.plt

## Post-Treatment (2015-18)
sdmx.rich.plt <- ggplot(sdmx.rich.pltdf, aes(x = Year, y = Richness)) +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Richness + se, ymin = Richness - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 8.5) +
  scale_fill_manual(values = sdmx.colors) +
  scale_color_manual(values = sdmx.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + no.y.axis + pref.panel.bord; sdmx.rich.plt

  ## Make a two-panel figure
sdmx.rich.fig <- egg::ggarrange(sdmx.14.rich.plt, sdmx.rich.plt, nrow = 1, widths = c(1, 2.5))

# Make two panel seedmix figure
plot_grid(sdmx.abun.fig, sdmx.rich.fig, labels = c("A", "B"), align = 'v', ncol = 1, nrow = 2)

# Save it
ggsave(plot = last_plot(), filename = "./Figures/Figure 4.pdf", width = 7, height = 9, units = "in")

##  ----------------------------------------------------------  ##
            # Native/Exotic Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Make a native/exotic dataframe
nv.ex.ver0 <- flr.lng %>%
  dplyr::select(Year:Patch, Herb.Trt, L48.Status, Number) %>%
  group_by(Year, Site, Patch, Herb.Trt, L48.Status) %>%
  dplyr::summarise(Number = sum(Number)) %>%
  tidyr::spread(key = "L48.Status", value = "Number", fill = 0) %>%
  as.data.frame()
str(nv.ex.ver0)

# Get a Percent Native column
nv.ex.ver0$Percent.Native <- with(nv.ex.ver0, (N / (N + E)) * 100)

# Fix the year column
unique(nv.ex.ver0$Year)
nv.ex.ver0$Year <- as.numeric(paste0("20", nv.ex.ver0$Year))
unique(nv.ex.ver0$Year)

# Split 2014 off
nv.ex.14 <- subset(nv.ex.ver0, Year == 2014)
nv.ex <- subset(nv.ex.ver0, Year != 2014)

## Pre-Treatment (2014)
nv.ex.14.pltdf <- summarySE(nv.ex.14, measurevar = "Percent.Native", groupvars = "Herb.Trt")
nv.ex.pltdf <- summarySE(nv.ex, measurevar = "Percent.Native", groupvars = c("Herb.Trt", "Year"))

nv.ex.14.plt <- ggplot(nv.ex.14.pltdf, aes(x = Herb.Trt, y = Percent.Native)) +
  geom_errorbar(aes(ymax = Percent.Native + se, ymin = Percent.Native - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt), position = dodge, size = 2.5, shape = 22:24) +
  #geom_text(label = "NS", x = 0.7, y = 120) + 
  scale_color_manual(values = sdmx.colors) +
  scale_fill_manual(values = sdmx.colors) +
  ylim(0, 70) +
  labs(x = "Pre-treatment", y = "Percent native flowers") +
  pref.theme + pref.panel.bord; nv.ex.14.plt

## Post-Treatment (2015-18)
nv.ex.plt <- ggplot(nv.ex.pltdf, aes(x = Year, y = Percent.Native)) +
  #geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_errorbar(aes(ymax = Percent.Native + se, ymin = Percent.Native - se,
                    color = Herb.Trt), width = 0.45, position = dodge) +
  geom_point(aes(fill = Herb.Trt, shape = Herb.Trt), size = 2, position = dodge) +
  labs(x = "Post-treatment") +
  ylim(0, 70) +
  scale_fill_manual(values = sdmx.colors) +
  scale_color_manual(values = sdmx.colors) +
  scale_shape_manual(values = 22:24) +
  pref.theme + theme(legend.position = c(0.3, 0.825)) + no.y.axis + pref.panel.bord; nv.ex.plt

## Make a two-panel figure
nv.ex.fig <- egg::ggarrange(nv.ex.14.plt, nv.ex.plt, nrow = 1, widths = c(1, 2.5))

# Save it
ggsave(plot = nv.ex.fig, filename = "./Figures/Figure 5.pdf", width = 8, height = 7, units = "in")


# END ####

