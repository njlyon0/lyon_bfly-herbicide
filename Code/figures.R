##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                    # Herbicide Side Project - Figures
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# Required libraries
library(ggplot2); library(cowplot)

# Set working directory
setwd("~/Documents/School/Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------------------------------------------------------  ##
                                 # Housekeeping ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Read in data
bf <- read.csv("./Data/bf-wide.csv")
flr <- read.csv("./Data/flr-wide.csv")
str(bf); str(flr)

# And get the treatment levels in the right order (alpha order doesn't really make sense for our purposes)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf$Herb.Trt); unique(flr$Herb.Trt)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
bf.colors <- c("Con" = "#003c30", #  shades of teal
               "Spr" = "#35978f",
               "SnS" = "#80cdc1")
flr.colors <- c("Con" = "#8c510a", # shades of brown
                "Spr" = "#bf812d",
                "SnS" = "#dfc27d")
sct.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.background = element_blank(), legend.title = element_blank())

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Butterfly Figure ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Abundance plot
bf.abun.plt <- ggplot(bf, aes(x = Year, y = Abundance)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_fill_manual(values = bf.colors) +
  scale_color_manual(values = bf.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.7, 0.9)); bf.abun.plt

# Species richness plot
bf.dens.plt <- ggplot(bf, aes(x = Year, y = Richness)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Butterfly Richness") +
  scale_fill_manual(values = bf.colors) +
  scale_color_manual(values = bf.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.1, 0.9)); bf.dens.plt

# Diversity plot
bf.dive.plt <- ggplot(bf, aes(x = Year, y = Diversity)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_fill_manual(values = bf.colors) +
  scale_color_manual(values = bf.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.1, 0.9)); bf.dive.plt

# Make three-panel butterfly figure
plot_grid(bf.abun.plt, bf.dens.plt, bf.dive.plt, labels = c("i", "ii", "iii"), ncol = 3, nrow = 1)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_1.pdf", 
                width = 8, height = 7, units = "in")

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Flower Figure ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Flower abundance plot
flr.abun.plt <- ggplot(flr, aes(x = Year, y = Abundance)) +
  geom_smooth(method = "lm", se = F, color = "black", linetype = 2) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Floral Abundance") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.7, 0.9)); flr.abun.plt

# Flower richness
flr.dens.plt <- ggplot(flr, aes(x = Year, y = Richness)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Floral Richness") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.05, 0.85)); flr.dens.plt

flr.dens.plt2 <- ggplot(flr, aes(x = Herb.Trt, y = Richness, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Floral Richness") +
  scale_fill_manual(values = flr.colors) +
  geom_text(label = "A", x = 0.9, y = 17.5) +
  geom_text(label = "AB", x = 1.8, y = 19) +
  geom_text(label = "B", x = 2.9, y = 21.5) +
  sct.theme + theme(legend.position = "none"); flr.dens.plt2

# Flower diversity
flr.dive.plt <- ggplot(flr, aes(x = Year, y = Diversity)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Floral Diversity") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0, 0.85)); flr.dive.plt

# Make three-panel flower figure
plot_grid(flr.abun.plt, 
         plot_grid(flr.dens.plt, flr.dens.plt2, labels = c("", ""), ncol = 1, nrow = 2),
          flr.dive.plt,
         labels = c("i", "ii", "iii"), ncol = 3, nrow = 1)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_2.pdf", 
                width = 8, height = 7, units = "in")

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Seedmix Figure ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the long format flower data
flr.lng <- read.csv("./Data/flr-long.csv")

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr.lng$Herb.Trt)
flr.lng$Herb.Trt <- factor(as.character(flr.lng$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr.lng$Herb.Trt)

# Make a seedmix dataframe too
sdmx <- flr.lng %>%
  filter(Seedmix == "X") %>%
  select(Year:Patch, Herb.Trt, Seedmix, Number) %>%
  group_by(Year, Site, Patch, Herb.Trt) %>%
  summarise(Abundance = sum(Number),
            Richness = vegan::specnumber(Number))
str(sdmx)

# Plot seedmix abundance
sdmx.abun.plt <- ggplot(sdmx, aes(x = Year, y = Abundance)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Seedmix Abundance") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.15, 0.85)); sdmx.abun.plt

# Seedmix Richness Plot
sdmx.rich.plt <- ggplot(sdmx, aes(x = Year, y = Richness)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Seedmix Richness") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0, 0.85)); sdmx.rich.plt

# Make two-panel seedmix figure
plot_grid(sdmx.abun.plt, sdmx.rich.plt, labels = c("i", "ii"), ncol = 2, nrow = 1)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_3.pdf", 
                width = 8, height = 7, units = "in")

##  ----------------------------------------------------------  ##
    # Native/Exotic Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Make a native/exotic dataframe
nv.ex <- flr.lng %>%
  select(Year:Patch, Herb.Trt, L48.Status, Number) %>%
  group_by(Year, Site, Patch, Herb.Trt, L48.Status) %>%
  summarise(Number = sum(Number)) %>%
  tidyr::spread(key = "L48.Status", value = "Number", fill = 0)
str(nv.ex)

# Get a Percent Native column
nv.ex$Percent.Native <- with(nv.ex, (N / (N + E)) * 100)

# Plot
nv.ex.plt <- ggplot(nv.ex, aes(x = Year, y = Percent.Native)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Percent Native Flowers") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = flr.colors) +
  scale_color_manual(values = flr.colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0, 0.85)); nv.ex.plt

# Save it
cowplot::ggsave(plot = last_plot(), filename = "./Figures/Figure_4.pdf", 
                width = 8, height = 7, units = "in")


# END ####

