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
myWD <- getwd()
myWD
  ## Should end in the project's directory

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
bf.rc2.abun <- ggplot(bf.rc2, aes(x = Year, y = Abundance)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_fill_manual(values = bf.cols) +
  scale_color_manual(values = bf.cols) +
  scale_shape_manual(values = 21:23) +
  theme(legend.position = c(0.7, 0.8), legend.title = element_blank()); bf.rc2.abun

# Species density/richness
bf.rc2.dens <- ggplot(bf.rc2, aes(x = Year, y = Richness)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Butterfly Richness") +
  scale_fill_manual(values = bf.cols) +
  scale_color_manual(values = bf.cols) +
  scale_shape_manual(values = 21:23) +
  theme(legend.position = c(0, 0.8), legend.title = element_blank()); bf.rc2.dens

# Diversity
bf.rc2.dive <- ggplot(bf.rc2, aes(x = Year, y = Diversity)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_fill_manual(values = bf.cols) +
  scale_color_manual(values = bf.cols) +
  scale_shape_manual(values = 21:23) +
  theme(legend.position = c(0.7, 0.2), legend.title = element_blank()); bf.rc2.dive

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Floral Graphs ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Abundance
flr.rc2.abun <- ggplot(flr.rc2, aes(x = Year, y = Abundance)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Floral Abundance") +
  scale_fill_manual(values = flr.cols) +
  scale_color_manual(values = flr.cols) +
  scale_shape_manual(values = 21:23) +
  theme(legend.position = c(0, 0.8), legend.title = element_blank()); flr.rc2.abun

# Species density/richness
flr.rc2.dens <- ggplot(flr.rc2, aes(x = Year, y = Richness)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Floral Richness") +
  scale_fill_manual(values = flr.cols) +
  scale_color_manual(values = flr.cols) +
  scale_shape_manual(values = 21:23) +
  theme(legend.position = c(0, 0.8), legend.title = element_blank()); flr.rc2.dens

# Diversity
flr.rc2.dive <- ggplot(flr.rc2, aes(x = Year, y = Diversity)) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15, size = 2) +
  labs(x = "Year", y = "Floral Diversity") +
  scale_fill_manual(values = flr.cols) +
  scale_color_manual(values = flr.cols) +
  scale_shape_manual(values = 21:23) +
  theme(legend.position = c(0.7, 0.8), legend.title = element_blank()); flr.rc2.dive

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

##  ----------------------------------------------------------------------------------------------------------  ##
                                # Figure Creation ####
##  ----------------------------------------------------------------------------------------------------------  ##
plot_grid(bf.rc2.abun, bf.rc2.dens, bf.rc2.dive, flr.rc2.abun, flr.rc2.dens, flr.rc2.dive, 
          labels = c("i", "ii", "iii", "iv", "v", "vi"), ncol = 3, nrow = 2)

cowplot::ggsave(plot = last_plot(), filename = "./Figures/Richardson2.pdf",
                width = 8, height = 7, units = "in")

# END ####



