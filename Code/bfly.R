##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                # Herbicide Side Project - Butterfly Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## What is the effect on butterflies and nectar resource plants of the anti-fescue treatments?
  ## Script Taxon: **Butterflies**

# Required libraries
library(vegan); library(ape); library(RRPP) # Calculate & Analyze
library(ggplot2); library(Rmisc) # Plot

# Set working directory
myWD <- getwd()
myWD
  ## Should end in the project's directory

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
                 # Housekeeping
##  ----------------------------------------------------------  ##
# Pull in data
bf.v0 <- read.csv("./Data/bf-wide.csv")
str(bf.v0)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(bf.v0$Herb.Trt)
bf.v0$Herb.Trt <- factor(as.character(bf.v0$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf.v0$Herb.Trt)

# Split off 2014
bf.14 <- subset(bf.v0, Year == 14)
bf <- subset(bf.v0, Year != 14)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("Con" = "#003c30", #  darkish teal
            "Spr" = "#35978f", # med. teal
            "SnS" = "#80cdc1") # lite teal
ns.color <- "#003c30" # dark teal
sct.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.background = element_blank(), legend.title = element_blank())
##  ----------------------------------------------------------------------------------------------------------  ##
                          # Pre-Treatment Analysis ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Cover your bases; any differences among patches pre-treatment?

# ABUNDANCE #
anova(lm.rrpp(log(Abundance) ~ Herb.Trt, data = bf.14, iter = 9999), effect.type = "F")
  ## NS

# RICHNESS #
anova(lm.rrpp(Richness ~ Herb.Trt, data = bf.14, iter = 9999), effect.type = "F")
  ## NS

# DIVERSITY #
anova(lm.rrpp(log(Diversity) ~ Herb.Trt, data = bf.14, iter = 9999), effect.type = "F")
  ## NS

##  ----------------------------------------------------------------------------------------------------------  ##
                           # Univariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
                 # Abundance ####
##  ----------------------------------------------------------  ##
# How does the abundance of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(log(Abundance) ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(log(Abundance) ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
  ## yr = marginally sig

# Plot the 'by treatment' results
ggplot(bf, aes(x = Year, y = Abundance)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.9, 0.9))

# Save it
ggplot2::ggsave("./Graphs/bf_abun.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
                # Richness ####
##  ----------------------------------------------------------  ##
# How does the species richness of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(Richness ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(Richness ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
  ## year = marginal

# Plot the 'by treatment' results
ggplot(bf, aes(x = Year, y = Richness)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Butterfly Richness") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.9, 0.125))

# Save it
ggplot2::ggsave("./Graphs/bf_rich.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
                 # Diversity ####
##  ----------------------------------------------------------  ##
# How does the diversity of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(log(Diversity) ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(log(Diversity) ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
  ## yr = sig, trt = NS

# Plot
ggplot(bf, aes(x = Year, y = Diversity)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.9, 0.125))

# Save it
ggplot2::ggsave("./Graphs/bf_dive.pdf", plot = last_plot())

##  ----------------------------------------------------------------------------------------------------------  ##
                       # Multivariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# Pull in nice clean dataset you just created
bf <- read.csv("./Data/bf-wide.csv")

# Fix the levels of the adaptive management column
unique(bf$Herb.Trt)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf$Herb.Trt)

# You'll want my PCoA function to make pretty ordinations
pcoa.3.ord <- function(mod, groupcol, g1, g2, g3, lntp1 = 1, lntp2 = 1, lntp3 = 1,
                       legcont, legpos = "topleft", plot.title = NULL) {
  ## mod = object returned by ape::pcoa
  ## groupcol = group column in the dataframe that contains those (not the matrix used in vegdist)
  ## g1 - g3 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 3 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  ## plot.title = if you want a title above the plot
  
  # Create plot
  plot(mod$vectors, display = 'sites', choice = c(1, 2), type = 'none', main = plot.title,
       xlab = paste0("PC1 (", round(mod$values$Relative_eig[1] * 100, digits = 2), "%)"),
       ylab = paste0("PC2 (", round(mod$values$Relative_eig[2] * 100, digits = 2), "%)"))
  ## Probably want the relative eigenvalues (% variation explained per axis) on the plot in an obvious way
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#003c30" #  darkish teal
  col2 <- "#35978f" # med. teal
  col3 <- "#80cdc1" # light teal
              
  # Add points for each group with a different color per group
  points(mod$vectors[groupcol == g1, 1], mod$vectors[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$vectors[groupcol == g2, 1], mod$vectors[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$vectors[groupcol == g3, 1], mod$vectors[groupcol == g3, 2], pch = 23, bg = col3)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3)
  
  # Ordinate SD ellipses around the centroid
  vegan::ordiellipse(mod$vectors, groupcol, 
                     col = c(g1 = col1, g2 = col2, g3 = col3),
                     display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         title = NULL,  cex = 1.15, 
         pch = c(21, 22, 23),
         pt.bg = c(col1, col2, col3))
  
}

# Select your community similarity/distance index (use the style of vegan::vegdist)
comm.dist <- "bray"

# Subset by year
bf14 <- subset(bf, bf$Year == 14)
bf15 <- subset(bf, bf$Year == 15)
bf16 <- subset(bf, bf$Year == 16)
bf17 <- subset(bf, bf$Year == 17)
bf18 <- subset(bf, bf$Year == 18)

# Make community matrices with no non-species columns
bf14.rsp <- as.matrix(bf14[,-c(1:5, (ncol(bf14)-2):ncol(bf14))])
bf15.rsp <- as.matrix(bf15[,-c(1:5, (ncol(bf15)-2):ncol(bf15))])
bf16.rsp <- as.matrix(bf16[,-c(1:5, (ncol(bf16)-2):ncol(bf16))])
bf17.rsp <- as.matrix(bf17[,-c(1:5, (ncol(bf17)-2):ncol(bf17))])
bf18.rsp <- as.matrix(bf18[,-c(1:5, (ncol(bf18)-2):ncol(bf18))])

# Get the chosen dissimilarity/distance metric for those communities 
bf14.dst <- vegdist(bf14.rsp, method = comm.dist)
bf15.dst <- vegdist(bf15.rsp, method = comm.dist)
bf16.dst <- vegdist(bf16.rsp, method = comm.dist)
bf17.dst <- vegdist(bf17.rsp, method = comm.dist)
bf18.dst <- vegdist(bf18.rsp, method = comm.dist)

# Get non-metric multidimensional scaling objects for each of 'em
bf14.pcoa <- pcoa(bf14.dst)
bf15.pcoa <- pcoa(bf15.dst)
bf16.pcoa <- pcoa(bf16.dst)
bf17.pcoa <- pcoa(bf17.dst)
bf18.pcoa <- pcoa(bf18.dst)

# Analyze!
anova(lm.rrpp(bf14.dst ~ Herb.Trt, data = bf14, iter = 9999), effect.type = "F")
## NS

anova(lm.rrpp(bf15.dst ~ Herb.Trt, data = bf15, iter = 9999), effect.type = "F")
## NS

anova(lm.rrpp(bf16.dst ~ Herb.Trt, data = bf16, iter = 9999), effect.type = "F")
## NS

anova(lm.rrpp(bf17.dst ~ Herb.Trt, data = bf17, iter = 9999), effect.type = "F")
## NS

anova(lm.rrpp(bf18.dst ~ Herb.Trt, data = bf18, iter = 9999), effect.type = "F")
## NS

# Set a quick shortcut for the legend contents of each (it'll be the same for all of 'em)
trt <- c("Con", "Spr", "SnS")

# Make ordinations!
pcoa.3.ord(bf14.pcoa, bf14$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legcont = trt, plot.title = "2014")
pcoa.3.ord(bf15.pcoa, bf15$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2015")
pcoa.3.ord(bf16.pcoa, bf16$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2016")
pcoa.3.ord(bf17.pcoa, bf17$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2017")
pcoa.3.ord(bf18.pcoa, bf18$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2018")

# Now make all of them in a single row and save it out
jpeg(file = 'Figures/Butterfly_PCoAs.jpg', width = 7, height = 5, units = "in", res = 720)

par(mfrow = c(2, 3))

pcoa.3.ord(bf14.pcoa, bf14$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legcont = trt, plot.title = "2014", legpos = 'bottomright')
pcoa.3.ord(bf15.pcoa, bf15$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2015")
pcoa.3.ord(bf16.pcoa, bf16$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2016")
pcoa.3.ord(bf17.pcoa, bf17$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2017")
pcoa.3.ord(bf18.pcoa, bf18$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS",
           legpos = NA, legcont = trt, plot.title = "2018")

dev.off()

# Set this back to the default so it doesn't mess up later graphs
par(mfrow = c(1 , 1))

##  ----------------------------------------------------------------------------------------------------------  ##
                                    # Misc Notes ####
##  ----------------------------------------------------------------------------------------------------------  ##
# NOTE ON STATS:
  ## "marginally sig" means p < 2*critical point
  ## "sig" means p < critical point
  ## "NS" stands for not significant (p > 2*critical point)
  ## We have no plans on reporting/discussing "marginally sig" results but they may be informative

# NOTE ON TRANSFORMATIONS
  ## Transformations were done to meet normality and variance assumptions of linear model
  ## See "modchk.R" for code that demonstrates which transformations best meet model assumptions

# NOTE ON COLORS:
  ## Colors are colorblind safe
  ## http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=8
