##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                # Herbicide Side Project - Butterfly Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## What is the effect on butterflies and nectar resource plants of the anti-fescue treatments?
  ## Script Taxon: **Butterflies**

# Required libraries
library(vegan); library(RRPP) # Calculate & Analyze
library(ggplot2); library(Rmisc) # Plot

# Set working directory
setwd("~/Documents/School/Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
                 # Housekeeping
##  ----------------------------------------------------------  ##
# Pull in data
bf <- read.csv("./Data/bf-wide.csv")
str(bf)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(bf$Herb.Trt)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf$Herb.Trt)

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
              # Species Density ####
##  ----------------------------------------------------------  ##
# How does the species richness of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(Richness ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(Richness ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
  ## Both NS

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

# You'll want the pairwise comparison functions & my NMS function
nms.3.ord <- function(mod, groupcol, g1, g2, g3, lntp1 = 1, lntp2 = 1, lntp3 = 1, title = NA,
                      legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g3 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 3 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none', xlab = "", ylab = "", main = title)
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#003c30" #  darkish teal
  col2 <- "#35978f" # med. teal
  col3 <- "#80cdc1" # lite teal
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 23, bg = col3)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3)
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, 
              col = c(g1 = col1, g2 = col2, g3 = col3),
              display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         pch = c(21, 22, 23), cex = 1.15, 
         pt.bg = c(col1, col2, col3))
  
}

# Pull in nice clean dataset you just created
bf <- read.csv("./Data/bf-wide.csv")

# Fix the levels of the adaptive management column
unique(bf$Herb.Trt)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf$Herb.Trt)

# Select your community similarity/distance index (use the style of vegan::vegdist)
comm.dist <- "jaccard"

#   Subset by year
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
bf14.mds <- metaMDS(bf14.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
bf15.mds <- metaMDS(bf15.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
bf16.mds <- metaMDS(bf16.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
bf17.mds <- metaMDS(bf17.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
bf18.mds <- metaMDS(bf18.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)

# Get the stress values in their own dataframe in case it becomes useful later
stress <- data.frame(Year = c("2014", "2015", "2016", "2017", "2018"),
                     stress.unadj = c(bf14.mds$stress, bf15.mds$stress, bf16.mds$stress, 
                                      bf17.mds$stress, bf18.mds$stress))
stress$stress.rounded <- round(stress$stress.unadj, digits = 3)
write.csv(stress, "./Summary Info/stress_bf.csv", row.names = F)
  ## ranges from 0 to 1 when engine = "monoMDS" (is a % with engine = "isoMDS")

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

# Make ordinations (for exploration purposes only)
nms.3.ord(bf14.mds, bf14$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt, title = "2014")
nms.3.ord(bf15.mds, bf15$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt, title = "2015")
nms.3.ord(bf16.mds, bf16$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt, title = "2016")
nms.3.ord(bf17.mds, bf17$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt, title = "2017")
nms.3.ord(bf18.mds, bf18$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt, title = "2018")


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
