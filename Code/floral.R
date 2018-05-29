##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                            # Herbicide Side Project - Floral Resource Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## Though my thesis focus has shifted elsewhere, the anti-fescue herbicide treatments still interest me.
  ## To that end, this analysis focuses entirely on herbicide treatment responses in grazed sites

# Required libraries
library(vegan); library(geomorph) # Calculate & Analyze
library(ggplot2); library(Rmisc) # Plot

# Set working directory
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

##  ----------------------------------------------------------------------------------------------------------  ##
                       # Univariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# NOTE ON STATS:
  ## "sig" means p < critical point of 0.05  
  ## "marginally sig" means p < critical point of 0.1
  ## "NS" stands for not significant (at either the 0.1 or 0.05 level)
    ### I have no plans on reporting/discussing "marginally sig" results,
    ### but they may be informative as I move forward

# NOTE ON TRANSFORMATIONS
  ## Transformations were done to meet normality and variance assumptions of linear model
  ## See "modchk.R" for actual code that demonstrates what I've done actually creates the best fit of the model

# NOTE ON COLORS:
  ## Colors are colorblind safe
  ## http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=8

# Clear environment to reduce error chances
rm(list = ls())

# Pull in nice clean dataset you just created
flr <- read.csv("./Data/actual_flr.csv")
str(flr)

# Make year a factor!
str(flr$Year)
flr$Year <- as.factor(flr$Year)
str(flr$Year)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr$Herbicide.Treatment)
flr$Herbicide.Treatment <- factor(as.character(flr$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(flr$Herbicide.Treatment)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("Con" = "#276419", # darkest green
            "Spr" = "#4d9221", # medium green
            "SnS" = "#7fbc41") # lightest green
sns.leg <- c("Con", "Spr", "SnS")
box.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.position = 'none')
sct.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.background = element_blank(), legend.title = element_blank())

# This function will take a huge pairwise comparison matrix and format it in a more intuitive way
## Also does multiple comparison adjustment (obvi)
simp.procD <- function(adv.procD.obj, p.dig = 4, crit.dig = 4){
  ## adv.procD.obj = object of a geomorph::advanced.procD.lm function
  ## p.dig = the number of digits you want the p value rounded to
  ## thresh = the upper threshold of the p values you want to keep
  
  # Get just the p values 
  pairs <- adv.procD.obj$P.means.dist
  ## You can refer to the whole output later to get relevant stats when you know what you're looking for
  
  # Want to ditch either the top diagonal or the bottom diagonal of the matrix of p-values
  ## These are the mirror image of the opposite triangle of the matrix
  ## I've arbitrarily chosen to eliminate the lower triangle, but it doesn't matter
  pairs[lower.tri(pairs, diag = T)] <- NA
  
  # Now get the p values out of that matrix
  pvals <- as.vector( round(pairs, digits = p.dig) )
  
  # Get the list of combinations that the matrix includes
  combos <- expand.grid(rownames(adv.procD.obj$P.means.dist), colnames(adv.procD.obj$P.means.dist))
  
  # Get those as vectors in their own right
  var1.vec <- combos$Var1
  var2.vec <- combos$Var2
  
  # And just in case you want one where they're combined...
  var.vecs <- paste(combos$Var1, "-", combos$Var2)
  
  # Okay, now make a dataframe of both your p values and your newly created variable vectors
  results <- data.frame(Comparisons = var.vecs,
                        Factor.1 = var1.vec, Factor.2 = var2.vec,
                        P.Values = pvals)
  
  # Ditch the NAs you inserted (aka the pvalues along the diagonal and in the triangle you eliminated)
  results2 <- results[complete.cases(results),]
  
  # Sequential Bonferroni Bit
  
  # For sequential Bonferroni you need to rank the pairs based on ascending p value
  results3 <- results2[order(results2$P.Values),] # order the comparisons
  rank <- c(1:length(results3$Comparisons)) # assign them a rank based on this order
  
  # Modify the critical point based on the rank of each sequential p value
  results3$Crit.Pt <- round( with(results3, ( (0.05 / (length(results3$Comparisons) + 1 - rank)) ) ), 
                             digits = crit.dig)
  ## Sequential bonferroni is calculated as show above, but in plain English it is like this:
  ## Each comparison gets it's own, sequential, critical point
  ## This is determined by dividing the standard critical point (0.05) by
  ## the total number of comparisons plus 1, minus the "rank" of the p value
  ## where lower p values have a lower rank
  ## The final pairwise comparison will always have a critical point of 0.05 in this method
  ### E.g. 6 pairwise comparisons + 1 - 6 (for the sixth one) = 1
  ### And 0.05 / 1 = 0.05 (duh)
  
  # Though you probably want to know if the stuff is significant at a glance
  results3$"P/Crit" <- results3$P.Values / results3$Crit.Pt
  
  # Now get the ranges of "significance" to be reduced to qualitative bits
  results3$Sig <- ifelse(test = results3$"P/Crit" > 2, yes = " ",
                         no = ifelse(test = results3$"P/Crit" > 0.2, yes = ".",
                                     no = ifelse(test = results3$"P/Crit" > 0.02, yes = "*",
                                                 no = ifelse(test = results3$"P/Crit" > 0.002, yes = "**", no = "***"))))
  ## Viewer discretion is advized when using this bonus column
  
  # Just in case you don't want to look in the guts of this function to see what * vs. ** means:
  message("Sig codes: P / Crit > 2 = ''
          0.2 < P/C ≤ 2 = '.'
          0.02 < P/C ≤ 0.2 = '*'
          0.002 < P/C ≤ 0.02 = '**'
          P/C ≤ 0.002 = '***'")
  
  # Get rid of the bothersome and distracting row numbering
  row.names(results3) <- NULL
  
  # And spit out the result
  return(results3)
  
}

##  ----------------------------------------------------------  ##
                # Abundance ####
##  ----------------------------------------------------------  ##
# How does the abundance of butterflies vary among adaptive management methods and over time?
procD.lm(Abundance ~ Herbicide.Treatment * Year, data = flr)
  ## interaction  = sig!

# Pairwise comps?
simp.procD(advanced.procD.lm(Abundance ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = flr))
  ## none sig...

# Get plotting dataframe
abun.pltdf <- summarySE(data = flr, measurevar = "Abundance",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
abun.pltdf$Year <- as.numeric(as.character(abun.pltdf$Year))

# Plot
ggplot(abun.pltdf, aes(x = Year, y = Abundance, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Floral Abundance") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# ggplot2::ggsave("./Graphs/flr_abun.pdf", plot = abun.plt)

##  ----------------------------------------------------------  ##
             # Species Density ####
##  ----------------------------------------------------------  ##
  # What about management effect on species density (~S)
procD.lm(Species.Density ~ Herbicide.Treatment * Year, data = flr)
## everything = sig

# Pairwise comps?
simp.procD(advanced.procD.lm(Species.Density ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = flr))
  ## Some marginal significance

# Get plotting dataframe
dens.pltdf <- summarySE(data = flr, measurevar = "Species.Density",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
dens.pltdf$Year <- as.numeric(as.character(dens.pltdf$Year))

# Plot
ggplot(dens.pltdf, aes(x = Year, y = Species.Density, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Species.Density + se, ymin = Species.Density - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Floral Species Density") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

#ggplot2::ggsave("./Graphs/flr_dens.pdf", plot = dens.plt)

##  ----------------------------------------------------------  ##
                 # Diversity ####
##  ----------------------------------------------------------  ##
# Run the test
procD.lm(Diversity ~ Herbicide.Treatment * Year, data = flr)
## year = marginally sig

# Get plotting dataframe
dive.pltdf <- summarySE(data = flr, measurevar = "Diversity",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
dive.pltdf$Year <- as.numeric(as.character(dive.pltdf$Year))

# Plot
ggplot(dive.pltdf, aes(x = Year, y = Diversity, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Floral Diversity") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

#ggplot2::ggsave("./Graphs/flr_dive.pdf", plot = dive.plt)

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Seedmix Success Evaluation ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get seedmix data
sdmx <- read.csv("./Data/explore_sdmx.csv")

# Fix the class/leveling of independent/explanatory variables
sdmx$Year <- as.factor(sdmx$Year)
sdmx$Herbicide.Treatment <- factor(as.character(sdmx$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(sdmx$Year); unique(sdmx$Herbicide.Treatment)

# Analyze each of them then switch to the plotting phase
procD.lm(Abundance ~ Herbicide.Treatment * Year, data = sdmx)
  ## interaction = NS

# Re-run without interaction term
procD.lm(Abundance ~ Herbicide.Treatment + Year, data = sdmx)
  ## Year = sig


simp.procD(advanced.procD.lm(Abundance ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = sdmx))

# Get plotting dataframe
smx.pltdf <- summarySE(data = sdmx, measurevar = "Abundance",
                       groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
smx.pltdf$Year <- as.numeric(as.character(smx.pltdf$Year))

# Plot
ggplot(smx.pltdf, aes(x = Year, y = Abundance, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Seed-mix Species Abundance") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Family Exploration ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the family data
legs <- read.csv("./Data/explore_legms.csv")
aster <- read.csv("./Data/explore_astrs.csv")
mint <- read.csv("./Data/explore_mints.csv")

# Fix the class/leveling of independent/explanatory variables
legs$Year <- as.factor(legs$Year)
legs$Herbicide.Treatment <- factor(as.character(legs$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(legs$Year); unique(legs$Herbicide.Treatment)
aster$Year <- as.factor(aster$Year)
aster$Herbicide.Treatment <- factor(as.character(aster$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(aster$Year); unique(aster$Herbicide.Treatment)
mint$Year <- as.factor(mint$Year)
mint$Herbicide.Treatment <- factor(as.character(mint$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(mint$Year); unique(mint$Herbicide.Treatment)

# Analyze each of them then switch to the plotting phase
# Asteraceae
procD.lm(Abundance ~ Herbicide.Treatment * Year, data = aster)
  ## all significant!
simp.procD(advanced.procD.lm(Abundance ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = aster))

# Get plotting dataframe
ast.pltdf <- summarySE(data = aster, measurevar = "Abundance",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
ast.pltdf$Year <- as.numeric(as.character(ast.pltdf$Year))

# Plot
ggplot(ast.pltdf, aes(x = Year, y = Abundance, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Aster Abundance") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# Fabaceae (legumes)
procD.lm(Abundance ~ Herbicide.Treatment * Year, data = legs)
  ## year = sig
procD.lm(Abundance ~ Herbicide.Treatment + Year, data = legs)

# Pairwise?
simp.procD(advanced.procD.lm(Abundance ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = legs))


# Get plotting dataframe
lgm.pltdf <- summarySE(data = legs, measurevar = "Abundance",
                       groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
lgm.pltdf$Year <- as.numeric(as.character(lgm.pltdf$Year))

# Plot
ggplot(lgm.pltdf, aes(x = Year, y = Abundance, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Legume Abundance") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# Lamiaceae (mints)
procD.lm(Abundance ~ Herbicide.Treatment * Year, data = mint)
  ## NS

##  ----------------------------------------------------------------------------------------------------------  ##
                        # Multivariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# You'll want the 'simp.procD' function & my NMS function
simp.procD <- function(adv.procD.obj, p.dig = 4, crit.dig = 4, sig.thresh = 0.05){
  ## adv.procD.obj = object of a geomorph::advanced.procD.lm function
  ## p.dig = the number of digits you want the p value rounded to
  ## thresh = the upper threshold of the p values you want to keep
  ## sig.thresh = What critical point do you want to start from (pre-multiple comparison adjustment)?
  
  # Get just the p values 
  ## You can refer to the whole output later to get relevant stats when you know what you're looking for
  pairs <- adv.procD.obj$P.means.dist
  
  # Want to ditch either the top diagonal or the bottom diagonal of the matrix of p-values
  ## These are redundant with the opposite triangle of the matrix
  ## I've arbitrarily chosen to eliminate the lower triangle, but it doesn't matter
  pairs[lower.tri(pairs, diag = T)] <- NA
  
  # Now get the p values out of that matrix
  pvals <- as.vector( round(pairs, digits = p.dig) )
  
  # Get the list of combinations that the matrix includes
  combos <- expand.grid(rownames(adv.procD.obj$P.means.dist), colnames(adv.procD.obj$P.means.dist))
  
  # Get those as vectors in their own right
  var1.vec <- combos$Var1
  var2.vec <- combos$Var2
  
  # And just in case you want one where they're combined...
  var.vecs <- paste(combos$Var1, "-", combos$Var2)
  
  # Okay, now make a dataframe of both your p values and your newly created variable vectors
  results <- data.frame(Comparisons = var.vecs,
                        Factor.1 = var1.vec, Factor.2 = var2.vec,
                        P.Values = pvals)
  
  # Ditch the NAs you inserted (aka the pvalues along the diagonal and in the triangle you eliminated)
  results2 <- results[complete.cases(results),]
  
  # Sequential Bonferroni Bit
  
  # For sequential Bonferroni you need to rank the pairs based on ascending p value
  results3 <- results2[order(results2$P.Values),] # order the comparisons
  rank <- c(1:length(results3$Comparisons)) # assign them a rank based on this order
  
  # Modify the critical point based on the rank of each sequential p value
  results3$Alpha.Pt <- round( with(results3, ( (sig.thresh / (length(results3$Comparisons) + 1 - rank)) ) ), 
                              digits = crit.dig)
  ## Sequential bonferroni is calculated as show above, but in plain English it is like this:
  ## Each comparison gets it's own, sequential, critical point
  ## This is determined by dividing the standard critical point (0.05) by
  ## the total number of comparisons plus 1, minus the "rank" of the p value
  ## where lower p values have a lower rank
  ## The final pairwise comparison will always have a critical point of 0.05 in this method
  ### E.g. 6 pairwise comparisons + 1 - 6 (for the sixth one) = 1
  ### And 0.05 / 1 = 0.05 (duh)
  
  # Though you probably want to know if the stuff is significant at a glance
  results3$Sig <- results3$P.Values - results3$Alpha.Pt
  
  # Now get the ranges of "significance" to be reduced to qualitative bits
  results3$Sig <- ifelse(test = results3$Sig >= 0.05, yes = " ",
                         no = ifelse(test = results3$Sig >= 0, yes = ".",
                                     no = ifelse(test = results3$Sig >= -0.01, yes = "**", no = "***")))
  ## Viewer discretion is advized when using this bonus column
  
  # Just in case you don't want to look in the guts of this function to see what * vs. ** means:
  message("Sig codes: P - Alpha < -0.01 '***' | ≥ -0.01 '**' | ≥ 0 '.' | ≥ 0.05 ' '")
  
  # Get rid of the bothersome and distracting row numbering
  row.names(results3) <- NULL
  
  # And spit out the result
  return(results3)
  
}
nms.3.ord <- function(mod, groupcol, g1, g2, g3, lntp1 = 1, lntp2 = 1, lntp3 = 1,
                      legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g3 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 3 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none', xlab = "", ylab = "")
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#276419" # darkest green
  col2 <- "#4d9221" # medium green
  col3 <- "#7fbc41" # lightest green
  
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
flr <- read.csv("./Data/actual_flr.csv")

# Fix adaptive management column levels
unique(flr$Herbicide.Treatment)
flr$Herbicide.Treatment <- factor(as.character(flr$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(flr$Herbicide.Treatment)

# Select your community similarity/distance index (use the style of vegan::vegdist)
comm.dist <- "kulczynski"

#   Subset by year
flr14 <- subset(flr, flr$Year == 2014)
flr15 <- subset(flr, flr$Year == 2015)
flr16 <- subset(flr, flr$Year == 2016)
flr17 <- subset(flr, flr$Year == 2017)
flr18 <- subset(flr, flr$Year == 2018)

# Make community matrices with no non-species columns
flr14.rsp <- as.matrix(flr14[,-c(1:5, (ncol(flr14)-2):ncol(flr14))])
flr15.rsp <- as.matrix(flr15[,-c(1:5, (ncol(flr15)-2):ncol(flr15))])
flr16.rsp <- as.matrix(flr16[,-c(1:5, (ncol(flr16)-2):ncol(flr16))])
flr17.rsp <- as.matrix(flr17[,-c(1:5, (ncol(flr17)-2):ncol(flr17))])
flr18.rsp <- as.matrix(flr18[,-c(1:5, (ncol(flr18)-2):ncol(flr18))])

# Get the chosen dissimilarity/distance metric for those communities 
flr14.dst <- vegdist(flr14.rsp, method = comm.dist)
flr15.dst <- vegdist(flr15.rsp, method = comm.dist)
flr16.dst <- vegdist(flr16.rsp, method = comm.dist)
flr17.dst <- vegdist(flr17.rsp, method = comm.dist)
flr18.dst <- vegdist(flr18.rsp, method = comm.dist)

# Get non-metric multidimensional scaling objects for each of 'em
flr14.mds <- metaMDS(flr14.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
flr15.mds <- metaMDS(flr15.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
flr16.mds <- metaMDS(flr16.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
flr17.mds <- metaMDS(flr17.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)
flr18.mds <- data.frame(stress = NA)

# Get the stress values in their own dataframe in case it becomes useful later
stress <- data.frame(Year = c("2014", "2015", "2016", "2017", "2018"),
                     stress.unadj = c(flr14.mds$stress, flr15.mds$stress, flr16.mds$stress, 
                                      flr17.mds$stress, flr18.mds$stress))
stress$stress.rounded = round(stress$stress.unadj, digits = 3)
write.csv(stress, "./Summary Info/stress_flr.csv", row.names = F)
  ## ranges from 0 to 1 when engine = "monoMDS" (is a % with engine = "isoMDS")

# Analyze!
procD.lm(flr14.dst ~ Herbicide.Treatment, data = flr14)
  ## NS

procD.lm(flr15.dst ~ Herbicide.Treatment, data = flr15)
  ## NS

procD.lm(flr16.dst ~ Herbicide.Treatment, data = flr16)
  ## NS

procD.lm(flr17.dst ~ Herbicide.Treatment, data = flr17)
  ## NS

# Set a quick shortcut for the legend contents of each (it'll be the same for all of 'em)
trt <- c("Con", "Spr", "SnS")

# Make ordinations!
nms.3.ord(flr14.mds, flr14$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr15.mds, flr15$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr16.mds, flr16$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr17.mds, flr17$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)

# Save out the significant ones
jpeg(file = "./Graphs/flr_nms14.jpg")

dev.off()

jpeg(file = "./Graphs/flr_nms15.jpg")

dev.off()

jpeg(file = "./Graphs/flr_nms16.jpg")

dev.off()

jpeg(file = "./Graphs/flr_nms17.jpg")

dev.off()


