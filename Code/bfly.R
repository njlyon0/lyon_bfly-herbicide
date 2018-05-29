##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                # Herbicide Side Project - Butterfly Code
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
    ## "marginally sig" means p < critical point of 0.1
    ## "sig" means p < critical point of 0.05
    ## "NS" stands for not significant (at either the 0.1 or 0.05 level)
    ## I have no plans on reporting/discussing "marginally sig" results,
      ## but they may be informative as I move forward

# NOTE ON TRANSFORMATIONS
    ## Transformations were done to meet normality and variance assumptions of linear model
    ## See "modchk.R" for actual code that demonstrates what I've done actually creates the best fit of the model

# NOTE ON COLORS:
    ## Colors are colorblind safe
    ## http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=8

# Clear environment to reduce error chances
rm(list = ls())

# Pull in nice clean dataset you just created
bf <- read.csv("./Data/actual_bf.csv")
str(bf)

# Make year a factor!
str(bf$Year)
bf$Year <- as.factor(bf$Year)
str(bf$Year)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(bf$Herbicide.Treatment)
bf$Herbicide.Treatment <- factor(as.character(bf$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(bf$Herbicide.Treatment)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("Con" = "#8e0152", # darkest pink
            "Spr" = "#c51b7d", # medium pink
            "SnS" = "#de77ae") # lightest pink
ns.color <- "#c51b7d"
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
# How does the abundance of butterflies vary among herbicide treatment patches and over time?
procD.lm(Abundance ~ Herbicide.Treatment * Year, data = bf)
  ## NS

# Get plotting dataframe
abun.pltdf <- summarySE(data = bf, measurevar = "Abundance",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
abun.pltdf$Year <- as.numeric(as.character(abun.pltdf$Year))

# Plot
ggplot(abun.pltdf, aes(x = Year, y = Abundance, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# ggplot2::ggsave("./Graphs/bf_abun.pdf", plot = abun.plt)

##  ----------------------------------------------------------  ##
              # Species Density ####
##  ----------------------------------------------------------  ##
# Between ref and control (species density this time)
procD.lm(Species.Density ~ Herbicide.Treatment * Year, data = bf)
  ## yr = marginally sig

# Try without interaction term (bcz it was NS anyway)
procD.lm(Species.Density ~ Herbicide.Treatment + Year, data = bf)

# Get pairwise comparison info
simp.procD(advanced.procD.lm(Species.Density ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = bf))

# Get plotting dataframe
dens.pltdf <- summarySE(data = bf, measurevar = "Species.Density",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
dens.pltdf$Year <- as.numeric(as.character(dens.pltdf$Year))

# Plot
ggplot(dens.pltdf, aes(x = Year, y = Species.Density, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Species.Density + se, ymin = Species.Density - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Butterfly Species Density") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.85))

# ggplot2::ggsave("./Graphs/bf_dens.pdf", plot = dens.plt)

##  ----------------------------------------------------------  ##
                 # Diversity ####
##  ----------------------------------------------------------  ##
# Run the test
procD.lm(Diversity ~ Herbicide.Treatment * Year, data = bf)
  ## year = marginal

# Run w/o that interaction term
procD.lm(Diversity ~ Herbicide.Treatment + Year, data = bf)
  ## yr = sig!

# pairwise comparisons?
simp.procD(advanced.procD.lm(Diversity ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = bf))
  ## NS

# Get plotting dataframe
dive.pltdf <- summarySE(data = bf, measurevar = "Diversity",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
dive.pltdf$Year <- as.numeric(as.character(dive.pltdf$Year))

# Plot
ggplot(dive.pltdf, aes(x = Year, y = Diversity, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# ggplot2::ggsave("./Graphs/bf_dive.pdf", plot = dive.plt)

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Family Exploration ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the family data
lyca <- read.csv("./Data/explore_lycaenids.csv")
pier <- read.csv("./Data/explore_pierids.csv")
nymp <- read.csv("./Data/explore_nymphalids.csv")

# Fix the class/leveling of independent/explanatory variables
lyca$Year <- as.factor(lyca$Year)
lyca$Herbicide.Treatment <- factor(as.character(lyca$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(lyca$Year); unique(lyca$Herbicide.Treatment)
pier$Year <- as.factor(pier$Year)
pier$Herbicide.Treatment <- factor(as.character(pier$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(pier$Year); unique(pier$Herbicide.Treatment)
nymp$Year <- as.factor(nymp$Year)
nymp$Herbicide.Treatment <- factor(as.character(nymp$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
str(nymp$Year); unique(nymp$Herbicide.Treatment)

# Analyze then plot for each family

# Pieridae (whites & sulphurs)
procD.lm(Number ~ Herbicide.Treatment * Year, data = pier)
  ## year = sig!

# Run without interaction
procD.lm(Number ~ Herbicide.Treatment + Year, data = pier)
  ## year = more sig

# Pairwise comps
simp.procD(advanced.procD.lm(Number ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = pier))
  ## 14 = A | 15 = AB | 16 = B | 17 = A

# Get plotting dataframe
pier.pltdf <- summarySE(data = pier, measurevar = "Number", groupvars = "Year")
pier.pltdf$Year <- as.numeric(as.character(pier.pltdf$Year))

# Plot
ggplot(pier.pltdf, aes(x = Year, y = Number, color = rep.int("Z", nrow(pier.pltdf)))) +
  geom_path(position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Number + se, ymin = Number - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Pieridae Number") +
  scale_color_manual(values = ns.color) +
  sct.theme + theme(legend.position = "none")

# Lycaenidae (blues & coppers & hairstreaks)
procD.lm(Number ~ Herbicide.Treatment * Year, data = lyca)
## year = sig!

# Run without interaction
procD.lm(Number ~ Herbicide.Treatment + Year, data = lyca)
  ## year = more sig

# Pairwise comps
simp.procD(advanced.procD.lm(Number ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = lyca))
  ## 14 = A | 15 = B | 16 = AB | 17 = AB

# Get plotting dataframe
lyca.pltdf <- summarySE(data = lyca, measurevar = "Number", groupvars = "Year")
lyca.pltdf$Year <- as.numeric(as.character(lyca.pltdf$Year))

# Plot
ggplot(lyca.pltdf, aes(x = Year, y = Number, color = rep.int("Z", nrow(lyca.pltdf)))) +
  geom_path(position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Number + se, ymin = Number - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Lycaenidae Number") +
  scale_color_manual(values = ns.color) +
  sct.theme + theme(legend.position = "none")

# Nymphalidae (brush foots)
procD.lm(Number ~ Herbicide.Treatment * Year, data = nymp)
  ## interaction = sig

simp.procD(advanced.procD.lm(Number ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = nymp))
  ## some sig!

# Get plotting dataframe
nymp.pltdf <- summarySE(data = nymp, measurevar = "Number",
                        groupvars = c("Composite.Variable", "Herbicide.Treatment", "Year"))
nymp.pltdf$Year <- as.numeric(as.character(nymp.pltdf$Year))

# Plot
ggplot(nymp.pltdf, aes(x = Year, y = Number, color = Herbicide.Treatment)) +
  geom_path(aes(group = Herbicide.Treatment), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Number + se, ymin = Number - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(2014.35, 2014.5, 2014.65, 2017.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Nymphalidae Number") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.5, 0.8))

##  ----------------------------------------------------------------------------------------------------------  ##
                       # Multivariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# You'll want the 'simp.procD' function & my NMS function
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
  col1 <- "#8e0152" # darkest pink
  col2 <- "#c51b7d" # medium pink
  col3 <- "#de77ae" # lightest pink
  
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
bf <- read.csv("./Data/actual_bf.csv")

# Fix the levels of the adaptive management column
unique(bf$Herbicide.Treatment)
bf$Herbicide.Treatment <- factor(as.character(bf$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(bf$Herbicide.Treatment)

# Select your community similarity/distance index (use the style of vegan::vegdist)
comm.dist <- "kulczynski"

#   Subset by year
bf14 <- subset(bf, bf$Year == 2014)
bf15 <- subset(bf, bf$Year == 2015)
bf16 <- subset(bf, bf$Year == 2016)
bf17 <- subset(bf, bf$Year == 2017)
bf18 <- subset(bf, bf$Year == 2018)

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
bf18.mds <- data.frame(stress = NA)

# Get the stress values in their own dataframe in case it becomes useful later
stress <- data.frame(Year = c("2014", "2015", "2016", "2017", "2018"),
                     stress.unadj = c(bf14.mds$stress, bf15.mds$stress, bf16.mds$stress, 
                                      bf17.mds$stress, bf18.mds$stress))
stress$stress.rounded <- round(stress$stress.unadj, digits = 3)
write.csv(stress, "./Summary Info/stress_bf.csv", row.names = F)
  ## ranges from 0 to 1 when engine = "monoMDS" (is a % with engine = "isoMDS")

# Analyze!
procD.lm(bf14.dst ~ Herbicide.Treatment, data = bf14)
  ## NS

procD.lm(bf15.dst ~ Herbicide.Treatment, data = bf15)
  ## NS

procD.lm(bf16.dst ~ Herbicide.Treatment, data = bf16)
  ## NS

procD.lm(bf17.dst ~ Herbicide.Treatment, data = bf17)
  ## NS

# Set a quick shortcut for the legend contents of each (it'll be the same for all of 'em)
trt <- c("Con", "Spr", "SnS")

# Make ordinations!
nms.3.ord(bf14.mds, bf14$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf15.mds, bf15$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf16.mds, bf16$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf17.mds, bf17$Herbicide.Treatment, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)

# Save out the significant ones
jpeg(file = "./Graphs/Multivariate/bf_nms14.jpg")

dev.off()

jpeg(file = "./Graphs/Multivariate/bf_nms15.jpg")

dev.off()

jpeg(file = "./Graphs/Multivariate/bf_nms16.jpg")

dev.off()

jpeg(file = "./Graphs/Multivariate/bf_nms17.jpg")

dev.off()




