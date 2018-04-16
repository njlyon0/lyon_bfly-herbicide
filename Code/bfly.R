##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                        # Lyon Thesis - Butterfly Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# START ####

# Required libraries
library(vegan); library(geomorph) # Calculate & Analyze
library(ggplot2); library(Rmisc) # Plot

# Set working directory
setwd("~/Documents/School/1. Iowa State/_MS Project/_AFRI Project/Lyon.Thesis-Bfly.Project")

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
unique(bf$Adaptive.Mgmt)
bf$Adaptive.Mgmt <- factor(as.character(bf$Adaptive.Mgmt), levels = c("BO", "PBG", "GB/H+", "H+"))
unique(bf$Adaptive.Mgmt)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("BO" = "#d73027", # red
            "PBG" = "#fdae61", # yellow
            "GB/H+" = "#abd9e9", # light blue
            "H+" =  "#313695", # blue
            "2014" = "#fc8d59", ## shades of orangey red for years
            "2015" = "#ef6548",
            "2016" = "#d7301f",
            "2017" = "#b30000")
adpt.leg <- c("BO", "PBG", "GB/H+", "H+")

# This function will take a huge pairwise comparison matrix and format it in a more intuitive way
  ## Also does multiple comparison adjustment (obvi)
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

##  ----------------------------------------------------------  ##
                 # Abundance ####
##  ----------------------------------------------------------  ##
# How does the abundance of butterflies vary among adaptive management methods and over time?
procD.lm(Abundance ~ Adaptive.Mgmt * Year, data = bf)
  ## Interaction = sig, mgmt = sig, yr = marginally sig

# Evidently not in this...
simp.procD(advanced.procD.lm(Abundance ~ Adaptive.Mgmt * Year, ~ 1, ~ Composite.Variable, data = bf))

# Maybe this...?
simp.procD(advanced.procD.lm(Abundance ~ Adaptive.Mgmt * Year, ~ 1, ~ Adaptive.Mgmt, data = bf))
  ## ooooOOOOoooo
  ## BO = A | PBG = B | GB = B | H+ = A

# Plot
abun.plt <- ggplot(bf, aes(x = Adaptive.Mgmt, y = Abundance, fill = Adaptive.Mgmt)) +
  geom_boxplot() +
  xlab("Adaptive Management") +
  scale_y_continuous("Butterfly Abundance", limits = c()) +
  scale_fill_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'none'); abun.plt

ggplot2::ggsave("./Graphs/Univariate/bf_abun.pdf", plot = abun.plt)

##  ----------------------------------------------------------  ##
              # Species Density ####
##  ----------------------------------------------------------  ##
# Between ref and control (species density this time)
procD.lm(Species.Density ~ Adaptive.Mgmt * Year, data = bf)
  ## mgmt = sig, all else = NS

# Get pairwise comparison info
simp.procD(advanced.procD.lm(Species.Density ~ Adaptive.Mgmt + Year, ~ 1, ~ Adaptive.Mgmt, data = bf))
  ## BO = A | PBG = A | GB = A | H+ = B

# Plot
dens.plt <- ggplot(bf, aes(x = Adaptive.Mgmt, y = Species.Density, fill = Adaptive.Mgmt)) +
  geom_boxplot() +
  xlab("Adaptive Management") +
  scale_y_continuous("Butterfly Species Density", limits = c()) +
  scale_fill_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'none'); dens.plt

ggplot2::ggsave("./Graphs/Univariate/bf_dens.pdf", plot = dens.plt)

##  ----------------------------------------------------------  ##
                 # Diversity ####
##  ----------------------------------------------------------  ##
# Run the test
procD.lm(Diversity ~ Adaptive.Mgmt * Year, data = bf)
  ## all sig!

simp.procD(advanced.procD.lm(Diversity ~ Adaptive.Mgmt * Year, ~ 1, ~ Composite.Variable, data = bf))
  # none sig...

# Get a plotting dataframe
dive.pltdf <- summarySE(data = bf, measurevar = "Diversity",
                        groupvars = c("Composite.Variable", "Year", "Adaptive.Mgmt"))

# Plot
dive.plt <- ggplot(dive.pltdf, aes(x = as.numeric(as.character(Year)), y = Diversity, color = Adaptive.Mgmt)) +
  geom_line(aes(group = Adaptive.Mgmt), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se), position = dodge) +
  geom_vline(xintercept = c(2014.5, 2015.5, 2016.5, 2017.5), linetype = c(1, 3, 3, 1)) +
  xlab("Year") +
  scale_y_continuous("Butterfly Diversity", limits = c()) +
  scale_color_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.8, 0.2), legend.background = element_blank(),
        legend.title = element_blank()); dive.plt

# ggplot2::ggsave("./Graphs/Univariate/bf_dive.pdf", plot = dive.plt)

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Family Exploration ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the family data
lyca <- read.csv("./Data/explore_lycaenids.csv")
pier <- read.csv("./Data/explore_pierids.csv")
nymp <- read.csv("./Data/explore_nymphalids.csv")

# Fix the class/leveling of independent/explanatory variables
lyca$Year <- as.factor(lyca$Year)
lyca$Adaptive.Mgmt <- factor(as.character(lyca$Adaptive.Mgmt), levels = c("BO", "PBG", "GB/H+", "H+"))
str(lyca$Year); unique(lyca$Adaptive.Mgmt)
pier$Year <- as.factor(pier$Year)
pier$Adaptive.Mgmt <- factor(as.character(pier$Adaptive.Mgmt), levels = c("BO", "PBG", "GB/H+", "H+"))
str(pier$Year); unique(pier$Adaptive.Mgmt)
nymp$Year <- as.factor(nymp$Year)
nymp$Adaptive.Mgmt <- factor(as.character(nymp$Adaptive.Mgmt), levels = c("BO", "PBG", "GB/H+", "H+"))
str(nymp$Year); unique(nymp$Adaptive.Mgmt)

# Analyze each of them then switch to the plotting phase
# Pieridae (whites & sulphurs)
procD.lm(Number ~ Adaptive.Mgmt * Year, data = pier)
  ## all sig
simp.procD(advanced.procD.lm(Number ~ Adaptive.Mgmt * Year, ~ 1, ~ Composite.Variable, data = pier))
  ## none sig

# Lycaenidae (blues & coppers & hairstreaks)
procD.lm(Number ~ Adaptive.Mgmt * Year, data = lyca)
  ## all sig
simp.procD(advanced.procD.lm(Number ~ Adaptive.Mgmt * Year, ~ 1, ~ Composite.Variable, data = lyca))
  ## none sig

# Nymphalidae (brush foots)
procD.lm(Number ~ Adaptive.Mgmt * Year, data = nymp)
  ## all sig (yr is close to NS)
simp.procD(advanced.procD.lm(Number ~ Adaptive.Mgmt * Year, ~ 1, ~ Composite.Variable, data = nymp))
  ## none sig

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
nms.ord <- function(mod, groupcol, g1, g2, g3, g4, lntp1 = 1, lntp2 = 1, lntp3 = 1, lntp4 = 1,
                    legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g4 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 4 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none', xlab = "", ylab = "")
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#d73027" # red
  col2 <- "#fdae61" # yellow
  col3 <- "#abd9e9" # light blue
  col4 <- "#4575b4" # blue
  
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 23, bg = col3)
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 24, bg = col4)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3, lntp4)
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, 
              col = c(g1 = col1, g2 = col2, g3 = col3, g4 = col4),
              display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         title = paste0("Stress = ", round(mod$stress, digits = 3)),
         ## The "title" of the legend will now be the stress of the NMS
         pch = c(21, 22, 23, 24), cex = 1.15, 
         pt.bg = c(col1, col2, col3, col4))
  
}

# Pull in nice clean dataset you just created
bf <- read.csv("./Data/actual_bf.csv")

# Fix the levels of the adaptive management column
unique(bf$Adaptive.Mgmt)
bf$Adaptive.Mgmt <- factor(as.character(bf$Adaptive.Mgmt), levels = c("BO", "PBG", "GB/H+", "H+"))
unique(bf$Adaptive.Mgmt)

# Select your community similarity/distance index (use the style of vegan::vegdist)
comm.dist <- "kulczynski"

#   Subset by year
bf14 <- subset(bf, bf$Year == 2014)
bf15 <- subset(bf, bf$Year == 2015)
bf16 <- subset(bf, bf$Year == 2016)
bf17 <- subset(bf, bf$Year == 2017)
bf18 <- subset(bf, bf$Year == 2018)

# Make community matrices with no non-species columns
bf14.rsp <- as.matrix(bf14[,-c(1:4, (ncol(bf14)-2):ncol(bf14))])
bf15.rsp <- as.matrix(bf15[,-c(1:4, (ncol(bf15)-2):ncol(bf15))])
bf16.rsp <- as.matrix(bf16[,-c(1:4, (ncol(bf16)-2):ncol(bf16))])
bf17.rsp <- as.matrix(bf17[,-c(1:4, (ncol(bf17)-2):ncol(bf17))])
bf18.rsp <- as.matrix(bf18[,-c(1:4, (ncol(bf18)-2):ncol(bf18))])

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
procD.lm(bf14.dst ~ Adaptive.Mgmt, data = bf14) # sig
simp.procD(advanced.procD.lm(bf14.dst ~ Adaptive.Mgmt, ~ 1, ~ Adaptive.Mgmt, data = bf14))
  ## BO = A | PBG = B | GB = B | H+ = AB

procD.lm(bf15.dst ~ Adaptive.Mgmt, data = bf15) # sig, but pairwise comps are marginal (real close tho)
simp.procD(advanced.procD.lm(bf15.dst ~ Adaptive.Mgmt, ~ 1, ~ Adaptive.Mgmt, data = bf15))
  ## BO = A | PBG = B | GB = B | H+ = B

procD.lm(bf16.dst ~ Adaptive.Mgmt, data = bf16) # sig
simp.procD(advanced.procD.lm(bf16.dst ~ Adaptive.Mgmt, ~ 1, ~ Adaptive.Mgmt, data = bf16))
  ## BO = A | PBG = B | GB = B | H+ = C

procD.lm(bf17.dst ~ Adaptive.Mgmt, data = bf17) # sig
simp.procD(advanced.procD.lm(bf17.dst ~ Adaptive.Mgmt, ~ 1, ~ Adaptive.Mgmt, data = bf17))
  ## BO = A | PBG = B | GB = B | H+ = B

# Set a quick shortcut for the legend contents of each (it'll be the same for all of 'em)
mgmt <- c("BO", "PBG", "GB/H+", "H+")

# Make ordinations!
nms.ord(bf14.mds, bf14$Adaptive.Mgmt, g1 = "BO", g2 = "PBG", g3 = "GB/H+", g4 = "H+", legcont = mgmt)
nms.ord(bf15.mds, bf15$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
nms.ord(bf16.mds, bf16$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
nms.ord(bf17.mds, bf17$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
#nms.ord(bf18.mds, bf18$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)

# Save out the significant ones
jpeg(file = "./Graphs/Multivariate/bf_nms14.jpg")
nms.ord(bf14.mds, bf14$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
dev.off()

jpeg(file = "./Graphs/Multivariate/bf_nms15.jpg")
nms.ord(bf15.mds, bf15$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
dev.off()

jpeg(file = "./Graphs/Multivariate/bf_nms16.jpg")
nms.ord(bf16.mds, bf16$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
dev.off()

jpeg(file = "./Graphs/Multivariate/bf_nms17.jpg")
nms.ord(bf17.mds, bf17$Adaptive.Mgmt, "BO", "PBG", "GB/H+", "H+", legcont = mgmt)
dev.off()




