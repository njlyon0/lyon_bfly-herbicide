##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                            # Herbicide Side Project - Floral Resource Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
## What is the effect on butterflies and nectar resource plants of the anti-fescue treatments?
## Script Taxon: **Butterflies**

# Required libraries
library(vegan); library(RRPP) # Calculate & Analyze
library(ggplot2); library(Rmisc) # Plot

# Set working directory
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
# Housekeeping
##  ----------------------------------------------------------  ##
# Pull in data
flr <- read.csv("./Data/flr-wide.csv")
str(flr)

# Make year a factor!
flr$Year <- as.factor(flr$Year)
str(flr$Year)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr$Herb.Trt)
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr$Herb.Trt)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("Con" = "#8c510a", # dark brown
            "Spr" = "#flr812d", # med. brown
            "SnS" = "#dfc27d") # light brown
ns.color <- "#8c510a" # dark brown
box.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.position = 'none')
sct.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.background = element_blank(), legend.title = element_blank())

# Erica Baken did a neat ammendment to the RRPP summary output 
summary.pairwise <- function (object, test.type = c("dist", "VC", "var"), angle.type = c("rad", "deg"),
                              stat.table = T, confidence = 0.95, show.vectors = F, ...) {
  
  test.type <- match.arg(test.type)
  angle.type <- match.arg(angle.type)
  x <- object
  if (test.type != "var") { # if you don't specify that the test.type is "var" (which means variances), see what the object should take
    if (is.null(x$LS.means)) 
      type = "slopes"  # this would be appropriate for linear regression analyses
    if (is.null(x$slopes)) 
      type = "means" # this would be appropriate for ANOVAs. For my data, that turned it into type = 'means'
  }
  else type <- "var" # ignore for my data
  RRPP:::print.pairwise(x) # Print onscreen the output from the fitted object
  cat("\n") # add a line in the output
  vars <- object$vars # needed. not sure why but setting something up with iterations I think
  if (type == "var") { # ignore for my data
    var.diff <- lapply(1:NCOL(vars), function(j) {
      v <- as.matrix(vars[, j])
      as.matrix(dist(v))
    })
    L <- d.summary.from.list(var.diff)
    cat("\nObserved variances by group\n\n")
    print(vars[, 1])
    if (stat.table) {
      tab <- makePWDTable(L)
      cat("\nPairwise distances between variances, plus statistics\n")
      print(tab)
    }
    else {
      cat("\nPairwise distances between variances\n")
      print(L$D)
      cat("\nPairwise", paste(L$confidence * 100, "%", 
                              sep = ""), "upper confidence limits between variances\n")
      print(L$CL)
      cat("\nPairwise effect sizes (Z) between variances\n")
      print(L$Z)
      cat("\nPairwise P-values between variances\n")
      print(L$P)
    }
  }
  if (type == "means") { # this is appropriate for my data
    cat("LS means:\n") 
    if (show.vectors) 
      print(x$LS.means[[1]])
    else cat("Vectors hidden (use show.vectors = TRUE to view)\n") # print out message for LS means output
    if (test.type == "dist") { # if type = dist (like my data)
      L <- RRPP:::d.summary.from.list(x$means.dist)  # THIS IS WHERE THE P VALUE LIST IS MADE - L$P
      if (stat.table) { # if you ask for it in a table, this is how it's made
        tab <- RRPP:::makePWDTable(L) # making the table
        cat("\nPairwise distances between means, plus statistics\n")
        print(tab) 
      }
      else { # ignore
        cat("\nPairwise distances between means\n")
        print(L$D)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits between means\n")
        print(L$CL)
        cat("\nPairwise effect sizes (Z) between means\n")
        print(L$Z)
        cat("\nPairwise P-values between means\n")
        print(L$P)
      }
    }
    if (test.type == "VC") {
      L <- r.summary.from.list(x$means.vec.cor)
      if (stat.table) {
        tab <- makePWCorTable(L)
        cat("\nPairwise statistics based on mean vector correlations\n")
        if (angle.type == "deg") {
          tab$angle <- tab$angle * 180/pi
          tab[, 3] <- tab[, 3] * 180/pi
        }
        print(tab)
      }
      else {
        cat("\nPairwise vector correlations between mean vectors\n")
        print(L$r)
        cat("\nPairwise angles between mean vectors\n")
        if (angle.type == "deg") 
          print(L$angle * 180/pi)
        else print(L$angle)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits for angles between mean vectors\n")
        if (angle.type == "deg") 
          print(L$aCL * 180/pi)
        else print(L$aCL)
        cat("\nPairwise effect sizes (Z) for angles between mean vectors\n")
        print(L$Z)
        cat("\nPairwise P-values for angles between mean vectors\n")
        print(L$P)
      }
    }
  }
  if (type == "slopes") {
    cat("Slopes (vectors of variate change per one unit of covariate change, by group):\n")
    if (show.vectors) 
      print(x$slopes[[1]])
    else cat("Vectors hidden (use show.vectors = TRUE to view)\n")
    if (test.type == "dist") {
      cat("\nSlope vector lengths\n")
      print(x$slopes.length[[1]])
      L <- d.summary.from.list(x$slopes.dist)
      if (stat.table) {
        tab <- makePWDTable(L)
        cat("\nPairwise absolute difference (d) between vector lengths, plus statistics\n")
        print(tab)
      }
      else {
        cat("\nPairwise absolute differences (d) between slope lengths\n")
        print(L$D)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits between slope lengths\n")
        print(L$CL)
        cat("\nPairwise effect sizes (Z) between slope lengths\n")
        print(L$Z)
        cat("\nPairwise P-values between slope lengths\n")
        print(L$P)
      }
    }
    if (test.type == "VC") {
      L <- r.summary.from.list(x$slopes.vec.cor)
      cat("\nPairwise statistics based on slopes vector correlations (r) and angles, acos(r)")
      cat("\nThe null hypothesis is that r = 1 (parallel vectors).")
      cat("\nThis null hypothesis is better treated as the angle between vectors = 0\n")
      if (stat.table) {
        tab <- makePWCorTable(L)
        if (angle.type == "deg") {
          tab$angle <- tab$angle * 180/pi
          tab[, 3] <- tab[, 3] * 180/pi
        }
        print(tab)
      }
      else {
        cat("\nPairwise vector correlations between slope vectors\n")
        print(L$r)
        cat("\nPairwise angles between slope vectors\n")
        if (angle.type == "deg") 
          print(L$angle * 180/pi)
        else print(L$angle)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits for angles between mean vectors\n")
        if (angle.type == "deg") 
          print(L$aCL * 180/pi)
        else print(L$aCL)
        cat("\nPairwise effect sizes (Z) for angles between slope vectors\n")
        print(L$Z)
        cat("\nPairwise P-values for angles between slope vectors\n")
        print(L$P)
      }
    }
  }
  return(tab)  # ADDED
}

# This function performs multiple comparison adjustment on the RRPP output fr pairwise comparisons
seq.bonf <- function(pairs.file, fac, crit.dig = 3){
  ## pairs.file = output from the modified "summary.pairwise" function in library(RRPP)
  ## fac = factor column in the dataframe (*MUST* be treated as "Factor" by R)
  ## crit.dig = number of digits to round alpha to (aka the critical point)
  
  # GET DATAFRAME
  df <- pairs.file
  
  # MULTIPLE COMPARISON ADJUSTMENT
  ## Sequential Bonferroni adjustment is what will be used here
  
  # Order the rows from lowest to highest p value
  results <- df[order(df$"Pr > d"), ]
  
  # Assign a rank based on that order
  rank <- c(1:length(results$P))
  
  # Now modify the critical point based on that rank (hence "sequential" Bonferroni)
  results$Alpha <- round( with(results, ( (0.05 / (length(results$"Pr > d") + 1 - rank)) ) ), digits = crit.dig)
  
  # Helpful to know how much larger the p value is than its critical point
  results$"P/Alpha" <- round( (results$"Pr > d" / results$Alpha), digits = crit.dig)
  
  # Now get the ranges of "significance" to be reduced to qualitative bits
  results$Sig <- ifelse(test = results$"P/Alpha" > 2, yes = " ",
                        no = ifelse(test = results$"P/Alpha" > 1, yes = ".",
                                    no = ifelse(test = results$"P/Alpha" > 0.2, yes = "*",
                                                no = ifelse(test = results$"P/Alpha" > 0.02, yes = "**",
                                                            no = ifelse(test = results$"P/Alpha" > 0.002, yes = "***", no = "****")))))
  ## Viewer discretion is advized when using this bonus column
  
  # Just in case you don't want to look in the guts of this function to see what * vs. ** means:
  message("Sig codes: P / Crit > 2 = ''
          1 < P/C ≤ 2 = '.'
          0.2 < P/C ≤ 1 = '*'
          0.02 < P/C ≤ 0.2 = '**'
          0.002 < P/C ≤ 0.02 = '***'
          P/C ≤ 0.002 = '****'")
  
  # And spit out the result
  return(results)
}

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Univariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
# Abundance ####
##  ----------------------------------------------------------  ##
# How does the abundance of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(Abundance ~ Herb.Trt * Year, data = flr, iter = 9999), effect.type = "F")
## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(Abundance ~ Herb.Trt + Year, data = flr, iter = 9999), effect.type = "F")
## yr = sig, trt = NS

# Fit two models, one for each of the two categorical variables
abun.trt.fit <- lm.rrpp(Abundance ~ Herb.Trt, data = flr, iter = 9999)
abun.year.fit <- lm.rrpp(Abundance ~ Year, data = flr, iter = 9999)

# And fit the pairwise comparison assessments
abun.trt.pairs <- summary.pairwise(pairwise(abun.trt.fit, fit.null = NULL, groups = flr$Herb.Trt))
abun.year.pairs <- summary.pairwise(pairwise(abun.year.fit, fit.null = NULL, groups = flr$Year))

# Get the pairwise results from those!
seq.bonf(pairs.file = abun.trt.pairs, fac = flr$Herb.Trt)
## NS

seq.bonf(pairs.file = abun.year.pairs, fac = flr$Year)
## 16 v 18 = sig

# Plot the 'by treatment' results
abun.plt <- ggplot(flr, aes(x = Herb.Trt, y = Abundance, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Butterfly Abundance") + 
  scale_fill_manual(values = colors) +
  box.theme; abun.plt

# Save it
#ggplot2::ggsave("./Graphs/flr_abun.pdf", plot = abun.plt)

# Get plotting dataframe
abun.pltdf <- summarySE(data = flr, measurevar = "Abundance",
                        groupvars = c("Composite.Variable", "Herb.Trt", "Year"))
abun.pltdf$Year <- as.numeric(as.character(abun.pltdf$Year))

# Plot
ggplot(abun.pltdf, aes(x = Year, y = Abundance, color = Herb.Trt)) +
  geom_path(aes(group = Herb.Trt), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Abundance + se, ymin = Abundance - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(14.35, 14.5, 14.65, 17.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# ggplot2::ggsave("./Graphs/flr_abun.pdf", plot = abun.plt)

##  ----------------------------------------------------------  ##
# Species Density ####
##  ----------------------------------------------------------  ##
# How does the species density of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(Species.Density ~ Herb.Trt * Year, data = flr, iter = 9999), effect.type = "F")
## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(Species.Density ~ Herb.Trt + Year, data = flr, iter = 9999), effect.type = "F")
## yr = sig, trt = NS

# Fit two models, one for each of the two categorical variables
dens.trt.fit <- lm.rrpp(Species.Density ~ Herb.Trt, data = flr, iter = 9999)
dens.year.fit <- lm.rrpp(Species.Density ~ Year, data = flr, iter = 9999)

# And fit the pairwise comparison assessments
dens.trt.pairs <- summary.pairwise(pairwise(dens.trt.fit, fit.null = NULL, groups = flr$Herb.Trt))
dens.year.pairs <- summary.pairwise(pairwise(dens.year.fit, fit.null = NULL, groups = flr$Year))

# Get the pairwise results from those!
seq.bonf(pairs.file = dens.trt.pairs, fac = flr$Herb.Trt)
## NS

seq.bonf(pairs.file = dens.year.pairs, fac = flr$Year)
## 16 v 18 = sig

# Plot the 'by treatment' results
dens.plt <- ggplot(flr, aes(x = Herb.Trt, y = Species.Density, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Butterfly Richness") + 
  scale_fill_manual(values = colors) +
  box.theme; dens.plt

# Save it
#ggplot2::ggsave("./Graphs/flr_dens.pdf", plot = dens.plt)

# Get plotting dataframe
dens.pltdf <- summarySE(data = flr, measurevar = "Species.Density",
                        groupvars = c("Composite.Variable", "Herb.Trt", "Year"))
dens.pltdf$Year <- as.numeric(as.character(dens.pltdf$Year))

# Plot
ggplot(dens.pltdf, aes(x = Year, y = Species.Density, color = Herb.Trt)) +
  geom_path(aes(group = Herb.Trt), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Species.Density + se, ymin = Species.Density - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(14.35, 14.5, 14.65, 17.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Butterfly Richness") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# ggplot2::ggsave("./Graphs/flr_dens.pdf", plot = dens.plt)

##  ----------------------------------------------------------  ##
# Diversity ####
##  ----------------------------------------------------------  ##
# How does the diversity of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(Diversity ~ Herb.Trt * Year, data = flr, iter = 9999), effect.type = "F")
## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(Diversity ~ Herb.Trt + Year, data = flr, iter = 9999), effect.type = "F")
## yr = sig, trt = NS

# Fit two models, one for each of the two categorical variables
dive.trt.fit <- lm.rrpp(Diversity ~ Herb.Trt, data = flr, iter = 9999)
dive.year.fit <- lm.rrpp(Diversity ~ Year, data = flr, iter = 9999)

# And fit the pairwise comparison assessments
dive.trt.pairs <- summary.pairwise(pairwise(dive.trt.fit, fit.null = NULL, groups = flr$Herb.Trt))
dive.year.pairs <- summary.pairwise(pairwise(dive.year.fit, fit.null = NULL, groups = flr$Year))

# Get the pairwise results from those!
seq.bonf(pairs.file = dive.trt.pairs, fac = flr$Herb.Trt)
## NS

seq.bonf(pairs.file = dive.year.pairs, fac = flr$Year)
## 16 v 18 = sig

# Plot the 'by treatment' results
dive.plt <- ggplot(flr, aes(x = Herb.Trt, y = Diversity, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Butterfly Diversity") + 
  scale_fill_manual(values = colors) +
  box.theme; dive.plt

# Save it
#ggplot2::ggsave("./Graphs/flr_dive.pdf", plot = dive.plt)

# Get plotting dataframe
dive.pltdf <- summarySE(data = flr, measurevar = "Diversity",
                        groupvars = c("Composite.Variable", "Herb.Trt", "Year"))
dive.pltdf$Year <- as.numeric(as.character(dive.pltdf$Year))

# Plot
ggplot(dive.pltdf, aes(x = Year, y = Diversity, color = Herb.Trt)) +
  geom_path(aes(group = Herb.Trt), position = dodge, lwd = .7) +
  geom_errorbar(aes(ymax = Diversity + se, ymin = Diversity - se), position = dodge, width = .4, lwd = .8) +
  geom_point(position = dodge, size = 2) +
  geom_vline(xintercept = c(14.35, 14.5, 14.65, 17.35), lty = c(1, 2, 3, 1)) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_color_manual(values = colors) +
  sct.theme + theme(legend.position = c(0.4, 0.8))

# ggplot2::ggsave("./Graphs/flr_dive.pdf", plot = dive.plt)

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
unique(flr$Herb.Trt)
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr$Herb.Trt)

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
procD.lm(flr14.dst ~ Herb.Trt, data = flr14)
  ## NS

procD.lm(flr15.dst ~ Herb.Trt, data = flr15)
  ## NS

procD.lm(flr16.dst ~ Herb.Trt, data = flr16)
  ## NS

procD.lm(flr17.dst ~ Herb.Trt, data = flr17)
  ## NS

# Set a quick shortcut for the legend contents of each (it'll be the same for all of 'em)
trt <- c("Con", "Spr", "SnS")

# Make ordinations!
nms.3.ord(flr14.mds, flr14$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr15.mds, flr15$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr16.mds, flr16$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr17.mds, flr17$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)

# Save out the significant ones
jpeg(file = "./Graphs/flr_nms14.jpg")

dev.off()

jpeg(file = "./Graphs/flr_nms15.jpg")

dev.off()

jpeg(file = "./Graphs/flr_nms16.jpg")

dev.off()

jpeg(file = "./Graphs/flr_nms17.jpg")

dev.off()

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

