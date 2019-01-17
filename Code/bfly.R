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
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
                 # Housekeeping
##  ----------------------------------------------------------  ##
# Pull in data
bf <- read.csv("./Data/bf-wide.csv")
str(bf)

# Make year a factor!
bf$Year <- as.factor(bf$Year)
str(bf$Year)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(bf$Herb.Trt)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(bf$Herb.Trt)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("Con" = "#003c30", #  darkish teal
            "Spr" = "#35978f", # med. teal
            "SnS" = "#80cdc1") # lite teal
yr.colors <- c("14" = "#ffeda0",
               "15" = "#feb24c",
               "16" = "#fc4e2a",
               "17" = "#bd0026",
               "18" = "#800026")
ns.color <- "#003c30" # dark teal
box.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.position = 'none')
sct.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   legend.background = element_blank(), legend.title = element_blank())

# Modification of RRPP's summary function to do multiple comparison adjustment as a matter of course
simp.rrpp <- function (object, test.type = c("dist", "VC", "var"), angle.type = c("rad", "deg"),
                       stat.table = T, confidence = 0.95, show.vectors = F, crit.dig = 3, ...) {
  
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
  
  # Make new dataframe
  df <- tab
  
  # The following steps are necessary for performing Sequential Bonferroni multiple comparison adjustment
  ## Order the rows from lowest to highest p value
  results <- df[order(df$"Pr > d"), ]
  
  ## Assign a rank based on that order
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
anova(lm.rrpp(log(Abundance) ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(log(Abundance) ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
  ## yr = sig, trt = NS

# Fit two models, one for each of the two categorical variables
abun.year.fit <- lm.rrpp(log(Abundance) ~ Year, data = bf, iter = 9999)

# And fit the pairwise comparison assessments
abun.year.pairs <- simp.rrpp(pairwise(abun.year.fit, fit.null = NULL, groups = bf$Year))

# Get the pairwise results from the sig results!
abun.year.pairs
  ## 16 v 18 = sig

# Plot the 'by treatment' results
abun.plt <- ggplot(bf, aes(x = Year, y = Abundance, fill = Year)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Year", y = "Butterfly Abundance") +
  scale_fill_manual(values = yr.colors) +
  geom_text(label = "AB", x = 0.7, y = 85) +
  geom_text(label = "AB", x = 1.7, y = 80) +
  geom_text(label = "A", x = 2.8, y = 120) +
  geom_text(label = "AB", x = 3.7, y = 87) +
  geom_text(label = "B", x = 4.8, y = 52) +
  box.theme; abun.plt

# Save it
ggplot2::ggsave("./Graphs/bf_abun.pdf", plot = abun.plt)

##  ----------------------------------------------------------  ##
              # Species Density ####
##  ----------------------------------------------------------  ##
# How does the species richness of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(Species.Density ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(Species.Density ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
  ## Both NS

# Plot the 'by treatment' results
dens.plt <- ggplot(bf, aes(x = Herb.Trt, y = Species.Density, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Butterfly Richness") + 
  scale_fill_manual(values = colors) +
  box.theme; dens.plt

# Save it
ggplot2::ggsave("./Graphs/bf_dens.pdf", plot = dens.plt)

##  ----------------------------------------------------------  ##
                 # Diversity ####
##  ----------------------------------------------------------  ##
# How does the diversity of butterflies vary among herbicide treatment patches and over time?
anova(lm.rrpp(log(Diversity) ~ Herb.Trt * Year, data = bf, iter = 9999), effect.type = "F")
## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(log(Diversity) ~ Herb.Trt + Year, data = bf, iter = 9999), effect.type = "F")
## yr = sig, trt = NS

# Fit the year model
dive.year.fit <- lm.rrpp(log(Diversity) ~ Year, data = bf, iter = 9999)

# And fit the pairwise comparison assessments
dive.year.pairs <- simp.rrpp(pairwise(dive.year.fit, fit.null = NULL, groups = bf$Year))

# Get the pairwise results from those!
dive.year.pairs
## 14 v 18 = sig
## 15 v 18 = sig

# Plot the 'by treatment' results
dive.plt <- ggplot(bf, aes(x = Year, y = Diversity, fill = Year)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Year", y = "Butterfly Diversity") +
  scale_fill_manual(values = yr.colors) +
  geom_text(label = "A", x = 0.8, y = 1.87) +
  geom_text(label = "A", x = 1.8, y = 1.83) +
  geom_text(label = "AB", x = 2.7, y = 1.89) +
  geom_text(label = "AB", x = 3.7, y = 1.9) +
  geom_text(label = "B", x = 4.8, y = 2.04) +
  box.theme; dive.plt

# Save it
ggplot2::ggsave("./Graphs/bf_dive.pdf", plot = dive.plt)

##  ----------------------------------------------------------------------------------------------------------  ##
                       # Species of Greatest Conservation Need ###
##  ----------------------------------------------------------------------------------------------------------  ##
# Get the data for long-form butterfly information
bf.lng <- read.csv("./Data/bf-long.csv")

# Subset it so you only have Species of Greatest Conservation Need (SGCN)
bf.lng.sgcn <- subset(bf.lng, bf.lng$SGCN == "X")

# Sum through so you get a total number of SGCN's
sgcn <- aggregate(Number ~ Year + Site + Patch + Herb.Trt, data = bf.lng.sgcn, FUN = sum)

# How many total SGCNs were observed?
sum(sgcn$Number)

# Exploratory plot
sgcn.plt <- ggplot(sgcn, aes(x = Herb.Trt, y = Number, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Bfly SGCN Abundance") + 
  scale_fill_manual(values = colors) +
  box.theme; sgcn.plt

##  ----------------------------------------------------------------------------------------------------------  ##
                       # Multivariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# You'll want the pairwise comparison functions & my NMS function
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
bf14.rsp <- as.matrix(bf14[,-c(1:6, (ncol(bf14)-2):ncol(bf14))])
bf15.rsp <- as.matrix(bf15[,-c(1:6, (ncol(bf15)-2):ncol(bf15))])
bf16.rsp <- as.matrix(bf16[,-c(1:6, (ncol(bf16)-2):ncol(bf16))])
bf17.rsp <- as.matrix(bf17[,-c(1:6, (ncol(bf17)-2):ncol(bf17))])
bf18.rsp <- as.matrix(bf18[,-c(1:6, (ncol(bf18)-2):ncol(bf18))])

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
nms.3.ord(bf14.mds, bf14$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf15.mds, bf15$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf16.mds, bf16$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf17.mds, bf17$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(bf18.mds, bf18$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)

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
