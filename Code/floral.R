##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                            # Herbicide Side Project - Floral Resource Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ## What is the effect on butterflies and nectar resource plants of the anti-fescue treatments?
  ## Script Taxon: **Nectar Resource Plants**

# Required libraries
library(vegan); library(RRPP) # Calculate & Analyze
library(dplyr); library(ggplot2); library(Rmisc) # Plot

# Set working directory
setwd("~/Documents/School/Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
                # Housekeeping
##  ----------------------------------------------------------  ##
# Pull in data
flr.v0 <- read.csv("./Data/flr-wide.csv")
str(flr.v0)

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr.v0$Herb.Trt)
flr.v0$Herb.Trt <- factor(as.character(flr.v0$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr.v0$Herb.Trt)

# Split off 2014
flr.14 <- subset(flr.v0, Year == 14)
flr <- subset(flr.v0, Year != 14)

# Graphing shortcuts
dodge <- position_dodge(width = 0.5)
colors <- c("Con" = "#8c510a", # dark brown
            "Spr" = "#bf812d", # med. brown
            "SnS" = "#dfc27d") # light brown
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
                            # Pre-Treatment Analysis ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Cover your bases; any differences among patches pre-treatment?

# ABUNDANCE #
anova(lm.rrpp(log(Abundance) ~ Herb.Trt, data = flr.14, iter = 9999), effect.type = "F")
  ## NS

# RICHNESS #
anova(lm.rrpp(Richness ~ Herb.Trt, data = flr.14, iter = 9999), effect.type = "F")
  ## NS

# DIVERSITY #
anova(lm.rrpp(sqrt(Diversity) ~ Herb.Trt, data = flr.14, iter = 9999), effect.type = "F")
  ## NS

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Univariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
                 # Abundance ####
##  ----------------------------------------------------------  ##
# How does the abundance of flowers vary among herbicide treatment patches and over time?
anova(lm.rrpp(log(Abundance) ~ Herb.Trt * Year, data = flr, iter = 9999), effect.type = "F")
  ## interaction = NS

# Drop the interaction (because NS) and re-run
anova(lm.rrpp(log(Abundance) ~ Herb.Trt + Year, data = flr, iter = 9999), effect.type = "F")
  ## Both NS

# Plot
ggplot(flr, aes(x = Year, y = Abundance)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(method = "lm", se = F, color = "black", linetype = 2) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Floral Abundance") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.9, 0.9))

# Save it
ggplot2::ggsave("./Graphs/flr_abun.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
              # Species Richness ####
##  ----------------------------------------------------------  ##
# How does the species density of flowers vary among herbicide treatment patches and over time?
anova(lm.rrpp(Richness ~ Herb.Trt * Year, data = flr, iter = 9999), effect.type = "F")
  ## interaction = NS

# Run w/o interaction
anova(lm.rrpp(Richness ~ Herb.Trt + Year, data = flr, iter = 9999), effect.type = "F")
  ## Both sig

# Fit a management only model and do pairwise comparisons
flr.rich.fit <- lm.rrpp(Richness ~ Herb.Trt, data = flr, iter = 9999)
flr.rich.pairs <- simp.rrpp(pairwise(flr.rich.fit, fit.null = NULL, groups = flr$Herb.Trt))
flr.rich.pairs
  ## Con = A | Spr = AB | SnS = B

# Plot
ggplot(flr, aes(x = Year, y = Richness)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Floral Richness") +
  geom_text(label = "NS", x = 14, y = 7900) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.9, 0.15))

# Save it
ggplot2::ggsave("./Graphs/flr_rich_1.pdf", plot = last_plot())

# Treatment plot
ggplot(flr, aes(x = Herb.Trt, y = Richness, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Floral Richness") +
  scale_fill_manual(values = colors) +
  
  sct.theme + theme(legend.position = "none")

# Save it
ggplot2::ggsave("./Graphs/flr_rich_2.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
                  # Diversity ####
##  ----------------------------------------------------------  ##
# How does the diversity of flowers vary among herbicide treatment patches and over time?
anova(lm.rrpp(sqrt(Diversity) ~ Herb.Trt * Year, data = flr, iter = 9999), effect.type = "F")
  ## interaction = NS

# Run without the interaction term
anova(lm.rrpp(sqrt(Diversity) ~ Herb.Trt + Year, data = flr, iter = 9999), effect.type = "F")
  ## Both sig

# Fit a management only model and do pairwise comparisons
flr.dive.fit <- lm.rrpp(sqrt(Diversity) ~ Herb.Trt, data = flr, iter = 9999)
flr.dive.pairs <- simp.rrpp(pairwise(flr.dive.fit, fit.null = NULL, groups = flr$Herb.Trt))
flr.dive.pairs
  ## Con = A | Spr = B | SnS = AB

# Plot
ggplot(flr, aes(x = Year, y = Diversity)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Floral Diversity") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.9, 0.15))

# Save it
ggplot2::ggsave("./Graphs/flr_dive_1.pdf", plot = last_plot())

# Treatment plot
ggplot(flr, aes(x = Herb.Trt, y = Diversity, fill = Herb.Trt)) +
  geom_boxplot(outlier.shape = 21) +
  labs(x = "Herbicide Treatment", y = "Floral Diversity") +
  scale_fill_manual(values = colors) +
  geom_text(label = "A", x = 0.9, y = 1.75) +
  geom_text(label = "B", x = 1.9, y = 2.25) +
  geom_text(label = "AB", x = 2.8, y = 2) +
  sct.theme + theme(legend.position = "none")

# Save it
ggplot2::ggsave("./Graphs/flr_dive_2.pdf", plot = last_plot())
                
##  ----------------------------------------------------------------------------------------------------------  ##
                  # Native/Exotic/Seed-mix Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
                  # Data Prep ####
##  ----------------------------------------------------------  ##
# Get the long format flower data
flr.lng <- read.csv("./Data/flr-long.csv")

# And get the treatment levels in the right order (alpha order doesn't really make sense here)
unique(flr.lng$Herb.Trt)
flr.lng$Herb.Trt <- factor(as.character(flr.lng$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr.lng$Herb.Trt)

# Make a native/exotic dataframe
nv.ex.v0 <- flr.lng %>%
  select(Year:Patch, Herb.Trt, L48.Status, Number) %>%
  group_by(Year, Site, Patch, Herb.Trt, L48.Status) %>%
  summarise(Number = sum(Number)) %>%
  tidyr::spread(key = "L48.Status", value = "Number", fill = 0)
str(nv.ex.v0)

# Make a seedmix dataframe too
sdmx.v0 <- flr.lng %>%
  filter(Seedmix == "X") %>%
  select(Year:Patch, Herb.Trt, Seedmix, Number) %>%
  group_by(Year, Site, Patch, Herb.Trt) %>%
  summarise(Abundance = sum(Number),
            Richness = vegan::specnumber(Number))
str(sdmx.v0)

# Get a Percent Native column
nv.ex.v0$Percent.Native <- with(nv.ex.v0, (N / (N + E)) * 100)

# Split both dataframes into 2014 versus everything else
nv.ex.14 <- subset(nv.ex.v0, Year == 14)
nv.ex <- subset(nv.ex.v0, Year != 14)
sdmx.14 <- subset(sdmx.v0, Year == 14)
sdmx <- subset(sdmx.v0, Year != 14)

##  ----------------------------------------------------------  ##
         # Seed-mix Analysis Plotting ####
##  ----------------------------------------------------------  ##
# ABUNDANCE (Seedmix) #
# Pre-treatment
anova(lm.rrpp(log(Abundance) ~ Herb.Trt, data = sdmx.14, iter = 9999), effect.type = "F")

# Post-treatment
anova(lm.rrpp(log(Abundance) ~ Herb.Trt * Year, data = sdmx, iter = 9999), effect.type = "F") ## NS
anova(lm.rrpp(log(Abundance) ~ Herb.Trt + Year, data = sdmx, iter = 9999), effect.type = "F")
  ## All NS

# Plot abundance
ggplot(sdmx, aes(x = Year, y = Abundance)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Seedmix Abundance") +
  geom_text(label = "NS", x = 15, y = 1800) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.85, 0.85))

# Save it
ggplot2::ggsave("./Graphs/flr_sdmx_abun.pdf", plot = last_plot())

# RICHNESS (Seedmix) #
# Pre-treatment
anova(lm.rrpp(Richness ~ Herb.Trt, data = sdmx.14, iter = 9999), effect.type = "F")

# Post-treatment
anova(lm.rrpp(Richness ~ Herb.Trt * Year, data = sdmx, iter = 9999), effect.type = "F") ## int = NS
anova(lm.rrpp(Richness ~ Herb.Trt + Year, data = sdmx, iter = 9999), effect.type = "F")
  ## All NS

# Plot
ggplot(sdmx, aes(x = Year, y = Richness)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Seedmix Richness") +
  geom_text(label = "NS", x = 15, y = 8.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.25, 0.85))

ggplot2::ggsave("./Graphs/flr_sdmx_dens.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
      # Native/Exotic Analysis & Plotting ####
##  ----------------------------------------------------------  ##
# Does the percent native flowers change with treatment and/or time?
# Pre-treatment baseline first
anova(lm.rrpp(Percent.Native ~ Herb.Trt, data = nv.ex.14, iter = 9999), effect.type = "F")

# Post-treatment
anova(lm.rrpp(Percent.Native ~ Herb.Trt * Year, data = nv.ex, iter = 9999), effect.type = "F")
anova(lm.rrpp(Percent.Native ~ Herb.Trt + Year, data = nv.ex, iter = 9999), effect.type = "F")
  ## Both NS

# Plot
ggplot(nv.ex, aes(x = Year, y = Percent.Native)) +
  geom_jitter(aes(fill = Herb.Trt, shape = Herb.Trt), width = 0.15) +
  geom_smooth(aes(color = Herb.Trt), method = "lm", se = F, linetype = 4) +
  labs(x = "Year", y = "Percent Native Flowers") +
  geom_text(label = "NS", x = 15.5, y = 80) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = 21:23) +
  sct.theme + theme(legend.position = c(0.45, 0.9))

ggplot2::ggsave("./Graphs/flr_native.pdf", plot = last_plot())

##  ----------------------------------------------------------------------------------------------------------  ##
                        # Multivariate Analysis and Plotting ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment to reduce error chances
rm(list = ls())

# Pull in nice clean dataset
flr <- read.csv("./Data/flr-wide.csv")

# Fix the levels of the adaptive management column
unique(flr$Herb.Trt)
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS"))
unique(flr$Herb.Trt)

# Make community matrix with no non-species columns
flr.rsp <- as.matrix(flr[,-c(1:5, (ncol(flr)-2):ncol(flr))])

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
  col1 <- "#8c510a" # dark brown
  col2 <- "#bf812d" # med. brown
  col3 <- "#dfc27d" # light brown
  
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

# Select your community similarity/distance index (use the style of vegan::vegdist)
comm.dist <- "jaccard"

#   Subset by year
flr14 <- subset(flr, flr$Year == 14)
flr15 <- subset(flr, flr$Year == 15)
flr16 <- subset(flr, flr$Year == 16)
flr17 <- subset(flr, flr$Year == 17)
flr18 <- subset(flr, flr$Year == 18)

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
flr18.mds <- metaMDS(flr18.dst, distance = comm.dist, engine = "monoMDS",
                    autotransform = F, expand = F, k = 2, try = 100)

# Get the stress values in their own dataframe in case it becomes useful later
stress <- data.frame(Year = c("2014", "2015", "2016", "2017", "2018"),
                     stress.unadj = c(flr14.mds$stress, flr15.mds$stress, flr16.mds$stress, 
                                      flr17.mds$stress, flr18.mds$stress))
stress$stress.rounded <- round(stress$stress.unadj, digits = 3)
write.csv(stress, "./Summary Info/stress_flr.csv", row.names = F)
  ## ranges from 0 to 1 when engine = "monoMDS" (is a % with engine = "isoMDS")

# Analyze!
anova(lm.rrpp(flr14.dst ~ Herb.Trt, data = flr14, iter = 9999), effect.type = "F")
  ## NS

anova(lm.rrpp(flr15.dst ~ Herb.Trt, data = flr15, iter = 9999), effect.type = "F")
  ## NS

anova(lm.rrpp(flr16.dst ~ Herb.Trt, data = flr16, iter = 9999), effect.type = "F")
  ## NS

anova(lm.rrpp(flr17.dst ~ Herb.Trt, data = flr17, iter = 9999), effect.type = "F")
  ## NS

anova(lm.rrpp(flr18.dst ~ Herb.Trt, data = flr18, iter = 9999), effect.type = "F")
  ## NS

# Set a quick shortcut for the legend contents of each (it'll be the same for all of 'em)
trt <- c("Con", "Spr", "SnS")

# Make ordinations!
nms.3.ord(flr14.mds, flr14$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr15.mds, flr15$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr16.mds, flr16$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr17.mds, flr17$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)
nms.3.ord(flr18.mds, flr18$Herb.Trt, g1 = "Con", g2 = "Spr", g3 = "SnS", legcont = trt)

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

