##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                              # Herbicide Side Project -  Modeling Checks
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
## 
# Need to do a fair bit of subsetting and special plotting to appropriately examine residuals/data distributions.
# Thus, this code will do that for each taxon being investigated and the outcomes will be reflected in the taxon-specific code
# Outcomes come in one of two flavors: 
  ##  1) Appropriate transformation for normal distribution and
  ##  2) Model Fit outcomes
# This will be split by both year and taxa, so this code may even outdistance the others in terms of length
# ALSO because I am not planning on actually using my "with round" analysis, I am leaving that out here

# tabula rasa
rm(list = ls())

setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Required libraries
library(plyr); library(dtplyr); library(stringr); library(tidyr) # Clean & Explore
library(vegan); library(RRPP) # Calculate & Analyze
library(ggplot2); library(gridExtra); library(cowplot) # Plot

# Bomb modeling check custom functions
  # BE SURE TO FIND OUT THE NAME OF THE STATS PERSON WHO WROTE THIS AND CITE/ACKNOWLEDGE THEM
resid_boxplot <- function(resid){
  r <- data.frame(resid)
  ggplot(r, aes(x = " ", y = resid)) +
    geom_boxplot() +
    theme_bw() +
    labs(x = " ", y = "Residuals", title = "Boxplot of Residuals")
}
resid_hist <- function(resid, bins = NA){
  if(is.na(bins)){
    bins = 30
    warning("By default, bins = 30 in resid_hist. If needed, specify an appropriate number of bins.")
  }
  resid <- data.frame(resid)
  names(resid) <- "Residual"
  ggplot(resid, aes(Residual)) +
    geom_histogram(aes(y = ..density.., fill = ..count..),
                   color = "black", fill = "grey82", bins = bins) +
    theme_bw() +
    stat_function(fun = dnorm, color = "blue",
                  args = list(mean = 0,
                              sd = sd(resid$Residual))) +
    xlim(c(-4 * sd(resid$Residual), 4 * sd(resid$Residual))) +
    labs(x = "Residual", y = "Density", title = "Histogram of Residuals")
}
resid_plot <- function(resid, pred){
  
  # Create a data frame with the residuals and predicted values
  model.values <- data.frame(resid = resid, pred = pred)
  
  # Create a residual plot
  ggplot(model.values, aes(x = pred, y = resid)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0) +
    labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot") +
    theme_bw(base_size = 10)
}
resid_qq <- function(resid){
  
  #Sort the data.
  Actual_Quantiles <- sort(resid)
  #Create inexes of residuals
  i <- 1:length(Actual_Quantiles)
  
  #Calculate theoretical normal quantiles
  norm_quant <- qnorm((i-0.375)/(length(Actual_Quantiles)+0.25))
  
  #Enter into data set
  Quantiles <- data.frame(Actual_Quantiles, norm_quant)
  
  ggplot(Quantiles, aes(norm_quant, Actual_Quantiles))+
    geom_point()+
    theme_bw()+
    labs(x="Quantile", y="Residual", title="Q-Q Plot")+
    geom_abline(intercept = mean(Actual_Quantiles), slope=sd(Actual_Quantiles),color="blue")
}
resid_panel <- function(resid, pred, bins = NA){
  
  # Create a data frame with the residuals and predicted values
  model.values <- data.frame(resid = resid, pred = pred)
  
  # Create a residual plot
  resid.plot <- resid_plot(resid, pred)
  
  # Create a histogram of the residuals
  if(is.na(bins)){
    bins = 30
    warning("By default, bins = 30 in resid_hist. If needed, specify an appropriate number of bins.")
  }
  resid.hist <- resid_hist(resid, bins = bins)
  
  # Create a q-q plot of the residuals
  resid.qq <- resid_qq(resid)
  
  # Create a boxplot of the residuals
  resid.boxplot <- resid_boxplot(resid)
  
  # Create a grid of the plots
  grid.arrange(resid.plot, resid.hist, resid.qq, resid.boxplot, ncol = 2, nrow = 2)
}

# Graphing cheats
  ## Treatment labels in the preferred order
cgr.labs <- c("Ref", "Con", "Spr", "SnS")

  ## Treatment colors 
cgr.colors <- c("Ref" = "#0868ac", "Con" = "#43a2ca", "Spr" = "#7bccc4", "SnS" = "#bae4bc")

  ## Colors to apply to each of the ways of transforming data (makes looking across years more intuitive)
trnsfrm.colors <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")

# plot_grid shortcuts
trns1.labs <- c("y", "log(y)", "sqrt(y)")
trns2.labs <- c("1/y", "y^.2")

##  ----------------------------------------------------------------------------------------------------------  ##
                         # Transformation Recommendations ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Obtained from code of following sections

# Bfly 2014 Data
    ## Abundance = 
    ## SpeciesDensity = 
    ## Diversity = 

# Bfly 2015-16
    ## Abundance = 
    ## SpeciesDensity = 
    ## Diversity = 

# Floral 2014 Data
    ## Abundance = 
    ## SpeciesDensity = 
    ## Diversity = 

# Floral 2015-16
    ## Abundance = 
    ## SpeciesDensity = 
    ## Diversity = 

##  ----------------------------------------------------------------------------------------------------------  ##
                              # Model Checking Outcomes
##  ----------------------------------------------------------------------------------------------------------  ##
# Plotted to the special folder in the "Graphs" Folder
    ## "./Graphs/Transformations/"

##  ----------------------------------------------------------------------------------------------------------  ##
                                      # Butterfly Data
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
           # Normality & Variance Checks
##  ----------------------------------------------------------  ##
# Get raw data
bf <- read.csv("./Data/Ready/bf_ready.csv")

# Subset into relevant portions
bf14 <- subset(bf, bf$Year == 2014)
bfhyp <- subset(bf, bf$Year != 2014)

##  -----------------------------  ##
         # Variance
##  -----------------------------  ##
# Check within year and within treatment variances to ensure they're approximately equal
ggplot(bf, aes(Fescue.Treatment, Abundance, color = Fescue.Treatment)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = cgr.labs) +
  scale_colour_manual(labels = cgr.labs, values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(bf, aes(Fescue.Treatment, SpeciesDensity, color = Fescue.Treatment)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = cgr.labs) +
  scale_colour_manual(labels = cgr.labs, values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(bf, aes(Fescue.Treatment, Diversity, color = Fescue.Treatment)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = cgr.labs) +
  scale_colour_manual(labels = cgr.labs, values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

##  -----------------------------  ##
      # Transformations
##  -----------------------------  ##
# Check distribution of data with no, log, sqrt, inverse, and power transformations within each year
bf.ab <- ddply(bf, c("Year"), summarise,
               None = Abundance,
               Log = log(Abundance),
               Inverse = 1/(Abundance),
               Power = (Abundance)^0.2,
               Square.Root = sqrt(Abundance))

# Plot 'em and take a look for which is consistently the best
bf.ab.none.plt <- ggplot(bf.ab, aes(x = None)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fef0d9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.ab.log.plt <- ggplot(bf.ab, aes(x = Log)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fdcc8a") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.ab.sq.plt <- ggplot(bf.ab, aes(x = Square.Root)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fc8d59") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.ab.inv.plt <- ggplot(bf.ab, aes(x = Inverse)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#e34a33") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.ab.pwr.plt <- ggplot(bf.ab, aes(x = Power)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#b30000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

plot_grid(bf.ab.none.plt, bf.ab.log.plt, bf.ab.sq.plt, labels = trns1.labs, ncol = 1, nrow = 3)
ggplot2::ggsave("./Graphs/Transformations/bf.ab.trnsfrm1.pdf", plot = last_plot())
plot_grid(bf.ab.inv.plt, bf.ab.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Transformations/bf.ab.trnsfrm2.pdf", plot = last_plot())

# Do likewise for species density (S / 10 min)
bf.dn <- ddply(bf, c("Year"), summarise,
               None = SpeciesDensity,
               Log = log(SpeciesDensity),
               Inverse = 1/(SpeciesDensity),
               Power = (SpeciesDensity)^0.2,
               Square.Root = sqrt(SpeciesDensity))

# Plot 'em and take a look for which is consistently the best
bf.dn.none.plt <- ggplot(bf.dn, aes(x = None)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fef0d9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dn.log.plt <- ggplot(bf.dn, aes(x = Log)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fdcc8a") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dn.sq.plt <- ggplot(bf.dn, aes(x = Square.Root)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fc8d59") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dn.inv.plt <- ggplot(bf.dn, aes(x = Inverse)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#e34a33") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dn.pwr.plt <- ggplot(bf.dn, aes(x = Power)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#b30000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

plot_grid(bf.dn.none.plt, bf.dn.log.plt, bf.dn.sq.plt, labels = trns1.labs, ncol = 1, nrow = 3)
ggplot2::ggsave("./Graphs/Transformations/bf.dn.trnsfrm1.pdf", plot = last_plot())
plot_grid(bf.dn.inv.plt, bf.dn.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Transformations/bf.dn.trnsfrm2.pdf", plot = last_plot())

# And for Shannon diversity (H')
bf.dv <- ddply(bf, c("Year"), summarise,
               None = Abundance,
               Log = log(Abundance),
               Inverse = 1/(Abundance),
               Power = (Abundance)^0.2,
               Square.Root = sqrt(Abundance))

# Plot 'em and take a look for which is consistently the best
bf.dv.none.plt <- ggplot(bf.dv, aes(x = None)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fef0d9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dv.log.plt <- ggplot(bf.dv, aes(x = Log)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fdcc8a") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dv.sq.plt <- ggplot(bf.dv, aes(x = Square.Root)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fc8d59") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dv.inv.plt <- ggplot(bf.dv, aes(x = Inverse)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#e34a33") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

bf.dv.pwr.plt <- ggplot(bf.dv, aes(x = Power)) +
  geom_density(aes(fill = rep.int("Z", nrow(bf.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#b30000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

plot_grid(bf.dv.none.plt, bf.dv.log.plt, bf.dv.sq.plt, labels = trns1.labs, ncol = 1, nrow = 3)
ggplot2::ggsave("./Graphs/Transformations/bf.dv.trnsfrm1.pdf", plot = last_plot())
plot_grid(bf.dv.inv.plt, bf.dv.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Transformations/bf.dv.trnsfrm2.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
                # Model Fit Checks
##  ----------------------------------------------------------  ##
# Because I am doing perANCOVA, I don't think I can actually check the residuals of that test
  ## HOWEVER, I believe I can fit an ANCOVA and check variance/normality assumptions
  ## This (if I am correct) will approximate the state of the data immediately prior to the permutation step

# 2014 bfly check (for CGRs)
bf14.abmod <- lm((Abundance^.2) ~ Fescue.Treatment, data = bf14)
bf14.abmodplt <- resid_panel(bf14.abmod$residuals, predict(bf14.abmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/bf14.abmod.pdf", plot = bf14.abmodplt)

bf14.dnmod <- lm(SpeciesDensity ~ Fescue.Treatment, data = bf14)
bf14.dnmodplt <- resid_panel(bf14.dnmod$residuals, predict(bf14.dnmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/bf14.dnmod.pdf", plot = bf14.dnmodplt)

bf14.dvmod <- lm(sqrt(Diversity) ~ Fescue.Treatment, data = bf14)
bf14.dvmodplt <- resid_panel(bf14.dvmod$residuals, predict(bf14.dvmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/bf14.dvmod.pdf", plot = bf14.dvmodplt)

# bfly CGR test
bfhyp.abmod <- lm((Abundance^.2) ~ Fescue.Treatment * Year, data = bfhyp)
bfhyp.abmodplt <- resid_panel(bfhyp.abmod$residuals, predict(bfhyp.abmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/bfhyp.abmod.pdf", plot = bfhyp.abmodplt)

bfhyp.dnmod <- lm(SpeciesDensity ~ Fescue.Treatment * Year, data = bfhyp)
bfhyp.dnmodplt <- resid_panel(bfhyp.dnmod$residuals, predict(bfhyp.dnmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/bfhyp.dnmod.pdf", plot = bfhyp.dnmodplt)

bfhyp.dvmod <- lm(sqrt(Diversity) ~ Fescue.Treatment * Year, data = bfhyp)
bfhyp.dvmodplt <- resid_panel(bfhyp.dvmod$residuals, predict(bfhyp.dvmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/bfhyp.dvmod.pdf", plot = bfhyp.dvmodplt)

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Floral Resources
##  ----------------------------------------------------------------------------------------------------------  ##
# START OF Floral
##  ----------------------------------------------------------  ##
          # Normality & Variance Checks
##  ----------------------------------------------------------  ##
# Get raw data
flr <- read.csv("./Data/Ready/flr_ready.csv")

# Subset out year '0'
flr14 <- subset(flr, flr$Year == 2014)
flrhyp <- subset(flr, flr$Year != 2014)

##  -----------------------------  ##
          # Variance
##  -----------------------------  ##
# Check within year and within treatment variances to ensure they're approximately equal
ggplot(flr, aes(Fescue.Treatment, Abundance, color = Fescue.Treatment)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = cgr.labs) +
  scale_colour_manual(labels = cgr.labs, values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(flr, aes(Fescue.Treatment, SpeciesDensity, color = Fescue.Treatment)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = cgr.labs) +
  scale_colour_manual(labels = cgr.labs, values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(flr, aes(Fescue.Treatment, Diversity, color = Fescue.Treatment)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = cgr.labs) +
  scale_colour_manual(labels = cgr.labs, values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

##  -----------------------------  ##
     # Transformations
##  -----------------------------  ##
# Check distribution of data with no, log, sqrt, inverse, and power transformations within each year
flr.ab <- ddply(flr, c("Year"), summarise,
               None = Abundance,
               Log = log(Abundance),
               Inverse = 1/(Abundance),
               Power = (Abundance)^0.2,
               Square.Root = sqrt(Abundance))

# Plot 'em and take a look for which is consistently the best
flr.ab.none.plt <- ggplot(flr.ab, aes(x = None)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fef0d9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.ab.log.plt <- ggplot(flr.ab, aes(x = Log)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fdcc8a") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.ab.sq.plt <- ggplot(flr.ab, aes(x = Square.Root)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fc8d59") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.ab.inv.plt <- ggplot(flr.ab, aes(x = Inverse)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#e34a33") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.ab.pwr.plt <- ggplot(flr.ab, aes(x = Power)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.ab)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#b30000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

plot_grid(flr.ab.none.plt, flr.ab.log.plt, flr.ab.sq.plt, labels = trns1.labs, ncol = 1, nrow = 3)
ggplot2::ggsave("./Graphs/Transformations/flr.ab.trnsfrm1.pdf", plot = last_plot())
plot_grid(flr.ab.inv.plt, flr.ab.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Transformations/flr.ab.trnsfrm2.pdf", plot = last_plot())

# Do likewise for species density (S / 10 min)
flr.dn <- ddply(flr, c("Year"), summarise,
               None = SpeciesDensity,
               Log = log(SpeciesDensity),
               Inverse = 1/(SpeciesDensity),
               Power = (SpeciesDensity)^0.2,
               Square.Root = sqrt(SpeciesDensity))

# Plot 'em and take a look for which is consistently the best
flr.dn.none.plt <- ggplot(flr.dn, aes(x = None)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fef0d9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dn.log.plt <- ggplot(flr.dn, aes(x = Log)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fdcc8a") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dn.sq.plt <- ggplot(flr.dn, aes(x = Square.Root)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fc8d59") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dn.inv.plt <- ggplot(flr.dn, aes(x = Inverse)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#e34a33") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dn.pwr.plt <- ggplot(flr.dn, aes(x = Power)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dn)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#b30000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

plot_grid(flr.dn.none.plt, flr.dn.log.plt, flr.dn.sq.plt, labels = trns1.labs, ncol = 1, nrow = 3)
ggplot2::ggsave("./Graphs/Transformations/flr.dn.trnsfrm1.pdf", plot = last_plot())
plot_grid(flr.dn.inv.plt, flr.dn.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Transformations/flr.dn.trnsfrm2.pdf", plot = last_plot())

# And for Shannon diversity (H')
flr.dv <- ddply(flr, c("Year"), summarise,
               None = Abundance,
               Log = log(Abundance),
               Inverse = 1/(Abundance),
               Power = (Abundance)^0.2,
               Square.Root = sqrt(Abundance))

# Plot 'em and take a look for which is consistently the best
flr.dv.none.plt <- ggplot(flr.dv, aes(x = None)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fef0d9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dv.log.plt <- ggplot(flr.dv, aes(x = Log)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fdcc8a") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dv.sq.plt <- ggplot(flr.dv, aes(x = Square.Root)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#fc8d59") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dv.inv.plt <- ggplot(flr.dv, aes(x = Inverse)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#e34a33") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

flr.dv.pwr.plt <- ggplot(flr.dv, aes(x = Power)) +
  geom_density(aes(fill = rep.int("Z", nrow(flr.dv)))) +
  facet_grid( ~ Year) +
  scale_fill_manual(values = "#b30000") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

plot_grid(flr.dv.none.plt, flr.dv.log.plt, flr.dv.sq.plt, labels = trns1.labs, ncol = 1, nrow = 3)
ggplot2::ggsave("./Graphs/Transformations/flr.dv.trnsfrm1.pdf", plot = last_plot())
plot_grid(flr.dv.inv.plt, flr.dv.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Transformations/flr.dv.trnsfrm2.pdf", plot = last_plot())

##  ----------------------------------------------------------  ##
                # Model Fit Checks
##  ----------------------------------------------------------  ##
# Because I am doing perANCOVA,
# I believe I can fit an ANCOVA and check variance/normality assumptions
## This (if I am correct) will approximate the state of the data immediately prior to the permutation step

# 2014 flrly check (for CGRs)
flr14.abmod <- lm((Abundance^.2) ~ Fescue.Treatment, data = flr14)
flr14.abmodplt <- resid_panel(flr14.abmod$residuals, predict(flr14.abmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/flr14.abmod.pdf", plot = flr14.abmodplt)

flr14.dnmod <- lm(SpeciesDensity ~ Fescue.Treatment, data = flr14)
flr14.dnmodplt <- resid_panel(flr14.dnmod$residuals, predict(flr14.dnmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/flr14.dnmod.pdf", plot = flr14.dnmodplt)

flr14.dvmod <- lm((Diversity^.2) ~ Fescue.Treatment, data = flr14)
flr14.dvmodplt <- resid_panel(flr14.dvmod$residuals, predict(flr14.dvmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/flr14.dvmod.pdf", plot = flr14.dvmodplt)

# flrly CGR test
flrhyp.abmod <- lm((Abundance^.2) ~ Fescue.Treatment * Year, data = flrhyp)
flrhyp.abmodplt <- resid_panel(flrhyp.abmod$residuals, predict(flrhyp.abmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/flrhyp.abmod.pdf", plot = flrhyp.abmodplt)

flrhyp.dnmod <- lm(SpeciesDensity ~ Fescue.Treatment * Year, data = flrhyp)
flrhyp.dnmodplt <- resid_panel(flrhyp.dnmod$residuals, predict(flrhyp.dnmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/flrhyp.dnmod.pdf", plot = flrhyp.dnmodplt)

flrhyp.dvmod <- lm((Diversity^.2) ~ Fescue.Treatment * Year, data = flrhyp)
flrhyp.dvmodplt <- resid_panel(flrhyp.dvmod$residuals, predict(flrhyp.dvmod), bins = 20)
ggplot2::ggsave("./Graphs/Residuals/flrhyp.dvmod.pdf", plot = flrhyp.dvmodplt)
