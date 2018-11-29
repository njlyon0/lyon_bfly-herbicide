##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                              # Herbicide Side Project -  Modeling Checks
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas Lyon

# PURPOSE ####
  ##  This code assesses model assumptions of data and checks transformation outcomes
  ## For both butterflies and flowers

# tabula rasa
rm(list = ls())

# Set working directory
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Required libraries
library(plyr); library(dtplyr); library(stringr); library(tidyr) # Clean & Explore
library(vegan); library(RRPP) # Calculate & Analyze
library(ggplot2); library(gridExtra); library(cowplot) # Plot

# Bomb modeling check custom functions
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

# Graphing shortcuts
trt.labs <- c("Con", "Spr", "SnS")
bf.colors <- c("Con" = "#003c30", "Spr" = "#35978f", "SnS" = "#80cdc1") # teals
flr.colors <- c("Con" = "#8c510a", "Spr" = "#bf812d", "SnS" = "#dfc27d") # browns

  ## Colors to apply to each of the ways of transforming data (makes looking across years more intuitive)
trnsfrm.colors <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")

# plot_grid shortcuts
trns1.labs <- c("y", "log(y)", "sqrt(y)")
trns2.labs <- c("1/y", "y^.2")

##  ----------------------------------------------------------------------------------------------------------  ##
                         # Transformation Recommendations ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Obtained from code of following sections

# Butterfly Data
    ## Abundance =  log
    ## Species.Density = untransformed
    ## Diversity = log

# Floral Data
    ## Abundance = log
    ## Species.Density = none
    ## Diversity = square root

##  ----------------------------------------------------------------------------------------------------------  ##
                     # Butterfly Normality and Variance Checks ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get data and make necessary formatting changes
bf <- read.csv("./Data/bf-wide.csv")
bf$Year <- as.factor(bf$Year)
bf$Herb.Trt <- factor(as.character(bf$Herb.Trt), levels = c("Con", "Spr", "SnS"))
str(bf); str(bf$Year); unique(bf$Herb.Trt)

##  ----------------------------------------------------------  ##
                    # Variance
##  ----------------------------------------------------------  ##
# Check within year and within treatment variances to ensure they're approximately equal
ggplot(bf, aes(Herb.Trt, Abundance, fill = Herb.Trt)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = trt.labs) +
  scale_fill_manual(labels = trt.labs, values = bf.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(bf, aes(Herb.Trt, Species.Density, fill = Herb.Trt)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = trt.labs) +
  scale_fill_manual(labels = trt.labs, values = bf.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(bf, aes(Herb.Trt, Diversity, fill = Herb.Trt)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = trt.labs) +
  scale_fill_manual(labels = trt.labs, values = bf.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

##  ----------------------------------------------------------  ##
                 # Transformations
##  ----------------------------------------------------------  ##
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
ggplot2::ggsave("./Graphs/Model Checks/trnsfrm.bf.ab.pdf", plot = last_plot())
plot_grid(bf.ab.inv.plt, bf.ab.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Model Checks/bf.ab.trnsfrm2.pdf", plot = last_plot())

# Do likewise for species density (S / 10 min)
bf.dn <- ddply(bf, c("Year"), summarise,
               None = Species.Density,
               Log = log(Species.Density),
               Inverse = 1/(Species.Density),
               Power = (Species.Density)^0.2,
               Square.Root = sqrt(Species.Density))

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
ggplot2::ggsave("./Graphs/Model Checks/bf.dn.trnsfrm1.pdf", plot = last_plot())
plot_grid(bf.dn.inv.plt, bf.dn.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Model Checks/bf.dn.trnsfrm2.pdf", plot = last_plot())

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
ggplot2::ggsave("./Graphs/Model Checks/bf.dv.trnsfrm1.pdf", plot = last_plot())
plot_grid(bf.dv.inv.plt, bf.dv.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Model Checks/bf.dv.trnsfrm2.pdf", plot = last_plot())

##  ----------------------------------------------------------------------------------------------------------  ##
                          # Butterfly Model Fit Checks ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Because I am doing perANCOVA, I don't think I can actually check the residuals of that test
  ## HOWEVER, I believe I can fit an ANCOVA and check variance/normality assumptions
  ## This (if I am correct) will approximate what is actually permuted

# Fit the model, call the resid_panel function, save it
bf.abmod <- lm(log(Abundance) ~ Herb.Trt * Year, data = bf)
bf.abmodplt <- resid_panel(bf.abmod$residuals, predict(bf.abmod), bins = 20)
ggplot2::ggsave("./Graphs/Model Checks/resid.bf.ab.pdf", plot = bf.abmodplt)

bf.dnmod <- lm(Species.Density ~ Herb.Trt * Year, data = bf)
bf.dnmodplt <- resid_panel(bf.dnmod$residuals, predict(bf.dnmod), bins = 20)
ggplot2::ggsave("./Graphs/Model Checks/resid.bf.dn.pdf", plot = bf.dnmodplt)

bf.dvmod <- lm(log(Diversity) ~ Herb.Trt * Year, data = bf)
bf.dvmodplt <- resid_panel(bf.dvmod$residuals, predict(bf.dvmod), bins = 20)
ggplot2::ggsave("./Graphs/Model Checks/resid.bf.dv.pdf", plot = bf.dvmodplt)

##  ----------------------------------------------------------------------------------------------------------  ##
                        # Floral Normality and Variance Checks ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Get data and make necessary formatting changes
flr <- read.csv("./Data/flr-wide.csv")
flr$Year <- as.factor(flr$Year)
flr$Herb.Trt <- factor(as.character(flr$Herb.Trt), levels = c("Con", "Spr", "SnS"))
str(flr); str(flr$Year); unique(flr$Herb.Trt)

##  -----------------------------  ##
          # Variance
##  -----------------------------  ##
# Check within year and within treatment variances to ensure they're approximately equal
ggplot(flr, aes(Herb.Trt, Abundance, fill = Herb.Trt)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = trt.labs) +
  scale_fill_manual(labels = trt.labs, values = flr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(flr, aes(Herb.Trt, Species.Density, fill = Herb.Trt)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = trt.labs) +
  scale_fill_manual(labels = trt.labs, values = flr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank())

ggplot(flr, aes(Herb.Trt, Diversity, fill = Herb.Trt)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  scale_x_discrete(limits = trt.labs) +
  scale_fill_manual(labels = trt.labs, values = flr.colors) +
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
ggplot2::ggsave("./Graphs/Model Checks/flr.ab.trnsfrm1.pdf", plot = last_plot())
plot_grid(flr.ab.inv.plt, flr.ab.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Model Checks/flr.ab.trnsfrm2.pdf", plot = last_plot())

# Do likewise for species density (S / 10 min)
flr.dn <- ddply(flr, c("Year"), summarise,
               None = Species.Density,
               Log = log(Species.Density),
               Inverse = 1/(Species.Density),
               Power = (Species.Density)^0.2,
               Square.Root = sqrt(Species.Density))

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
ggplot2::ggsave("./Graphs/Model Checks/flr.dn.trnsfrm1.pdf", plot = last_plot())
plot_grid(flr.dn.inv.plt, flr.dn.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Model Checks/flr.dn.trnsfrm2.pdf", plot = last_plot())

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
ggplot2::ggsave("./Graphs/Model Checks/flr.dv.trnsfrm1.pdf", plot = last_plot())
plot_grid(flr.dv.inv.plt, flr.dv.pwr.plt, labels = trns2.labs, ncol = 1, nrow = 2)
ggplot2::ggsave("./Graphs/Model Checks/flr.dv.trnsfrm2.pdf", plot = last_plot())

##  ----------------------------------------------------------------------------------------------------------  ##
                            # Floral Model Fit Checks ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Fit the model, call the resid_panel function, save it
flr.abmod <- lm(log(Abundance) ~ Herb.Trt * Year, data = flr)
flr.abmodplt <- resid_panel(flr.abmod$residuals, predict(flr.abmod), bins = 20)
ggplot2::ggsave("./Graphs/Model Checks/resid.flr.ab.pdf", plot = flr.abmodplt)

flr.dnmod <- lm(Species.Density ~ Herb.Trt * Year, data = flr)
flr.dnmodplt <- resid_panel(flr.dnmod$residuals, predict(flr.dnmod), bins = 20)
ggplot2::ggsave("./Graphs/Model Checks/resid.flr.dn.pdf", plot = flr.dnmodplt)

flr.dvmod <- lm(sqrt(Diversity) ~ Herb.Trt * Year, data = flr)
flr.dvmodplt <- resid_panel(flr.dvmod$residuals, predict(flr.dvmod), bins = 20)
ggplot2::ggsave("./Graphs/Model Checks/resid.flr.dv.pdf", plot = flr.dvmodplt)
