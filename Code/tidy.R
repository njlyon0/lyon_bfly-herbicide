##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                                # Herbicide Side Project - Tidy Code
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Written by Nicholas J Lyon

# PURPOSE ####
  ## Eventually will publish this whole Git repository (fingers crossed),
  ## for people who just take my data cleaning on faith, this script would be more than they wanted
  ## For a specific subset of people though, they may want to see my process
  ## SO, this code cleans the following data files (in order of appearance):
      # 1. Floral Resource Data
      # 2. Butterfly Data

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/1. Iowa State/Collaborations/'Herbicide Project/Herbicide.WD")

# Set required libraries
library(plyr); library(stringr); library(tidyr); library(vegan)

##  ----------------------------------------------------------------------------------------------------------  ##
               # Floral Resource Data Tidying and Response Calculation ####
##  ----------------------------------------------------------------------------------------------------------  ##
# Clear environment
rm(list = ls())

##  ----------------------------------------------------------  ##  
         # Floral Community Data Tidying
##  ----------------------------------------------------------  ##
# Get site information (from MS Access)
siteindex <- read.csv("./Indices/site17info.csv")

# Get the treatment index I created (from other, more complex, indices)
trmntindex <- read.csv("./Indices/trmntinfo.csv")

# And get the raw data
flr <- read.csv("./Data/Raw/flr17_raw.csv")
str(flr)

# Subset out all years of interest
flr_v1 <- as.data.frame(subset(flr, flr$ButterflyTransID >= "1468"))

# Remove useless columns
flr_v2 <- flr_v1[ ,-c(1, 4)]

# Add in site codes
flr_v2$DataCode <- siteindex$Data.Code[match(flr_v2$ButterflyTransID, siteindex$ButterflyTransID)]
flr_v2$DataCode <- gsub("BES", "BSH", flr_v2$DataCode)
flr_v2$DataCode <- gsub("- ", "-", flr_v2$DataCode)

# Remove transect IDs
flr_v3 <- flr_v2[,-1]

# Sum across all 5 sections such that you are left with one column of full-transect counts
flr_v3$TransectTotals <- rowSums(flr_v3[,2:6])

# Remove section counts now that they are not relevant
flr_v4 <- flr_v3[,-c(2:6)]

# Need to standardize species names
  ## R responds to upper v. lowercase differently, so coerce everything in that column to lowercase
flr_v4$Nectar.Plant.Name <- tolower(flr_v4$Nectar.Plant.Name)

  ## Check to see how many species R thinks you have
sort(unique(flr_v4$Nectar.Plant.Name)) # 199 (whee)

  ## Remove blank rows and bad IDs(could do this earlier, but this is where I got to it)
flr_v5 <- flr_v4[!(flr_v4$Nectar.Plant.Name == ""),]
flr_v6 <- flr_v5[!(flr_v5$Nectar.Plant.Name == "yellow flower"),]
flr_v7 <- flr_v6[!(flr_v6$Nectar.Plant.Name == "yellow clover"),]
flr_v7 <- flr_v7[!(flr_v7$Nectar.Plant.Name == "pink legume"),]
flr_v7 <- flr_v7[!(flr_v7$Nectar.Plant.Name == "accidental row"),]
flr_v7 <- flr_v7[!(flr_v7$Nectar.Plant.Name == "thistle"),]
flr_v7 <- flr_v7[!(flr_v7$Nectar.Plant.Name == "trifolium sp. yellow"),]
flr_v7 <- flr_v7[!(flr_v7$Nectar.Plant.Name == "coreopsis"),]
flr_v7 <- flr_v7[!(flr_v7$DataCode == "ACCIDENTAL ROW"),]
sort(unique(flr_v7$Nectar.Plant.Name)) # down to 190

# Synonyms, single spelling errors, and spacing issues
flr_v7$Nectar.Plant.Name <- gsub("lesser stitchwort", "chickweed", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("large black medick", "black medick", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("purple coneflower \\(in late july)", "purple coneflower", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("woodsorrel", "oxalis", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("horse nettle", "horsenettle", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("small cinquefoil", "common cinquefoil", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("heal all", "self-heal", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("desmodium", "showy tick trefoil", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("ticktrefoil", "tick trefoil", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("native blackberry", "blackberry", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("chickory", "chicory", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("thimble weed", "thimbleweed", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("coreopsis palmata", "prairie coreopsis", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("common plantain", "broadleaf plantain", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("verbena", "vervain", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("purple vervain", "hoary vervain", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("false sunflower", "oxeye sunflower", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub(" eyed", "-eyed", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("silene sp.", "white campion", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("loostrife", "loosestrife", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("round head", "round-headed", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("broad leaf", "broadleaf", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("black eye", "black-eyed", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("bed straw", "bedstraw", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("cluver", "culver", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("cup plants", "cup plant", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("downey", "downy", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("lobilia", "lobelia", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("rosin weed", "rosinweed", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("strawberries", "strawberry", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("sulphur", "sulfur", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("loose strife", "loosestrife", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("venus", "venus'", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("virgina", "virginia", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("ox daisy", "oxeye daisy", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("purple clover", "purple prairie clover", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("prairie mullein", "moth mullein", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("penny cress", "field pennycress", flr_v7$Nectar.Plant.Name)

# Multiple spelling errors for same species
flr_v7$Nectar.Plant.Name <- gsub("alphalfa|afalfa", "alfalfa", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("ciquef|cinquf", "cinquef", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("tall cinquefoil|pale yellow cinquefoil", "prairie cinquefoil", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("bee balm|monarda fistulosa|bergamont|bergomont", "wild bergamot", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("field bindweed|bindweed", "morning glory", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("prairie milkwort|purple milkwort|^milkwort$", "field milkwort", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("lance leaf|lance|lance-leafed|lanceleaf", "ribwort", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("partidge|patridge|prartridge", "partridge", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("lobilia", "lobelia", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("shepards|sheppards", "shepherd's", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("ohio spiderwork|spider wort", "spiderwort", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("bunchweed|bunch flower", "bunchflower", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("white lobelia|white spike lobelia", "pale spike lobelia", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("black berries|black berry", "blackberry", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("pririe|prarie", "prairie", flr_v7$Nectar.Plant.Name)

# Less complex -> more complex (for matching to "flrindex" later on)
flr_v7$Nectar.Plant.Name <- gsub("^tick trefoil$", "showy tick trefoil", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^parsnip$", "wild parsnip", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^cinquefoil$", "common cinquefoil", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^plantain$", "ribwort plantain", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^oxalis$", "yellow oxalis", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^bunchflower$", "virginia bunchflower", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^agrimony$", "swamp agrimony", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^goldenrod$", "old field goldenrod", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^yellow flax$|^grooved flax$","grooved yellow flax", flr_v7$Nectar.Plant.Name)
flr_v7$Nectar.Plant.Name <- gsub("^white indigo$|^wild indigo$", "white wild indigo", flr_v7$Nectar.Plant.Name)
sort(unique(flr_v7$Nectar.Plant.Name)) # ending at 117. Aren't you glad you took the time to tidy?

# Now, create patch codes and year information
flr_v7$Site <- as.factor(str_sub(flr_v7$DataCode, 1, 3))
flr_v7$Patch <- as.factor(str_sub(flr_v7$DataCode, 1, 5))
flr_v7$Year <- paste0("20", str_sub(flr_v7$DataCode, 8, 9))

# Add in useful variables in their own column
flr_v7$Herbicide.Treatment <- trmntindex$Herbicide.Treatment[match(flr_v7$Patch, trmntindex$Patch)]
flr_v7$Composite.Variable <- paste0(flr_v7$Year, "-", flr_v7$Herbicide.Treatment)
str(flr_v7)

# Now subset out all unwanted sites
sort(unique(flr_v7$Site))
flr_v8 <- subset(flr_v7, flr_v7$Site == "GIL" | flr_v7$Site == "LTR" | flr_v7$Site == "PYW")
sort(unique(flr_v8$Site))

# Sum so that each pasture has only one cell per species per year
flr_v9 <- aggregate(TransectTotals ~ Composite.Variable + Herbicide.Treatment + Year + Site + Patch +
                      Nectar.Plant.Name, data = flr_v8, FUN = sum)

# Save a quick-and-dirty clean version
write.csv(flr_v9, "./Data/clean_flr.csv", row.names = F)

# Might want annual totals
aggregate(TransectTotals ~ Year, data = flr_v9, FUN = sum)

##  ----------------------------------------------------------  ##  
       # Floral Community Metric Calculation
##  ----------------------------------------------------------  ##
# Push dataframes from long to wide format for calculations
flr_wide <- spread(flr_v9, Nectar.Plant.Name, TransectTotals, fill = 0)

# Create dataframes for saving other calculations
flr_wide_v2 <- flr_wide

# Calculate abundance, density, and diversity and stuff into the bigger dataframe
flr_wide_v2$Abundance <- as.vector(rowSums(flr_wide[,-c(1:5)]))
flr_wide_v2$Species.Density <- as.vector(specnumber(flr_wide[,-c(1:5)]))
flr_wide_v2$Diversity <- as.vector(diversity(flr_wide[,-c(1:5)]))

# Pre-saving check
ncol(flr_wide_v2)
str(flr_wide_v2[,c(1:7, (ncol(flr_wide_v2)-2):ncol(flr_wide_v2))])

# Save
write.csv(flr_wide_v2, file = "./Data/actual_flr.csv", row.names = F)

##  ----------------------------------------------------------  ##
    # Species/Family-Level Floral Exploration
##  ----------------------------------------------------------  ##
# Get the species information index file
flrindex <- read.csv("./Indices/flrinfo.csv")

# Get the ready-made version of the clean floral data
flr_cln <- read.csv("./Data/clean_flr.csv")
str(flr_cln)

# Need to migrate over key information about each species
flr_cln$L48.Status <- flrindex$L48.Status[match(flr_cln$Nectar.Plant.Name, tolower(flrindex$Common.Name))]
flr_cln$Family <- flrindex$Family[match(flr_cln$Nectar.Plant.Name, tolower(flrindex$Common.Name))]
flr_cln$Seedmix <- flrindex$Seedmix[match(flr_cln$Nectar.Plant.Name, tolower(flrindex$Common.Name))]

# Ensure transfer was successful
sort(unique(flr_cln$Nectar.Plant.Name[is.na(flr_cln$L48.Status)]))
sort(unique(flr_cln$Nectar.Plant.Name[is.na(flr_cln$Family)]))
sort(unique(flr_cln$Nectar.Plant.Name[is.na(flr_cln$Seedmix)]))
  ## "factor(0)" = no NAs (success!) 

# Make a dataframe for interesting subsets of the data
natives <- flr_cln
legs <- subset(flr_cln, flr_cln$Family == "Fabaceae")[, c(1:7)]
astr <- subset(flr_cln, flr_cln$Family == "Asteraceae")[, c(1:7)]
mint <- subset(flr_cln, flr_cln$Family == "Lamiaceae")[, c(1:7)]
sdmx <- subset(flr_cln, flr_cln$Seedmix == "X")[, c(1:7)]

# Reduce the natives dataframe to just a percentage of native spp.
natives_v2 <- aggregate(TransectTotals ~ Composite.Variable + Year + Herbicide.Treatment + Site + Patch +
                        L48.Status, data = natives, FUN = sum)
natives.wide <- spread(natives_v2, L48.Status, TransectTotals, fill = 0)
natives.wide$Percent.Native <- as.vector( ((natives.wide$N / (natives.wide$N + natives.wide$E) * 100) ) )

# Get abundance and richness of seedmix species
sdmx.wide <- spread(sdmx, Nectar.Plant.Name, TransectTotals, fill = 0)
sdmx.wide.v2 <- sdmx.wide
sdmx.wide.v2$Abundance <- as.vector(rowSums(sdmx.wide[,-c(1:5)]))
sdmx.wide.v2$Species.Density <- as.vector(specnumber(sdmx.wide[,-c(1:5)]))

# Get within family abundances for each of those dataframes (Legumes, Asters, and Mints)
legs.wide <- spread(legs, Nectar.Plant.Name, TransectTotals, fill = 0)
legs.wide$Abundance <- as.vector( rowSums(legs.wide[,-c(1:6)]))
astr.wide <- spread(astr, Nectar.Plant.Name, TransectTotals, fill = 0)
astr.wide$Abundance <- as.vector( rowSums(astr.wide[,-c(1:6)]))
mint.wide <- spread(mint, Nectar.Plant.Name, TransectTotals, fill = 0)
mint.wide$Abundance <- as.vector( rowSums(mint.wide[,-c(1:6)]))

# Save interesting datafiles
write.csv(natives.wide, "./Data/explore_flr_natives.csv", row.names = F)
write.csv(sdmx.wide.v2, "./Data/explore_sdmx.csv", row.names = F)
write.csv(legs.wide, "./Data/explore_legms.csv", row.names = F)
write.csv(astr.wide, "./Data/explore_astrs.csv", row.names = F)
write.csv(mint.wide, "./Data/explore_mints.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                  # Butterfly Data Cleaning and Response Calculation ####
##  ----------------------------------------------------------------------------------------------------------  ##
##  ----------------------------------------------------------  ##
             # Butterfly Data Cleaning
##  ----------------------------------------------------------  ##
# Clear environment
rm(list = ls())

# Get necessary indices
siteindex <- read.csv("./Indices/site17info.csv")
trmntindex <- read.csv("./Indices/trmntinfo.csv")

# Now get the actual data
bf <- read.csv("./Data/Raw/bf17_raw.csv")

# Subset out all years of interest
bf_v1 <- as.data.frame(subset(bf, bf$BFLY.TransID >= "1468"))

# Remove useless columns
bf_v2 <- bf_v1[ ,c(2, 4:5)]

# Add in site codes
bf_v2$DataCode <- siteindex$Data.Code[match(bf_v2$BFLY.TransID, siteindex$ButterflyTransID)]
bf_v2$DataCode <- gsub("BES", "BSH", bf_v2$DataCode)
bf_v2$DataCode <- gsub("- ", "-", bf_v2$DataCode)

# Remove transect IDs
bf_v3 <- bf_v2[,-1]

# Reorder
bf_v4 <- bf_v3[,c(3, 1:2)]

# Need to standardize species names
bf_v4$BFLY.Common.Name <- tolower(bf_v4$BFLY.Common.Name)
sort(unique(bf_v4$BFLY.Common.Name)) # started with 92 "species"

# Remove blank rows &  unidentified butterflies
bf_v5 <- bf_v4[!(bf_v4$BFLY.Common.Name == ""),]
bf_v5 <- bf_v5[-grep("unknown", bf_v5$BFLY.Common.Name), ]
bf_v5 <- bf_v5[!(bf_v5$BFLY.Common.Name == "accidental row"),]
bf_v5 <- bf_v5[!(bf_v5$BFLY.Common.Name == "copper"),]
bf_v5 <- bf_v5[!(bf_v5$BFLY.Common.Name == "blue copper"),]
sort(unique(bf_v5$BFLY.Common.Name)) # at 62 now

# Now force different spellings (let's call them "synonyms") of the same species to be the same name
bf_v5$BFLY.Common.Name <- gsub("fritilary", "fritillary", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("grey", "gray", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("etg|etb", "eastern tailed blue", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("1", "", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("sulpur|sulfur", "sulphur", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("duskwing", "duskywing", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("wild indigo duskywing", "indigo duskywing", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("^buckeye$", "common buckeye", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("^comma$", "eastern comma", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("^wood satyr$", "little wood satyr", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("^wood nymph$", "common wood nymph", bf_v5$BFLY.Common.Name)
bf_v5$BFLY.Common.Name <- gsub("^tiger swallowtail$", "eastern tiger swallowtail", bf_v5$BFLY.Common.Name)
sort(unique(bf_v5$BFLY.Common.Name)) # end at 46

# Get site and year out of the huge serial data code
bf_v5$Site <- str_sub(bf_v5$DataCode, 1, 3)
bf_v5$Patch <- str_sub(bf_v5$DataCode, 1, 5)
bf_v5$Year <- paste0("20", str_sub(bf_v5$DataCode, 8, 9))

# Foolish error, but for some butterflies the number is recorded as 0; make it 1
sort(unique(bf_v5$Number))
bf_v5$Number <- as.numeric(gsub("^0$", "1", bf_v5$Number))
sort(unique(bf_v5$Number))

# Add in useful variables in their own column
bf_v5$Herbicide.Treatment <- trmntindex$Herbicide.Treatment[match(bf_v5$Patch, trmntindex$Patch)]
bf_v5$Composite.Variable <- paste0(bf_v5$Year, "-", bf_v5$Herbicide.Treatment)

# Now subset out all unwanted sites
sort(unique(bf_v5$Site))
bf_v6 <- subset(bf_v5, bf_v5$Site == "GIL" | bf_v5$Site == "LTR" | bf_v5$Site == "PYW")
sort(unique(bf_v6$Site))

# Sum across pastures
bf_v7 <- aggregate(Number ~ Composite.Variable + Year + Herbicide.Treatment + Site + Patch +
                     BFLY.Common.Name, data = bf_v6, FUN = sum)
str(bf_v7)

# Save a quick clean version
write.csv(bf_v7, "./Data/clean_bf.csv", row.names = F)

# Get annual abundances in case that is of value
aggregate(Number ~ Year, data = bf_v7, FUN = sum)

##  ----------------------------------------------------------  ##
     # Butterfly Community Metric Calculation
##  ----------------------------------------------------------  ##
# Push dataframes from long to wide format for calculations
bf_wide <- spread(bf_v7, BFLY.Common.Name, Number, fill = 0)

# Create dataframes for saving calculations to
bf_wide_v2 <- bf_wide

# Calculate abundance, species density, and diversity for the data frame and stuff into summary dataframes
bf_wide_v2$Abundance <- as.vector(rowSums(bf_wide[,-c(1:5)]))
bf_wide_v2$Species.Density <- as.vector(specnumber(bf_wide[,-c(1:5)]))
bf_wide_v2$Diversity <- as.vector(diversity(bf_wide[,-c(1:5)], index = "shannon"))

# Idiot check
ncol(bf_wide_v2)
str(bf_wide_v2[,c(1:7, (ncol(bf_wide_v2)-2):ncol(bf_wide_v2))])

# SAVE
write.csv(bf_wide_v2, file = "./Data/actual_bf.csv", row.names = F)

##  ----------------------------------------------------------  ##
      # Species/Family-Level Bfly Exploration
##  ----------------------------------------------------------  ##
# Read in clean, long-wise data
bf_cln <- read.csv("./Data/clean_bf.csv")

# And butterfly index file
bflyindex <- read.csv("./Indices/bfinfo.csv") 

# Need to migrate over key information about each species
bf_cln$L48.Status <- bflyindex$L48.Status[match(bf_cln$BFLY.Common.Name, tolower(bflyindex$Common.Name))]
bf_cln$Family <- bflyindex$Family[match(bf_cln$BFLY.Common.Name, tolower(bflyindex$Common.Name))]
bf_cln$SGCN <- bflyindex$SGCN[match(bf_cln$BFLY.Common.Name, tolower(bflyindex$Common.Name))]

# Ensure transfer was successful
sort(unique(bf_cln$BFLY.Common.Name[is.na(bf_cln$L48.Status)]))
sort(unique(bf_cln$BFLY.Common.Name[is.na(bf_cln$Family)]))
sort(unique(bf_cln$BFLY.Common.Name[is.na(bf_cln$SGCN)]))
  ## "factor(0)" = no NAs (success!) 

# Check to see which things have enough data to be interesting
aggregate(Number ~ L48.Status , data = bf_cln, FUN = sum)
aggregate(Number ~ Family , data = bf_cln, FUN = sum)
aggregate(Number ~ SGCN, data = bf_cln, FUN = sum)
  ## Not really a lot of butterflies in any of these categories
  ## But makes sense to build the pipeline to split out at least the three biggest families

# Get the abundances (and ditch species/other info variables)
bfly.fam <- aggregate(Number ~ Composite.Variable + Year + Herbicide.Treatment + Site + Family,
                         data = bf_cln, FUN = sum)

# Get the most abundant families their own dataframes!
lyca <- subset(bfly.fam, bfly.fam$Family == "Lycaenidae")
pier <- subset(bfly.fam, bfly.fam$Family == "Pieridae")
nymp <- subset(bfly.fam, bfly.fam$Family == "Nymphalidae")

# Save 'em
write.csv(lyca, "./Data/explore_lycaenids.csv", row.names = F)
write.csv(pier, "./Data/explore_pierids.csv", row.names = F)
write.csv(nymp, "./Data/explore_nymphalids.csv", row.names = F)

##  ----------------------------------------------------------------------------------------------------------  ##
                                  # Data Dictionaries
##  ----------------------------------------------------------------------------------------------------------  ##
# Each dictionary is for the columns of the first version of that object
# Novel columns introduced/created in this code are described where they are added, not here

# Data Dictionary: "siteindex" ####
## ButterflyTransID: Four-digit numeric code shared between floral and butterfly subdatasheets in MS Access
  ### Links unique combinations of pasture, patch, whit, round, and year.
## Data.Code: Pasture, patch with whittaker number, year, round
## DataCodeSansHyphens: Data.Code info but without hyphens
## Pasture: Full pasture name (unabbreviated)
## Patch: Spelled out patch (e.g. "Center", "North", etc.)
## Whittaker: Each patch has two transects, called "Whittakers". This tells which it is (either 1 or 2)
## Date: Date of data collection for both butterfly and floral data from that transect
## Nectar.Observer: Person identifying and recording floral resource data in the field
## Nectar.Data.Enterer: Peron entering the datasheet for that transects' floral resource data
## Nectar.Entry.Date: Date floral resource data were entered
## Bfly.Observer: Person identifying and recording butterfly data in the field
## Butterfly.Data.Enterer: Peron entering the datasheet for that transects' butterfly data
## Butterfly.Entry.Date: Date butterfly data were entered
## Round: Either 1 or 2 to demonstrate which transect visit (of 2 visits per summer)
## Wind: Windspeed at start of transect in kilometers per hour (kph or km/hr)
## Temperature: Temperature at start of transect in degrees Celsius
## Cloud.Cover: Percentage cloud cover, sometimes with qualitative adjustments written in (D:)
## Start.Time: Time at which transect was started for the butterfly observer
## End.Time: Time at which the transect was ended for the butterfly observer
## Comments: Qualitative comments about habitat, taxa seen off-transect, weather, etc.
## Comments.2: Qualitative comments about possible treatment-related observations
## BtflyData.Checked.by: Name of data checker (quality controller) for butterfly data
## NectData.Checked.by: Name of data checker (quality controller) for floral resource data

# Data Dictionary: "trmntindex" ####
## Pasture: Three-character abbreviation for each pasture (aka site)
## Patch: Pasture abbreviation, hyphen, and a single-letter patch location abbreviation
## Adaptive.Mgmt: Abbreviation for the 'overall' treatment of that site (graze+burn+herbicide)
## Herbicide.Treatment: Abbreviation for which herbicide treatment was applied

# Data Dictionary: "flr" ####
## NectarRecordID: Four-digit numeric code for each floral resource plant observed on each transect
## ButterflyTransID: Four-digit numeric code shared between floral and butterfly subdatasheets in MS Access
  ### Links unique combinations of pasture, patch, whittaker, round, and year.
## Nectar.Plant.Name: Common name of the floral resource plant being observed
## USDA.scientific.name: Scientific name of that floral resource plant
  ### NOTE: frequently left unfilled out, addressed later
## Section 1-5: Number of flowering ramets (i.e. stems) of that species per 20 meter section of the transect

# Data Dictionary: "flrindex" ####
## Common.Name: The English name for that floral species (synonyms addressed later)
## Genus: self-explanatory
## species: Specific epithet of that species
## Family: self-explanatory
## Seedmix: X for yes, O for no Re: the DNR's restoration seedmix on part of H+ & GB/H+ sites
## L48.Status: Considered exotic or native in the lower 48 United States?
## Bfly.Resource: Known in the literature to be a resource for butterflies (INCOMPLETE)
## Bee.Resource: Known in the literature to be a resource for bees (INCOMPLETE)
## Other.Common.Names: Other names for this species that are in the raw data

# Data Dictionary: "bf" ####
##  ButterflyID: Five-digit numeric code for each observation of a butterfly
  ### Shared only when multiple butterflies were interacting and were therefor recorded at the same time
## BFLY.TransID: Four-digit numeric code shared between floral and butterfly subdatasheets in MS Access
  ### Links unique combinations of pasture, patch, whit, round, and year.
## Time: Time when butterfly was observed
## BFLY.Common.Name: Common name of the butterfly observed
## Number: Number of that butterfly observed (usually one, though not always)
## Activity: Either spelled out or abbreviation for the activity of the butterfly at the time of observation
## Distance..m.: The perpendicular distance to the transect from the butterfly at the time of first sighting
## ID: Record of if the butterfly was caught or if it were visually observed, how good of a sighting it was
  ### I.e. "GS" for good site", "PS" for poor sight, etc.
## Section: Section of the transect the butterfly was observed in
## Outside.Transect..: Whether the butterfly was in the "Debinksi space"
  ## (an imaginary 5x5 meter square that the observer stands in the middle of one edge of).
  ### Can be either "yes" or "out" for butterflies further away/outside the Debinski space or
  ### "no"/"in" for butterflies that were not outside the box/inside it.
## Sex: For well-observed, sexually dimorphic species, whether the individual was male (M) or female (F)
## Nectar.Plant.Common.Names: If the butterfly was observed to be nectaring on a floral resource plant,
  ### the common name of that plant is reported here 
## Nectar.species: Scientific name of that nectar plant
## Comments: When an ID cannot be made with confidence, best guesses are included here.

# Data Dictionary: "bflyindex" ####
## Common.Name: The English name for that floral species (synonyms addressed later)
## Genus: self-explanatory
## species: Specific epithet of that species
## Family: self-explanatory
## Subfamily: self-explanatory
## L48.Status: Considered exotic or native in the lower 48 United States?
## Host.Fam: Family of plants typically used as larval hosts (INCOMPLETE)
## Host.Sp.1-4: Known host plant species where available (INCOMPLETE)

# END ####
