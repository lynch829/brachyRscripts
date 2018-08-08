# Load libraries ----------------------------------------------------------

library(tidyverse)
library(DescTools)
library(FSA)
library(magrittr)
# Set path to project folder and subfolder --------------------------------

home <- getwd()
rawData <- file.path(home, file = "rawData")
rData <- file.path(home, file = 'rData')
tidyData <- file.path(home, file = 'tidyData')

# Read in and tidy data------------------------------------------------------------

rsr1FreshBiomass <- read.csv(file.path(rawData, file = "rsr1_Plant_Fresh_Weight.csv"))
rsr1DryBiomass <- read.csv(file.path(rawData, file = "rsr1_Plant_Dry_Weight.csv"))

head(rsr1FreshBiomass)
head(rsr1DryBiomass)

# Data Exploration --------------------------------------------------------

rsr1Biomass <- as.tibble(cbind(rsr1FreshBiomass, rsr1DryBiomass))

rsr1Biomass %<>%                      # Calculate Root Shoot ratio
  mutate(BDT208_RSratio = BDT208_Root/BDT208_Shoot) %>% 
  mutate(BDT193_RSratio = BDT193_Root/BDT208_Shoot) %>% 
  mutate(BDT211_RSratio = BDT208_Root/BDT208_Shoot)

head(rsr1Biomass)

rsr1Biomass <- rsr1Biomass[c(1, 4:6, 13, 2, 7:9, 14, 3, 10:12, 15)] #Reorder the variables

massVec <- c("FreshBiomass", "DryBiomass", "ShootBiomass", "RootBiomass", "RSratio")
counter <- c(1, 6, 11)

for (v in massVec){
  bm <- as.tibble(select(rsr1Biomass, counter))
  bm <- rename(bm, BDT208=1, BDT193=2, BDT211=3)
  bm <- gather(bm, key = rsr1Lines, value = value)
  bm <- mutate(bm, rsr1Lines = as.factor(rsr1Lines))
  assign(paste(v), bm)
  KW <- kruskal.test(value ~ rsr1Lines, data = bm)
  assign(paste(v, "KW", sep = ""), KW)
  DT <- dunnTest(value ~ rsr1Lines, data = bm, method = 'bh')
  assign(paste(v, "DT", sep = ""), DT)
  gg <- ggplot(data=bm, aes(x=rsr1Lines, y = value, fill = rsr1Lines)) +
    geom_boxplot() 
  assign(paste(v, "plot", sep =""), gg)
  rm(v, bm, gg, KW, DT)
  counter <- counter + 1
}

FreshBiomassplot
DryBiomassplot
ShootBiomassplot
RootBiomassplot
RSratioplot

saveRDS()

write.csv()
