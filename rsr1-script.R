library(tidyverse)
library(FSA)
library(DescTools)
## Data Tidying

home <- getwd() # Save path to home directory as an object
rawData <- file.path(home, file = 'rawData') # Save path to data directory as object
rData <- file.path(home, file = 'rData')
gabiRSR1 <- read.csv(file.path(rawData, file = 'gabi-Leaf-area-and-weight.csv')) # Import data collected by Gabija
gabiRSR1 <- as.tibble(gabiRSR1)
head(gabiRSR1)
gabiRSR1 %>% 
  mutate( plabel = as.factor(gsub(".tif:", "", gabiRSR1$Label))) -> gabiRSR1 # Create a new variable in order to remove '.tif' from the plant labels

gabiRSR1 %>%
  mutate( plabel = as.factor(substr(gabiRSR1$plabel, 1, 8))) -> gabiRSR1 # Remove the Roman numeral identifiers for replicates

levels(gabiRSR1$State) # Gabi used 'POS' and 'NEG' to indicate hygromycin positive and negative plants. The next two lines of codes changes them to 'hg+' and 'hg-' respectively
levels(gabiRSR1$State)[2] <- "hg+"
levels(gabiRSR1$State)[1] <- "hg-"
levels(gabiRSR1$Rate) # In the next two lines 'S' is changed 'slow' and 'R' to 'rapid' For growth rate
levels(gabiRSR1$Rate)[1] <- "rapid"
levels(gabiRSR1$Rate)[2] <- "slow"
gabiRSR1 %>%
  mutate( SLA = Area/Weight) %>% # recalculates SLA
  select( -Label ) ->  gabiRSR1 # Drops the Label column
 
amalRSR1 <- read.csv(file.path(rawData, file = 'amal-Leaf-area-and-weight.csv')) # Import data collected by Amal
amalRSR1 <- as.tibble(amalRSR1)
head(amalRSR1) # Shows Amal's data has some columns we don't need.
amalRSR1 <- select(amalRSR1, Label, Area, Weight..g., Hg) # Select what we need.
amalRSR1 %>%
  mutate( plabel = gsub("_", "", amalRSR1$Label)) -> amalRSR1 # Make a new column that has '-' removed from the plant labels.

amalRSR1 %>%
  mutate( plabel = paste("BDT_", amalRSR1$plabel, sep = "")) -> amalRSR1 # Add 'BDT_' to the plant labels
head(amalRSR1)

amalRSR1 %>%
  mutate(plabel = as.factor(substr(amalRSR1$plabel, 1, 8))) %>% # Remove roman numerals from the plant label.
  select(Area, Weight..g., Hg, plabel) %>%
  rename( "Weight" = "Weight..g.", "State" = "Hg") -> amalRSR1 #R Rename the columns to match Gabi's

amalRSR1 %>%
  mutate(SLA = Area/Weight) -> amalRSR1 # Add an SLA column

amalGR <- read.csv(file.path(rawData, file = "amal-hg-status-and-growth-rate.csv")) # Amal has the growth rate data in another file. Here it is imported.
amalGR <- as.tibble(amalGR)
head(amalGR)

amalGR %>%
  rename("plabel" = "ID", "Rate" = "Growth", "State" = "Hg") %>% 
  mutate(plabel = gsub(" ", "", amalGR$ID)) -> amalGR  # Rename the columns and remove space from plant label,

amalGR %>%
  mutate(plabel = paste("BDT_", amalGR$plabel, sep = "")) -> amalGR # Add 'BDT_' to the plant labels
mergedamalRSR1 <- merge(amalRSR1, amalGR, by=c("plabel", "State"))
levels(mergedamalRSR1$Rate)
levels(mergedamalRSR1$Rate)[1] <- 'rapid'
levels(mergedamalRSR1$Rate)[2] <- 'slow'
head(mergedamalRSR1)
head(gabiRSR1)
dim(mergedamalRSR1)
dim(gabiRSR1)
rsr1 <- rbind(mergedamalRSR1, gabiRSR1) # Combine Gabi and Amal's Data
rsr1 <- as.tibble(rsr1)

save.image(file = "rsr1Data.RData")

########saveRDS(vegGrowthData, file.path(rData, file = 'barleyVegGrowthData'))
########write.csv(vegGrowthData, file.path(tidyData, file = 'barleyVegGrowthData-tidy.csv'))

# Visualisaton ------------------------------------------------------------



# Stat test ---------------------------------------------------------------

## Chi square test on the categorical variables Growth rate and Hygromycin status
rsr1HgGrtable <- table(rsr1$State, rsr1$Rate)
rsr1HgGrtable
chisqrsr1HgGr <- chisq.test(rsr1HgGrtable)
chisqrsr1HgGr

## Kruskal Wallis and Dunn test for Leaf Area
(rsr1AreaKW <- kruskal.test(Area ~ plabel, data = rsr1)) # Kruskal Wallis test for differences between the plants
rsr1AreaDT <- dunnTest(Area ~ plabel, data = rsr1, method = "bh") #Dunn's post hoc test
rsr1AreaDTresult <- as.tibble(rsr1AreaDT$res) # subsets result data from Dunn's test.
filter(rsr1AreaDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

## Kruskal Wallis and Dunn test for Leaf weight
(rsr1WeightKW <- kruskal.test(Weight ~ plabel, data = rsr1)) # Kruskal Wallis test for differences between the plants
rsr1WeightDT <- dunnTest(Weight ~ plabel, data = rsr1, method = "bh") #Dunn's post hoc test
rsr1WeightDTresult <- as.tibble(rsr1WeightDT$res) # subsets result data from Dunn's test.
filter(rsr1WeightDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

## Kruskal Wallis and Dunn test for Leaf SLA
(rsr1SLAKW <- kruskal.test(SLA ~ plabel, data = rsr1)) # Kruskal Wallis test for differences between the plants
rsr1SLADT <- dunnTest(SLA ~ plabel, data = rsr1, method = "bh") #Dunn's post hoc test
rsr1SLADTresult <- as.tibble(rsr1SLADT$res) # subsets result data from Dunn's test.
filter(rsr1SLADTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values. Returns no values. There are no significant differences between the samples.

save.image(file.path(rData, file = 'rsr1-data-analysis.RData'))

rsr1GrainWeight <- read.csv(file.path(rawData, file = 'RSR1_20_Grain_Weight.csv')) # Import grain weight data
head(rsr1GrainWeight)
rsr1GrainWeight %>%
  rename("Weight" = "Weight_.g.", "State" = "Hgm_Stat") -> rsr1GrainWeight
Desc(rsr1GrainWeight$Weight ~ rsr1GrainWeight$State)
plot(Weight ~ State, data = rsr1GrainWeight) # A boxplot shows hg+ positive grains are heavier.
t.test(Weight ~ State, data = rsr1GrainWeight) # There are
