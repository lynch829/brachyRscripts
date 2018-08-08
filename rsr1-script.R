
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(FSA)
library(DescTools)

# Set path to project folder and subfolder --------------------------------
# This requires setting the working using  Ctrl+Shift+H

home <- getwd() # Save path to home directory as an object
rawData <- file.path(home, file = 'rawData') # Save path to data directory as object
rData <- file.path(home, file = 'rData')
tidyData <- file.path(home, file = 'tidyData')

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
levels(gabiRSR1$Rate) # In the next two lines 'S' is changed to 'slow' and 'R' to 'rapid' For growth rate
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
dim(rsr1)
rsr1LeafandGrowthData <- as.tibble(rsr1)


saveRDS(rsr1LeafandGrowthData, file = file.path(rData, file = 'rsr1LeafandGrowthData'))
write.csv(rsr1LeafandGrowthData, file = file.path(tidyData, file = 'rsr1LeafandGrowthData.csv'))

# Visualisaton ------------------------------------------------------------



# Stat test ---------------------------------------------------------------

## Chi square test on the categorical variables Growth rate and Hygromycin status
(rsr1HgGrtable <- table(rsr1$State, rsr1$Rate))
(chisqrsr1HgGr <- chisq.test(rsr1HgGrtable))

## Kruskal Wallis and Dunn test for Leaf Area
(rsr1LeafAreaKW <- kruskal.test(Area ~ plabel, data = rsr1LeafandGrowthData)) # Kruskal Wallis test for differences between the plants
rsr1LeafAreaDT <- dunnTest(Area ~ plabel, data = rsr1LeafandGrowthData, method = "bh") #Dunn's post hoc test
rsr1LeafAreaDTresult <- as.tibble(rsr1LeafAreaDT$res) # subsets result data from Dunn's test.
filter(rsr1LeafAreaDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

## Kruskal Wallis and Dunn test for Leaf weight
(rsr1LeafWeightKW <- kruskal.test(Weight ~ plabel, data = rsr1LeafandGrowthData)) # Kruskal Wallis test for differences between the plants
rsr1LeafWeightDT <- dunnTest(Weight ~ plabel, data = rsr1LeafandGrowthData, method = "bh") #Dunn's post hoc test
rsr1LeafWeightDTresult <- as.tibble(rsr1LeafWeightDT$res) # subsets result data from Dunn's test.
filter(rsr1LeafWeightDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

## Kruskal Wallis and Dunn test for Leaf SLA
(rsr1SLAKW <- kruskal.test(SLA ~ plabel, data = rsr1LeafandGrowthData)) # Kruskal Wallis test for differences between the plants
rsr1SLADT <- dunnTest(SLA ~ plabel, data = rsr1LeafandGrowthData, method = "bh") #Dunn's post hoc test
rsr1SLADTresult <- as.tibble(rsr1SLADT$res) # subsets result data from Dunn's test.
filter(rsr1SLADTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values. Returns no values. There are no significant differences between the samples.

save.image(file.path(rData, file = 'rsr1-data-analysis.RData'))


# Grain Weight Analysis ---------------------------------------------------

rsr1GrainWeight <- read.csv(file.path(rawData, file = 'RSR1_20_Grain_Weight.csv')) # Import grain weight data
head(rsr1GrainWeight)
rsr1GrainWeight %<>%
  rename("Weight" = "Weight_.g.", "State" = "Hgm_Stat") %>% 
  as.tibble()
Desc(rsr1GrainWeight$Weight ~ rsr1GrainWeight$State)
barplot(Weight ~ Sample, data = rsr1GrainWeight) # A boxplot shows hg+ positive grains are heavier.
t.test(Weight ~ State, data = rsr1GrainWeight) # There is statistical significant difference in 20 grain weight between the hg+ and hg- plants

grainWeightPlot1 <- ggplot(rsr1GrainWeight, aes(x = reorder(Sample, Weight), y=Weight, fill = State)) +
  geom_bar(stat="identity") +
  labs(title=NULL, x="B distachyon RSR1i Lines", y = "
       Weight of 20 grains (mg)")

grainWeightPlot2 <- ggplot(rsr1GrainWeight, aes(x=State, y=Weight, fill=State)) +
  geom_boxplot() +
  labs(title=NULL, x="Hygromycin Status", y = "Weight of 20 grains (mg)")

grainWeightPlot1
grainWeightPlot2

saveRDS(rsr1GrainWeight, file = file.path(rData, file = 'rsr1GrainWeight'))
write.csv(rsr1GrainWeight, file = file.path(tidyData, file = 'rsr1GrainWeight.csv'))


# Grain Dimension Analysis ------------------------------------------------

rsr1GrainDimension <- read.csv(file.path(rawData, file = 'RSR1_Grain_Dimension.csv'))
head(rsr1GrainDimension)
dim(rsr1GrainDimension)
rsr1GrainDimension %<>% 
  rename("Sample" = "Label", "gLength" = "Major..l.", "gWidth" = "Minor..w.") %>%
  group_by(Sample) %>% 
  as.tibble()
  
Desc(rsr1GrainDimension$gLength ~ rsr1GrainDimension$Sample)
Desc(rsr1GrainDimension$gWidth ~ rsr1GrainDimension$Sample)
Desc(rsr1GrainDimension$Area ~ rsr1GrainDimension$Sample)


## Kruskal Wallis and Dunn test for Grain Length
(rsr1GrainLengthKW <- kruskal.test(gLength ~ Sample, data = rsr1GrainDimension)) # Kruskal Wallis test for differences between the plants
rsr1GrainLengthDT <- dunnTest(gLength ~ Sample, data = rsr1GrainDimension, method = "bh") #Dunn's post hoc test
rsr1GrainLengthDTresult <- as.tibble(rsr1GrainLengthDT$res) # subsets result data from Dunn's test.
filter(rsr1GrainLengthDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

## Kruskal Wallis and Dunn test for Grain Width
(rsr1GrainWidthKW <- kruskal.test(gWidth ~ Sample, data = rsr1GrainDimension)) # Kruskal Wallis test for differences between the plants
rsr1GrainWidthDT <- dunnTest(gWidth ~ Sample, data = rsr1GrainDimension, method = "bh") #Dunn's post hoc test
rsr1GrainWidthDTresult <- as.tibble(rsr1GrainWidthDT$res) # subsets result data from Dunn's test.
filter(rsr1GrainWidthDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

## Kruskal Wallis and Dunn test for Grain Area
(rsr1GrainAreaKW <- kruskal.test(Area ~ Sample, data = rsr1GrainDimension)) # Kruskal Wallis test for differences between the plants
rsr1GrainAreaDT <- dunnTest(Area ~ Sample, data = rsr1GrainDimension, method = "bh") #Dunn's post hoc test
rsr1GrainAreaDTresult <- as.tibble(rsr1GrainAreaDT$res) # subsets result data from Dunn's test.
filter(rsr1GrainAreaDTresult, P.adj < 0.05) # Asks for values in the result table that has less than 0.05 p adjusted values returns no values. There are no significant differences between the samples.

# RSR1 Grain Dimension Visualisation --------------------------------------

rsr1GrainDimension %>% 
  ggplot(aes(x = reorder(Sample, gLength), y=gLength, fill = State)) +
  geom_boxplot()

rsr1GrainDimension %>% 
  ggplot(aes(x = reorder(Sample, gWidth), y=gWidth, fill = State)) +
  geom_boxplot()

rsr1GrainDimension %>% 
  ggplot(aes(x = reorder(Sample, Area), y=Area, fill = State)) +
  geom_boxplot()

# Below, the mean and standard deviations are calculated and used to make other barplots. The features are similar.

rsr1GrainMeanArea1 <- tapply(rsr1GrainDimension$Area, rsr1GrainDimension$Sample, mean)
sampNames <- as.tibble(names(rsr1GrainMeanArea1))

rsr1GrainMeanArea <- unname(tapply(rsr1GrainDimension$Area, rsr1GrainDimension$Sample, mean))
rsr1GrainAreaSD <- unname(tapply(rsr1GrainDimension$Area, rsr1GrainDimension$Sample, sd))
rsr1GrainAreaMSD <- as.tibble(cbind(rsr1GrainMeanArea, rsr1GrainAreaSD, sampNames))

rsr1GrainMeanLength <- unname(tapply(rsr1GrainDimension$gLength, rsr1GrainDimension$Sample, mean))
rsr1GrainLengthSD <- unname(tapply(rsr1GrainDimension$gLength, rsr1GrainDimension$Sample, sd))
rsr1GrainMeanLengthMSD <- as.tibble(cbind(rsr1GrainMeanLength, rsr1GrainLengthSD, sampNames))

rsr1GrainMeanWidth <- unname(tapply(rsr1GrainDimension$gWidth, rsr1GrainDimension$Sample, mean))
rsr1GrainWidthSD <- unname(tapply(rsr1GrainDimension$gWidth, rsr1GrainDimension$Sample, sd))
rsr1GrainMeanWidthMSD <- as.tibble(cbind(rsr1GrainMeanWidth, rsr1GrainWidthSD, sampNames))

rm(rsr1GrainMeanArea1, sampNames, rsr1GrainMeanArea, rsr1GrainAreaSD, rsr1GrainMeanLength, 
   rsr1GrainLengthSD, rsr1GrainMeanWidth, rsr1GrainWidthSD)  # Clear objects no longer needed.


# Barplots for Grain Dimensions -------------------------------------------
# These plots are basic. I probably wont use them.
rsr1GrainLengthMSD %<>% 
  mutate(Sample = value) %>% 
  select(-value)

rsr1GrainAreaMSD %>% 
  ggplot(aes(x = reorder(Sample, rsr1GrainMeanLength), y=rsr1GrainMeanLength)) +
  geom_bar(stat="identity")

rsr1GrainWidthMSD %<>% 
  mutate(Sample = value) %>% 
  select(-value)

rsr1GrainAreaMSD %>% 
  ggplot(aes(x = reorder(Sample, rsr1GrainMeanWidth), y=rsr1GrainMeanWidth)) +
  geom_bar(stat="identity")

rsr1GrainAreaMSD %<>% 
  mutate(Sample = value) %>% 
  select(-value)

rsr1GrainAreaMSD %>% 
  ggplot(aes(x = reorder(Sample, rsr1GrainMeanArea), y=rsr1GrainMeanArea)) +
  geom_bar(stat="identity")


  