 # Script name: climbats_1_formatData.R
#                  
# Goal: format the climbats data for presentation and downloading in the app
#
# Data source: climbats WP 1
#
# Author: Jasja Dekker
#
# Date Created: 2022-01-27
#
# Copyright (c) Jasja Dekker, 2022
# Email: info@jasjadekker.nl
#
# Notes:

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
options(tibble.print_max = Inf) # show full tibble

# commonly used packages
library(readxl)     # for data reading
library(tidyverse)	# for data wrangling


selectedField <- c("TraitCategory", "verbatimScientificName", "verbatimTraitName", "verbatimTraitValue", 
"verbatimTraitValueSD", "verbatimTraitUnit", "ShortReference", "Area", "AdditionalInfo1")

# we must split the trait vlaue into two columns
# one of kwalitative, one of quantitative data.
# so read as char, join trait type from definition file
#  dupplicate trait column and convert to numeric  

# read data Acoustics  --------------------------------------------------------------
# NOTE ! verbatimTraitValue containts text!
colTypes <- c("cccnncccnccccccc")
dataAc <- readr::read_csv("data/Traits/acoustic_signature.csv", col_types = colTypes)
dataAc$TraitCategory <- c("Acoustic signature")
dataAc <- dataAc %>% select(all_of(selectedField) )

# read data climatic association  --------------------------------------------------------------
# NOTE ! verbatimTraitValue containts text!
colTypes <- c("ncccnncccnccccccc")
dataClim<- readr::read_csv("data/Traits/climatic_associations.csv", col_types = colTypes)
dataClim$TraitCategory <- c("Climatic associations")
dataClim <- dataClim %>% select(all_of(selectedField) )


# read data diet  ------------------
colTypes <- c("nccccnnccccncccccccc")
dataDiet <- readr::read_csv("data/Traits/diet.csv", col_types = colTypes)
dataDiet$TraitCategory <- c("Diet")
dataDiet <- dataDiet %>% 
  select(all_of(selectedField) ) %>%
  filter(is.na(verbatimTraitValue)==FALSE)

# read data distribution  ------------------
colTypes <- c("nccccnnccccncccccccc")
dataDist <- readr::read_csv("data/Traits/distribution.csv", col_types = colTypes)
dataDist$TraitCategory <- c("Distribution")
dataDist <- dataDist %>% 
  select(all_of(selectedField) ) %>%
  filter(is.na(verbatimTraitValue)==FALSE)


# read data Foragng habitat ---------------
colTypes <- c("nccccnnccccncccccccc")
dataFor <- readr::read_csv("data/Traits/foraging_habitat.csv", col_types = colTypes)
dataFor$TraitCategory <- c("Foraging habitat")
dataFor <- dataFor %>% 
  select(all_of(selectedField) ) %>%
  filter(is.na(verbatimTraitValue)==FALSE)


# read data Genetic composition  --------------------------------------------------------------
colTypes <- c("nccccncccncncccccccc")
dataGen <- readr::read_csv("data/Traits/genetic_composition.csv", col_types = colTypes)
dataGen$TraitCategory <- c("Genetic composition")
dataGen$verbatimTraitValueSD <- ""
dataGen <- dataGen %>% select(selectedField) 


# read data Life history -----------------
colTypes <- c("cccnncccnccccccccc")
dataLife <- readr::read_csv("data/Traits/life_history.csv", col_types = colTypes)
dataLife$TraitCategory <- c("Life history")
dataLife <- dataLife %>% select(all_of(selectedField) )


# read data morphology  --------------------------------------------------------------
colTypes <- c("cccnncccncccccccc")
dataMorph <- readr::read_csv("data/Traits/morphology.csv", col_types = colTypes)
dataMorph$TraitCategory <- c("Morphology")
dataMorph <- dataMorph %>% select(selectedField) 

# read data Pathogens  --------------------------------------------------------------
colTypes <- c("ncccnnncccnccccccc")
dataPath <- readr::read_csv("data/Traits/pathogens.csv", col_types = colTypes)
dataPath$TraitCategory <- c("Pathogens")
dataPath <- dataPath %>% select(selectedField) 

# read data phenology  --------------------------------------------------------------
colTypes <- c("ncccnncccncccccccc")
dataPhen <- readr::read_csv("data/Traits/phenology.csv", col_types = colTypes)
dataPhen$TraitCategory <- c("Phenology")
dataPhen <- dataPhen %>% select(selectedField) 


# read data physiology  --------------------------------------------------------------
colTypes <- c("ncccnccccnccccccc")
dataPhys <- readr::read_csv("data/Traits/physiology.csv", col_types = colTypes)
dataPhys$TraitCategory <- c("Physiology")
dataPhys <- dataPhys %>% 
  rename( ShortReference = 'Short ref') %>%
  select(selectedField)
    

# read data roost type --------------------------------------------------------------
colTypes <- c("ncccnnncccnccccccc")
dataRoost <- readr::read_csv("data/Traits/roost_type.csv", col_types = colTypes)
dataRoost$TraitCategory <- c("Roost type")
dataRoost <- dataRoost %>% select(selectedField) 


# read data spatial behaviour --------------------------------------------------------------
colTypes <- c("ncccnncccncnccccccccc")
dataSpat<- readr::read_csv("data/Traits/spatial_behaviour.csv", col_types = colTypes)
dataSpat$TraitCategory <- c("Spatial behaviour")
dataSpat <- dataSpat %>% select(selectedField) 

#merge
fullData <- rbind(
                  dataAc,
                  dataClim,
                  dataDiet,
                  dataDist,   # categorical
                  dataFor,
                  dataGen,    # adds non-numeric data to value field
                  dataLife,
                  dataMorph,
                  dataPath,
                  dataPhen, 
                  dataPhys,
                  dataRoost, 
                  dataSpat)



str(fullData)

# numeric trait value
fullData$verbatimTraitValueNum <- as.numeric(fullData$verbatimTraitValue)

#make species names nicer.
fullData$verbatimScientificName <- gsub("_"," ",fullData$verbatimScientificName)

# brackets with units
fullData$verbatimTraitUnit <- ifelse(is.na(fullData$verbatimTraitUnit) == TRUE, "",
  paste("(",fullData$verbatimTraitUnit,")", sep=""))

# add trait data type from metadata
traitDefs <- readr::read_csv("data/Metadata/Trait_description.csv") %>%
  select(Category, Name, Unit, Type, Definition, comments)
fullData <- dplyr::left_join(fullData, traitDefs, by = c("verbatimTraitName" ="Name"))

# inspect for unmatched
fullData %>% filter(is.na(Category)) %>% group_by(TraitCategory,verbatimTraitName) %>% tally()



# make trait names nicer
fullData$verbatimTraitName <-  gsub("([a-z])([A-Z])","\\1 \\2", fullData$verbatimTraitName)

saveRDS(fullData, file="data/combinedData.RDS")
