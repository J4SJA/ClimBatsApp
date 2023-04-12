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


selectedField <- c("TraitCategory", "verbatimSpeciesName", "verbatimTraitName", "verbatimTraitValue", 
"verbatimTraitValueSD", "verbatimTraitUnit", "ShortReference", "Area", "AdditionalInfo1")


# read data Acoustics  --------------------------------------------------------------
colTypes <- c("text","text","numeric","numeric","numeric","text","text","text","numeric","text","text","text","text","text","text","text")
dataAc <- readxl::read_excel("data/acoustic_formatted.xlsx" , na="NA", sheet="Sheet2", col_types = colTypes)
dataAc$TraitCategory <- c("Acoustics")
dataAc <- dataAc %>% select(all_of(selectedField) )

# NOTE ! verbatimTraitValue containts text!
colTypes <- c("text","text","numeric","numeric","numeric","text","text","text","numeric","text","text","text","text","text","text","text")

dataDem <- readxl::read_excel("data/demographic_formatted.xlsx" , na="NA", sheet="Sheet1", col_types = colTypes)
dataDem$TraitCategory <- c("Demographics")
dataDem <- dataDem %>% select(all_of(selectedField) )

# diet -> alleen niche breadth?
colTypes <- c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text","text", 
              "text",   "numeric", "text", "text", "text","text","text","text","text")
dataDiet <- readxl::read_excel("data/diet_formatted.xlsx", na="NA")
dataDiet$TraitCategory <- c("Diet")
dataDiet <- dataDiet %>% 
  select(all_of(selectedField) ) %>%
  filter(is.na(verbatimTraitValue)==FALSE)


# read data Genetics  --------------------------------------------------------------
dataGen <- readxl::read_excel("data/genetic_formatted.xlsx", na="NA")
dataGen$TraitCategory <- c("Genetic data")
dataGen$verbatimTraitValueSD <- ""
dataGen <- dataGen %>% select(selectedField) 

# trait is hier niet numeric'

# read data morphology  --------------------------------------------------------------
dataMorph <- readxl::read_excel("data/morphology_formatted.xlsx", na="NA")
dataMorph$TraitCategory <- c("Morphology")
dataMorph <- dataMorph %>% select(selectedField) 


# read data phenology  --------------------------------------------------------------
dataPens <- readxl::read_excel("data/phenology_formatted.xlsx", na="NA")
dataPens$TraitCategory <- c("Phenology")
dataPens <- dataPens %>% select(selectedField) 

# read data physiology  --------------------------------------------------------------
dataPhys <- readxl::read_excel("data/physiology_formatted.xlsx", na="NA")
dataPhys$TraitCategory <- c("Physiology")
dataPhys <- dataPhys %>% 
  rename( ShortReference = 'Short ref') %>%
  select(selectedField)
    


# read data preferenendum  --------------------------------------------------------------
dataPref <- readxl::read_excel("data/preferundum_formatted.xlsx", na="NA")
dataPref$TraitCategory <- c("Physiology")
dataPref <- dataPref %>% select(selectedField) 


# read data reproduction  --------------------------------------------------------------
dataRep <- readxl::read_excel("data/reproduction_formatted.xlsx", na="NA")
dataRep$TraitCategory <- c("Reproduction")
dataRep <- dataRep %>% select(selectedField) 


# roost is categorical data only -----------------------------------------------------------


# read data spatial behaviour --------------------------------------------------------------
colTypes <- c("text", "text", "numeric", "numeric", "numeric",
              "text","text", "text", "text","text", 
              "text",   "text", "text", "numeric",            "numeric",
              "text", "text","text","text","text")
dataSpat <- readxl::read_excel("data/spatialbehaviour_formatted.xlsx",  na = "NA")
dataSpat$TraitCategory <- c("Spatial Behaviour")
dataSpat$Area <- "NA"
dataSpat <- dataSpat %>% select(selectedField) 

#merge
fullData <- rbind(
                  dataAc,
                  dataDem,
                  #dataDiet,   # categorical
                  #dataGen,    # adds non-numeric data to value field
                  dataMorph, 
                  dataPens, 
                  dataPhys, 
                  dataPref, 
                  dataRep, 
                  dataSpat)
str(fullData)

#make species names nicer.
fullData$verbatimSpeciesName <- gsub("_"," ",fullData$verbatimSpeciesName)

# make traits nicer
fullData$verbatimTraitName <-  gsub("([a-z])([A-Z])","\\1 \\2", fullData$verbatimTraitName)

# brackets with units
fullData$verbatimTraitUnit <- ifelse(is.na(fullData$verbatimTraitUnit) == TRUE, "",
  paste("(",fullData$verbatimTraitUnit,")", sep=""))

fullData <- fullData %>% filter(is.na(verbatimTraitName) == FALSE)

fullData %>% filter(is.na(verbatimTraitUnit)==TRUE)

saveRDS(fullData, file="data/combinedData.RDS")

