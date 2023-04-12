# Script name: climbats_1_generate_completeness.R
#                  
# Goal: generate completeness plot for shiny app
#
# Author: Jasja Dekker
#
# Date Created: 2022-10-01
#
# Copyright (c) Jasja Dekker, 2022
# Email: info@jasjadekker.nl
#
# Notes:

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
options(tibble.print_max = Inf) # show full tibble

# commonly used packages
library(tidyverse)
library(data.table)
library(forcats)

# a colorblind-safe palette of 12 values
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
														 "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888",
														 "#444444")

# order trait categories as in paper
orderRequired <- c(
	"Genetic composition",
	"Physiology",
	"Morphology",
	"Acoustic signature",
	"Climatic associations",
	"Foraging habitat",
	"Roost type",
	"Diet",
	"Spatial behaviour",
	"Life history",
	"Pathogens",
	"Phenology",
	"Distribution")

myData <- readRDS("data/combinedData.RDS")

# cleanup and order
myData$label <- paste(myData$TraitCategory, "-", myData$verbatimTraitName)
myData$TraitCategory <- factor(myData$TraitCategory, levels= orderRequired )

# remove unmatched traits:
myData <- myData %>% filter(is.na(Type) == FALSE)

myData$Type<-fct_recode(myData$Type, "Categorical" = "Categorical_nominal", "Quantitative"="Quantitative","Quantitative"="Quantitative_continuous","Quantitative"="Quantitative_discrete" )

myData <- data.table(myData) %>%
  .[verbatimTraitName == "MinBodyTemp", verbatimTraitName := "BodyTempTNZ"]

# cleanup and order
myData$label <- paste(myData$TraitCategory, "-", myData$verbatimTraitName)
myData$TraitCategory <- factor(myData$TraitCategory, levels= orderRequired )


completeness <- myData %>% group_by(label, verbatimScientificName) %>%
              tally() 

myData$verbatimTraitName<-as.factor(myData$verbatimTraitName)
myData$order<-as.numeric(myData$order)
myData$verbatimTraitName2<-fct_reorder(myData$verbatimTraitName,myData$order,.desc=F)
levels(myData$verbatimTraitName2)


p1 <- ggplot(data=myData %>% 
         group_by(verbatimTraitName, TraitCategory,verbatimScientificName, Type) %>%
         tally(),
       aes(x=verbatimScientificName, y=fct_relevel(verbatimTraitName,rev), color=TraitCategory, shape=Type))+
  geom_point(size=2)+scale_shape_manual(values=c(17, 15))+
  labs(x="",
       y="",
       fill="Number of data")+
  facet_grid(TraitCategory~., scales="free_y",space = "free_y", switch = "y")+
  scale_color_manual(values=safe_colorblind_palette)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5,face = "italic" ))+
  theme()+
  theme( 
  	# remove strip:
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    
  )

p1
ppi <- 400
grDevices::png("www/figure2_final2.png", width=11*ppi, height=15*ppi, res=ppi)
cowplot::plot_grid(p1, align = "v", ncol=1, nrow = 1, rel_heights = c(1))
dev.off()

ggsave("figures/figure2_data_type.jpg",  width=16, height=20, units="cm")

