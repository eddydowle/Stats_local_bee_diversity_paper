#Eddy Dowle
#13/01/2025

library(BeeBDC)
library(tidyverse)
library(countrycode)
library(ggrepel)

bee_tax <- beesTaxonomy()
bee_check<-beesChecklist()

#these are updated so some variation will exist with plots created at a later date
#for posterity I have downloaded a copy from when I made the figure
#bee_check<-read.table('Bee_checklist_download8Jan2024.txt',header=T,sep='\t',row.names = NULL,quote='')
#bee_tax<-read.table('Bee_taxonomy_download8Jan2024.txt',header=T,sep='\t',row.names = NULL,quote='')

#landmass data downloaded from here: https://www.nationsonline.org/oneworld/countries_by_area.htm
land_mass_by_country2<-read.csv('landarea_bycountry_nationsoline.csv',header=T,quote='')

#summarise bee counts per country
countries_summarised<-bee_check %>% group_by(rNaturalEarth_name) %>% summarise(unique_species = n_distinct(species), unique_families = n_distinct(family), unique_genus = n_distinct(genus),unique_subfamilies = n_distinct(subfamily))

#merge land mass and 
countries_summarised_landmass<-full_join(countries_summarised,land_mass_by_country2,by=c('rNaturalEarth_name'='Country_mod'))

#find none matches
countries_summarised_landmass_nomatch<-anti_join(countries_summarised,land_mass_by_country2,by=c('rNaturalEarth_name'='Country_mod'))
#generally match a few non-matches due to different definitions of countries etc

#filter countries by size, keeping 7 extra smaller island nations
countries_summarised_landmass_nzishsize<-countries_summarised_landmass %>% filter(Area_km2 %in% (100000:400000)|rNaturalEarth_name=='Fiji'|rNaturalEarth_name=='Taiwan'|rNaturalEarth_name=='Sri Lanka'|rNaturalEarth_name=='Mauritius'|rNaturalEarth_name=='Uruguay'|rNaturalEarth_name=='Ireland'|rNaturalEarth_name=='Iceland') %>% filter(!is.na(unique_species))

#add in 3 digit country code
bee_data_country<-countries_summarised_landmass_nzishsize %>% mutate(country_code=countrycode(rNaturalEarth_name,origin='country.name',destination = 'iso3c'))

#create a columns for colouring
bee_data_country<-bee_data_country %>% mutate(Name_colour=case_when(Country=='New Zealand' ~'red',
                                                  Country!='New Zealand' ~'black'))

#plot
ggplot(bee_data_country,aes(Area_km2,unique_species))+
  geom_point(aes(size=unique_families,colour=unique_genus))+
  theme_bw()+
  scale_colour_gradient()+
  geom_label_repel(aes(label=country_code),nudge_x=0.5, box.padding=0.35,point.padding=0.5,force=1,colour=bee_data_country$Name_colour,size=7)+
  labs(x='Country km2',y='Number Unique Species',colour='Unique Genera', size='Unique Families')

