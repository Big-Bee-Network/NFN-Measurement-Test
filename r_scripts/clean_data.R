##
rm(list=ls())
library(tidyverse)
library(readxl)

#########
#read in unreconciled nfn data and clean
#########

nfn_unreconciled = read_csv("22471_unreconciled.csv") %>%
  mutate(catalogNumber = gsub("\\..*", "",Filename)) %>%# add a column for catalog number
  rename(tegula_x1 = `T2_1 Tegula length: x1`, tegula_x2 = `T2_1 Tegula length: x2`,
         tegula_y1 = `T2_1 Tegula length: y1`, tegula_y2 = `T2_1 Tegula length: y2`,
         scale_x1 = `T2_1 Set scale (0.5 cm): x1`, scale_x2 = `T2_1 Set scale (0.5 cm): x2`,
         scale_y1 = `T2_1 Set scale (0.5 cm): y1`, scale_y2 =`T2_1 Set scale (0.5 cm): y2`)

#calculate intertegular distance in pixels using pythagorean theorem

a_tegula=abs(nfn_unreconciled$tegula_x2 - nfn_unreconciled$tegula_x1)
b_tegula=abs(nfn_unreconciled$tegula_y2 - nfn_unreconciled$tegula_y1)

nfn_unreconciled$itd_pixels  = sqrt(a_tegula^2 + b_tegula^2)

#calculate scalebar distance in pixels using pythagorean theorem
a_scale = abs(nfn_unreconciled$scale_x2 - nfn_unreconciled$scale_x1)
b_scale = abs(nfn_unreconciled$scale_y2 - nfn_unreconciled$scale_y1)

nfn_unreconciled$scaledist_pixels = sqrt(a_scale^2 + b_scale^2)

#convert from pixels to cm
scaledist_pixels = nfn_unreconciled$scaledist_pixels
itd_pixels = nfn_unreconciled$itd_pixels

# cm/pixel = 0.5 cm / scaledist_pixels
# to get itd in cm multiply itd_pixels by the number of cm/pixel
itd_cm = itd_pixels*.5/scaledist_pixels

nfn_unreconciled$itd_cm <- itd_cm

with(nfn_unreconciled, hist(itd_cm))

nfn_unreconciled %>% filter(itd_cm >.7) %>% select(itd_cm)

############
#next read in researcher measurements, join and clean
###
alec <- read_excel("imageJ-21776.xlsx",  sheet = "ImageJ-Alec") %>%
  mutate(data_collector= 'alec')
emma <- read_excel("imageJ-21776.xlsx",  sheet = "ImageJ-Emma")%>%
  mutate(data_collector= 'emma') %>%
  filter(!is.na(`Measurement-tegula (cm)`))
kaytlin <- read_excel("imageJ-21776.xlsx",  sheet = "ImageJ-Kaytlin")%>%
  mutate(data_collector= 'kaytlin') 
luz <- read_excel("imageJ-21776.xlsx",  sheet = "ImageJ-Luz")%>%
  mutate(data_collector= 'luz')
rosie <- read_excel("imageJ-21776.xlsx",  sheet = "ImageJ-Rosie")%>%
  mutate(data_collector= 'rosie')

# bind dataframes from all researchers together 
# and add column for catalog number
researcher = alec %>% bind_rows(emma,kaytlin, luz, rosie) %>%
  rename(tegula_cm = 'Measurement-tegula (cm)',scale_cm = "True-measurement-scale (cm)") %>%
  mutate(catalogNumber = gsub("\\..*", "",Filename)) %>%
  select(catalogNumber, tegula_cm, scale_cm, data_collector, everything()) %>%
  select(-'red = bad measurement')%>% 
  mutate(itd_corrected = tegula_cm/scale_cm*.5)
  
# check that each researcher has the same number of measurements
researcher %>% group_by(data_collector) %>% summarize(n=n()) #emma's missing some. we'll have to fix that


# # save files as processed data
# write_csv(researcher, 'processed_data/researcher_measurements_28aug2023.csv')
# write_csv(nfn_unreconciled, 'processed_data/nfn_measurements_28aug2023.csv')
