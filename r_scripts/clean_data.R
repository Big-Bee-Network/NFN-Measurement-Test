##
rm(list=ls())
library(tidyverse)


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
#next read in researcher measurements and clean


