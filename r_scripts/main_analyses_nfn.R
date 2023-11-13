rm(list = ls())
library(tidyverse)

#get bee species data
ucsb = read_csv("/Users/colleen/Dropbox/Fall_2014/ucsb/bees_restorations/raw_data/occurrences.csv")%>% 
  select(catalogNumber,scientificName)

#write data processing function to apply to both researcher and nfn datasets
processing_fn = function(df){
  df %>% left_join(ucsb) %>%
    filter(!is.na(scientificName)) %>% #get rid of anything not identified in our collection
    filter(scientificName != "Lasioglossum incompletum") #get rid of the stray lasioglossum
}

# # save files as processed data
researcher = read_csv('processed_data/researcher_measurements_19sep2023.csv') %>% processing_fn
nfn_long = read_csv('processed_data/nfn_measurements_28aug2023.csv') %>%
  rename(able_to_measure = `T3_1 Were you able to measure the bee?`) %>%
  filter(able_to_measure != 'No') %>%
  processing_fn



#make boxplot and run stats
# median of nfn measurements
# vs median of the researcher measurements
my_range = function(vec){
  range_vec = range(vec)
  range_vec[2]-range_vec[1]
}
researcher_sum = researcher %>% 
  group_by(catalogNumber, scientificName) %>% 
  summarize(mean_itd = mean(itd_corrected),
            median_itd = median(itd_corrected),
            n=n(), sd_itd = sd(itd_corrected),range_itd = my_range(itd_corrected),
            iqr_itd = IQR(itd_corrected)) %>%
  mutate(cv_itd = mean_itd/sd_itd, method = 'researcher')



#do the same thing but with the nfn data
nfn_sum = nfn_long %>% 
  group_by(catalogNumber, scientificName) %>% 
  summarize(mean_itd = mean(itd_cm),
            median_itd = median(itd_cm),
            n=n(), sd_itd = sd(itd_cm), 
            range_itd = my_range(itd_cm),
            iqr_itd = IQR(itd_cm)) %>%
  mutate(cv_itd = mean_itd/sd_itd) %>%
  mutate(method = 'nfn')

#
data = researcher_sum %>% bind_rows(nfn_sum) 



with(data, boxplot(median_itd ~ method))
with(data, boxplot(cv_itd ~ method))
with(data, boxplot(sd_itd ~ method))

data_wide = data %>% 
  pivot_wider(names_from = method, values_from = c(mean_itd,median_itd,sd_itd,cv_itd,n, range_itd, iqr_itd))%>%
  mutate(diff_median_itd = median_itd_nfn-median_itd_researcher) %>%
  mutate(diff_iqr_itd = iqr_itd_nfn-iqr_itd_researcher) %>%
  mutate(percent_diff_median = 100*abs(median_itd_nfn - median_itd_researcher)/ (median_itd_nfn + median_itd_researcher)/2 ,
         percent_diff_mean  = 100*abs(mean_itd_nfn - mean_itd_researcher)/ (mean_itd_nfn + mean_itd_researcher)/2,
         percent_diff_iqr  = 100*abs(iqr_itd_nfn - iqr_itd_researcher)/ (iqr_itd_nfn + iqr_itd_researcher)/2)


#get number of species
data_wide %>% group_by(scientificName) %>% summarize(n=n()) %>%
  arrange(desc(n))
paste0("there are ", nrow(data_wide), ' individuals from ', n_distinct(data_wide$scientificName), " taxa.")

#now try with connected lines and run the paired t-test 
# (or wilconxin signed rank test - check what alec did)

loc_nonhost=1.2; loc_host=1.8
my_cols=RColorBrewer::brewer.pal(8,"Accent")
color_vec=adjustcolor(with(data,my_cols[as.factor(method)]),.2)
color_vec2=adjustcolor(with(data,my_cols[as.factor(method)]),.7)
black_shade=adjustcolor('black',.1)

##start with plots of the mean and sd
# pdf('figures/boxpot_mean_sd_wlines.pdf', width = 12)
par(mfrow=c(1,2),mar=c(4.5,5.5,3,2),cex.lab=1.3,cex.axis=1,cex=1.5)
with(data,
     stripchart(mean_itd~(method),
                ylab= "mean intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$mean_itd_nfn[i], loc_host, data_wide$mean_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(mean_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
##now do same thing with the sd
with(data,
     stripchart(sd_itd~(method),
                ylab= "standard deviation \nintertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$sd_itd_nfn[i], loc_host, data_wide$sd_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(sd_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
#dev.off()


##now do same thing with the range and iqr
par(mfrow=c(1,2))
with(data,
     stripchart(iqr_itd~(method),
                ylab= "interquartile range \nintertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$iqr_itd_nfn[i], loc_host, data_wide$iqr_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(iqr_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))


with(data,
     stripchart(range_itd~(method),
                ylab= " range \nintertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$range_itd_nfn[i], loc_host, data_wide$range_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(range_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))



#dev.off()


# pdf('figures/boxpot_mean_sd_nolines.pdf', width = 12)
par(mfrow=c(1,2),mar=c(4.5,5.5,3,2),cex.lab=1.3,cex.axis=1,cex=1.5)
with(data,
     stripchart(mean_itd~(method),
                ylab= "mean intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
# for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$mean_itd_nfn[i], loc_host, data_wide$mean_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(mean_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
##now do same thing with the sd
with(data,
     stripchart(sd_itd~(method),
                ylab= "standard deviation \nintertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
# for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$sd_itd_nfn[i], loc_host, data_wide$sd_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(sd_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
# dev.off()

##
# plot hist of the % difference between the means
histblue = adjustcolor('lightskyblue1',.8)
# pdf("figures/histogram_percentdiff mean.pdf")
par(mfrow=c(1,1))
with(data_wide, hist(percent_diff_mean,main = "", col = histblue, xlab = "% difference between mean researcher \nand community scientist measurements"))
# dev.off()


##histogram of bees with wide percent difference
most_error = data_wide %>% arrange(desc(percent_diff_mean)) %>% slice(1:6)
# pdf("figures/histograms_most_error.pdf",width=12)
par(mfrow=c(2,3))
for(i in 1:nrow(most_error)){
  cat_numb = most_error[i,]$catalogNumber
  my_df = nfn_long %>% filter(catalogNumber %in% cat_numb)
  with(my_df, hist(itd_cm, xlab = "intertegular distance (cm)",main="",col = histblue))
}
# dev.off()
 

##plot of the median
# pdf('figures/boxplot_median_iqr_wlines.pdf', width =12)
par(mfrow=c(1,2),mar=c(4.5,5.5,3,2),cex.lab=1.3,cex.axis=1,cex=1.5)
with(data,
     stripchart(median_itd~(method),
                ylab= "median intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientist','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$median_itd_nfn[i], loc_host, data_wide$median_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(median_itd~method,boxwex=c(.35,.35),
                       xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))

with(data,
     stripchart(iqr_itd~(method),
                ylab= "interquartile range - \nintertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientists','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$iqr_itd_nfn[i], loc_host, data_wide$iqr_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(iqr_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))

# dev.off()


# pdf('figures/boxplot_median_nolines.pdf')
par(mfrow=c(1,1),mar=c(4.5,5.5,3,2),cex.lab=1.3,cex.axis=1,cex=1.5)
with(data,
     stripchart(median_itd~(method),
                ylab= "median intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientist','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
# for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$median_itd_nfn[i], loc_host, data_wide$median_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(median_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
# dev.off()


##now plot a histogram of the differences
hist(data_wide$diff_median_itd, xlab = "community scientist measurement - researcher_measurement")
abline(v=0,col='red')


#run paired t-test or signed rank test if not normally distributed
library(ggpubr)
ggqqplot(data_wide$diff_median_itd)
shapiro.test(data_wide$diff_median_itd) #significantly different from a normal distribution


with(data_wide, wilcox.test(median_itd_nfn, median_itd_researcher, paired = TRUE, alternative = "two.sided"))

# do same thing for our measure of variation:
#run paired t-test or signed rank test if not normally distributed
ggqqplot(data_wide$diff_iqr_itd)
shapiro.test(data_wide$diff_iqr_itd) #significantly different from a normal distribution


with(data_wide, wilcox.test(iqr_itd_nfn, iqr_itd_researcher, paired = TRUE, alternative = "two.sided"))
mean(data_wide$percent_diff_iqr)
median(data_wide$percent_diff_iqr)
max(data_wide$percent_diff_iqr)

#change to percent difference
# pdf("figures/histogram_percentdiff_median.pdf")
with(data_wide,
     hist(percent_diff_median, main = "",col=histblue, xlab = "% difference between median researcher \nand community scientist measurements"))
# dev.off()
#mean(data_wide$percent_diff_median)
median(data_wide$percent_diff_median)
max(data_wide$percent_diff_median)


