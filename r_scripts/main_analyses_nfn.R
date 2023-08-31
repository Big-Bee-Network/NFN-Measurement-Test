rm(list = ls())
library(tidyverse)

# # save files as processed data
researcher = read_csv('processed_data/researcher_measurements_28aug2023.csv')
nfn_long = read_csv('processed_data/nfn_measurements_28aug2023.csv') %>%
  rename(able_to_measure = `T3_1 Were you able to measure the bee?`) %>%
  filter(able_to_measure != 'No') 
  


#make boxplot and run stats
# median of nfn measurements
# vs median of the researcher measurements

researcher_sum = researcher %>% group_by(catalogNumber) %>% 
  summarize(mean_itd = mean(itd_corrected),
            median_itd = median(itd_corrected),
            n=n(), sd_itd = sd(itd_corrected)) %>%
  mutate(cv_itd = mean_itd/sd_itd, method = 'researcher')


#do the same thing but with the nfn data
nfn_sum = nfn_long %>% 
  group_by(catalogNumber) %>% 
  summarize(mean_itd = mean(itd_cm),
            median_itd = median(itd_cm),
            n=n(), sd_itd = sd(itd_cm)) %>%
  mutate(cv_itd = mean_itd/sd_itd) %>%
  mutate(method = 'nfn')

#
data = researcher_sum %>% bind_rows(nfn_sum)

with(data, boxplot(median_itd ~ method))
with(data, boxplot(cv_itd ~ method))
with(data, boxplot(sd_itd ~ method))

data_wide = data %>% 
  pivot_wider(names_from = method, values_from = c(mean_itd,median_itd,sd_itd,cv_itd,n))%>%
  mutate(diff_median_itd = median_itd_nfn-median_itd_researcher) %>%
  mutate(percent_diff_median = 100*abs(median_itd_nfn - median_itd_researcher)/ (median_itd_nfn + median_itd_researcher)/2 ,
         percent_diff_mean  = 100*abs(mean_itd_nfn - mean_itd_researcher)/ (mean_itd_nfn + mean_itd_researcher)/2)

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
with(data_wide, hist(percent_diff_mean,main = "", col=hist_blue, xlab = "% difference between mean researcher \nand community scientist measurements"))
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
pdf('figures/boxplot_median_wlines.pdf')
par(mfrow=c(1,1),mar=c(4.5,5.5,3,2),cex.lab=1.3,cex.axis=1,cex=1.5)
with(data,
     stripchart(median_itd~(method),
                ylab= "median intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientist','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide)){segments(loc_nonhost, data_wide$median_itd_nfn[i], loc_host, data_wide$median_itd_researcher[i],lty=2,col=black_shade)}
with(data,boxplot(median_itd~method,boxwex=c(.35,.35),
                       xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
dev.off()


pdf('figures/boxplot_median_nolines.pdf')
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
dev.off()


##now plot a histogram of the differences
hist(data_wide$diff_median_itd, xlab = "community scientist measurement - researcher_measurement")
abline(v=0,col='red')


#run paired t-test or signed rank test if not normally distributed
library(ggpubr)
ggqqplot(data_wide$diff_median_itd)
shapiro.test(data_wide$diff_median_itd) #significantly different from a normal distribution


with(data_wide, wilcox.test(median_itd_nfn, median_itd_researcher, paired = TRUE, alternative = "two.sided"))

#change to percent difference
pdf("figures/histogram_percentdiff_median.pdf")
with(data_wide,
     hist(percent_diff_median, main = "",col=histblue, xlab = "% difference between mean researcher \nand community scientist measurements"))
dev.off()

#let's see what happens if we remove outliers from nfn measurements
a = nfn_long %>% split(.$catalogNumber)
df=a[[10]]

#code below flags outliers in nfn_long
nfn_outliers_long = nfn_long %>% split(.$catalogNumber) %>% map_dfr(function(df){
  (out <- boxplot.stats(df$itd_cm)$out)
  outlier_index = which(df$itd_cm %in% out)
 
  df$outlier = F
  df[outlier_index,]$outlier = T
  
  return(df)
})

nrow(nfn_outliers_long %>% 
  filter(!outlier))
nrow(nfn_outliers_long %>% 
       filter(outlier))

#repeat with outliers removed
nfn_outliers = nfn_outliers_long %>% 
  filter(!outlier) %>%
  group_by(catalogNumber) %>% 
  summarize(mean_itd = mean(itd_cm),
            median_itd = median(itd_cm),
            n=n(), sd_itd = sd(itd_cm)) %>%
  mutate(cv_itd = mean_itd/sd_itd) %>%
  mutate(method = 'nfn')

data_filtered = researcher_sum %>% bind_rows(nfn_outliers)
data_wide_filtered = data_filtered %>% 
  pivot_wider(names_from = method, values_from = c(mean_itd,median_itd,sd_itd,cv_itd,n))%>%
  mutate(diff_median_itd = median_itd_nfn-median_itd_researcher) %>%
  mutate(percent_diff_median = 100*abs(median_itd_nfn - median_itd_researcher)/ (median_itd_nfn + median_itd_researcher)/2    )



with(data_filtered,
     stripchart(median_itd~(method), main='outliers removed',
                ylab= "median intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientist','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide_filtered)){segments(loc_nonhost, data_wide_filtered$median_itd_nfn[i], loc_host, data_wide_filtered$median_itd_researcher[i],lty=2,col=black_shade)}
with(data_filtered,boxplot(median_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))
##now do same thing with the sd
with(data_filtered,
     stripchart(sd_itd~(method),
                ylab= "median intertegular distance",
                xlab='data collectors',
                group.names=c('community \nscientist','researchers'),
                vertical=T,pch=16,col=unique(color_vec2),cex=.5,at=c(loc_nonhost,loc_host)))
for(i in 1:nrow(data_wide_filtered)){segments(loc_nonhost, data_wide_filtered$sd_itd_nfn[i], loc_host, data_wide_filtered$sd_itd_researcher[i],lty=2,col=black_shade)}
with(data_filtered,boxplot(sd_itd~method,boxwex=c(.35,.35),
                  xaxt = "n" ,xlab='data collectors',pch=1,col=unique(color_vec),alpha=.1,at=c(loc_nonhost,loc_host),add=T))


##now plot a histogram of the differences
hist(data_wide_filtered$diff_median_itd, xlab = "community scientist measurement - researcher_measurement", main = 'outliers removed')
abline(v=0,col='red')

#change to percent difference
pdf('figures/hist_outliers_removed.pdf')
par(mfrow=c(1,1))
with(data_wide_filtered,
     hist(percent_diff_median,main = 'outliers removed', col = histblue, xlab = '% difference between researcher \nand community scientist measurements'))
dev.off()
#
check_these = most_error$catalogNumber
nfn_outliers_long
par(mfrow=c(2,3))
for(i in 1:nrow(most_error)){
  cat_numb = most_error[i,]$catalogNumber
  my_df = nfn_outliers_long %>% filter(catalogNumber %in% cat_numb)
  with(my_df, hist(itd_cm, xlab = "intertegular distance (cm)",main="",col = histblue))
}

#run paired t-test or signed rank test if not normally distributed
library(ggpubr)
ggqqplot(data_wide_filtered$diff_median_itd)
shapiro.test(data_wide_filtered$diff_median_itd) #significantly different from a normal distribution


with(data_wide_filtered, wilcox.test(median_itd_nfn, median_itd_researcher, paired = TRUE, alternative = "two.sided"))

