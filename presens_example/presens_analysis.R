# Example for using the presens respirometry system (Hofmann Lab, UCSB) to measure oxygen consumption rates in urchin larvae
# Code and example modified from Dr. Umihiko Hoshijima

dat = read.csv('~/Dropbox/experiments/LoadedpHOX_2018/loaded_presns_resp.csv', na.strings=c("","NA"))
dat_vials = read.csv('~/Dropbox/experiments/LoadedpHOX_2018/uBODtares-031210.csv')
colnames(dat_vials) = c('vial', 'volume_uL')

library(plyr)
library(tidyverse)

# get rid of NAN rows at bottom: 
dat = dat[complete.cases(dat$o2_1),]

# fill buckets and start times in empty cells
dat = tidyr::fill(dat, buckets, time_start)

# Tally up counted numbers: 
dat$num_total = dat$num_syringe+dat$num_counted


#Make different rows for treat, treat_mom, treat_larval.
dat$treat = substr(dat$buckets, 1,2)
dat$treat_mom = substr(dat$buckets, 1,1)
dat$treat_larval = substr(dat$buckets, 2,2)


# make column of average oxygen, as well as variance. 
dat$o2_avg = dat %>% select(o2_1, o2_2, o2_3) %>%
  apply(1, mean)
dat$o2_var = dat %>% select(o2_1, o2_2, o2_3) %>%
  apply(1, var)

# For every bucket, get the "oxygen consumed" (difference from blank)
# this is in micromolar. 
dat = plyr::ddply(dat, 'buckets', function(x){
  a = dplyr::filter(x, type == 'blank') 
  blank_mean = mean(a$o2_avg)
  x$o2.consumed = blank_mean-x$o2_avg
  return(x)
})

# Units in the Marsh and Manahan paper are weird. we are going to do picomolar per hour.  

# change micromolar to micromoles.
# first add vial volumes: 
dat = left_join(dat, dat_vials, by = 'vial') #this is in micromoles. 

dat$time_elapsed = as.numeric(lubridate::hm(dat$time_end) - lubridate::hm(dat$time_start))/3600

dat$o2_avg_std = dat$o2.consumed * dat$volume_uL /1000000 *1000  / dat$time_elapsed
# divide uL by a million to make it umoles per liter
# multiply by 1000 to get it to nanomoles
# divide by time in hours to get nanomoles per hour

ggplot(dat, aes(x = num_total, y = o2_avg_std, color = treat_mom, linetype = treat_larval))+geom_point()+geom_smooth(method = 'lm')+theme_minimal()


# Let's get rid of two outliers: 
# Vial 25 and 55. 
 # dplyr::filter(dat, num_total>400 & o2_avg_std < 4000)
dat = dplyr::filter(dat, num_total<400 | o2_avg_std > 4)


ggplot(dat, aes(x = num_total, y = o2_avg_std, color = treat_mom, linetype = treat_larval))+geom_point()+geom_smooth(method = 'lm')+theme_minimal()


ggplot(dat, aes(x = num_total, y = o2_avg_std, color = treat_larval))+geom_point()+geom_smooth(method = 'lm')+theme_minimal() + facet_grid(.~treat_mom)


# running an ancova: 
ancova_resp = lm(o2_avg_std ~ num_total*treat, data = dat)
anova(ancova_resp)

ancova_resp = lm(o2_avg_std ~ 
    num_total + 
    num_total: treat_mom + 
    num_total: treat_larval + 
    num_total:treat_mom:treat_larval, data = dat)
anova(ancova_resp)

