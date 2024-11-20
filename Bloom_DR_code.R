### Code for Bloom Disturbance Recovery paper 


library(dplyr)
library(lubridate)
library(ggplot2)
library(remotes)
#remotes::install_github("jonathan-walter/disturbhf")
library(disturbhf)


# YEARS METHOD 
chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "year", 'peak.z', 'peak.date')

for (y in unique(dat_doymeans_filt$Year)){
  test <- dat_doymeans_filt %>%  filter(Year==y) %>% select(doy, Chlorophyll) %>% na.omit()
  ref <- dat_doymeans_filt %>%  filter(Year!=y) %>% select(doy, Chlorophyll)%>% na.omit()
  
  ref_all = data.frame(tt=unname(ref$doy), yy=unname(ref$Chlorophyll))
  testy = data.frame(tt=unname(test$doy), yy=unname(test$Chlorophyll))
  
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  print(y)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, y, test$peakz, test$peak.date))
}



# REFERENCE SUBSET 
dat_doymeans_filt$chl_z<- scale(dat_doymeans_filt$Chlorophyll)

chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "year", 'peak.z', 'peak.date')

## set concentration to below 2 z scores 
for (y in unique(dat_doymeans_filt$Year)){
  test <- dat_doymeans_filt %>%  filter(Year==y) %>% select(doy, Chlorophyll) %>% na.omit()
  ref <- dat_doymeans_filt %>%  filter(Year!=y & Chlorophyll <= 9.07) %>% select(doy, Chlorophyll)%>% na.omit()
  
  ref_all = data.frame(tt=unname(ref$doy), yy=unname(ref$Chlorophyll))
  testy = data.frame(tt=unname(test$doy), yy=unname(test$Chlorophyll))
  
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  print(y)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, y, test$peakz, test$peak.date))
}




## FIGURE 1 
# PANEL A 

panel_A <- "plot(R15$DOYtrunc, R15$Manual_Chl, 
                     ylab = expression(paste('Chlorophyll-a (', mu, 'g L'^{-1}, ')')), 
                     xlab = 'Day of Year', 
                     pch = 19, 
                     cex.lab = 2, 
                     xlim = c(138, 248), yaxt='n',xaxt='n')
                 axis(side = 2, las = 1, cex.axis=1.5)
                 axis(side = 1, at = seq(150, 248, by = 30),cex.axis=1.5)
                 lines(R15$DOYtrunc, R15$Manual_Chl)
                 points(r_ref_comp$DOYtrunc, r_ref_comp$Manual_Chl, pch = 2, cex = 0.75)
                 text(x = 138.5, y = 39.5, labels = 'A', pos = 2, cex = 2)
                 legend('topright', legend = c('Test Data', 'Reference Data'), 
                        pch = c(19, 2), col = c('black', 'black'), bty = 'n', cex = 2)
                 rect(145, -3, 155, 45, col = adjustcolor('blue', alpha.f = 0.25), border = NA)
                 rect(173, -3, 183, 45, col = adjustcolor('red', alpha.f = 0.25), border = NA)"
eval(parse(text = panel_A))


# ECDF PLOTS 


R15_ecdf <- R15 %>% filter(DOYtrunc >= 173 & DOYtrunc <= 183)
ecdf_data_bloom <- ecdf(R15_ecdf$Manual_Chl)
plot(ecdf_data_bloom, main = "ECDF Plot", xlab = "Chlorophyll-a", ylab = "Cumulative Probability",xlim = c(0, 40), col='red')


r_test15_ecdf_nonbloom <- R15 %>% filter(DOYtrunc >= 145 & DOYtrunc <= 155)
ecdf_data_non_bloom <- ecdf(r_test15_ecdf_nonbloom$Manual_Chl)
plot(ecdf_data_non_bloom, main = "ECDF Plot", xlab = "Chlorophyll-a", ylab = "Cumulative Probability",xlim = c(0, 40), col='red')


ecdf_ref <- ecdf(r_ref_comp$Manual_Chl)
plot(ecdf_ref, main = "ECDF Plot", xlab = "Chlorophyll-a", ylab = "Cumulative Probability",xlim = c(0, 40), col='blue')


environment <- environment(ecdf_data_bloom)
test_data_ECDF_part <- data.frame(x = environment$x,
                                  y= environment$y)


environment_ref <- environment(ecdf_ref)
ref_data_ECDF_part <- data.frame(x = environment_ref$x,
                                 y= environment_ref$y)

environment_non_bloom <- environment(ecdf_data_non_bloom)
non_bloom_ECDF_part <- data.frame(x = environment_non_bloom$x,
                                  y= environment_non_bloom$y)


## PANEL B


combined_code_panelB<- "plot(test_data_ECDF_part,main = NULL, xlab = expression(paste('Chlorophyll-a (', mu, 'g L'^{-1}, ')')), ylab = 'Cumulative Probability',xlim = c(0, 40),ylim=c(-.0005,1), type='l', col='red', lwd=5, cex.lab=2, cex.axis=1.5, yaxt='n')\n
x_values <- c(test_data_ECDF_part$x, rev(ref_data_ECDF_part$x))\n
y_values <- c(test_data_ECDF_part$y, rev(ref_data_ECDF_part$y))\n
polygon(x_values, y_values, col = adjustcolor('red', alpha.f = 0.25), border='darkred',lwd=5)\n
\n
axis(side = 2, las = 1, cex.axis=1.5)\n
no_dist_x_values <- c(non_bloom_ECDF_part$x, rev(ref_data_ECDF_part$x))\n
no_dist_y_values <- c(non_bloom_ECDF_part$y, rev(ref_data_ECDF_part$y))\n
polygon(no_dist_x_values, no_dist_y_values, col = adjustcolor('blue', alpha.f = 0.25), border='blue', lwd=5)\n
lines(non_bloom_ECDF_part, type='l', lty=2, lwd=5, col='blue')\n
\n
lines(ref_data_ECDF_part, type='l', lty=1, lwd=6)\n
legend('bottomright', legend = c('Test Data (Non-bloom Period)', 'Reference Data', 'Test Data (Bloom Period)'), \n
       col = c('blue', 'black', 'red'), lty = c(1, 1, 1), lwd= 2,bty = 'n', cex=2)\n
\n
text(x = 0, y = .95, labels = 'B', pos = 3, cex = 2)"

# Evaluate the combined code
eval(parse(text = combined_code_panelB))



## RUN ALGORITHM TO GET Z SCORE PLOT 
chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "year", 'peak.z', 'peak.date')
y=2015
for (y in unique(r_test$Year)){
  test <- r_test %>%  filter(Year==y) %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  ref <-  r_ref_comp %>% na.omit()
  
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=14, stride=3, ddiff_method = "integral")
  
  par(mfrow=c(2,1))
  # plot(testint[, c("wleft", "ddiff")], type="l", lwd=2)
  # plot(testint[, c("wleft", "zz")], type="l", lwd=2, col="red")
  print(y)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, y, test$peakz, test$peak.date))
}

chla <- chla %>% rename(Year = y)

r_chlplot <- r_test %>% ggplot(., aes(x=DOYtrunc, y=Manual_Chl)) + geom_point() + facet_wrap(~Year) +
  geom_vline(data = chla, aes(xintercept=test.dist.date), color='red') +
  geom_vline(data = chla, aes(xintercept=test.recov.date), color='green4') +
  labs(x="Day of Year", y='Chlorophyll-a', title="Peter Chlorophyll")+
  theme(plot.title = element_text(hjust = 0.5))


## PANEL C

combined_z_score_plot <- "plot(testint[, c('wleft', 'zz')], type='l', lwd=2, col='black', ylab='z-score', xlab='Day of Year',xlim=c(138,248), cex.lab=2,xaxt='n', yaxt='n')\naxis(side = 2, las = 1, cex.axis=1.5)\naxis(side = 1, at = seq(150, 248, by = 30), cex.axis=1.5)\nabline(h=2.5, col='red')\nlegend('topright', legend = 'Disturbance Threshold', col = 'red', lty = 1, cex = 2, bty='n')\ntext(x = 138, y = 23.5, labels = 'C', pos = 3, cex = 2)"

eval(parse(text = combined_z_score_plot))






###### UNCERTAINTY + NOISE ANALYSIS ######

# ADD NOISE TO TEST DATA #


## ADD NOISE - example 20%

chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "i", 'peak.z', 'peak.date')
for (i in 1:100){
  test1 <- R15 %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  test <- test1 %>% mutate(Manual_Chl = Manual_Chl + pmax(runif(n(), -0.2 * Manual_Chl, 0.2 * Manual_Chl), -Manual_Chl))
  ref <-  r_ref_comp %>% na.omit()
  
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  par(mfrow=c(2,1))
  print(i)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, i, test$peakz, test$peak.date))
}

R15 %>% ggplot(., aes(x=DOYtrunc, y=Manual_Chl)) + geom_point()  +
  geom_vline(data = chla, aes(xintercept=test.dist.date), color='red') +
  geom_vline(data = chla, aes(xintercept=test.recov.date), color='green4') +
  labs(x="Day of Year", y='Chlorophyll-a', title="Noise to test data")+
  theme(plot.title = element_text(hjust = 0.5))


result_df <- chla %>%
  group_by(i) %>%
  summarise(dist_dates = list(test.dist.date),
            recov_dates = list(test.recov.date))

pls20 <- result_df %>%
  group_by(dist_dates, recov_dates) %>%
  summarize(count = n()) %>%
  ungroup() %>% mutate(variability =20)






# THINNING REFERENCE DATA - example 100 reference observations 
chla <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "i", 'peak.z', 'peak.date', 'mean')
for (i in 1:100){
  test <- R15 %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  ref1 <-  r_ref_comp %>% na.omit()
  ref <- ref1[sample(nrow(ref1), 100, replace = FALSE),]
  mean_ref <- ref %>% summarize(mean_value = mean(Manual_Chl))
  
  
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  par(mfrow=c(2,1))
  print(i)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, i, mean_ref, test$peakz, test$peak.date))
}

result100 <- chla %>%
  group_by(test.dist.date, test.recov.date) %>%
  summarize(count = n()) %>%
  ungroup() %>% mutate(RUNS = 100)


result_df_100 <- chla %>%
  group_by(i) %>%
  summarise(dist_dates = list(test.dist.date),
            recov_dates = list(test.recov.date))

pls_100 <- result_df %>%
  group_by(dist_dates, recov_dates) %>%
  summarize(count = n()) %>%
  ungroup()


