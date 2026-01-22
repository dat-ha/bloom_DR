setwd("~/desktop/FINAL_LEGIT")


## EXPERIMENTAL LAKES
dat <- read.csv("Experimental_lakes_chl.csv")

dat$Year <- as.factor(dat$Year)
dat$Date <- as.Date(dat$DOYtrunc - 1, origin = paste0(dat$Year, "-01-01"))


r_ref_comp <- dat %>% filter(!Year %in% c(2013, 2014, 2015, 2019)& Lake == "R") %>% na.omit()
r_ref_comp <- r_ref_comp %>%  filter(DOYtrunc != 260)
r_test <- dat %>%
  filter(Year %in% c(2013, 2014, 2015, 2019) & Lake == "R") %>% na.omit()


## ALGO FOR PETER 
chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "year", 'peak.z', 'peak.date')
for (y in unique(r_test$Year)){
  test <- r_test %>%  filter(Year==y) %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  ref <-  r_ref_comp  %>% na.omit() %>%
    arrange(Year, DOYtrunc)
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  print(y)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, y, test$peakz, test$peak.date))
}

chla <- chla %>% rename(Year = y)



###### FIG 2 - PETER LAKE MULTI PANEL #####

# ADD DUMMY YEAR TO PLOT DATE INSTEAD OF DOY ON X AXIS
dat$Year <- as.factor(dat$Year)
dat$Date <- as.Date(dat$DOYtrunc - 1, origin = paste0(dat$Year, "-01-01"))
dat$MonthDay <- format(dat$Date, "%m-%d")
dat$MonthDayDate <- as.Date(paste0("2000-", dat$MonthDay))


r_ref_comp <- dat %>% filter(!Year %in% c(2013, 2014, 2015, 2019)& Lake == "R") %>% na.omit()
r_ref_comp <- r_ref_comp %>%  filter(DOYtrunc != 260)
r_test <- dat %>%
  filter(Year %in% c(2013, 2014, 2015, 2019) & Lake == "R") %>% na.omit()


chla$disturbMonthDay2000 <- as.Date(
  paste0("2000-", format(as.Date(chla$test.dist.date - 1, 
                                 origin = paste0(chla$Year, "-01-01")), "%m-%d")))

chla$recoveryMonthDay2000 <- as.Date(
  paste0("2000-", format(as.Date(chla$test.recov.date - 1, 
                                 origin = paste0(chla$Year, "-01-01")), "%m-%d")))

r_ref_comp_2 <- r_ref_comp %>% select(-Year)
r_test_no15 <- r_test %>% filter((Year %in% c(2013, 2014, 2019)))
R_algo_outputs_fig1 <- chla %>% filter((Year %in% c(2013, 2014, 2019))) 

nutrient_addition_periods <- data.frame(
  Year = c(2013, 2014, 2019),
  xmin = c(154, 153, 161),
  xmax = c(238, 241, 237)
)

nutrient_addition_periods <- nutrient_addition_periods %>%
  mutate(
    xmin_date = as.Date(xmin - 1, origin = "2000-01-01"),
    xmax_date = as.Date(xmax - 1, origin = "2000-01-01"))

fig2 <- r_test_no15 %>% 
  ggplot(aes(x = MonthDayDate, y = Manual_Chl)) +
  geom_point(
    data = r_ref_comp_2,
    aes(x = MonthDayDate, y = Manual_Chl),
    inherit.aes = FALSE,
    color = "grey70",
    alpha = 0.4
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(~Year, ncol=1,scales = "free_y") +
  geom_vline(data = R_algo_outputs_fig1, aes(xintercept = disturbMonthDay2000), color = 'red') +
  geom_vline(data = R_algo_outputs_fig1, aes(xintercept = recoveryMonthDay2000), color = 'green4') +
  geom_rect(
    data = nutrient_addition_periods,
    aes(xmin = xmin_date, xmax = xmax_date, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "blue",
    alpha = 0.1
  ) +labs(x="Date", y = expression("Chlorophyll-a (" * mu * "g L"^{-1} * ")"))+
  theme_classic()



### TUESDAY PLOTS ###
t_ref_comp <- dat %>% filter(!Year %in% c(2013, 2014, 2015)& Lake == "T") %>% na.omit()
t_ref_comp <- t_ref_comp %>%  filter(DOYtrunc != 261)
t_test <- dat %>%
  filter(Year %in% c(2013, 2014, 2015) & Lake == "T") %>% na.omit()


chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "year", 'peak.z', 'peak.date')
for (y in unique(r_test$Year)){
  test <- t_test %>%  filter(Year==y) %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  ref <-  t_ref_comp  %>% na.omit() %>%
    arrange(Year, DOYtrunc)
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  print(y)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, y, test$peakz, test$peak.date))
}

chla <- chla %>% rename(Year = y)


dat$Year <- as.factor(dat$Year)
dat$Date <- as.Date(dat$DOYtrunc - 1, origin = paste0(dat$Year, "-01-01"))
dat$MonthDay <- format(dat$Date, "%m-%d")
dat$MonthDayDate <- as.Date(paste0("2000-", dat$MonthDay))


t_ref_comp <- dat %>% filter(!Year %in% c(2013, 2014, 2015)& Lake == "T") %>% na.omit()
t_ref_comp <- r_ref_comp %>%  filter(DOYtrunc != 261)
t_test <- dat %>%
  filter(Year %in% c(2013, 2014, 2015) & Lake == "T") %>% na.omit()


chla$disturbMonthDay2000 <- as.Date(
  paste0("2000-", format(as.Date(chla$test.dist.date - 1, 
                                 origin = paste0(chla$Year, "-01-01")), "%m-%d")))

chla$recoveryMonthDay2000 <- as.Date(
  paste0("2000-", format(as.Date(chla$test.recov.date - 1, 
                                 origin = paste0(chla$Year, "-01-01")), "%m-%d")))

t_ref_comp_2 <- t_ref_comp %>% select(-Year)

nutrient_addition_periods <- data.frame(
  Year = c(2013, 2014, 2015),
  xmin = c(154, 153, 152),
  xmax = c(238, 241, 240)
)

nutrient_addition_periods <- nutrient_addition_periods %>%
  mutate(
    xmin_date = as.Date(xmin - 1, origin = "2000-01-01"),
    xmax_date = as.Date(xmax - 1, origin = "2000-01-01"))

figure_3 <- t_test %>% 
  ggplot(aes(x = MonthDayDate, y = Manual_Chl)) +
  geom_point(
    data = t_ref_comp_2,
    aes(x = MonthDayDate, y = Manual_Chl),
    inherit.aes = FALSE,
    color = "grey70",
    alpha = 0.4
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(~Year, ncol=1,scales = "free_y") +
  geom_vline(data = chla, aes(xintercept = disturbMonthDay2000), color = 'red') +
  geom_vline(data = chla, aes(xintercept = recoveryMonthDay2000), color = 'green4') +
  geom_rect(
    data = nutrient_addition_periods,
    aes(xmin = xmin_date, xmax = xmax_date, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "blue",
    alpha = 0.1
  ) +labs(x="Date", y = expression("Chlorophyll-a (" * mu * "g L"^{-1} * ")"))+
  theme_classic()



### UNCERTAINTY - add noise to test data. change 0.3 to 0.2 or 0.1
r_test_15 <- r_test %>% filter(Year == 2015)
set.seed(68)

chla <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "i", 'peak.z', 'peak.date')
for (i in 1:100){
  test1 <- r_test_15 %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  test <- test1 %>% mutate(Manual_Chl = Manual_Chl + pmax(runif(n(), -0.3 * Manual_Chl, 0.3 * Manual_Chl), -Manual_Chl))
  ref <-  r_ref_comp %>% na.omit()%>% arrange(Year, DOYtrunc)
  
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  print(i)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, i, test$peakz, test$peak.date))
}

chla$dist_length <- chla$test.recov.date - chla$test.dist.date
sqrt(mean((chla$dist_length - 41)^2))
sqrt(mean((chla$test.peakz - 28.892547)^2))

### UNCERTAINTY - THIN REFERENCE DATA WITH Peter Lake 2015 

r_test_15 <- dat %>% filter(Year == 2015)%>% na.omit()

set.seed(68)
chla <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(chla) <- c("dist.date", "recov.date", "i", 'peak.z', 'peak.date', 'mean')
for (i in 1:100){
  test <- r_test_15 %>% select(DOYtrunc, Manual_Chl) %>% na.omit()
  ref <- r_ref_comp %>%
    slice_sample(n = 100) %>%
    arrange(Year, DOYtrunc) 
  mean_ref <- ref %>% summarize(mean_value = mean(Manual_Chl))
  
  ref_all = data.frame(tt=unname(ref$DOYtrunc), yy=unname(ref$Manual_Chl))
  ref_all$tt <- as.integer(ref_all$tt)
  testy = data.frame(tt=unname(test$DOYtrunc), yy=unname(test$Manual_Chl))
  testint <-mwdistdiffz(testy,ref_all, 
                        wwidth=10, stride=1, ddiff_method = "integral")
  print(i)
  alarm<-disturbalarm(testint, dthres=2.5)
  test<-alarmfilter(alarm, 1)
  chla <- rbind(chla, data.frame(test$dist.date, test$recov.date, i, mean_ref, test$peakz, test$peak.date))
}



test_lakes <- read.csv("Monitored_Lakes_Chlorophyll.csv")

### CHLOROPHYLL FOR MONITORED LAKES

### comment out filter command in ref data depending on what method you want to use

chla <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(chla) <- c("Lake", "dist.date", "recov.date", "Year", "peak.z", "peak.date")

for (lake_name in unique(test_lakes$Lake)) {
  
  lake_data <- test_lakes %>% filter(Lake == lake_name)

  for (y in unique(lake_data$Year)) {
    
    test <- lake_data %>%
      filter(Year == y) %>%
      select(DOY, Chlorophyll) %>%
      na.omit()

    if (nrow(test) < 20) {
      message(paste("Skipping Lake:", lake_name, "Year:", y, "- less than 20 obs"))
      next
    }
    
    ref <- lake_data %>%
      filter(Year != y) %>%
      mutate(zscore = as.numeric(scale(Chlorophyll))) %>%
      #filter(zscore < 2) %>%
      arrange(Year, DOY) %>%
      select(DOY, Chlorophyll) %>%
      na.omit()
    
    ref_all <- data.frame(tt = unname(ref$DOY), yy = unname(ref$Chlorophyll))
    testy <- data.frame(tt = unname(test$DOY), yy = unname(test$Chlorophyll))
    
    testint <- mwdistdiffz(testy, ref_all, wwidth = 10, stride = 1, ddiff_method = "integral")
    print(paste("Lake:", lake_name, "Year:", y))
    
    alarm <- disturbalarm(testint, dthres = 2.5)
    test_filtered <- alarmfilter(alarm, 1)

    chla <- rbind(
      chla,
      data.frame(
        Lake = lake_name,
        dist.date = test_filtered$dist.date,
        recov.date = test_filtered$recov.date,
        Year = y,
        peak.z = test_filtered$peakz,
        peak.date = test_filtered$peak.date
      )
    )
  }
}



### PHYCOCYANIN FOR MONITORED LAKES

test_lakes <- read.csv("Monitored_Lakes_Phycocyanin.csv")
phyco <- NULL
for (lake_name in unique(test_lakes$Lake)) {
  
  lake_data <- test_lakes %>% filter(Lake == lake_name)

  for (y in unique(lake_data$Year)) {
    
    test <- lake_data %>%
      filter(Year == y) %>%
      select(DOY, Phycocyanin) %>%
      na.omit()
    
    if (nrow(test) < 20) {
      message(paste("Skipping Lake:", lake_name, "Year:", y, "- less than 20 obs"))
      next
    }
    
    ref <- lake_data %>%
      filter(Year != y) %>%
      mutate(zscore = as.numeric(scale(Phycocyanin))) %>%
      #filter(zscore < 2) %>%
      arrange(Year, DOY) %>%
      select(DOY, Phycocyanin) %>%
      na.omit()
    
    ref_all <- data.frame(tt = unname(ref$DOY), yy = unname(ref$Phycocyanin))
    testy <- data.frame(tt = unname(test$DOY), yy = unname(test$Phycocyanin))
    
    testint <- mwdistdiffz(testy, ref_all, wwidth = 10, stride = 1, ddiff_method = "integral")
    print(paste("Lake:", lake_name, "Year:", y))
    
    alarm <- disturbalarm(testint, dthres = 2.5)
    test_filtered <- alarmfilter(alarm, 1)

    phyco <- bind_rows(
      phyco,
      data.frame(
        Lake = lake_name,
        dist.date = test_filtered$dist.date,
        recov.date = test_filtered$recov.date,
        Year = y,
        peak.z = test_filtered$peakz,
        peak.date = test_filtered$peak.date
      )
    )
  }
  
}




final <- final %>%
  mutate(Lake = gsub(" ", "\n", Lake, fixed = TRUE))

final <- final %>%
  mutate(Lake = case_when(
    Lake == "Erie45165" ~ "Erie (Station 45165)",
    Lake == "LittleRock" ~ "Little Rock",
    Lake == "PrairieLake" ~ "Prairie Lake",
    Lake == "PrairiePothole" ~ "Prairie Pothole",
    Lake == "Oswego" ~ "Ontario (Oswego Buoy)",
    Lake == "Tempe" ~ "Tempe Town",
    TRUE ~ Lake  
  ))


max_vals <- final %>%
  group_by(Lake) %>%
  summarise(max_peak = max(peak.z, na.rm = TRUE))

p1 <- ggplot(final, aes(x = Lake, y = peak.z)) +
  geom_boxplot() +
  geom_point(data = max_vals %>% filter(max_peak > 65),
             aes(x = Lake, y = 65),
             shape = 8,        
             color = "red",   
             size = 4) +      
  theme_classic() +
  labs(x = NULL, y = "Bloom Magnitude (z-score)") +
  scale_y_continuous(limits = c(0, 65))

max_vals_length <- final %>%
  group_by(Lake) %>%
  summarise(max_length = max(dist_length, na.rm = TRUE))

p2 <- ggplot(final, aes(x = Lake, y = dist_length)) +
  geom_boxplot() +
  geom_point(data = max_vals_length %>% filter(max_length > 120),
             aes(x = Lake, y = 120),
             shape = 8,       
             color = "red",   
             size = 4) +     
  theme_classic() +
  labs(x = "Lake", y = "Bloom Duration (days)") +
  scale_y_continuous(limits = c(0, 120))


figure_6 <- p1 / p2


final$Trophic_Status <- factor(final$Trophic_Status,
                               levels = c("Oligotrophic", "Mesotrophic", "Eutrophic"))


figure_7 <- ggplot(final, aes(x = peak.z,
                         y = dist_length,
                         color = Trophic_Status)) +
  geom_point(size = 3) +             
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Oligotrophic" = "blue",
      "Mesotrophic"  = "brown",
      "Eutrophic"    = "green"
    )
  ) +
  labs(
    x = "Bloom Magnitude (z-score)",
    y = "Bloom Duration (days)",
    color = "Trophic Status"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14),
    panel.grid = element_blank(),
    legend.position = c(0.95, 0.05),       
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "black") 
  )



