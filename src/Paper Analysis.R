# libraries used
library(caret)
library(dplyr)
library(ggpubr)
# gun death dataset
gun_deaths_us_1999_2019 <- read.csv("gun_deaths_us_1999_2019.csv")
gun_deaths_us_1999_2019 <- as.data.frame(gun_deaths_us_1999_2019)
gundeaths_cut <- gun_deaths_us_1999_2019[-c(1, 3:5, 7)]
gundeaths_cut <- subset(gundeaths_cut, !gundeaths_cut$Year > 2017)
gundeaths_cut <- gundeaths_cut[-c(5:10)]
# View(gundeaths_cut)

gundeaths_cut$Year <- as.character(gundeaths_cut$Year)
gundeaths_cond <- gundeaths_cut %>%
    group_by(across(where(is.character))) %>%
    summarise(across(where(is.numeric), sum, na.rm = T), .groups = "drop")

gundeaths_cond$Rate <- 0
for (i in 1:nrow(gundeaths_cond)) {
    r <- ((gundeaths_cond$Deaths[i] / gundeaths_cond$Population[i]) * 100000)
    gundeaths_cond$Rate[i] <- r
}

#north dakota in 2013 reported a 70% decrease in populatio, so, being higly
 #unlikely we ruled it out
gundeaths_cond <- gundeaths_cond[-717,]
# View(gundeaths_cond)

# law provision dataset
law_provision_norm <- read.csv("law_provision_norm.csv")
law_provision_norm <- as.data.frame(law_provision_norm)
law_provision_norm <- subset(law_provision_norm, !law_provision_norm$year < 1999)
proc_lawprov <- preProcess(as.data.frame(law_provision_norm$lawtotal),
    method = c("range")
)
law_provision_norm$index <- predict(proc_lawprov, as.data.frame(law_provision_norm$lawtotal))
# View(law_provision_norm)

# merging
gundeaths_cond <- gundeaths_cond %>%
    rename(
        "state" = "State_Name",
        "year" = "Year"
    )

merged <- merge(law_provision_norm, gundeaths_cond, by = c("state", "year"))
# View(merged)

# dataset description, descriptive statistics and relevant plots
#gun deaths dataset description
mean_gd <- mean(gundeaths_cond$Deaths)
sd_gd <- sd(gundeaths_cond$Deaths)

mean_rate <- mean(gundeaths_cond$Rate)
sd_rate <- sd(gundeaths_cond$Rate)

gundeaths_cond %>%
  ggplot(aes(Deaths, Population/100000, color = state))+
  geom_point( alpha = 0.5 ) + 
  theme_bw() + 
  labs(x = "Deaths",
       y = "Population (in hundred thousands)",
       title = "Deaths and Population") 
 

gundeaths_cond %>%
  ggplot(aes(Deaths, Population/100000, color = state))+
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  labs(x = "Deaths",
       y = "Population (in hundred thousands)",
       title = "Deaths and Population") +
  facet_wrap(~year) +
  theme(legend.position = "none")


gundeaths_cond %>%
  ggplot(aes(Deaths), color = state)+ 
  geom_histogram(fill = "brown")  + 
  labs(x = "Deaths",
       y = NULL,
       title = "Histogram of Deaths") + 
  theme_bw()+
  theme(axis.ticks.y = element_blank())

gundeaths_cond %>%
  ggplot(aes( year, Rate, color = state))+
  geom_point() + 
  facet_wrap(~state) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = "Year",
       y = "Death Rate (per 100,000)",
       title = "Death rate over time")

#law provision dataset
mean_lt <- mean(law_provision_norm$lawtotal)
sd_lt <- sd(law_provision_norm$lawtotal)

mean_index <- mean(unlist(law_provision_norm$index))
sd_index <- sd(unlist(law_provision_norm$index))


law_provision_norm %>% 
  ggplot(aes(unlist(index), state, color = year))+
  geom_point()+
  geom_vline(xintercept = mean_index)+
  theme_bw()+ 
  labs(x = "Gun Regulation Index",
       y = "State",
       title = "Gun Regulation Index in states")

law_provision_norm %>%
  ggplot(aes(year, unlist(index), color = year))+
  geom_point()+
  facet_wrap(~state)+
  theme_bw() +
  theme(legend.position = "none")+
  labs(x = "Year",
       y = "Gun Restriction Index",
       title = "Gun restriction over time")

merged$Score <- ifelse(unlist(merged$index) <0.25, "Very Low",
                       ifelse(unlist(merged$index) <0.5, "Low",
                              ifelse(unlist(merged$index) <0.75, "Medium",
                                     "High")))

merged %>%
  ggplot(aes(Score)) +
  geom_bar(fill = "purple")+
  coord_flip()+
  theme_bw()+
  labs(x = "Index score",
       y = "State count (all time)",
       title = "Index score count" )


#analysis

v_meandr <- as.vector(0)
for(i in 1:19){
  v_meandr[i] <- mean(gundeaths_cond$Rate[gundeaths_cond$year == as.character(i + 1998)])
}

v_dsum <- as.vector(0)
for(i in 1:19){
  v_dsum[i] <- sum(gundeaths_cond$Deaths[gundeaths_cond$year == as.character(i + 1998)])/3100
}


df_dry <- data.frame(x = 1999:2017, y = v_meandr, z = v_dsum ) 

plotA <- df_dry %>%
  ggplot(aes(x,y)) +
  geom_point(size = v_dsum,aes(color = "brown"))+
  geom_smooth(method = lm, aes(color = "brown"))+
  geom_line(color = "red")+
  theme_bw()+
  theme(legend.position = "none") +
  labs(x = "Years",
       y = "Mean Death Rate",
       title = "Death Rate over time") 


v_meanindex <- as.vector(0)
for(i in 1:19){
  v_meanindex[i] <- mean(unlist(law_provision_norm$index)[law_provision_norm$year == (i+1998) ], na.rm = T)
}

df_wet <- data.frame(x = 1999:2017, y = v_meanindex ) 
plotB <-  df_wet %>%
  ggplot(aes(x,y))+
           geom_point(size = 5,aes(color = "green"))+
                        geom_smooth(method = lm, aes(color = "green"))+
                        geom_line(aes(color = "grey"))+
                        theme_bw()+
                        theme(legend.position = "none")+
                        labs(x = "Years",
                             y = "Mean Gun Regulation Index",
                             title = "Mean Gun Regulation index over time")

ggarrange(plotA, plotB, nrow = 1, ncol = 2)

cor(x = unlist(merged$index), y = merged$Rate,  method = "pearson", use = "complete.obs") #total corr
cor(x = unlist(merged$index), y = merged$year, method = "pearson", use = "complete.obs")  #years and Index corr

merged %>%
  ggplot(aes(unlist(index),Rate, color = year ))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = "Gun Regulation Index",
       y = "Gun Death Rate",
       title = "Gun death vs Index",
       subtitle = "All states" )

verylow_p <- merged %>%
  filter(Score == "Very Low") %>%
  ggplot(aes(unlist(index),Rate, color = year ))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = "Gun Regulation Index",
       y = "Gun Death Rate",
       subtitle = "Very Low Score states" )+
  theme(legend.position = "none")

low_p <- merged %>%
  filter(Score == "Low") %>%
  ggplot(aes(unlist(index),Rate, color = year ))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = "Gun Regulation Index",
       y = "Gun Death Rate",
       subtitle = "Low Score states" )+
  theme(legend.position = "none")

medium_p <- merged %>%
  filter(Score == "Medium") %>%
  ggplot(aes(unlist(index),Rate, color = year ))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = "Gun Regulation Index",
       y = "Gun Death Rate",
       subtitle = "Medium Score states" )+
  theme(legend.position = "none")

high_p <- merged %>%
  filter(Score == "High") %>%
  ggplot(aes(unlist(index),Rate, color = year ))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = "Gun Regulation Index",
       y = "Gun Death Rate",
       subtitle = "High Score states" )+
  theme(legend.position = "none")

ggarrange(verylow_p, low_p, medium_p, high_p, nrow = 2, ncol = 2)

verylow_df <- merged[merged$Score == "Very Low",]
  cor(unlist(verylow_df$index),verylow_df$Rate, method = "pearson", use = "complete.obs") #very low states correlation
  
low_df <- merged[merged$Score == "Low",]
  cor(unlist(low_df$index),low_df$Rate, method = "pearson", use = "complete.obs") #low states correlation

medium_df <- merged[merged$Score == "Medium",]
  cor(unlist(medium_df$index),medium_df$Rate, method = "pearson", use = "complete.obs")#medium states correlation
  
high_df <- merged[merged$Score == "High",]
  cor(unlist(high_df$index),high_df$Rate, method = "pearson", use = "complete.obs")#high states correlation
  
gdgr_lm <- lm(Rate ~unlist(index), data = merged )
summary(gdgr_lm)
  