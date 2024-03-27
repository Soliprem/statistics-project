# libraries used
library(caret)
library(dplyr)

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
  theme_bw()

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

v_meandr <- as.vector(0)
for(i in 1:19){
 v_meandr[i] <- mean(gundeaths_cond$Rate[gundeaths_cond$year == as.character(i + 1998)])
}

v_dsum <- as.vector(0)
for(i in 1:19){
  v_dsum[i] <- sum(gundeaths_cond$Deaths[gundeaths_cond$year == as.character(i + 1998)])/3100
}


df_dry <- data.frame(x = 1999:2017, y = v_meandr, z = v_dsum ) 
df_dry %>%
ggplot(aes(x,y)) +
  geom_point(size = v_dsum,aes(color = "brown"))+
  geom_smooth(method = lm, aes(color = "brown"))+
  geom_line(color = "red")+
  theme_bw()+
  theme(legend.position = "none") +
  labs(x = "Years",
       y = "Mean Death Rate",
       title = "Death Rate over time") 
 
    
    
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
  ggplot(aes(year, unlist(index), color = state))+
  geom_point()+
  facet_wrap(~state)+
  theme_bw() +
  theme(legend.position = "none")+
  labs(x = "Year",
       y = "Gun Restriction Index",
       title = "Gun restriction over time")
  
# ifelse(unlist(law_provision_norm$index) <0.25, labels = "Very Low", 
#        ifelse(unlist(law_provision_norm$index) <0.50, labels = "Low",
#               ifelse(unlist(law_provision_norm$index) <0.75, labels = "Medium",
#                      labels = "High"))
# )

#analysis