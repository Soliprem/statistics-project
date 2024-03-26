library(caret)
library(dplyr)

# gun death dataset 
gun_deaths_us_1999_2019 <- read.csv("../../data/gun_deaths_us_1999_2019.csv")
gun_deaths_us_1999_2019 <- as.data.frame(gun_deaths_us_1999_2019)
gundeaths_cut <- gun_deaths_us_1999_2019[-c(1, 3:5, 7)]
gundeaths_cut <- subset(gundeaths_cut, !gundeaths_cut$Year > 2017)
gundeaths_cut <- gundeaths_cut[-c(5:10)]


merged_guns <- data.frame(50, 50)
deaths <- 0
merged_guns[, deaths] <- 0
merged_guns[, population] <- 0
for (i in nrow(gundeaths_cut)) {
  state <- gundeaths_cut$State_Name[i]
  year <- gundeaths_cut$Year[i]
  county_deaths <- gundeaths_cut[i, "Deaths"]
  county_population <- gundeaths_cut[i, "Population"]
  if (state %in% merged_guns$state) {
    merged_guns$deaths[state] <- merged_guns$deaths[state] + gundeaths_cut$Deaths[i]
    merged_guns$population[state] <- merged_guns$population[state] + gundeaths_cut$Population[i]
  }
}


# law provision dataset 

law_provision_norm <- as.data.frame(law_provision_norm)
law_provision_norm <- subset(law_provision_norm, !law_provision_norm$year < 1999)
proc_lawprov <- preProcess(as.data.frame(law_provision_norm$lawtotal),
    method = c("range")
)
law_provision_norm$index <- predict(proc_lawprov, as.data.frame(law_provision_norm$lawtotal))
View(law_provision_norm)

# merging

