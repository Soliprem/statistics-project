gun_deaths_us_1999_2019 <- as.data.frame(gun_deaths_us_1999_2019)
gundeaths_cut <- gun_deaths_us_1999_2019[ -c(1,3:5,7)]
gundeaths_cut2 <- subset(gundeaths_cut, !gundeaths_cut$Year > 2017)
View(gundeaths_cut2)
