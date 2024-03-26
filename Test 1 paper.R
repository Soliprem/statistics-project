# gun death dataset WIP (summing values by year and state)
gun_deaths_us_1999_2019 <- as.data.frame("gun_deaths_us_1999_2019")
gundeaths_cut <- gun_deaths_us_1999_2019[-c(1, 3:5, 7)]
gundeaths_cut <- subset(gundeaths_cut, !gundeaths_cut$Year > 2017)
gundeaths_cut <- gundeaths_cut[-c(5:10)]


colSums(gundeaths_cut[, 3:4])
v_state <- gundeaths_cut$State_Name == "Alabama"
v_year <- gundeaths_cut$Year == 1999
v_filter <- v_state & v_year
v_check <- (v_filter)
spec_sum <- function(selected_year, selected_state) {
  colSums(gundeaths_cut[, 3:4])
}
View(gundeaths_cut)


# law provision dataset usable!

law_provision_norm <- as.data.frame(law_provision_norm)
law_provision_norm <- subset(law_provision_norm, !law_provision_norm$year < 1999)
library(caret)
proc_lawprov <- preProcess(as.data.frame(law_provision_norm$lawtotal),
  method = c("range")
)
law_provision_norm$index <- predict(proc_lawprov, as.data.frame(law_provision_norm$lawtotal))
View(law_provision_norm)

# merging
