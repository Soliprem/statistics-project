#show link: underline
= Documentationo of the analysis work
We began by sourcing the following datasets:
- #link("https://www.kaggle.com/datasets/ahmedeltom/us-gun-deaths-by-county-19992019")[Deaths by County]
- #link("https://www.kaggle.com/datasets/jboysen/state-firearms?select=raw_data.csv")[Firearms Provisionsi in US States]

We then proceeded to reconfigure the data:
```r
# libraries used
library(caret)
library(dplyr)

# gun death dataset
gun_deaths_us_1999_2019 <- read.csv("../data/gun_deaths_us_1999_2019.csv")
gun_deaths_us_1999_2019 <- as.data.frame(gun_deaths_us_1999_2019)
gundeaths_cut <- gun_deaths_us_1999_2019[-c(1, 3:5, 7)]
gundeaths_cut <- subset(gundeaths_cut, !gundeaths_cut$Year > 2017)
gundeaths_cut <- gundeaths_cut[-c(5:10)]
View(gundeaths_cut)

gundeaths_cut$Year <- as.character(gundeaths_cut$Year)
gundeaths_cond <- gundeaths_cut %>%
    group_by(across(where(is.character))) %>%
    summarise(across(where(is.numeric), sum, na.rm = T), .groups = "drop")

gundeaths_cond$Rate <- 0
for (i in 1:nrow(gundeaths_cond)) {
    r <- ((gundeaths_cond$Deaths[i] / gundeaths_cond$Population[i]) * 100000)
    gundeaths_cond$Rate[i] <- r
}
View(gundeaths_cond)

# law provision dataset
law_provision_norm <- read.csv("law_provision_norm.csv")
law_provision_norm <- as.data.frame(law_provision_norm)
law_provision_norm <- subset(law_provision_norm, !law_provision_norm$year < 1999)
proc_lawprov <- preProcess(as.data.frame(law_provision_norm$lawtotal),
    method = c("range")
)
law_provision_norm$index <- predict(proc_lawprov, as.data.frame(law_provision_norm$lawtotal))
View(law_provision_norm)

# merging
gundeaths_cond <- gundeaths_cond %>%
    rename(
        "state" = "State_Name",
        "year" = "Year"
    )

merged <- merge(law_provision_norm, gundeaths_cond, by = c("state", "year"))
View(merged)
```
This code handles the pruning of the dates in the datasets, so that they both cover the same timeframe, and the merging of the county-by-county data into states-by-states data. The new dataset is thus an aggregation of the data of the two original sources by year and state, such that we obtain data formatted in the followign way:
#table(columns: (auto, auto, auto, auto, auto),
[*Year*], [*State_Name*],[*Deaths*],[*Population*], [*Rate*],
[1999], [Alabama], [605], [3047241], [19.854025],
[...], [...], [...], [...], [...])
