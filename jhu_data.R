# COVID-19 stats tracker--USA version, data cleaning script
# Mikaela Springsteen, contactmspringsteen@gmail.com

# COVID-19 data from Johns Hopkins University:
# https://github.com/CSSEGISandData/COVID-19

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# load data
# JHU
total <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
# The Atlantic (The COVID Tracking Project)
atl <- as.data.frame(data.table::fread("https://covidtracking.com/api/v1/states/daily.csv"))
# state / country stats
statestats <- read.csv("statestats.csv")

# JHU data

# remove Population column from deaths
deaths <- select(deaths, -Population)

# 0 observations to NA
covid_cases_usa <- list(total, deaths)
covid_cases_usa <- lapply(covid_cases_usa, function(df) {
  df[ , c(12:ncol(df))][df[ , c(12:ncol(df))] == 0] <- NA
  df
})

# reshape
covid_cases_usa[[1]] <- gather(covid_cases_usa[[1]], Date, Total, -c(1:11))
covid_cases_usa[[2]] <- gather(covid_cases_usa[[2]], Date, Deaths, -c(1:11))

# select, rename variables
names(covid_cases_usa[[1]])[7] <- "State"
names(covid_cases_usa[[2]])[7] <- "State"
covid_cases_usa[[1]] <- select(covid_cases_usa[[1]], State, Date, Total)
covid_cases_usa[[2]] <- select(covid_cases_usa[[2]], State, Date, Deaths)

# format variables
covid_cases_usa <- lapply(covid_cases_usa, function(df) {
  df$Date <- as.Date(df$Date, "%m/%d/%y")
  df
})

# summarize JHU data by state
covid_cases_usa[[1]] <- aggregate(data = covid_cases_usa[[1]], Total ~ State + Date, sum, drop = FALSE)
covid_cases_usa[[2]] <- aggregate(data = covid_cases_usa[[2]], Deaths ~ State + Date, sum, drop = FALSE)

# keep complete cases
covid_cases_usa <- lapply(covid_cases_usa, function(df) {
  df <- df[complete.cases(df), ]
  df
})

# merge covid_cases_usa
covid_cases_usa <- covid_cases_usa %>% reduce(left_join, by = c("Date", "State"))

# add DayCount variable
covid_cases_usa <- covid_cases_usa %>% group_by(State) %>% mutate(DayCount = row_number())

# add Day variable (day 1 = the first day a state has at least 50 cases)
Day_dat <- covid_cases_usa %>% group_by(State) %>% filter(Total >= 50) %>% mutate(Day = row_number())
covid_cases_usa <- merge(covid_cases_usa, Day_dat, all = TRUE)

# add NewCases variable
covid_cases_usa <- covid_cases_usa %>% group_by(State) %>% mutate(NewCases = Total - lag(Total, default = first(Total)))

# add NewDeaths variable
covid_cases_usa <- covid_cases_usa %>% group_by(State) %>% mutate(NewDeaths = Deaths - lag(Deaths, default = first(Deaths)))

# merge with statestats
covid_cases_usa <- merge(covid_cases_usa, statestats, by = c("State"), all = TRUE)

# add Totalper100_000 variable
covid_cases_usa$Totalper100_000 <- (covid_cases_usa$Total/covid_cases_usa$Population)*100000

# add TotalRate variable
covid_cases_usa$TotalRate <- covid_cases_usa$Total/covid_cases_usa$Population

# add DeathRate variable
covid_cases_usa$DeathRate <- covid_cases_usa$Deaths/covid_cases_usa$Total

# add Deathsper100_000 variable
covid_cases_usa$Deathsper100_000 <- (covid_cases_usa$Deaths/covid_cases_usa$Population)*100000

# add Population_hundthou variable
covid_cases_usa$Population_hundthou <- (covid_cases_usa$Population)/100000

# The Atlantic (The COVID Tracking Project) data

# format variables
atl$date <- as.Date(as.character(atl$date), "%Y%m%d")

# state abbreviations to full names
statebyabbr <- tibble(State = state.name) %>%
  bind_cols(tibble(state = state.abb)) %>% 
  bind_rows(tibble(State = c("District of Columbia", "Guam", "American Samoa", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"), state = c("DC", "GU", "AS", "MP", "PR", "VI")))
atl <- left_join(atl, statebyabbr, by = "state")

# select variables
atl <- select(atl, date, State, totalTestResults, negative, positive, recovered, death, hospitalizedCurrently, hospitalizedCumulative, inIcuCurrently, inIcuCumulative, onVentilatorCurrently, onVentilatorCumulative)

# rename variables
names(atl) <- c("Date", "State", "atlTests", "atlNegativeTests", "atlCases", "atlRecoveries", "atlDeaths", "atlCurrentlyHospitalized", "atlHospitalized", "atlCurrentlyInICU", "atlInICU", "atlCurrentlyOnVentilator", "atlOnVentilator")

# 0 observations of Tests to NA
atl[ , c(3:13)][atl[ , c(3:13)] == 0] <- NA

# remove obs where testing had not yet occured
atl <- atl[complete.cases(atl[ , 3]),]

# sort by date
atl <- atl[order(atl$Date),]

# add DayCount variable (days from first case)
atl <- atl %>% group_by(State) %>% mutate(atlDayCount = row_number())

# add Day variable (day 1 = the first day a state has at least 50 cases)
Day_dat <- atl %>% group_by(State) %>% filter(atlCases >= 50) %>% mutate(atlDay = row_number())
atl <- merge(atl, Day_dat, all = TRUE) 

# merge atl and covid_cases_usa
covid_cases_usa <- merge(covid_cases_usa, atl, by = c("State", "Date"), all = TRUE)

# add atlTestsper100_000 variable
covid_cases_usa$atlTestsper100_000 <- (covid_cases_usa$atlTests/covid_cases_usa$Population)*100000

# add atlCasesper100_000 variable
covid_cases_usa$atlCasesper100_000 <- (covid_cases_usa$atlCases/covid_cases_usa$Population)*100000

# remove cruise ships
covid_cases_usa <- filter(covid_cases_usa, State != "Diamond Princess")
covid_cases_usa <- filter(covid_cases_usa, State != "Grand Princess")

# restructuring for app
covid_cases_usa <- covid_cases_usa %>% drop_na(atlDay)
covid_cases_usa <- select(covid_cases_usa, -DayCount, -NewCases, -NewDeaths, -atlNegativeTests, -atlRecoveries, -atlDayCount)

# write csv
write.csv(covid_cases_usa, "covid_cases_usa.csv", row.names = FALSE)
rm(list=ls())

