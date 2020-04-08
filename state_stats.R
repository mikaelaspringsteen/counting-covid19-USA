# COVID-19 stats tracker--USA version, state stats compilation
# Mikaela Springsteen, contactmspringsteen@gmail.com

# US Census data:
# https://www.census.gov/quickfacts/fact/table/US/PST045219

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# load data
USCensus <- read.csv("data/USCensus.csv", stringsAsFactors = FALSE)
supp <- read.csv("data/supplement.csv", stringsAsFactors = FALSE)

# flip rows/columns
statestats <- data.frame(t(USCensus[-1]))
names(statestats) <- USCensus[, 1]
statestats$State <- rownames(statestats)
suppstats <- data.frame(t(supp[-1]))
names(suppstats) <- supp[, 1]
suppstats$State <- rownames(suppstats)

# bind datasets
statestats <- bind_rows(statestats, suppstats)

# correct state name formatting
statestats$State <- gsub("\\.", " ", statestats$State)

# write csv
write.csv(statestats, "statestats.csv", row.names = FALSE)
rm(list=ls())
