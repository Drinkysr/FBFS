### Examining colon and rectal and stomach cancer rates by SEER registry
## in answer to questions about nitrates in water supplies

library(tidyverse)
library(readxl)

# Load the data
ca <- read_csv("stomachColonIncidence.csv")

names(ca) <- c("ageGroup", "yrDx", "state", "type", "rate", "count", "pop")

ca <- ca %>% 
     filter(state != "Remaining values",
            type != "Remaining values") %>% 
     mutate(yrNum = as.numeric(substr(yrDx, 1, 4)),
            rate = as.numeric(rate),
            count = as.numeric(count),
            ia = state == "Iowa")

## Create a line plot for rates over time, with facets for each age group
## colored by state

ca %>% 
  filter(type == "Stomach") %>%
  ggplot(aes(x = yrNum, y = rate, color = state, group = state)) +
  geom_line(lwd = 1.5, aes(lty = ia)) +
     scale_color_brewer(palette = "Blues") +
  facet_wrap(~ ageGroup, scales = "free") +
  labs(title = "Stomach Cancer Rates by SEER Registry",
       x = "Year of Diagnosis",
       y = "Cancer Rate per 100,000") +
  theme_dark() +
  theme(legend.position = "bottom")

ca %>% 
  filter(type == "Colorectal") %>%
  ggplot(aes(x = yrNum, y = rate, color = state, group = state)) +
  geom_line(lwd = 1.5, aes(lty = ia)) +
     scale_color_brewer(palette = "Blues") +
  facet_wrap(~ ageGroup, scales = "free") +
  labs(title = "Colon and Rectal Cancer Rates by SEER Registry",
       x = "Year of Diagnosis",
       y = "Cancer Rate per 100,000") +
  theme_dark() +
  theme(legend.position = "bottom")

## CDC wonder data
stom <- read_csv("StomachCAFBFS.csv") %>% 
     filter(is.na(Notes))
names(stom) <- c("notes", "state", "state_x", "ageGrp_x", "ageGrp", "year_x", "year", "deaths", "pop", "rate")
stom <- stom %>% 
     mutate(year = as.numeric(year),
            pop = as.numeric(pop),
            deaths = as.numeric(deaths),
          rate = deaths / pop * 100000) %>% 
     filter(ageGrp %in% c("45-54", "55-64", 
                     "65-74", "75-84", "85+")) %>%
     select(state, ageGrp, year, deaths, pop, rate) %>% 
     mutate(IA = state == "Iowa")

# get averarge rates across states, grouped by year and age group
stom_avg <- stom %>% 
     group_by(year, ageGrp) %>% 
     summarise(deaths = sum(deaths, na.rm = TRUE),
               pop = sum(pop, na.rm = TRUE)) %>% 
     mutate(rate = deaths/pop * 100000,
            state = "AVG",
            IA = TRUE) %>%
     ungroup() %>% 
     select(state, ageGrp, year, deaths, pop, rate, IA)

stom <- rbind(stom, stom_avg) %>% 
     mutate(avg = ifelse(state == "AVG", "Average", "Iowa"))

## Create a line plot for rates over time, with facets for each age group
## colored by state
stom %>% 
     filter(ageGrp != "45-54",
            state %in% c("Iowa", "AVG"),
            year != 2025) %>% 
  ggplot(aes(x = year, y = rate, color = avg, group = state)) +
  geom_line(lwd = 1) +
  facet_wrap(~ ageGrp) +
  scale_color_manual(values = c("gray", "darkblue")) +
  labs(title = "Stomach Cancer Death Rates by State",
       subtitle = "Iowa vs. Average of Other FB States",
       x = "Year of Diagnosis",
       y = "Death Rate per 100,000") +
  theme_dark() +
  theme(legend.position = "bottom")



## Colorectal
colr <- read_csv("ColorectalCAFBFS.csv") %>% 
     filter(is.na(Notes))

names(colr) <- c("notes", "state", "state_x", "ageGrp_x", "ageGrp", "year_x", "year", "deaths", "pop", "rate")
colr <- colr %>% 
     mutate(year = as.numeric(year),
            pop = as.numeric(pop),
            deaths = as.numeric(deaths),
          rate = deaths / pop * 100000) %>% 
     filter(ageGrp %in% c("45-54", "55-64", 
                     "65-74", "75-84", "85+")) %>%
     select(state, ageGrp, year, deaths, pop, rate) %>% 
     mutate(IA = state == "Iowa")
# get averarge rates across states, grouped by year and age group
colr_avg <- colr %>% 
     group_by(year, ageGrp) %>% 
     summarise(deaths = sum(deaths, na.rm = TRUE),
               pop = sum(pop, na.rm = TRUE)) %>% 
     mutate(rate = deaths/pop * 100000,
            state = "AVG",
            IA = TRUE) %>%
     ungroup() %>% 
     select(state, ageGrp, year, deaths, pop, rate, IA)

colr <- rbind(colr, colr_avg) %>%
     mutate(avg = ifelse(state == "AVG", "Average", "Iowa"))     

colr %>% 
     filter(ageGrp != "85+",
               state %in% c("Iowa", "AVG"),
            year != 2025) %>% 
  ggplot(aes(x = year, y = rate, color = avg, group = state)) +
  geom_line(lwd = 1) +
  facet_wrap(~ ageGrp) +
  scale_color_manual(values = c("gray", "darkblue")) +
  labs(title = "Colorectal Cancer Death Rates by State",
       subtitle = "Iowa vs. Average of Other FB States",
       x = "Year of Diagnosis",
       y = "Death Rate per 100,000") +
  theme_dark() +
  theme(legend.position = "bottom")

