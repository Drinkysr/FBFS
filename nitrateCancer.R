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
