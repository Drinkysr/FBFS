## Alcohol and Liver disease deaths from CDC WONDER
library(readxl)
library(tidyverse)
library(plotly)

alc <- read_xlsx("AlcoholLiverDisease.xlsx", sheet = "AlcoholLiverDisease")
alc <- alc[1:147,] #remove notes

alc <- alc %>% 
     filter(is.na(Notes))

names(alc) <- tolower(make.names(names(alc)))

alc <- alc %>% 
     select(ten.year.age.groups.code, sex, year.code:crude.rate)
names(alc) <- c("age_group", "sex", "year", "deaths", "population", "rate")

alc <- alc %>% 
     filter(year <= 2024) %>% 
     mutate(rate = round(deaths/population * 100000, 2))

alc1 <- alc %>% 
     filter(age_group %in% c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")) %>%
     ggplot(aes(x = year, y = rate, color = age_group)) +
     geom_line(lwd = 1) +
     facet_wrap(~sex) +
     theme_dark() +
     theme(text = element_text(size = 16, color = "black")) +
     scale_color_brewer(palette = "PuRd") +
     #scale_y_log10() +
     labs(x = "Year", y = "Rate per 100,000", color = "Age Group")

ggplotly(alc1) %>%
     plotly::layout(legend=list(x=0, y=-0.2,
                                xanchor='left',
                                yanchor='bottom',
                                orientation='h'))
q2019 <- alc %>% 
     filter(year == 2019) %>% 
     select(sex, age_group, rate) %>% 
     rename(rate_2019 = rate)

alc <- left_join(alc, q2019)
alc <- alc %>% 
     mutate(mr_2019 = round(rate /rate_2019, 2)) 

alc2 <- alc %>% 
     filter(age_group %in% c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")) %>%
     ggplot(aes(x = year, y = mr_2019, color = age_group)) +
     geom_line(lwd = 1) +
     facet_wrap(~sex) +
     theme_dark() +
     theme(text = element_text(size = 16, color = "black")) +
     scale_color_brewer(palette = "Blues") +
     #scale_y_log10() +
     labs(x = "Year", y = "Mortality Ratio", color = "Age Group")

ggplotly(alc2) %>%
     plotly::layout(legend=list(x=0, y=-0.2,
                                xanchor='left',
                                yanchor='bottom',
                                orientation='h'))


## now for cirrhosis
cir <- read_xlsx("cirrhosis.xlsx", sheet = "Cirrhosis")


cir <- cir[1:138,] #remove notes

cir <- cir %>% 
     filter(is.na(Notes))

names(cir) <- tolower(make.names(names(cir)))

cir <- cir %>% 
     select(ten.year.age.groups.code, sex, year.code:crude.rate)
names(cir) <- c("age_group", "sex", "year", "deaths", "population", "rate")

cir <- cir %>% 
     filter(year <= 2024) %>% 
     mutate(rate = round(deaths/population * 100000, 2))

cir1 <- cir %>% 
     filter(age_group %in% c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")) %>%
     ggplot(aes(x = year, y = rate, color = age_group)) +
     geom_line(lwd = 1) +
     facet_wrap(~sex) +
     theme_dark() +
     theme(text = element_text(size = 16, color = "black")) +
     scale_color_brewer(palette = "PuRd") +
     #scale_y_log10() +
     labs(x = "Year", y = "Rate per 100,000", color = "Age Group")

ggplotly(cir1) %>%
     plotly::layout(legend=list(x=0, y=-0.2,
                                xanchor='left',
                                yanchor='bottom',
                                orientation='h'))
q2019 <- cir %>% 
     filter(year == 2019) %>% 
     select(sex, age_group, rate) %>% 
     rename(rate_2019 = rate)

cir <- left_join(cir, q2019)
cir <- cir %>% 
     mutate(mr_2019 = round(rate /rate_2019, 2)) 

cir2 <- cir %>% 
     filter(age_group %in% c("25-34", "35-44", "45-54", "55-64", "65-74", "75-84")) %>%
     ggplot(aes(x = year, y = mr_2019, color = age_group)) +
     geom_line(lwd = 1) +
     facet_wrap(~sex) +
     theme_dark() +
     theme(text = element_text(size = 16, color = "black")) +
     scale_color_brewer(palette = "Blues") +
     #scale_y_log10() +
     labs(x = "Year", y = "Mortality Ratio", color = "Age Group")

ggplotly(cir2) %>%
     plotly::layout(legend=list(x=0, y=-0.2,
                                xanchor='left',
                                yanchor='bottom',
                                orientation='h'))
