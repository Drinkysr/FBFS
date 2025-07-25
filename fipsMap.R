## FIPS map
library(tigris)
library(dplyr)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(plotly)


# Your list of FIPS codes (example)
target_fips <- read.table("fips.txt", header = TRUE)$FIPS  # Replace with your actual codes
target_states <- c("MN","IA","SD","NE","KS","NM","AZ","UT","ND","WI","OK","WY","MT", "ID","CO")

options(tigris_use_cache = TRUE)
counties_sf <- counties(cb = TRUE, year = 2020, class = "sf")

# Get state shapefiles (for borders)
states_sf <- states(cb = TRUE, year = 2020, class = "sf")

# Exclude Alaska (FIPS state = 02), Hawaii (15), Puerto Rico (72)
counties_sf <- counties_sf %>%
     filter(!STATEFP %in% c("02", "15", "72"))

# Flag selected counties
counties_sf <- counties_sf %>%
     mutate(is_target = ifelse(GEOID %in% target_fips, "High Income", ifelse(STUSPS %in% target_states, "FB State", "Other")))

# Plot
ggplot(counties_sf) +
     geom_sf(aes(fill = is_target), color = "white", size = 0.1) +
     scale_fill_manual(values = c("High Income" = "blue", "FB State" = "gray", "Other" = "gray90")) +
     geom_sf(data = states_sf, fill = NA, color = "black", size = 0.5) +  # State borders
     coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +  # Crop to continental US
     theme_minimal() +
     labs(title = "Higher Income Counties in Farm Bureau States",
          fill = "")

library(readxl)
#read data from the excel file on the SixGroupHighIncome tab
d <- read_excel("FBFS_COD_Research.xlsx", sheet = "SixGroupHighIncome")
d <- d %>% 
     select(Year, Sex, `Ten-Year Age Groups Code`, Deaths:MR_2019) %>% 
     filter(!is.na(Year)) %>% 
     mutate(Year = case_when(Year == "2024 (provisional)" ~ 2024,
                             Year == "2025 (provisional and partial)" ~ 2025,
                             TRUE ~ as.numeric(Year)),
            MR_2019 = round(MR_2019,2)) %>% 
     filter(Year <= 2024)
names(d) <- c("year","sex","age","deaths","population","crude_rate","q_2019", "MR")

## chart of crude rates by year, with lines colored by age group and sex in panels
p1 <- ggplot(d, aes(x = year, y = crude_rate, color = age)) +
     geom_line(lwd = 1) +
     facet_wrap(~sex) +
     labs(title = NULL,
          x = "Year",
          y = "Crude Rate") +
     theme_minimal() +
     scale_color_brewer(palette = "PuRd") +
     #scale_y_log10() +
     # enlarge and darken the text
     theme(text = element_text(size = 16, color = "black"),
           legend.position = "bottom") +
     labs(color = "Age Group", x = NULL)
ggplotly(p1) %>%
     plotly::layout(legend=list(x=0, y=-0.2,
                                xanchor='left',
                                yanchor='bottom',
                                orientation='h'))

## Same plot but use the mr_2019 column for rate ratios
dk_blues <- brewer.pal(name="Blues",n=9)[4:9]
p2 <- ggplot(d, aes(x = year, y = MR, color = age)) +
     geom_line(lwd = 1) +
     facet_wrap(~sex) +
     labs(title = "Mortality Rate Ratios vs. 2019 by Year and Age Group",
          x = "Year",
          y = "Rate Ratio (vs. 2019)") +
     theme_minimal() +
     scale_color_manual(values = dk_blues) +
     # scale_y_log10() +
     # enlarge and darken the text
     theme(text = element_text(size = 16, color = "black"),,
           legend.position = "bottom") +
     labs(color = "Age Group", 
          title = NULL, x = NULL)
ggplotly(p2) %>%
     plotly::layout(legend=list(x=0, y=-0.2,
                                xanchor='left',
                                yanchor='bottom',
                                orientation='h'))

## By cause
c <- read_excel("FBFS_COD_Research.xlsx", sheet = "provisionalData")
names(c) <- c("notes","year","yr","sex","sex_c","ucd_cause", "cause_code", "deaths", "population", "crude_rate", "rate", "q_2019", "MR", "EDR")

c <- c %>%
     filter(yr <= 2024,
            deaths >= 50) %>% 
     select(yr, sex, ucd_cause, cause_code, deaths, rate, q_2019, MR, EDR)

#find which cause codes are in all of the years
causes <- c %>%
     group_by(cause_code) %>%
     filter(n() == 14) %>%
     ungroup() %>%
     select(cause_code) %>%
     distinct()

c <- c %>% filter(cause_code %in% causes$cause_code)
## change ucd_cause variable to include everything up to the first parenthesis
c$ucd_cause <- sub("\\(.*", "", c$ucd_cause)
c$MR <- round(c$MR, 2)
c$EDR <- round(c$EDR, 1)

c <- c %>% 
     mutate(ucd_cause = ifelse(substr(ucd_cause, 1, 3) == "Acc", "Accidental Poisoning", ucd_cause))
# Plot EDR of each cause by year with sex as panel variable
p3 <- ggplot(c, aes(x = yr, y = EDR)) +
     geom_line(aes(color = sex)) +
     facet_wrap(~ucd_cause) +
     labs(title = "Excess Death Rate by Cause of Death",
          x = "Year",
          y = "Excess Death Rate") +
     theme_minimal() +
     scale_color_brewer(palette = "Set1") +
     #scale_y_log10() +
     # enlarge and darken the text
     theme(legend.position = "none") +
     labs(color = "Cause of Death", x = NULL)
ggplotly(p3)

p4 <- ggplot(c, aes(x = yr, y = MR)) +
     geom_line(aes(color = sex)) +
     facet_wrap(~ucd_cause) +
     labs(title = "Mortality Ratio by Cause of Death",
          x = "Year",
          y = "Excess Death Rate") +
     theme_minimal() +
     scale_color_brewer(palette = "Set1") +
     #scale_y_log10() +
     # enlarge and darken the text
     theme(legend.position = "none") +
     labs(color = "Cause of Death", x = NULL)
ggplotly(p4)
