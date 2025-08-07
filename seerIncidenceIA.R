### SEER incidence study of Iowa cancer rates vs. other SEER sites

library(tidyverse)

ia <- read_csv("CAIncidenceIAvsOthers1975up.csv") 
names(ia) <- c("year", "ia", "cancer", "rate", "count", "pop")

ia <- ia %>% 
     filter(year != "1975-2021")
ia$year <- as.numeric(ia$year)

ia %>% 
     ggplot(aes(x = year, y = rate, color = ia)) +
     geom_line(alpha = 0.5) +
     scale_color_brewer(palette = "Set1") +
     geom_smooth(method = "loess") +
     facet_wrap(~cancer, scales = "free_y") +
     theme_minimal()


ia2 <- read_csv("CAIncidenceIAvsOthers1975upv2.csv") 
names(ia2) <- c("year", "ia", "cancer", "rate", "count", "pop")

ia2 <- ia2 %>% 
     filter(year != "1975-2021")
ia2$year <- as.numeric(ia2$year)

ia2 %>% 
     ggplot(aes(x = year, y = rate, color = ia)) +
     geom_line(alpha = 0.5) +
     scale_color_brewer(palette = "Set1") +
     geom_smooth(method = "loess") +
     facet_wrap(~cancer, scales = "free_y") +
     theme_minimal()


ia3 <- read_csv("CAIncidenceIAvsOthers1975upv3.csv") 
names(ia3) <- c("year", "ia", "cancer", "rate", "count", "pop")

ia3 <- ia3 %>% 
     filter(year != "1975-2021")
ia3$year <- as.numeric(ia3$year)

ia3 %>% 
     ggplot(aes(x = year, y = rate, color = ia)) +
     geom_line(alpha = 0.5) +
     scale_color_brewer(palette = "Set1") +
     geom_smooth(method = "loess") +
     facet_wrap(~cancer, scales = "free_y") +
     theme_minimal()

ia3 %>% 
     filter(cancer %in% c("Bladder and Kidney", "Brain and Nerves", "Breast",
                          "Colorectal", "Esophagus and Stomach", "Head and Neck",
                          "Liver and IHBD", "Lung and Bronchus", "Melanoma of the Skin")) %>% 
     ggplot(aes(x = year, y = rate, color = ia)) +
     geom_line(alpha = 0.5) +
     scale_color_brewer(palette = "Set1") +
     geom_smooth(method = "loess") +
     facet_wrap(~cancer, scales = "free_y") +
     theme_minimal()
