library(ggplot2)
library(tidyverse)
library(usmap)
library(janitor)
library(ggeasy)
library(scales)
library(patchwork)
library(maps)
library(dplyr)

jail_rate_county_state <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

data_time_frame <- jail_rate_county_state %>%
  filter(year >= 1963) %>%
  group_by(state) %>%
  summarise(native_jail_pop_rate = mean(native_jail_pop_rate, na.rm = TRUE))

data_time_frame <- data_time_frame %>% clean_names()

heat_map <- plot_usmap(data = data_time_frame, values = "native_jail_pop_rate", color = NA) +
  scale_fill_continuous(low = "yellow", high = "red", name = "Incarceration Rate", na.value = "grey50") +
  easy_move_legend(to = c("right")) + 
  labs(title = "Native American Incarceration Rates by State", caption = "Data Source: Vera") +
  theme(panel.background = element_rect(colour = "white"))
