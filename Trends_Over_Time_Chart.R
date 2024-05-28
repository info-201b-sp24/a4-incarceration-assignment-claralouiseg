library(ggplot2)
library(dplyr)

jail_rate_county_state <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

WA_rate <- jail_rate_county_state %>% filter(state == "WA" & year >= 1990)

WA_white_jail_rate <- WA_rate %>%
  group_by(year) %>%
  summarise(rate = mean(white_jail_pop_rate, na.rm = TRUE)) %>%
  mutate(state = 'Washington', race = 'White')

WA_native_jail_rate <- WA_rate %>%
  group_by(year) %>%
  summarise(rate = mean(native_jail_pop_rate, na.rm = TRUE)) %>%
  mutate(state = 'Washington', race = 'Native American')

time_frame <- jail_rate_county_state %>% filter(year >= 1990)

US_white_jail_rate <- time_frame %>%
  group_by(year) %>%
  summarise(rate = mean(white_jail_pop_rate, na.rm = TRUE)) %>%
  mutate(state = 'All States', race = 'White')

US_native_jail_rate <- time_frame %>%
  group_by(year) %>%
  summarise(rate = mean(native_jail_pop_rate, na.rm = TRUE)) %>%
  mutate(state = 'All States', race = 'Native American')

chart_data <- bind_rows(WA_white_jail_rate, WA_native_jail_rate, US_white_jail_rate, US_native_jail_rate)

Trends_Over_Time_Chart <- ggplot(chart_data, aes(x = year, y = rate, color = interaction(state, race))) + geom_line(linewidth = 1) +
labs(title = 'Incarceration Rate Trends: Native American and White',
x = 'Year', y = 'Incarceration Rate per 100,000 Individuals', color = 'Race on the State and National Level') +
scale_color_manual(values = c("Washington.White" = "skyblue", "All States.White" = "orange",
"Washington.Native American" = "red", "All States.Native American" = "lightgreen")) + 
scale_x_continuous(breaks = seq(1990, max(chart_data$year), by = 5))
theme_minimal() + theme(legend.title = element_blank())

