library(ggplot2)
library(dplyr)
library(tidyr)

jail_rate_county_state <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

time_frame_chart2 <- jail_rate_county_state %>% filter(year >= 1990)

urbanicity_incarceration_rates <- time_frame_chart2 %>%
  group_by(urbanicity) %>%
  summarise(
    White_U.S. = mean(white_jail_pop_rate, na.rm = TRUE),
    Native_U.S. = mean(native_jail_pop_rate, na.rm = TRUE)
  )

wa_native_incarceration_rates <- time_frame_chart2 %>%
  filter(state == "WA") %>%
  group_by(urbanicity) %>%
  summarise(Native_Washington = mean(native_jail_pop_rate, na.rm = TRUE))

chart2_data <- left_join(urbanicity_incarceration_rates, wa_native_incarceration_rates, by = "urbanicity") %>%
  pivot_longer(cols = c(White_U.S., Native_U.S., Native_Washington), names_to = "Race", values_to = "IncarcerationRate")

Proportion_Race_Urbanicity <- ggplot(chart2_data, aes(x = urbanicity, y = IncarcerationRate, fill = Race)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = 'Incarceration Rates by Urbanicity: Native American and White',
       x = 'Urbanicity',
       y = 'Mean Incarceration Rate per 100,000 Individuals',
       fill = 'Race and Area') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
