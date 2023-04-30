library(rgee)
library(tidyverse)
library(innovar)
ee_Initialize()

data("Peru")

loreto <- Peru %>% 
  filter(dep == "LORETO")

loreto_region <- loreto %>% 
  summarise() %>% 
  sf_as_ee()

loreto_reg_temp <- get_climate(
  from = '1958-01-01',
  to = '2021-12-31',
  by = 'month',
  band = 'aet',
  fun = "mean",
  region = loreto_region
  )

tidydata <- loreto_reg_temp %>%
  pivot_longer(cols = `aet1958-01`:`aet2021-12`) %>%
  mutate(fecha = substr(name, 4, 12)) %>%
  mutate(
    year = as.integer(substr(fecha, 1, 4)),
    aet = as.double(value)
  ) %>%
  select(year, aet) %>%
  group_by(year) %>%
  summarise(aet = mean(aet))

temp_ref <- tidydata %>%
  filter(year %in% c(1988:1990)) %>%
  select(aet) %>% 
  summarise(aet = mean(aet))

ref <- temp_ref  %>%
  select(aet) %>% pull()

p1 <- tidydata %>%
  ggplot(aes(x = year,y = 1, fill = aet)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#D10021",
    mid = "#FFFFFF",
    high ="#0575B6",
    midpoint = ref) +
  labs(
    subtitle = "(1958-2020)",
    caption = "Source: Elaborado por Innovalab con datos de TerraClim",
    x = "",
    y = ""
  ) +
  labs(title = "LORETO") +
  scale_x_continuous(breaks = seq(from = 1958, to = 2020, by = 10)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  xlim(NA,2022)
p1

ggsave(
  filename = "Loreto.svg",
  plot = last_plot(),
  bg = "white",
  width = 10,
  height = 5)