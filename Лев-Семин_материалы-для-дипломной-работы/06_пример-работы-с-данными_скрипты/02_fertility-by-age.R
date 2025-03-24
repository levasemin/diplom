library(tidyverse)

# Возрастные коэффициенты рождаемости (Россия в целом, однолетние группы женщин)
asfr <- read_csv("04_данные-о-рождаемости-по-возрасту/BRaO2012-2022.txt") %>%
  filter(Reg == "643",
         Group == "T",
         Year %in% 2015:2022) %>%
  select(-Reg, -Group) %>%
  rename(year = Year) %>%
  gather(age, fertility_rate, -year) %>%
  mutate(fertility_rate = as.numeric(fertility_rate),
         fertility_rate = fertility_rate / 1000000,
         age = str_remove(age, "BrOAa"),
         age = as.integer(age))
asfr
# Среднегодовая численность женщин (Россия в целом, однолетние группы)
popul_female <- read_csv("04_данные-о-рождаемости-по-возрасту/PopBa2012-2022.txt") %>%
  filter(Reg == "643",
         Group == "T",
         Year %in% 2015:2022) %>%
  select(-Reg, -Group) %>%
  rename(year = Year) %>%
  gather(age, popul_female, -year) %>%
  mutate(popul_female = as.numeric(popul_female),
         age = str_remove(age, "PopBa"),
         age = as.integer(age))
popul_female
# Численность родившихся детей в разрезе однолетних групп матерей (в тысячах)
births_by_age <- popul_female %>%
  left_join(asfr, by = c("year", "age")) %>%
  mutate(births = popul_female * fertility_rate / 1000)
births_by_age
rm(asfr, popul_female)

ggplot(
  births_by_age %>%
    rename(`Возраст матери, лет` = age,
           `Число рожденных детей, тысяч` = births),
  aes(`Возраст матери, лет`, `Число рожденных детей, тысяч`, group = year)
) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(15, 55, by = 5)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 22)) +
  facet_wrap(~ year, scales = "free_x")

# Обязательное проверьте, соответствуют ли total значениям совокупной
# численности родившихся согласно данным Росстата
births_total <- births_by_age %>%
  group_by(year) %>%
  summarise(births_total = sum(births)) %>%
  ungroup()
