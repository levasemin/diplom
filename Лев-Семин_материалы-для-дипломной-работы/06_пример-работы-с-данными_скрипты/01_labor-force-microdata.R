# haven - R-пакет для импорта файлов SPSS (с расширением sav)


library(haven)
library(tidyverse)
library(readr)

lf_microdata_17_allvars <- read_csv(
  "01_исходные-данные_орс-росстата/микроданные-2023/bd_ors_2017.csv"
)

lf_microdata_17 <- lf_microdata_17_allvars %>%
  select(lf_type = struktak, year_weight = vesa_ob) %>%
  as_factor() %>%
  group_by(lf_type) %>%
  summarise(population = sum(year_weight)) %>%
  ungroup() %>%
  spread(lf_type, population) %>%
  mutate(`Рабочая сила` =
           `Занятые` +
           `Безработные`,
         `Все население старше 15 лет` =
           `Рабочая сила` +
           `экономически неактивные`,
         `Уровень участия в составе рабочей силы, %` =
           `Рабочая сила` / `Все население старше 15 лет` * 100)

rm(lf_microdata_17_allvars)

write.csv(lf_microdata_17, file='01_исходные-данные_орс-росстата/микроданные-2023/aggreg/aggreg17.csv')
