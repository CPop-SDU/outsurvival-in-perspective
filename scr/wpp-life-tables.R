#===============================================================================
# 2022-03-23 -- data
# WPP life tables
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(readxl)
library(countrycode)

# xlsx form here: https://population.un.org/wpp/Download/Standard/Mortality

both <- read_excel(
    "dat/wpp/WPP2019_MORT_F17_1_ABRIDGED_LIFE_TABLE_BOTH_SEXES.xlsx",
    skip = 16,
    na = "..."
) %>% 
    set_names(
        c("index", "variant", "region", "notes", "country_code", "type", "parent_code", "period", "age", "age_interval", "mx", "qx", "px", "lx", "dx", "lx2", "sx", "tx", "ex", "ax")
    ) %>% 
    mutate(
        sex = "b",
        iso3c = country_code %>%  countrycode(destination = "iso3c", origin = "un"),
        iso2 = country_code %>%  countrycode(destination = "iso2c", origin = "un"),
        year_begin = period %>% str_sub(1, 4) %>% as.numeric,
        year_end = period %>% str_sub(6, 9) %>% as.numeric
    )

females <- read_excel(
    "dat/wpp/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.xlsx",
    skip = 16,
    na = "..."
) %>% 
    set_names(
        c("index", "variant", "region", "notes", "country_code", "type", "parent_code", "period", "age", "age_interval", "mx", "qx", "px", "lx", "dx", "lx2", "sx", "tx", "ex", "ax")
    ) %>% 
    mutate(
        sex = "f",
        iso3c = country_code %>%  countrycode(destination = "iso3c", origin = "un"),
        iso2 = country_code %>%  countrycode(destination = "iso2c", origin = "un"),
        year_begin = period %>% str_sub(1, 4) %>% as.numeric,
        year_end = period %>% str_sub(6, 9) %>% as.numeric
    )

males <- read_excel(
    "dat/wpp/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.xlsx",
    skip = 16,
    na = "..."
) %>% 
    set_names(
        c("index", "variant", "region", "notes", "country_code", "type", "parent_code", "period", "age", "age_interval", "mx", "qx", "px", "lx", "dx", "lx2", "sx", "tx", "ex", "ax")
    ) %>% 
    mutate(
        sex = "m",
        iso3c = country_code %>%  countrycode(destination = "iso3c", origin = "un"),
        iso2 = country_code %>%  countrycode(destination = "iso2c", origin = "un"),
        year_begin = period %>% str_sub(1, 4) %>% as.numeric,
        year_end = period %>% str_sub(6, 9) %>% as.numeric
    )


# a function to calculate standard deviations
# see suppl to 10.1126/science.aau5811
sd_dx <- function(age, dx, lx, ex, ax){
    {dx / lx[1] * (age + ax - ex[1])^2} %>% rev %>% cumsum %>% rev %>% sqrt
}

lt_abr <- bind_rows(
    both,
    females,
    males
) %>% 
    # root to 1 instead of 100,000
    mutate(
        lx = lx / 1e5, 
        dx = dx / 1e5,
        lx2 = lx2 / 1e5,
        tx = tx / 1e5
    ) %>% 
    group_by(region, iso2, year_begin, sex) %>% 
    mutate(
        sd = sd_dx(age, dx, lx, ex, ax)
    ) %>% 
    ungroup()


save(lt_abr, file = "dat/lt_abr.rda")
