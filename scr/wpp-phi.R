#===============================================================================
# 2022-03-23 -- outliving
# WPP life tables -- whole world
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(sf)
library(hrbrthemes)
library(patchwork)


load("dat/lt_abr.rda")

# calculate phi
wpp_phi <- lt_abr %>% 
    filter(!sex == "b") %>% 
    transmute(
        region, iso2,
        sex, year_begin, period, age, lx, dx, ex, sd
    ) %>% 
    pivot_wider(names_from = sex, values_from = lx:sd) %>% 
    mutate_at(6:13, function(x){x %>% paste %>% as.numeric}) %>% # hard-coded crutch
    drop_na(year_begin) %>% 
    group_by(region, iso2, year_begin, period) %>% 
    summarise(
        phi = sum(head(dx_f, -1) * tail(lx_m, -1), na.rm = T) + 
            sum(dx_f * dx_m, na.rm = T) / 2,
        e0_f = ex_f %>% first,
        e0_m = ex_m %>% first,
        sd_f = sd_f %>% first,
        sd_m = sd_m %>% first
    ) %>% 
    ungroup()

save(wpp_phi, file = "dat/wpp_phi.rda")

# get world map outline (you might need to install the package)
world_outline <- spData::world %>% 
    st_as_sf()

# let's use a fancy projection
world_outline_robinson <- world_outline %>% 
    st_transform(crs = "ESRI:54030")

# produce borders layer
country_borders <- world_outline_robinson %>% 
    rmapshaper::ms_innerlines()


# merge the data and borders
df_map <- world_outline_robinson %>% 
    left_join(wpp_phi, by = c("iso_a2" = "iso2"))

# map!
df_map %>% 
    filter(year_begin == 2015) %>% 
    ggplot()+
    geom_sf(aes(fill = phi), color = NA)+
    geom_sf(data = country_borders, size = .1, color = "#ffffff")+
    scale_fill_viridis_b(option = "G", breaks = seq(.25, .5, .05), begin = .1)+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.15, .4)
    )+
    labs(
        title = "Outsurvival of males across the world, 2015-19",
        caption = "Data: WPP 2019",
        fill = "phi"
    )

ggsave(filename = "fig/phi-wpp-2015.png", width = 6.4, height = 3.6, 
       type = "cairo-png", bg = "#ffffff")


# map all years together
m2015 <- df_map %>% 
    filter(year_begin == 2015) %>% 
    ggplot()+
    geom_sf(aes(fill = phi), color = NA)+
    geom_sf(data = country_borders, size = .1, color = "#ffffff")+
    geom_text(
        data = . %>% st_drop_geometry %>% distinct(period),
        aes(label = period), 
        x = 2e6, y = -5e6, size = 7, color = "#3a3a3a50",
        family = "Roboto Slab", fontface = 2
    )+
    scale_fill_viridis_b(option = "G", breaks = seq(.25, .5, .05), begin = .1)+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.15, .4)
    )+
    labs(
        fill = "phi"
    )

m5080 <- df_map %>% 
    filter(year_begin %in% seq(1950, 1980, 10)) %>% 
    ggplot()+
    geom_sf(aes(fill = phi), color = NA)+
    geom_sf(data = country_borders, size = .05, color = "#ffffff")+
    geom_text(
        data = . %>% st_drop_geometry %>% distinct(period),
        aes(label = period), 
        x = 25e5, y = -5e6, size = 3.4, color = "#3a3a3a50",
        family = "Roboto Slab", fontface = 2
    )+
    scale_fill_viridis_b(option = "G", breaks = seq(.25, .5, .05), begin = .1)+
    facet_wrap(~period, nrow = 1)+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_blank(),
        legend.position = "none"
    )

m9010 <- df_map %>% 
    filter(year_begin %in% seq(1990, 2010, 10)) %>% 
    ggplot()+
    geom_sf(aes(fill = phi), color = NA)+
    geom_sf(data = country_borders, size = .05, color = "#ffffff")+
    geom_text(
        data = . %>% st_drop_geometry %>% distinct(period),
        aes(label = period), 
        x = 25e5, y = -5e6, size = 3.4, color = "#3a3a3a50",
        family = "Roboto Slab", fontface = 2
    )+
    scale_fill_viridis_b(option = "G", breaks = seq(.25, .5, .05), begin = .1)+
    facet_wrap(~period, ncol = 1)+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_blank(),
        legend.position = "none"
    )

out <- (
    m5080 / 
        (m2015+ m9010 + plot_layout(widths = c(3, 1)))
) +
    plot_layout(heights = c(1, 3))

# can't get rid of the extra space, will do manually iin Inkscape
ggsave(filename = "fig/phi-wpp-all.pdf", width = 6.4, height = 6.4, 
       device = cairo_pdf, bg = "#ffffff")
