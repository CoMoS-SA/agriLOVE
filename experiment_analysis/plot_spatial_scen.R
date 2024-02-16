load("../agrilowe/spatial_scen_500/collapsed_spatial_scen.RData")

library(magrittr)
library(tidyverse)
library(tibbletime)
#library(latex2exp)

#palettes

palettes <- tibble(
    variable = c("trans_likel", "time_scarcity", "mean_forests", "end_trans", "trans_speed"),
    high =     c("#d0d1e6",     "#a50f15",       "#006d2c",      "#980043",   "#54278f"),
    low =      c("#045a8d",     "#fee0d2",       "#ffeda0",      "#d4b9da",   "#bcbddc")
)


#### DATA BUILDING ####
#create time series for agricultural dynamics
data %>% 
    filter(producer!=0 & producer!=9999) %>% 
    select(mc, lat, lon, time, agri, simul) %>% 
    group_by(mc, simul,time) %>% 
    summarise(
        mc_sum_1 = sum(agri==1),
        mc_sum_2 = sum(agri==2),
        n = n()
    ) %>% mutate(
        Conventional = mc_sum_1/n,
        Sustainable = mc_sum_2/n
    ) -> agri_share_data

#create time series for excess demand and forestry
data_t %>% 
    select(time, mc, excess_demand, forests, simul) %>% 
    mutate(forests=forests/36) %>% 
    group_by(time, simul) %>% 
    summarise(
        mean_excess = mean(excess_demand),
        mean_forests = mean(forests),
        forest_se = sd(forests)/(sqrt(n()))
    ) %>% ungroup() -> excess_forest_data

data_t %>% 
    group_by(mc, simul) %>% 
    summarise(
        time_scarcity = sum(excess_demand[time>100]>0.05)/400
    ) -> food_scarcity_data


agri_share_data %>% 
    mutate(
        lock_sust = case_when(
            Conventional[time==500]==1 ~ "Lock",
            Sustainable[time==500]>0.9 ~ "Trans",
            T ~ "Int"
        )
    ) %>% 
    mutate(date = lubridate::ymd(time + 2000, truncated = 2L)) %>% 
    as_tbl_time(index = date) %>% 
    mutate(
        lag_sust = lag(Sustainable)
    ) %>% 
    group_by(mc,simul) %>% 
    mutate(
        start_trans = ifelse(lock_sust[1L]=="Trans", 
                             max(time[(Sustainable>=0.1) & (lag_sust<0.1) & (time!=1)]) - 100,
                             as.numeric(NA)),
        end_trans = ifelse(lock_sust[1L]=="Trans", 
                           min(time[(Sustainable>=0.9) & (lag_sust<0.9) & (time!=1)]) - 100,
                           as.numeric(NA))
    ) %>% 
    ungroup %>% 
    as_tibble() %>% 
    select(-date) %>% 
    mutate(
        trans_speed = end_trans - start_trans 
    ) -> trans_data


trans_data %>% 
    group_by(mc,simul) %>% 
    summarise(
        lock_sust = lock_sust[1L],
        start_trans = start_trans[1L],
        end_trans = end_trans[1L],
        trans_speed = trans_speed[1L]
    ) %>% 
    group_by(simul) %>% 
    summarise(
        trans_likel = sum(lock_sust=="Trans")/n(),
        start_trans = mean(start_trans[lock_sust=="Trans"], na.rm = T)/400,
        end_trans = mean(end_trans[lock_sust=="Trans"], na.rm = T)/400,
        trans_speed = mean(trans_speed[lock_sust=="Trans"], na.rm = T)/400
    ) %>% 
    left_join(
        .,
        excess_forest_data %>% filter(time==500) %>% select(-time),
        by = "simul"
    ) %>% 
    left_join(
        .,
        food_scarcity_data %>% 
            group_by(simul) %>% 
            summarise(
                time_scarcity = mean(time_scarcity)
            ),
        by = "simul"
    ) -> simul_data


simul_data %>% 
    #labeling() %>% 
    select(-start_trans, -mean_excess, -trans_speed) %>% 
    pivot_longer(cols = trans_likel:time_scarcity, names_to = "variable", values_to="value") %>%
    left_join(
        .,
        palettes,
        by="variable"
    ) %>% 
    mutate(
        variable = factor(
            variable,
            levels = c("trans_likel", "time_scarcity", "mean_forests", "end_trans"),
            labels = c("Transition Likelihood", "Food Scarcity\n(% of time steps >5%)", "Remaining Forests\n(% of initial)", "Transition End Date\n(% of total time)")
        )
    ) %>% 
    split(., .$variable) -> plot_data

plot_data %<>% 
    map(function(x){
        x %>% add_column(br=list(waiver()))
    })

plot_data %>% 
    map(function(x){
        x %>% 
            ggplot(aes(x=simul, y=1, fill=value)) +
            geom_tile(color="grey", size=0.1) + 
            facet_wrap(.~variable) + 
            scale_fill_gradient(low=x$low[1], high = x$high[1], na.value = "#f7f4f9", breaks=x$br[[1]], labels=scales::percent_format(accuracy = 1)) +
            geom_text(aes(x=simul, y=1, label=paste0(round(value*100,0),"%"))) +
            xlab(expression("Time of introduction (% of total time)"~(d^s))) +
            ylab(expression("Policy Intensity"~(tau))) +
            theme(
                legend.position = "none",
                axis.title = element_blank(),
                strip.text = element_text(size=12),
                legend.title = element_blank(),
                panel.background = element_blank(),
                strip.background = element_blank(), 
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            )
    }) -> plots


tibble(
    simul=rep(1:3,4),
    variable=rep(c("Transition Likelihood", "Food Scarcity\n(% of time steps >5%)", "Remaining Forests\n(% of initial)", "Transition End Date\n(% of total time)"),each=3),
    col = c(
        ggplot_build(plots[[1]])$data[[1]]["fill"] %>% pull(),
        ggplot_build(plots[[2]])$data[[1]]["fill"] %>% pull(),
        ggplot_build(plots[[3]])$data[[1]]["fill"] %>% pull(),
        ggplot_build(plots[[4]])$data[[1]]["fill"] %>% pull()
        )
) %>% 
    mutate(variable=factor(variable)) -> pdc


simul_data %>% 
    #labeling() %>% 
    select(-start_trans, -mean_excess, -trans_speed) %>% 
    pivot_longer(cols = trans_likel:time_scarcity, names_to = "variable", values_to="value") %>%
    left_join(
        .,
        palettes,
        by="variable"
    ) %>% 
    mutate(
        variable = factor(
            variable,
            levels = c("trans_likel", "time_scarcity", "mean_forests", "end_trans"),
            labels = c("Transition Likelihood", "Food Scarcity\n(% of time steps >5%)", "Remaining Forests\n(% of initial)", "Transition End Date\n(% of total time)")
        )
    ) %>% 
    filter(!is.na(variable)) %>% 
    left_join(
        .,
        pdc, 
        by=c("simul", "variable")
        ) %>% 
    split(., .$simul) -> pd


pd %>% 
    map(function(x){
        
        x %>% select(col) %>% pull() -> fr
        names(fr) <- pd[[1]] %>% select(variable) %>% pull() 
        
        x %>% 
            ggplot(aes(x=simul, y=variable, fill=variable)) +
            geom_tile(color="grey", size=0.1) + 
            #facet_wrap(.~variable, ncol=1) + 
            scale_fill_manual(values = fr) +
            scale_y_discrete(limits=rev) +
            geom_text(aes(x=simul, y=variable, label=paste0(round(value*100,0),"%"))) +
            theme(
                legend.position = "none",
                axis.title = element_blank(),
                strip.text = element_text(size=12),
                legend.title = element_blank(),
                panel.background = element_blank(),
                strip.background = element_blank(), 
                axis.text.x = element_blank(),
                axis.ticks = element_blank()
            )
        
    }) -> plots2


data %>% 
    filter(mc==1, time==1) %>% 
    split(., .$simul) -> a

a %>% 
    map(function(x){
        x %>% 
            ggplot() + 
            geom_tile(aes(x=lon, y=-lat, fill=factor(agri)), color="white") +
            scale_fill_manual(values = c("0"="#006d2c", "1"="#ef8a62", "2"="#67a9cf")) + 
            theme(
                panel.background = element_blank(),
                axis.title = element_blank(), 
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none"
            )
    }) -> plots3

p1 <- cowplot::plot_grid(plots2[[1]], plots3[[1]])
p2 <- cowplot::plot_grid(plots2[[2]], plots3[[2]])
p3 <- cowplot::plot_grid(plots2[[3]], plots3[[3]])

first_line <- cowplot::plot_grid(p2, p3, labels = c('A', 'B'))
sec_line <- cowplot::plot_grid(NULL, p1, NULL, labels = c('', 'C', ''), rel_widths = c(0.25, 0.5, 0.25), nrow=1)

cowplot::plot_grid(
    first_line, sec_line, nrow = 2
) -> g_fin

ggsave("charts/heatmap_spatial_scen.pdf", g_fin, width=9, height=4.6, units = "in")

