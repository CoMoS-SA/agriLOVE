load("~/Documents/agrilowe/flag_tau_tau_delta_500/collapsed_flag_tau_tau_delta_500.RData")

library(magrittr)
library(tidyverse)
library(tibbletime)

palettes <- tibble(
    variable = c("trans_likel", "time_scarcity", "mean_forests", "end_trans", "trans_speed"),
    high =     c("#d0d1e6",     "#a50f15",       "#006d2c",      "#980043",   "#54278f"),
    low =      c("#023858",     "#fee0d2",       "#ffeda0",      "#d4b9da",   "#bcbddc")
)

labeling <- function(data_input){
    data_input %>% mutate(
        simul1 = case_when(
            simul>=1 & simul<=7 ~ 1,
            simul>7 & simul<=14 ~ 2
        ),
        simul2 = case_when(
            simul==1 | simul==14 ~ 0,
            simul==2 | simul==8 ~ 0.1,
            simul==3 | simul==9 ~ 0.2,
            simul==4 | simul==10 ~ 0.3,
            simul==5 | simul==11 ~ 0.4,
            simul==6 | simul==12 ~ 0.5,
            simul==7 | simul==13 ~ 0.6
        )
    ) %>% 
        mutate(
            simul1 = factor(simul1,
                            levels = c(1, 2),
                            labels = c("\u2191 Conventional\n\u2193 Sustainable","\u2191 Sustainable\n\u2193 Conventional")
            ),
            simul2 = factor(simul2,
                            levels = c(0,0.1,0.2,0.3, 0.4,0.5,0.6),
                            labels = c("Baseline","\u00B110%","\u00B120%","\u00B130%","\u00B140%","\u00B150%","\u00B160%")
            )
        )
}


#### DATA BUILDING ####


#create time series for excess demand and forestry
data_t %>% 
    select(time, mc, excess_demand, forests, simul) %>% 
    group_by(time, simul) %>% 
    summarise(
        mean_excess = mean(excess_demand),
        mean_forests = mean(forests)/36
    ) %>% ungroup() -> excess_forest_data

data_t %>% 
    group_by(mc, simul) %>% 
    summarise(
        time_scarcity = sum(excess_demand[time>100]>0.05)/400
    ) -> food_scarcity_data


#create time series for agricultural dynamics
data %>% 
    filter(simul%in%c(1:10)) %>% 
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
    ) -> agri_share_data1


data %>% 
    filter(simul%in%c(11:21)) %>% 
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
    ) -> agri_share_data2

remove(data)
agri_share_data <- rbind(agri_share_data1, agri_share_data2)
remove(agri_share_data1, agri_share_data2)


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


rbind(
    simul_data,
    simul_data %>% 
        filter(simul==1) %>% 
        mutate(simul=14)
) -> simul_data


simul_data %>% 
    labeling() %>% 
    select(-simul, -start_trans, -mean_excess, -trans_speed) %>% 
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
            labels = c("Transition Likelihood", "Food Scarcity (% of time steps >5%)", "Remaining Forests (% of initial)", "Transition End Date (% of total time)")
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
            ggplot(aes(x=simul2, y=simul1, fill=value)) +
            geom_tile(color="grey", size=0.1) + 
            facet_wrap(.~variable) + 
            scale_fill_gradient(low=x$low[1], high = x$high[1], na.value = "#f7f4f9", breaks=x$br[[1]], labels=scales::percent_format(accuracy = 1)) +
            xlab(expression("Time of introduction (% of total time)"~(d^s))) +
            ylab(expression("Policy Intensity"~(tau))) +
            theme(
                legend.position = "right",
                axis.title = element_blank(),
                strip.text = element_text(size=12),
                legend.title = element_blank(),
                panel.background = element_blank(),
                strip.background = element_blank()
            )
    }) -> plots

heatmap <- gridExtra::grid.arrange(grobs = plots, nrow=2, 
                                   #left=grid::textGrob(expression("Switching Intensity"~(tau)), rot=90),
                                   bottom=grid::textGrob((expression("Difference from baseline"~(tau))))
)

#to be saved in local, needs quartz
saveRDS(plot_data, "flag_tau_tau_delta_500/heatmap.rds")

