load("tau_ray_500/collapsed/collapsed_1_3.RData")

library(magrittr)
library(tidyverse)
library(tibbletime)
#library(latex2exp)


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
    #filter(Conventional[time==500]==1 | Sustainable[time==500]>0.9) %>% 
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

saveRDS(data_t, "tau_ray_500/collapsed/data_t_1_3.rds")
saveRDS(trans_data, "tau_ray_500/collapsed/trans_data_1_3.rds")
saveRDS(simul_data, "tau_ray_500/collapsed/simul_data_1_3.rds")