load("../agrilowe/demand_500_random//collapsed_demand_500_random.RData")

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
            simul==1 ~ 0.75,
            simul==2 ~ 0.8,
            simul==3 ~ 0.85,
            simul==4 ~ 0.9,
            simul==5 ~ 0.95,
            simul==6 ~ 1,
            simul==7 ~ 1.05,
            simul==8 ~ 1.1,
            simul==9 ~ 1.15,
            simul==10 ~ 1.2,
            simul==11 ~ 1.25
        )
    ) %>% 
        mutate(
            simul1 = factor(simul1,
                            levels = c(0.75,0.8,0.85,0.9,0.95, 1, 1.05, 1.10, 1.15, 1.20, 1.25),
                            labels = c("-25%","-20%", "-15%", "-10%", "-5%", "Baseline", "5%", "10%", "15%", "20%", "25%")
            )
        )
}

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
    ) -> trans_data2





trans_data2 %>% 
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
                time_scarcity_mean = mean(time_scarcity),
                time_scarcity_se = sd(time_scarcity)/(sqrt(n()))
            ),
        by = "simul"
    ) -> simul_data

simul_data %>% 
    rowwise() %>% 
    mutate(
        trans_likel_lower=binom::binom.confint(x=trans_likel*500, n=500, conf.level=.95) %>% 
            filter(method=="wilson") %>% 
            pull(lower),
        trans_likel_upper=binom::binom.confint(x=trans_likel*500, n=500, conf.level=.95) %>% 
            filter(method=="wilson") %>% 
            pull(upper)
    ) %>% 
    ungroup() %>% 
    mutate(
        time_scarcity_lower = time_scarcity_mean - 1.96*time_scarcity_se,
        time_scarcity_upper = time_scarcity_mean + 1.96*time_scarcity_se,
        forests_lower = mean_forests - 1.96*forest_se,
        forests_upper = mean_forests + 1.96*forest_se
    ) -> plot_data


rbind(
    plot_data %>% 
        labeling() %>% 
        select(simul1, simul, trans_likel, trans_likel_lower, trans_likel_upper) %>% 
        rename(
            mean=trans_likel,
            lower = trans_likel_lower,
            upper = trans_likel_upper
        ) %>% 
        add_column(var="likelihood"),
    
    plot_data %>% 
        labeling() %>% 
        select(simul1, simul, time_scarcity_mean, time_scarcity_lower, time_scarcity_upper) %>% 
        rename(
            mean=time_scarcity_mean,
            lower = time_scarcity_lower,
            upper = time_scarcity_upper
        ) %>% 
        add_column(var="scarcity"),
    
    plot_data %>% 
        labeling() %>% 
        select(simul1, simul, mean_forests, forests_lower, forests_upper) %>% 
        rename(
            mean=mean_forests,
            lower = forests_lower,
            upper = forests_upper
        ) %>% 
        add_column(var="forests")
) %>% 
    mutate(
        var = factor(var, 
                     levels=c("likelihood", "scarcity", "forests"),
                     labels = c("Transition Likelihood", "Food Scarcity (% of time steps >5%)", "Remaining Forests (% of initial)")
        )
    ) %>% 
    ggplot() + 
    geom_point(aes(x=simul1, y=mean, colour=var), size=1.5) +
    geom_ribbon(aes(x=simul, ymin=lower, ymax = upper, fill=var), alpha=0.3) +
    geom_line(aes(x=simul, y=mean, colour=var)) +
    facet_wrap(.~var, scales="free_y") + 
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("Transition Likelihood"="#023858", "Food Scarcity (% of time steps >5%)"="#a50f15", "Remaining Forests (% of initial)"="#006d2c")) +
    scale_fill_manual( values = c("Transition Likelihood"="#023858", "Food Scarcity (% of time steps >5%)"="#a50f15", "Remaining Forests (% of initial)"="#006d2c")) +
    xlab("Policy treshold (% of remaining forests)") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(color="black"),
        panel.grid.minor = element_blank()
    ) -> g_dem_pol
