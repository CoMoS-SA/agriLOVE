load("sussub_time_500_random/collapsed_sussub_time_500_random.RData")

library(magrittr)
library(tidyverse)
library(tibbletime)

labeling <- function(data_input){
    data_input %>% mutate(
        simul1 = case_when(
            simul%in%c(1,17,18) ~ 0,
            simul>1 & simul<=4 ~ 0.1,
            simul>4 & simul<=7 ~ 0.2,
            simul>7 & simul<=10 ~ 0.3,
            simul>10 & simul<=13 ~ 0.4,
            simul>13 & simul<=16 ~ 0.5
        ),
        simul2 = case_when(
            simul==2 | simul==5 | simul==8 | simul==11 | simul==14 | simul==1 ~ 1,
            simul==3 | simul==6 | simul==9 | simul==12 | simul==15 | simul==17 ~ 2,
            simul==4 | simul==7 | simul==10 | simul==13 | simul==16 | simul==18 ~ 3
        )
    ) %>% 
        mutate(
            simul1 = factor(simul1,
                            levels = c(0,0.1,0.2,0.3,0.4,0.5),
                            labels = c("0%\n(Baseline)","10%","20%", "30%", "40%", "50%")
            ),
            simul2 = factor(simul2,
                            levels = c(1,2,3),
                            labels = c("Early","Medium","Late")
            )
        )
}

#### DATA BUILDING ####

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

remove(data_t)
gc()

#create time series for agricultural dynamics
data %<>% 
    filter(producer!=0 & producer!=9999) %>% 
    select(mc, lat, lon, time, agri, simul)

gc()

data %>% 
    filter(simul%in%c(1:8)) %>% 
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
    filter(simul%in%c(9:16)) %>% 
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

rbind(
    agri_share_data1,
    agri_share_data2
) -> agri_share_data

remove(agri_share_data1, agri_share_data2)
gc()



agri_share_data %>% 
    filter(!(mc==71 & simul==3)) %>% 
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
    ) -> trans_data_p1


agri_share_data %>% 
    filter(mc==71, simul==3) %>% 
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
                             max(time[(Sustainable>=0.11) & (lag_sust<0.11) & (time!=1)]) - 100,
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
    ) -> trans_data_p2

rbind(trans_data_p1, trans_data_p2) -> trans_data

remove(trans_data_p1, trans_data_p2)




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

#add two lines for two baselines
rbind(
    plot_data,
    plot_data %>% 
        filter(simul==1) %>% 
        mutate(simul=17),
    plot_data %>% 
        filter(simul==1) %>% 
        mutate(simul=18)
) -> plot_data 



rbind(
    plot_data %>% 
        labeling() %>% 
        select(simul1,simul2, simul, trans_likel, trans_likel_lower, trans_likel_upper) %>% 
        rename(
            mean=trans_likel,
            lower = trans_likel_lower,
            upper = trans_likel_upper
        ) %>% 
        add_column(var="likelihood"),
    
    plot_data %>% 
        labeling() %>% 
        select(simul1,simul2, simul, time_scarcity_mean, time_scarcity_lower, time_scarcity_upper) %>% 
        rename(
            mean=time_scarcity_mean,
            lower = time_scarcity_lower,
            upper = time_scarcity_upper
        ) %>% 
        add_column(var="scarcity"),
    
    plot_data %>% 
        labeling() %>% 
        select(simul1,simul2, simul, mean_forests, forests_lower, forests_upper) %>% 
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
    geom_hline(data=. %>% filter(simul==1), aes(yintercept=mean), colour="#ec7014", linetype=2) +
    geom_point(data=. %>% filter(!(simul%in%c(17,18))),aes(x=simul1, y=mean, colour=var, shape=simul2), size=1.3,position = position_dodge(width=0.5)) +
    geom_linerange(data=. %>% filter(!(simul%in%c(17,18))), aes(x=simul1, y=mean, ymin=lower, ymax=upper, colour=var, group=simul2), size=0.5, alpha=0.3, linetype=1, position = position_dodge(width=0.5)) +
    geom_line( aes(x=simul1, y=mean, colour=var, group=simul2, linetype=simul2),size=0.3,position = position_dodge2(width=c(0.000001,0.5,0.5,0.5,0.5,0.5))) +
    geom_text(data=. %>% filter(simul==1), aes(x=c(5,4.58,2), y=c(0.164, 0.119, 0.068), label=c("No policy", "No\npolicy", "No policy")), colour="#ec7014", size=2.8, lineheight=0.75) +
    facet_wrap(.~var, scales="free_y") + 
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("Transition Likelihood"="#023858", "Food Scarcity (% of time steps >5%)"="#a50f15", "Remaining Forests (% of initial)"="#006d2c")) +
    scale_fill_manual( values = c("Transition Likelihood"="#023858", "Food Scarcity (% of time steps >5%)"="#a50f15", "Remaining Forests (% of initial)"="#006d2c")) +
    xlab("Subsidy intensity") +
    guides(colour = "none", shape=guide_legend(title="Policy timing", title.position = "left", title.hjust = 0.5, keywidth=3), linetype=guide_legend(title="Policy timing", title.position = "left", keywidth =3)) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        panel.grid.minor = element_blank(),
        legend.box.background = element_rect(colour = "#d9d9d9"),
        panel.border = element_rect(colour = "#d9d9d9"),
        panel.grid.major.x = element_blank()
    ) -> g_pol

g_pol

ggsave("charts/g_sussub_time.pdf", g_pol, width=9, height = 4, scale=0.9)

