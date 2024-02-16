
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
            simul<=7 ~ 1,
            simul>7 & simul<=14 ~ 2,
            simul>14 & simul<=21 ~ 3,
            simul>21 & simul<=28 ~ 4,
            simul>28 & simul<=35 ~ 5,
            simul>35 & simul<=42 ~ 6,
            simul>42 & simul<=49 ~ 7
        ),
        simul2 = case_when(
            simul==1 | simul==8  | simul==15 | simul==22 | simul==29 | simul==36 | simul==43 ~ 0.25,
            simul==2 | simul==9  | simul==16 | simul==23 | simul==30 | simul==37 | simul==44 ~ 0.5,
            simul==3 | simul==10 | simul==17 | simul==24 | simul==31 | simul==38 | simul==45 ~ 0.75,
            simul==4 | simul==11 | simul==18 | simul==25 | simul==32 | simul==39 | simul==46 ~ 1,
            simul==5 | simul==12 | simul==19 | simul==26 | simul==33 | simul==40 | simul==47 ~ 1.25,
            simul==6 | simul==13 | simul==20 | simul==27 | simul==34 | simul==41 | simul==48 ~ 1.5,
            simul==7 | simul==14 | simul==21 | simul==28 | simul==35 | simul==42 | simul==49 ~ 1.75
        )
    ) %>% 
        mutate(
            simul1 = factor(simul1,
                            levels = c(1,2,3,4,5,6,7),
                            labels = c("1","2 (Baseline)", "3", "4", "5", "6", "7")
            ),
            simul2 = factor(simul2,
                            levels = c(0.25,0.5,0.75,1,1.25,1.5,1.75),
                            labels = c("-75%","-50%","-25%","Baseline","+25%", "+50%", "+75%")
            )
        )
}



#### DATA BUILDING ####
sim1 <- readRDS("tau_ray_500/collapsed/simul_data_1_3.rds")
sim2 <- readRDS("tau_ray_500/collapsed/simul_data_4_7.rds")

trans1 <- readRDS("tau_ray_500/collapsed/trans_data_1_3.rds")
trans2 <- readRDS("tau_ray_500/collapsed/trans_data_4_7.rds")

data_t1 <- readRDS("tau_ray_500/collapsed/data_t_1_3.rds")
data_t2 <- readRDS("tau_ray_500/collapsed/data_t_4_7.rds")

simul_data<- rbind(sim1,sim2 %>% mutate(simul=simul+21))
trans_data<- rbind(trans1,trans2 %>% mutate(simul=simul+21))
data_t<- rbind(data_t1,data_t2 %>% mutate(simul=simul+21))

remove(sim1, sim2, trans1, trans2, data_t1, data_t2)

##### PLOT ####

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
            ggplot(aes(x=simul1, y=simul2, fill=value)) +
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
                                   left=grid::textGrob(expression("Switching Intensity"~(tau)), rot=90),
                                   bottom=grid::textGrob((expression("Ray of Observation"~(d^s))))
)

pal_cor <- c(
    "High Switching Intensity" = "#66bd63",
    "Low Switching Intensity"  = "#66bd63",
    "Large Ray of Observation" = "#ef8a62",
    "Small Ray of Observation" = "#ef8a62"
)

lt_cor <- c(
    "High Switching Intensity" = 1,
    "Low Switching Intensity"  = 2,
    "Large Ray of Observation" = 1,
    "Small Ray of Observation" = 2
)

trans_data %>% 
    left_join(
        .,
        data_t %>% 
            group_by(mc, simul) %>% 
            summarise(
                final_forests = forests[time=500]/36
            ),
        by=c("mc", "simul")
    ) %>% 
    mutate(
        trans_speed = trans_speed/400,
        end_trans = end_trans/400
    ) %>% 
    filter(time==500) %>% #just to have one value for mc, they are all the same
    filter(lock_sust=="Trans") %>% 
    select(simul, mc, final_forests, trans_speed, end_trans) %>% 
    pivot_longer(cols = c(final_forests,trans_speed), names_to = "variable", values_to="value") %>% 
    labeling() %>% 
    mutate(
        variable = factor(
            variable,
            levels = c("final_forests", "trans_speed"),
            labels = c("Remaining Forests (% of initial)", "Transition Length (% of total time)"),
        ),
        sim2 = case_when(
            simul2 %in% c("-75%", "-50%", "-25%", "Baseline") ~ "Low Switching Intensity",
            T ~ "High Switching Intensity"
        ),
        sim1 = case_when(
            simul1 %in% c("1","2 (Baseline)","3") ~ "Small Ray of Observation",
            T ~ "Large Ray of Observation"
        )
    ) %>% 
    mutate(
        sim2=factor(sim2, levels= c("High Switching Intensity", "Low Switching Intensity")),
        sim1=factor(sim1, levels= c("Small Ray of Observation", "Large Ray of Observation")),
    ) %>% 
    ggplot(aes(x=end_trans, y=value)) + 
    geom_point(size=0.4, color="#878787", alpha=0.8) +
    geom_smooth(aes(color=sim2, linetype=sim2), size=0.7, method = "lm", se=F) +
    geom_smooth(aes(color=sim1, linetype=sim1), size=0.7, method = "lm", se=F) +
    facet_wrap(.~variable, scales="free") +
    scale_color_manual(values = pal_cor, breaks=c("High Switching Intensity","Low Switching Intensity", "Large Ray of Observation", "Small Ray of Observation")) +
    scale_linetype_manual(values = lt_cor, breaks=c("High Switching Intensity","Low Switching Intensity", "Large Ray of Observation", "Small Ray of Observation")) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition End Date (% of total time)") +
    guides(color=guide_legend(nrow = 1, byrow = T)) +
    theme_light() +
    theme(
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"),
        legend.key.width = unit(0.8, "cm")
    )-> g_corr


trans_data %>% 
    left_join(
        .,
        data_t %>% 
            group_by(mc, simul) %>% 
            summarise(
                final_forests = forests[time=500]/36
            ),
        by=c("mc", "simul")
    ) %>% 
    mutate(
        trans_speed = trans_speed/400,
        end_trans = end_trans/400,
        start_trans = start_trans/400
    ) %>% 
    filter(time==500) %>% #just to have one value for mc, they are all the same
    filter(lock_sust=="Trans") %>% 
    select(simul, mc, final_forests, trans_speed, start_trans) %>% 
    pivot_longer(cols = c(final_forests,trans_speed), names_to = "variable", values_to="value") %>% 
    labeling() %>% 
    mutate(
        variable = factor(
            variable,
            levels = c("final_forests", "trans_speed"),
            labels = c("Remaining Forests (% of initial)", "Transition Length (% of total time)"),
        ),
        sim2 = case_when(
            simul2 %in% c("-75%", "-50%", "-25%", "Baseline") ~ "Low Switching Intensity",
            T ~ "High Switching Intensity"
        ),
        sim1 = case_when(
            simul1 %in% c("1","2 (Baseline)","3") ~ "Small Ray of Observation",
            T ~ "Large Ray of Observation"
        )
    ) %>% 
    ggplot(aes(x=start_trans, y=value)) + 
    geom_point(size=0.7, color="#878787") +
    geom_smooth(aes(color=sim2, linetype=sim2), size=0.7, method = "lm", se=F) +
    geom_smooth(aes(color=sim1, linetype=sim1), size=0.7, method = "lm", se=F) +
    facet_wrap(.~variable, scales="free") +
    scale_color_manual(values = pal_cor, breaks=c("High Switching Intensity","Low Switching Intensity", "Large Ray of Observation", "Small Ray of Observation")) +
    scale_linetype_manual(values = lt_cor, breaks=c("High Switching Intensity","Low Switching Intensity", "Large Ray of Observation", "Small Ray of Observation")) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition Start Date (% of total time)") +
    guides(color=guide_legend(nrow = 1, byrow = T)) +
    theme_light() +
    theme(
        legend.position = "bottom",
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"),
        legend.key.width = unit(0.8, "cm")
    )-> g_corr2



#### For appendix

trans_data %>% 
    left_join(
        .,
        data_t %>% 
            group_by(mc, simul) %>% 
            summarise(
                final_forests = forests[time=500]/36
            ),
        by=c("mc", "simul")
    ) %>% 
    mutate(
        trans_speed = trans_speed/400,
        end_trans = end_trans/400,
        start_trans = start_trans/400
    ) %>% 
    filter(time==500) %>% #just to have one value for mc, they are all the same
    filter(lock_sust=="Trans") %>% 
    select(simul, mc, trans_speed, start_trans) %>% 
    labeling() %>% 
    mutate(
        sim2 = case_when(
            simul2 %in% c("-75%", "-50%", "-25%", "Baseline") ~ "Low Switching Intensity",
            T ~ "High Switching Intensity"
        ),
        sim1 = case_when(
            simul1 %in% c("1","2 (Baseline)","3") ~ "Small Ray of Observation",
            T ~ "Large Ray of Observation"
        )
    ) -> corr_start_data

corr_start_data %>% 
    ggplot(aes(x=start_trans, y=trans_speed)) + 
    geom_point(size=0.7, color="#878787") +
    geom_smooth(aes(color=sim2, linetype=sim2), size=0.8, method = "lm", se=F) +
    geom_smooth(aes(color=sim1, linetype=sim1), size=0.8, method = "lm", se=F) +
    scale_color_manual(values = pal_cor) +
    scale_linetype_manual(values = lt_cor) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition Start Date (% of total time)") +
    ylab("Transition Length (% of total time)") +
    guides(color=guide_legend(nrow = 2, byrow = T)) +
    theme_light() +
    theme(
        legend.position = c(.75,.85),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"),
        legend.background = element_rect(colour = "grey", size=0.3),
        legend.key.width = unit(1.2, "cm")
    ) -> corr_start

corr_start_data %>% 
    ggplot(aes(x=start_trans, y=trans_speed)) + 
    geom_point(size=0.4, color="#878787", alpha=0.8) +
    geom_smooth(size=0.8, method = "lm", se=F, color="#ef3b2c") +
    scale_color_manual(values = pal_cor) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition Start Date (% of total time)") +
    ylab("Transition Length (% of total time)") +
    guides(color=guide_legend(nrow = 2, byrow = T)) +
    theme_light() +
    theme(
        legend.position = c(.92,.86),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"),
        legend.background = element_rect(colour = "grey", size=0.3),
        legend.key.width = unit(1.2, "cm")
    ) -> corr_start_all


ggsave("charts/heatmap_ray_vs_tau.pdf", heatmap, width=9.3, height=7.2, units = "in", scale=1.02)
ggsave("charts/corr_ray_vs_tau.pdf", g_corr, width=8, height=3.8, units = "in")
ggsave("charts/corr2_ray_vs_tau.pdf", g_corr2, width=8, height=3.8, units = "in")
ggsave("charts/corr_start_ray_vs_tau.pdf", corr_start, width=9, height=5, units = "in")
ggsave("charts/corr_start_all_ray_vs_tau.pdf", corr_start_all, width=6, height=3, units = "in")

