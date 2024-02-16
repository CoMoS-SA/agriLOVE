
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
            simul<=7 ~ 60,
            simul>7 & simul<=14 ~ 80,
            simul>14 & simul<=21 ~ 100,
            simul>21 & simul<=28 ~ 120,
            simul>28 & simul<=35 ~ 140,
            simul>35 & simul<=42 ~ 160,
            simul>42 & simul<=49 ~ 180
        ),
        simul2 = case_when(
            simul==1 | simul==8  | simul==15 | simul==22 | simul==29 | simul==36 | simul==43 ~ 0.018,
            simul==2 | simul==9  | simul==16 | simul==23 | simul==30 | simul==37 | simul==44 ~ 0.027,
            simul==3 | simul==10 | simul==17 | simul==24 | simul==31 | simul==38 | simul==45 ~ 0.036,
            simul==4 | simul==11 | simul==18 | simul==25 | simul==32 | simul==39 | simul==46 ~ 0.045,
            simul==5 | simul==12 | simul==19 | simul==26 | simul==33 | simul==40 | simul==47 ~ 0.054,
            simul==6 | simul==13 | simul==20 | simul==27 | simul==34 | simul==41 | simul==48 ~ 0.063,
            simul==7 | simul==14 | simul==21 | simul==28 | simul==35 | simul==42 | simul==49 ~ 0.072
        )
    ) %>% 
        mutate(
            simul1 = factor(simul1,
                            levels = c(60,80,100,120,140,160,180),
                            labels = c("-40%","-20%", "Baseline", "+20%", "+40%", "+60%", "+80%")
            ),
            simul2 = factor(simul2,
                            levels = c(0.018,0.027,0.036,0.045,0.054,0.063,0.072),
                            labels = c("-60%","-40%","-20%","Baseline","+20%", "+40%", "+60%")
            )
        )
}



#### DATA BUILDING ####
sim1 <- readRDS("start_growth_500/collapsed/simul_data_60_100.rds")
sim2 <- readRDS("start_growth_500/collapsed/simul_data_120_180.rds")

trans1 <- readRDS("start_growth_500/collapsed/trans_data_60_100.rds")
trans2 <- readRDS("start_growth_500/collapsed/trans_data_120_180.rds")

data_t1 <- readRDS("start_growth_500/collapsed/data_t_60_100.rds")
data_t2 <- readRDS("start_growth_500/collapsed/data_t_120_180.rds")

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
                                   left=grid::textGrob(expression("Soil Degradation Lateness"~(M)), rot=90),
                                   bottom=grid::textGrob((expression("Soil Degradation Speed"~(b))))
)

pal_cor <- c(
    "Early Degradation" = "#fc8d59",
    "Late Degradation"  = "#fc8d59",
    "Slow Degradation"  = "#3690c0",
    "Fast Degradation"  = "#3690c0"
)

lt_cor <- c(
    "Early Degradation" = 1,
    "Late Degradation"  = 2,
    "Slow Degradation"  = 1,
    "Fast Degradation"  = 2
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
        sim1 = case_when(
            simul1 %in% c("+20%", "+40%", "+60%", "+80%") ~ "Late Degradation",
            T ~ "Early Degradation"
        ),
        sim2 = case_when(
            simul2 %in% c("+20%", "+40%", "+60%") ~ "Fast Degradation",
            T ~ "Slow Degradation"
        )
    ) %>% 
    mutate(
        sim1=factor(sim1, levels= c("Early Degradation", "Late Degradation")),
        sim2=factor(sim2, levels= c("Slow Degradation", "Fast Degradation")),
    ) %>%
    ggplot(aes(x=end_trans, y=value)) + 
    geom_point(size=0.4, color="#878787", alpha=0.8) +
    geom_smooth(aes(color=sim1, linetype=sim1), size=0.7, method = "lm", se=F) +
    geom_smooth(aes(color=sim2, linetype=sim2), size=0.7, method = "lm", se=F) +
    facet_wrap(.~variable, scales="free") +
    scale_color_manual(values = pal_cor, breaks=c("Early Degradation", "Late Degradation", "Slow Degradation", "Fast Degradation")) +
    scale_linetype_manual(values = lt_cor, breaks=c("Early Degradation", "Late Degradation", "Slow Degradation", "Fast Degradation")) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition End Date (% of total time)") +
    guides(color=guide_legend(nrow = 1, byrow = T)) +
    #guides(linetype=guide_legend(nrow = 1, byrow = T)) +
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
        sim1 = case_when(
            simul1 %in% c("+20%", "+40%", "+60%", "+80%") ~ "Late Degradation",
            T ~ "Early Degradation"
        ),
        sim2 = case_when(
            simul2 %in% c("+20%", "+40%", "+60%") ~ "Fast Degradation",
            T ~ "Slow Degradation"
        )
    ) %>% 
    ggplot(aes(x=start_trans, y=value)) + 
    geom_point(size=0.7, color="#878787") +
    geom_smooth(aes(color=sim1, linetype=sim1), size=0.7, method = "lm", se=F) +
    geom_smooth(aes(color=sim2, linetype=sim2), size=0.7, method = "lm", se=F) +
    facet_wrap(.~variable, scales="free") +
    scale_color_manual(values = pal_cor, breaks=c("Early Degradation", "Late Degradation", "Slow Degradation", "Fast Degradation")) +
    scale_linetype_manual(values = lt_cor, breaks=c("Early Degradation", "Late Degradation", "Slow Degradation", "Fast Degradation")) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition Start Date (% of total time)") +
    guides(color=guide_legend(nrow = 1, byrow = T)) +
    #guides(linetype=guide_legend(nrow = 2, byrow = F)) +
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
        sim1 = case_when(
            simul1 %in% c("+20%", "+40%", "+60%", "+80%") ~ "Late Degradation",
            T ~ "Early Degradation"
        ),
        sim2 = case_when(
            simul2 %in% c("+20%", "+40%", "+60%") ~ "Fast Degradation",
            T ~ "Slow Degradation"
        )
    ) -> corr_start_data

corr_start_data %>% 
    ggplot(aes(x=start_trans, y=trans_speed)) + 
    geom_point(size=0.7, color="#878787") +
    geom_smooth(aes(color=sim1, linetype=sim1), size=0.8, method = "lm", se=F) +
    geom_smooth(aes(color=sim2, linetype=sim2), size=0.8, method = "lm", se=F) +
    scale_color_manual(values = pal_cor) +
    scale_linetype_manual(values = lt_cor) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    xlab("Transition Start Date (% of total time)") +
    ylab("Transition Length (% of total time)") +
    guides(color=guide_legend(nrow = 2, byrow = T)) +
    #guides(linetype=guide_legend(nrow = 2, byrow = F)) +
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
    #guides(linetype=guide_legend(nrow = 2, byrow = F)) +
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


ggsave("charts/heatmap_start_growth.pdf", heatmap, width=9.3, height=7.2, units = "in", scale=1.02)
ggsave("charts/corr_start_growth.pdf", g_corr, width=8, height=3.8, units = "in")
ggsave("charts/corr2_start_growth.pdf", g_corr2, width=8, height=3.8, units = "in")
ggsave("charts/corr_start_start_growth.pdf", corr_start, width=9, height=5, units = "in")
ggsave("charts/corr_start_all_start_growth.pdf", corr_start_all, width=6, height=3, units = "in")

