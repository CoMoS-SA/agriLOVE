load("demand_500/demand1.RData")
transient <- 100
library(gridExtra)
library(tidyverse)

pal <- c("demand"="#2166ac",
         "supply"="#b2182b")
palp <- c("food_price"="black")


scaleFUN <- function(x) sprintf("%.2f", x)

mc_sel <- c(1,29,20)
bound <- tibble(
    min = c(90, 118, 123), 
    max = c(142, 229, 400)
)
tit <- c("Early Transition", "Late Transition: Overshooting and Recovery", "Conventional Lock-in")
ax <- c("", "Time", "")

g_demand <- list()
g_price <- list()

for(i in 1:length(mc_sel)){
    data_t %>% 
        gather(variable, value, -time, -mc) %>% 
        filter(time>transient,variable=="supply" | variable=="demand",mc==mc_sel[i], time==transient +1) %>% 
        select(variable,value) -> normalizer 
    
    data_t %>% 
        gather(variable, value, -time, -mc) %>% 
        filter(time>transient,variable=="supply" | variable=="demand",mc==mc_sel[i]) %>% 
        #mutate(value=case_when(
        #    variable=="demand" ~ value/normalizer$value[normalizer$variable=="demand"],
        #    variable=="supply" ~ value/normalizer$value[normalizer$variable=="supply"]
        #)) %>% 
        mutate(value=value/normalizer$value[normalizer$variable=="supply"]) %>% 
        mutate(time = time - transient) %>% 
        mutate(title = tit[i]) %>% 
        ggplot() +
        geom_line(aes(x=time, y=value*100, color=variable), size=0.4) +
        facet_wrap(title~.) +
        #scale_y_continuous(labels=scaleFUN) +
        scale_color_manual(values = pal,
                           labels=c("Demand", "Supply")) +
        #geom_ribbon(aes(x=time, ymin=(mean - 1.96*se), ymax=(mean+1.96*se), fill=variable), alpha=0.2, size=0)+ 
        annotate("rect", fill = "#9ebcda", alpha = 0.3, 
                 xmin = bound$min[i], xmax = bound$max[i],
                 ymin = -Inf, ymax = Inf) + 
        xlab("Time")+
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = c(0.8,0.2),
              legend.title = element_blank(),
              axis.title.y = element_blank(),
              strip.background = element_blank(),
              legend.text = element_text(size=8), 
              panel.border = element_rect(colour = "#d9d9d9")
        ) -> g_demand[[i]]
    
    data_t %>% 
        gather(variable, value, -time, -mc) %>% 
        filter(time>transient, mc==mc_sel[i]) %>% 
        filter(variable=="food_price", time==transient+1) %>% 
        select(value) %>% as.numeric() -> normalizer 
    
    
    data_t %>% 
        gather(variable, value, -time, -mc) %>% 
        filter(time>transient, mc==mc_sel[i]) %>% 
        filter(variable=="food_price") %>% 
        mutate(value=value/normalizer) %>% 
        mutate(time = time - transient) %>% 
        mutate(title = tit[i]) %>% 
        #mutate(value = scale(value)) %>% 
        ggplot() +
        geom_line(aes(x=time, y=value*100, color=variable), size=0.4) +
        #scale_y_continuous(labels=scaleFUN) +
        scale_color_manual(values = palp,labels="Food Price") +
        facet_wrap(title~.) +
        annotate("rect", fill = "#9ebcda", alpha = 0.3, 
                 xmin = bound$min[i], xmax = bound$max[i],
                 ymin = -Inf, ymax = Inf) + 
        #geom_ribbon(aes(x=time, ymin=(mean - 1.96*se), ymax=(mean+1.96*se), fill=variable), alpha=0.2, size=0)+ 
        xlab(ax[i])+
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = c(0.8,0.2),
              legend.title = element_blank(),
              axis.title.y = element_blank(),
              strip.background = element_blank(),
              strip.text = element_blank(),
              legend.text = element_text(size=8), 
              panel.border = element_rect(colour = "#d9d9d9")
        ) -> g_price[[i]]
}

cowplot::plot_grid(
    cowplot::plot_grid(
        g_demand[[1]] + theme(legend.position = "none", axis.title = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()), 
        g_demand[[2]] + theme( axis.title = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()), 
        g_demand[[3]] + theme(legend.position = "none", axis.title = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()), 
        nrow=1),
    cowplot::plot_grid(
        g_price[[1]] + scale_y_continuous(breaks = c(99,100,101)) + theme(legend.position = "none"), 
        g_price[[2]] + theme(axis.title.x = element_text(size=13)), 
        g_price[[3]] + theme(legend.position = "none"), 
        nrow=1),
    nrow=2, rel_heights = c(0.49,0.51)
) -> scen


data %>% 
    filter(mc%in%mc_sel, time%in%c(100, 200, 300, 400, 500)) %>% 
    mutate(
        time = factor(
            time,
            levels=c(100, 200, 300, 400, 500),
            labels=c("Initial", "Time = 100", "Time = 200", "Time = 300", "Time = 400")
        ),
        mc = factor(
            mc,
            levels = c(1, 29, 20),
            labels = c("Early Transition", "Late Transition:\nOvershooting and Recovery", "Conventional Lock-in")
        ),
        agri = factor(
            agri,
            levels = c(1,2,0),
            labels = c("Conventional", "Sustainable", "Forest")
        )
    ) %>% 
    ggplot() + 
    geom_tile(aes(x=lon, y=-lat, fill=agri), color="white") +
    facet_grid(mc~time) +
    scale_fill_manual(values = c("Forest"="#006d2c", "Conventional"="#ef8a62", "Sustainable"="#67a9cf")) + 
    theme(
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size=13)
    ) -> spatial_scen

ggsave("charts/scenarios2.pdf", scen, width=9, height=4.5, units = "in")
ggsave("charts/diffusion_map.pdf", spatial_scen, width=8, height=5.2, units = "in", scale=1.1)

##### Diffusion ####

data %>% 
    filter(producer!=0 & producer!=9999) %>% 
    select(mc, lat, lon, time, agri) %>% 
    group_by(mc,time) %>% 
    summarise(
        mc_sum_1 = sum(agri==1),
        mc_sum_2 = sum(agri==2),
        n = n()
    ) %>% mutate(
        Conventional = mc_sum_1/n,
        Sustainable = mc_sum_2/n
    ) -> data_g_trans


rm(list=setdiff(ls(), c("transient", "data_g_trans")))
load("../agrilowe/deg_switch_500/collapsed_deg_switch_500.RData")


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
        lock_likel = sum(lock_sust=="Lock")/n(),
        int_likel = sum(lock_sust=="Int")/n(),
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
    filter(simul%in%c(1,3)) %>% 
    mutate(
        simul = ifelse(simul==1, "Soil Degradation ON","Soil Degradation OFF")
    ) %>% 
    select(simul, trans_likel, lock_likel) %>% 
    mutate(
        trans_likel = paste0(round(trans_likel*100,0), "%"),
        lock_likel = paste0(round(lock_likel*100,0), "%"),
    ) %>% 
    rename(
        "Transition\nLikelihood" = "trans_likel",
        "Lock-in\nLikelihood" = "lock_likel",
        " " = "simul"
        )-> tab 

mpal <- colorRampPalette(c("#d6604d", "#4393c3", "#fee090"))(500) 

library(gridExtra)


data_g_trans %>% 
    filter(time>=transient) %>% 
    mutate(time=time-100) %>% 
    ggplot() + 
    geom_line(aes(x=time, y=Sustainable, colour=factor(mc)), size=0.2) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values=mpal) +
    annotation_custom(
        grob = tableGrob(
            tab, rows = NULL,
            theme = ttheme_minimal(base_size = 9, padding = unit(c(2, 2), "mm")),
            ),
        xmin = 20, xmax = 90,
        ymin = 0.5, ymax = 0.75
    ) +
    xlab("Time") +
    theme_bw() +
    theme(
        legend.position = "none", 
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#d9d9d9"),
        axis.title.x = element_text(size=13)
    ) -> g_trans


ggsave("charts/s_shape.pdf", g_trans, width=11.5, height=5.2, units = "in", scale=0.8)

