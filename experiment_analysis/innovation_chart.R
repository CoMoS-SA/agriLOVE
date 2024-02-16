library(tidyverse)

theta_min           <- -0.2                 # lower truncation point of innovation distribution (for conventional if land degradation is on)
theta_max           <- 0.4        
agri_growth_penalty <- 0.17                 # tunes the growth penalty for sustainable. 0.03 means 30% less. 
lower_asymp         <- 0                                              # the lower asymptote
upper_asymp_conv    <- mean(c(theta_min , theta_max ))*(1)            # upper asymptote of conventional farmers
growth_conv         <- 0.045                                          # loss growth of conventional farmers
start_time_conv     <- 150#100         
asymp_asym_conv     <- 1     


gen_logis <- function(t,a,k,b,m,v){
    value <- a + (k-a)/( (1 + exp(-b*(t-m)))^(1/v) )
    return(value)
}

tibble(
    time = 1:400
) %>% 
    mutate(
        loss = gen_logis(time,a=lower_asymp, k=upper_asymp_conv, b=growth_conv, m= start_time_conv ,v= asymp_asym_conv),
        loss1 = loss + 0.02,
        loss2 = loss - 0.02
    ) %>% 
    ggplot() + 
    geom_line(aes(x=time, y=loss)) + 
    geom_line(data=. %>% filter(time>120 & time<220), aes(x=time-30, y=loss), arrow = arrow(length = unit(0.2, "cm"),ends="last", type="closed"), colour="#d6604d") +
    geom_hline(aes(yintercept=0.1), linetype=3) +
    geom_hline(aes(yintercept=0), linetype=3) +
    scale_y_continuous(breaks = c(0, 0.1), labels = c(0, expression(over(theta[max]^C - theta[min]^C,2)))) +
    scale_x_continuous(breaks=c(0,400)) +
    annotate(geom = "text", x = 90, y = 0.08, label = "Soil\ndegradation",colour="#d6604d", size=3) +
    theme_bw() + 
    theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color="grey"),
        axis.ticks = element_line(color="grey")
    ) -> inset

tibble(
    x = seq(0, 1, 0.001),
) %>%
    mutate(
        conv_dens = (dbeta(x,2,2)/(abs(theta_max) + abs(theta_min)) ), 
        sust_dens = (dbeta(x,2,2)/(abs((theta_max)*(1-agri_growth_penalty)) + abs(theta_min)) ),
        conv = (x* (abs(theta_max) + abs(theta_min)) ) + theta_min,
        sust = (x* (abs((theta_max)*(1-agri_growth_penalty)) + abs(theta_min)) ) + theta_min
    ) %>% 
    select(-x) %>% 
    pivot_longer(c(conv, sust), names_to = "type", values_to = "x") %>% 
    mutate(
        value = case_when(
            type=="conv" ~ conv_dens,
            T ~ sust_dens
        ),
        type = factor(type, levels=c("sust","conv"), labels=c("Sustainable", "Conventional"))
    ) %>% 
    select(-conv_dens, -sust_dens) %>% 
    ggplot() + 
    geom_line(aes(x=x, y=value, color=type)) + 
    geom_ribbon(aes(x=x, ymax=value, ymin=0, fill=type), alpha=0.5) + 
    geom_segment(aes(x=-0.1, xend=0.685, y=4.3, yend=4.3), colour="transparent") +
    geom_segment(aes(x=-0.22, xend=0.42, y=0, yend=0), colour="grey") + 
    geom_text(data=tibble(x=c(theta_min, 0, 0.1, theta_max*(1-agri_growth_penalty), theta_max),y=c(-0.16,-0.16,-0.27,-0.16,-0.16),
                          label=c(expression(theta[min]^C==theta[min]^S),0,expression(over(theta[max]^C - theta[min]^C,2)),expression(theta[max]^S),expression(theta[max]^C))), 
              aes(x=x,y=y,label=label), parse = T, size=3.5) +
    geom_segment(aes(x=theta_min, xend=theta_min, y=0, yend=-0.04), colour="grey") + 
    geom_segment(aes(x=0, xend=0, y=0, yend=-0.04), colour="grey") + 
    geom_segment(aes(x=0.1, xend=0.1, y=0, yend=-0.04), colour="grey") + 
    geom_segment(aes(x=theta_max*(1-agri_growth_penalty), xend=theta_max*(1-agri_growth_penalty), y=0, yend=-0.04), colour="grey") + 
    geom_segment(aes(x=theta_max, xend=theta_max, y=0, yend=-0.04), colour="grey") + 
    annotate(geom = "text", x = 0.1, y = 3.3, label = expression(Innovation~by~type~of~agriculture~(IN[it]))) +
    scale_fill_manual(values = c("Sustainable"="#4393c3", "Conventional"="#d6604d")) +
    scale_color_manual(values = c("Sustainable"="#4393c3", "Conventional"="#d6604d")) +
    #guides(fill = guide_legend(nrow=1)) +
    theme_minimal() + 
    theme(
        legend.position = c(0.12, 0.65),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()
    ) -> g

g

g +
    annotation_custom(
        ggplotGrob(inset), 
        xmin = 0.22, xmax = 0.7, ymin = 1.6, ymax = 4.3
    ) + 
    annotate(geom = "text", x = 0.28, y = 2.95, label = expression(D[it]),
             angle = 90, size=4)  +
    annotate(geom = "text", x = 0.49, y = 1.65, label = "Periods of Land Usage\nwith Conventional Agriculture", size=3) +
    geom_segment(aes(x=0.1, xend=0.297, y=0, yend=4.1), colour="grey", size=0.2, linetype=2) +
    geom_segment(aes(x=0, xend=0.297, y=0, yend=1.99), colour="grey", size=0.2, linetype=2)-> f

f


ggsave("Desktop/inn.pdf", f, width=9, height = 4.8, scale=1)
knitr::plot_crop("Desktop/inn.pdf")
