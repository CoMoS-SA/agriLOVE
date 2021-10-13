# load libraries and data, perform preliminary operations on data
library(reshape2)
require(tidyverse)
library(knitr)
library(pander)
library(reshape2)

#load("~/Projects/agrilove/data3.RData")#load data

theme_set(theme_light())

##### CELL VARIABLES #####
# Use melt to get tidy data and then do inner merge
world_tidy      <- melt(world,      varnames = c("lat", "lon", "time", "mc"), value.name = "producer")   %>% as_tibble()
profit_tidy     <- melt(profit,     varnames = c("lat", "lon", "time", "mc"), value.name = "profit")     %>% as_tibble()
RDint_tidy      <- melt(RDint,      varnames = c("lat", "lon", "time", "mc"), value.name = "RDint")      %>% as_tibble()
cost_tidy       <- melt(cost,       varnames = c("lat", "lon", "time", "mc"), value.name = "cost")       %>% as_tibble()
L_tidy          <- melt(L,          varnames = c("lat", "lon", "time", "mc"), value.name = "L")          %>% as_tibble()
output_tidy     <- melt(output,     varnames = c("lat", "lon", "time", "mc"), value.name = "output")     %>% as_tibble()
pot_output_tidy <- melt(pot_output, varnames = c("lat", "lon", "time", "mc"), value.name = "pot_output") %>% as_tibble()
sales_tidy      <- melt(sales,      varnames = c("lat", "lon", "time", "mc"), value.name = "sales")      %>% as_tibble()
wealth_tidy     <- melt(wealth,     varnames = c("lat", "lon", "time", "mc"), value.name = "wealth")     %>% as_tibble()
revenues_tidy   <- melt(revenues,   varnames = c("lat", "lon", "time", "mc"), value.name = "revenues")   %>% as_tibble()
tot_cost_tidy   <- melt(tot_cost,   varnames = c("lat", "lon", "time", "mc"), value.name = "tot_cost")   %>% as_tibble()
theta_tidy      <- melt(theta,      varnames = c("lat", "lon", "time", "mc"), value.name = "theta")      %>% as_tibble()
agri_tidy       <- melt(agri,       varnames = c("lat", "lon", "time", "mc"), value.name = "agri")       %>% as_tibble()
price_land_tidy <- melt(price_land, varnames = c("lat", "lon", "time", "mc"), value.name = "price_land") %>% as_tibble()
loss_tidy       <- melt(loss,       varnames = c("lat", "lon", "time", "mc"), value.name = "loss")       %>% as_tibble()
gain_tidy       <- melt(gain,       varnames = c("lat", "lon", "time", "mc"), value.name = "gain")       %>% as_tibble()
unfilled_demand_tidy <- melt(unfilled_demand, varnames = c("lat", "lon", "time", "mc"), value.name = "unfilled_demand") %>% as_tibble()

# put all tidy datasets into a list
list_tidy <- list(world_tidy, profit_tidy, RDint_tidy, cost_tidy, L_tidy, output_tidy, 
                  pot_output_tidy, sales_tidy, wealth_tidy, revenues_tidy, tot_cost_tidy, theta_tidy,
                  agri_tidy, price_land_tidy, loss_tidy, gain_tidy, unfilled_demand_tidy)

# dynamic merge components of the list
# NOTE: data containes "tidy" data. Each row is a single point in the grid at a certain point in time
data <- Reduce(
    function(x, y, ...) inner_join(x, y, ...), 
    list_tidy
)

data$producer <- as.factor(data$producer)

# remove tidy datasets and list
remove(world_tidy, profit_tidy, RDint_tidy, cost_tidy, L_tidy, output_tidy,
       pot_output_tidy, sales_tidy, wealth_tidy, revenues_tidy, tot_cost_tidy, theta_tidy,
       agri_tidy, unfilled_demand_tidy, price_land_tidy, loss_tidy, gain_tidy, list_tidy)

##### TIME VECTORS #####
bankrupt_tidy         <- melt(bankrupt,         varnames = c("time", "mc"), value.name = "bankrupt")      %>% as_tibble() 
food_price_tidy       <- melt(food_price,       varnames = c("time", "mc"), value.name = "food_price")    %>% as_tibble() 
demand_tidy           <- melt(demand,           varnames = c("time", "mc"), value.name = "demand")        %>% as_tibble() 
wage_tidy             <- melt(wage,             varnames = c("time", "mc"), value.name = "wage")          %>% as_tibble()
excess_demand_tidy    <- melt(excess_demand,    varnames = c("time", "mc"), value.name = "excess_demand") %>% as_tibble() 
innovators_tidy       <- melt(innovators,       varnames = c("time", "mc"), value.name = "innovators")    %>% as_tibble()
imitators_tidy        <- melt(imitators,        varnames = c("time", "mc"), value.name = "imitators")     %>% as_tibble()
supply_tidy           <- melt(supply,           varnames = c("time", "mc"), value.name = "supply")        %>% as_tibble()
forests_tidy          <- melt(forests,          varnames = c("time", "mc"), value.name = "forests")       %>% as_tibble()

# put all tidy datasets into a list
list_tidy <- list(bankrupt_tidy, food_price_tidy, demand_tidy, wage_tidy, excess_demand_tidy,
                  innovators_tidy, imitators_tidy, supply_tidy, forests_tidy)

# dynamic merge components of the list
data_t <- Reduce(
    function(x, y, ...) inner_join(x, y, ...), 
    list_tidy
)

# remove tidy datasets and list
remove(bankrupt_tidy, food_price_tidy, demand_tidy, wage_tidy, excess_demand_tidy,
       innovators_tidy, imitators_tidy, supply_tidy, forests_tidy, list_tidy)

##### PRODUCER VARIABLES #####

p_pot_output_tidy       <- reshape2::melt(p_pot_output,       varnames = c("producer", "time", "mc"), value.name = "p_pot_output") %>% as_tibble()  
p_output_tidy           <- reshape2::melt(p_output,           varnames = c("producer", "time", "mc"), value.name = "p_output") %>% as_tibble()   
p_cost_tidy             <- reshape2::melt(p_cost,             varnames = c("producer", "time", "mc"), value.name = "p_cost") %>% as_tibble() 
p_sales_tidy            <- reshape2::melt(p_sales,            varnames = c("producer", "time", "mc"), value.name = "p_sales") %>% as_tibble() 
p_unfilled_demand_tidy  <- reshape2::melt(p_unfilled_demand,  varnames = c("producer", "time", "mc"), value.name = "p_unfilled_demand") %>% as_tibble() 
p_revenues_tidy         <- reshape2::melt(p_revenues,         varnames = c("producer", "time", "mc"), value.name = "p_revenues") %>% as_tibble() 
p_tot_cost_tidy         <- reshape2::melt(p_tot_cost,         varnames = c("producer", "time", "mc"), value.name = "p_tot_cost") %>% as_tibble() 
p_profit_tidy           <- reshape2::melt(p_profit,           varnames = c("producer", "time", "mc"), value.name = "p_profit") %>% as_tibble() 
p_wealth_tidy           <- reshape2::melt(p_wealth,           varnames = c("producer", "time", "mc"), value.name = "p_wealth") %>% as_tibble()
existing_producers_tidy <- reshape2::melt(existing_producers, varnames = c("producer", "time", "mc"), value.name = "existing_producers") %>% as_tibble()
cost_share_fitness_tidy <- reshape2::melt(cost_share_fitness, varnames = c("producer", "time", "mc"), value.name = "cost_share_fitness") %>% as_tibble()
mkt_share_tidy          <- reshape2::melt(mkt_share,          varnames = c("producer", "time", "mc"), value.name = "mkt_shares") %>% as_tibble()
fitness_tidy            <- reshape2::melt(fitness,            varnames = c("producer", "time", "mc"), value.name = "fitness") %>% as_tibble()

# put all tidy datasets into a list
list_tidy <- list(p_pot_output_tidy, p_output_tidy, p_cost_tidy, p_sales_tidy, p_revenues_tidy, p_tot_cost_tidy, p_profit_tidy, p_wealth_tidy, existing_producers_tidy, cost_share_fitness_tidy, mkt_share_tidy, fitness_tidy,  p_unfilled_demand_tidy
)

# dynamic merge components of the list
data_p <- Reduce(
    function(x, y, ...) inner_join(x, y, ...), 
    list_tidy
)

data_p <- data_p %>% filter(existing_producers!=0)

# remove tidy datasets and list
remove(p_pot_output_tidy, p_output_tidy, p_cost_tidy, p_sales_tidy, p_unfilled_demand_tidy, p_revenues_tidy, p_tot_cost_tidy, p_profit_tidy, p_wealth_tidy, existing_producers_tidy, cost_share_fitness_tidy, mkt_share_tidy, fitness_tidy, list_tidy
)


#### MARKET CONCENTRATION ####

data %>% filter(producer!=0 & producer!=9999) %>%  group_by(time,mc, producer) %>% summarize(
    largest = n()
) %>% group_by(time) %>% 
    summarize(
        largest = max(largest)
    ) -> max_max

data_p %>% filter(producer!=0 & producer!=9999) %>% group_by(mc,time) %>% 
    summarise(
        mc_index = sum(mkt_shares^2)
    ) %>% group_by(time) %>% 
    summarise(
        index = mean(mc_index),
        se_index = sd(mc_index)/sqrt(n())
    ) -> herf

data %>% filter(producer!=0 & producer!=9999) %>%  group_by(time,mc, producer) %>% summarize(
    largest = n()
) %>% group_by(time, mc) %>% 
    summarize(
        largest = max(largest)
    ) %>% group_by(time) %>%
    summarise(
        mean_max_producer = mean(largest),
        sd_max_producer = sd(largest)/sqrt(n())
    ) %>% add_column(
        mean_herf = herf$index,
        se_herf = herf$se_index,
        max_max = max_max$largest
    ) -> data_market_conc

remove(max_max, herf)


####### POLISHING AND SAVING ######
#remove unecessary file and save the image or only macro variable
ls()

remove(acq_exp, active_producers, agri, auction_f, bankrupt, ber_mean, bid_exp, bidders, bids, cells_on_sale, ci, close_cell, conv_neighb, conv_pos_init, converter, cost, cost_share_fitness,
cr, deforestation_f, deforestation_prob, deg_t, demand, demand_noise, div, divisors, entry_f, excess_demand, existing_producers, exit_rest, f_corn_col, f_corn_row,
fitness, food_price, forest_gone, forests, gain, gen_logis, get_close_cells, get_ray_exact_cells, h, hire_f, i, ii, ik, il, IM_scaling, IMint, imitation, imitators, IMM, init_forest_n, init_land, innovate, innovators, j, k, kk, l, L, lab_share_cost,
land_share_cost, loss, market_f, max_IM, max_RD, mean_entry_f, mean_fit, mkt_share, mkt_share_cell_on_sale, mkt_share_growth, n_conv_aux, n_property, n_sust_aux, ncol_forests, neighb, new_farm_bye_periods, nf, non_forest_agri_bycol, 
non_forest_pos_init, nrow_forests, o, ordered_bids, output, output_lost, p, p_bankrupt, p_bidding, p_cost, p_L, p_output, p_pot_output, p_profit, p_profit_f, p_revenues, p_sales, p_sales_growth, p_tot_cost,
p_unfilled_demand, p_wealth, pos_loss, pot_output, preliminary_f, price_land, price_land_growth, prod_growth, production_f, profit, profit_f,
prop, prop_agri, property, ray_closest_cell, ray_sep, ray_separation, rd_f, RD_scaling, RDint, reborn, res_demand,
res_output, revenues, sales, sales_growth, sharable_cost_frac, supply, sust_neighb, sust_pos_init, switch_f, switcher, temp_new_mkt_shares,
temp_old_mkt_shares,temp_price, temp_sales, theta, theta_band, theta_forest, tot_cost, unexploited, unfilled_demand, unshocked_price_land, update_mkt_share,
updater, value_sale, wage, wage_growth, wage_shock, wasteland_coord, wastelands, wealth, wealth_share_prop, weather_f, world, z, zz)

if(save_data==1){ #save data
    
    if(save_macro_var==1){ #save only macro variables
        
        saveit <- function(..., file) {
            x <- list(...)
            save(list=names(x), file=file, envir=list2env(x))
        }
        
        saveit(
            data             = data[,c("time", "mc", "lat", "lon", "producer", "output", "theta", "agri", "profit", "revenues")],
            data_p           = data_p[,c("time", "mc", "producer", "p_output", "p_unfilled_demand", "mkt_shares", "p_profit")],
            data_t           = data_t,
            data_market_conc = data_market_conc,
            sales_counter    = sales_counter,     
            bankrupt_counter = bankrupt_counter,  
            rebound_counter  = rebound_counter,   
            switch_counter   = switch_counter,    
            abandon_counter  = abandon_counter,
            file             = paste0(data_path,".RData")
            )

    }else{ #save all relevant data
        save.image(paste0(data_path,".RData"))
    }
}
