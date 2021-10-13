#PARAMETERS 

# Labor
alpha               <- 0.8                  # labor share in production
labor_eps           <- 0.05                 # labor sensitivity to unfilled demand (to remove)
ud_sens             <- 0.3                  # labor sensitivity to unfilled demand
wealth_buffer       <- 0.7                  # maximum share of liquid assets invested to hire labor

# Market Behavior
eps_price           <- 0.02                 # price sensitivity to excess demand
eps_share           <- 0.5                  # replicator dynamics sensitivity
omega_cost          <- 0.05                 # weight of unit cost in fitness
omega_unfilled      <- 1-omega_cost         # weight of unfilled demand in fitness

# Miscellaneous
demand_rate         <- 4                    # rate of growth of demand #3 
wage_growth         <- 0.4                  # wage growth
price_forest        <- 0                    # prices of forests
price_conv          <- 2                    # initial price of convetionally farmed lands #10
price_sust          <- 15                   # initial price of sustainably farmed lands #10
sharable_cost_frac  <- 0                    # share of fixed costs sharable with cells owned by the same farmer
price_land_growth   <- 0.1                  # price of land growth #0.38 
price_land_var      <- 0.05                 # multiplicative iid shock on price of land, no-memory
new_farm_bye_periods<- 10
ergodic_trans       <- 100                  # transient for non ergodic processes (switching and soil degradation)

# Cells on sale
rho                 <- 0.3                  # bid sensitivity to geographical closeness
psi                 <- 50                   # bid sensitivity to unfilled demand
min_land_price      <- 0.3                  # minimum offer in auction
wealth_share_bid    <- 0.1                  # share of liquid assets used for bids
ud_mean_time        <- 5

# Switching Behavior
ray                 <- 2                    # ray of observation
switch_time         <- 30                   # periods after which one can switch
wind_switch         <- 30                   # number of periods observed when evaluating neighbors performance (max = switch time)
tau                 <- 1                    # tunes propensity to switch
tau_delta           <- 0                    # gains and loss with respect to tau for conventional and sustainables (see flag_tau) 

# RD
rd_effort           <- 0.1                  # share of revenues devoted to innovation
theta_forest        <- 0                    # forest productivity
theta_min           <- -0.2                 # lower truncation point of innovation distribution (for conventional if land degradation is on)
theta_max           <- 0.4                  # upper truncation point of innovation distribution (for conventional if land degradation is on)
theta_min_sust      <- -0.2                 # lower truncation point of innovation distribution for sustainable
theta_max_sust      <- 0.3                  # upper truncation point of innovation distribution for sustainable
iota                <- 2                    # tunes the probability of succesful innovation (and imitation)
agri_growth_penalty <- 0.17                 # tunes the growth penalty for sustainable. 0.03 means 30% less. 

# Imitation
imit_effort         <- 0.05                 # share of revenues devoted to imitation
imit_frac           <- 0.01                 # to be removed?
mu_imit             <- 0.01                 # share of imitated cell productivity in linear combination with self productivity
mu_band             <- 0.01                 # share of best cell productivity amonf those owned in linear combination with self productivity

# Land productivity and deterioration
theta_init          <- 2                    # mean initial productity of different cells 
theta_init_pen_sust <- 0.7                  # penalty in initialization for sustainable (set to 1 if sust=conv)
var_theta_init      <- 1
lambda              <- 0                    # tunes the conventional loss
deg_t               <- 10                   # periods affecting conventional loss

# Soil Deterioration logistic version
lower_asymp         <- 0                                              # the lower asymptote
upper_asymp_conv    <- mean(c(theta_min , theta_max ))*(1)            # upper asymptote of conventional farmers
growth_conv         <- 0.045                                          # loss growth of conventional farmers
start_time_conv     <- 100                                            # starting time. Doesn't change the shape, just shifts the logistic on the right/left. If you want the logistic to be simmetric (entire S) on positive domain, set it equal to time/2
start_time_sust     <- time/4
upper_asymp_sust    <- mean(c(theta_min , theta_max ))*(0.5)          # upper asymptote of sustainable farmers
growth_sust         <- 0.01                                           # loss growth of sustainable farmers
asymp_asym_conv     <- 1                                              # >0 , affects near which asymptote maximum growth occurs. For a flex in the middle, set to 1.
asymp_asym_sust     <- 1 

# Climate Box 
alpha_drought       <- 0.2
beta_drought        <- 4
alpha_flood         <- 0.4
beta_flood          <- 4
flood_frequency     <- 1
spatial_corr_flood  <- 1.2
max_flood_dist      <- 5
droughts_frequency  <- 1
spatial_corr_drought<- 1.2
max_drought_dist    <- 5

# Deforestation
deforestation_prob  <-  1                                                       
forest_theta_gain   <-  0.05
price_deforestation <-  0
time_to_forest      <-  50
def_rho             <-  0.1

# Climate shocks
time_floods         <- 300
var_flood           <- 0.2
floods_no           <- 1
shocked_cells       <- array(0,dim=c(floods_no,2)); colnames(shocked_cells)    <- c("row","col")  

# insert cell coordinates for shocked cells (number of rows equal to floods_no)
if(flag_shock_cell==1){
    shocked_cells[1,] <- c(11,7) #max prod
}
if(flag_shock_cell==2){
    shocked_cells[1,] <- c(12,4) #90th percentile
}
if(flag_shock_cell==3){
    shocked_cells[1,] <- c(4,14) #median
}
if(flag_shock_cell==4){
    shocked_cells[1,] <- c(6,1) #10th percentile
}
if(flag_shock_cell==5){
    shocked_cells[1,] <- c(13,13) #min prod
}

# if climate box active, load baseline demand
if(flag_climate==2){
    load("baseline_climate_demand.RData")
}

max_labor <- 0.5
eps_wage  <- 0.10

# if deforestation policy is active
if(flag_def_policy!=0){
    tresh_def_pol <- 0.1
}

if(flag_conv_tax==1|flag_conv_tax==2|flag_conv_tax==3|flag_conv_tax==4|flag_conv_tax==5){
    conv_tax <- 0.1
}

