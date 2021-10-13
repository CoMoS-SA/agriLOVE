load("~/Projects/agrilowe/spatial_scenarios_initialization.RData")

conv_pos_loc = get(paste0("conv_pos_",scen_flag))
sust_pos_loc = get(paste0("sust_pos_",scen_flag))
forest_pos_loc = get(paste0("forest_pos_",scen_flag))
theta_scen_loc = get(paste0("theta_scen_",scen_flag))

agri               <- array(0, dim = c(x,y,time,mc))
agri[,,1,] <- 1

for(j in 1:nrow(sust_pos_loc)){
    agri[sust_pos_loc[j,"lat"],sust_pos_loc[j,"lon"],1,] <- 2
}
for(j in 1:nrow(forest_pos_loc)){
    agri[forest_pos_loc[j,"lat"],forest_pos_loc[j,"lon"],1,] <- 0
}

world[,,1,] <- -1
for(j in 1:nrow(forest_pos_loc)){
    world[forest_pos_loc[j,"lat"],forest_pos_loc[j,"lon"],1,] <- 0
}


# assign producers
init_land <- 1:(x*y-nrow(forest_pos_loc))
world[,,1,][which(world[,,1,]!=0, arr.ind = T)] <- sample(init_land, (x*y-nrow(forest_pos_loc)))
# assign market shares
existing_producers[1:sum(init_land!=0),1,]  <- 1                                # gives the position of producers still existing: if 1 existing, 0 otherwise 
mkt_share[1:sum(init_land!=0),1,]           <- 1/((x*y) - nrow(forest_pos_loc))

#### Global Settings (equal along celles)

profit[,,1,]                <- 0
wealth[,,1,]                <- 120#70
demand[1,]                  <- 225
wage[1,]                    <- 1.5#1.2
food_price[1,]              <- 13
L[,,1,]                     <- 0.5#0.1
for(o in 1:dim(p_L)[1]){                                                        # compute labor at producer level. Use the same value at cell and producer level
    p_L[o,1,] <- 0.5*sum(world[,,1,1]==o) 
}
unshocked_price_land[,,1,]  <- price_conv

# Randomize vintages
switcher[,]                 <- sample(1:switch_time, x*y, replace = T)
if(flag_suspend_ergodic==1){
    switcher[,] <- switcher[,] + ergodic_trans                                  # make switcher coherent when non-ergodic processes are suspended during transient
}

for(ext_j in 1:nrow(theta_scen_loc)){
    rr <- as.numeric(theta_scen_loc[ext_j,"lat"])
    cc <- as.numeric(theta_scen_loc[ext_j,"lon"])
    mcmc <- as.numeric(theta_scen_loc[ext_j,"mc"])
    theta[rr,cc,1,mcmc] <- as.numeric(theta_scen_loc[ext_j,"theta"])
    RDint[rr,cc,1,]     <- 1
    sales[rr,cc,1,]     <- 5
}

    