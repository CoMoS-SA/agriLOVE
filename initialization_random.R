init_forest_n <- 36
init_land <- c(rep(0, init_forest_n), 1:(x*y-init_forest_n))

for (pc in 1:mc) {
    world[,,1,pc] <- sample(init_land, x*y)  
    existing_producers[1:sum(init_land!=0),1,pc]  <- 1    
    mkt_share[1:sum(init_land!=0),1,pc]           <- 1/((x*y)-init_forest_n)
}


#### Global Settings (equal along celles)

profit[,,1,]                <- 0
wealth[,,1,]                <- 120#70
demand[1,]                  <- 225
wage[1,]                    <- 1.5#1.2
food_price[1,]              <- 13
L[,,1,]                     <- 0.5#0.1
for(o in 1:dim(p_L)[1]){ #compute labor at producer level. Use the same value at cell and producer level
    p_L[o,1,] <- 0.5*sum(world[,,1,1]==o) 
}
unshocked_price_land[,,1,]  <- price_conv


# Randomize vintages
for (pc in 1:mc) {
    switcher[,pc]                 <- sample(1:switch_time, x*y, replace = T)
}

if(flag_suspend_ergodic==1){
    switcher[,] <- switcher[,] + ergodic_trans #make switcher coherent when non-ergodic processes are suspended during transient
}


# Non-forest initialiaztions

init_theta <- readRDS("auxiliary/init_theta.rds")

for (pc in 1:mc) {
    non_forest_pos_init <- which(world[,,1,pc]!=0, arr.ind = T)
    if(nrow(init_theta)!=nrow(non_forest_pos_init)){print("Error in initialization: inconsistency between native productivities and number of producers")}
    
    cbind(
        non_forest_pos_init[sample(1:nrow(non_forest_pos_init)),],
        init_theta
    ) -> aux
    
    j <- 1
    for (j in 1:nrow(aux)) {
        RDint[aux[j,1],aux[j,2],1,pc]     <- 1
        sales[aux[j,1],aux[j,2],1,pc]     <- 5
        revenues[aux[j,1],aux[j,2],1,pc]  <- 5*food_price[1,1]
        theta[aux[j,1],aux[j,2],1,pc] <- aux[j,3]
        agri[aux[j,1],aux[j,2],1,pc] <- aux[j,4]
    }
}
