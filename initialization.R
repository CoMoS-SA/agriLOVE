#### INITIALIZATION
share_init_forest_flag <- 1                                         # if one, set only the number of cell forests, otherwise set the proportion
#### Assign land to producers
if(flag_central_forest==1){
    if(share_init_forest_flag == 1){
        init_forest_n   <- 36
    }else{
        init_forest     <- 0.1
        init_forest_n   <- round(x*y*init_forest)
    }
    if(is.integer(sqrt(init_forest_n))){                            #if we can make a perfect square out of it
        nrow_forests <- sqrt(init_forest_n)
        ncol_forests <- nrow_forests
    }else{
        div <- divisors(init_forest_n)                              # get integer divisors
        if((length(div)%%2)==0){                                    # if even number of divisors
            nrow_forests <- div[floor(length(div)/2)]
        }else{
            nrow_forests <- median(div)
        }
        ncol_forests <- init_forest_n/nrow_forests
    }
    f_corn_row <- ceiling((x- nrow_forests)/2)                      # forest upper left corner row
    f_corn_col <- floor((y- ncol_forests)/2)                        # forest upper left corner row
    world[,,1,] <- -1
    world[(f_corn_row+1):(f_corn_row+nrow_forests),(f_corn_col+1):(f_corn_col+ncol_forests),1,] <- 0 # assign forest to the centered rectangle

    init_land <- 1:(x*y-init_forest_n)
    world[,,1,][which(world[,,1,]!=0, arr.ind = T)] <- sample(init_land, (x*y-init_forest_n))
    
    existing_producers[1:sum(init_land!=0),1,]  <- 1                # gives the position of producers still existing: if 1 existing, 0 otherwise 
    mkt_share[1:sum(init_land!=0),1,]           <- 1/((x*y) - init_forest_n)
}else{
    # base initialization
    init_forest_n <- 36
    if(flag_init_prod==1){                                                                  # "one cell per producer" initialization
        init_land                                   <- c(rep(0, init_forest_n), 1:(x*y-init_forest_n))
        world[,,1,]                                 <- sample(init_land, x*y)               # same productivity initialization across MC
        existing_producers[1:sum(init_land!=0),1,]  <- 1                                    # gives the position of producers still existing: if 1 existing, 0 otherwise 
        mkt_share[1:sum(init_land!=0),1,]           <- 1/((x*y)-init_forest_n)
    }else if(flag_init_prod==2|flag_init_prod==3){                                          # a fraction of producers starts with two cells. Should make them adjacent
        frac_double                                 <- 0.4
        prod_double                                 <- ((1-init_forest)*x*y)*frac_double/2
        single_cells                                <- (1-init_forest)*(1-frac_double)*x*y
        init_land                                   <- c(rep(0, init_forest*x*y),rep(1:prod_double,2),(prod_double+1):(prod_double+single_cells))
        world[,,1,]                                 <- sample(init_land, x*y)               # same productivity initialization across MC
        existing_producers[1:max(init_land),1,]     <- 1                                    # gives the position of producers still existing: if 1 existing, 0 otherwise
        if(flag_init_prod==2){                                                              # every producer (even those with more than one property) starts with equal shares
            mkt_share[1:max(init_land),1,]                  <- 1/max(init_land)
        }else{                                                                              # producers with two cells starts with double market share
            mkt_share[1:prod_double,1,]                     <- (1/((x*y)*(1-init_forest)))*2
            mkt_share[(prod_double+1):max(init_land),1,]    <- (1/((x*y)*(1-init_forest)))
        }
        
    }else{
        print("ERROR: Wrong producer initialization")
    }
}
    

#### Global Settings (equal along cells)

profit[,,1,]                <- 0
wealth[,,1,]                <- 120
demand[1,]                  <- 225
wage[1,]                    <- 1.5
food_price[1,]              <- 13
L[,,1,]                     <- 0.5
for(o in 1:dim(p_L)[1]){                                            # compute labor at producer level. Use the same value at cell and producer level
    p_L[o,1,] <- 0.5*sum(world[,,1,1]==o) 
}
unshocked_price_land[,,1,]  <- price_conv

# Randomize vintages
switcher[,]                 <- sample(1:switch_time, x*y, replace = T)
if(flag_suspend_ergodic==1){
    switcher[,] <- switcher[,] + ergodic_trans                      # make switcher coherent when non-ergodic processes are suspended during transient
}

# Non-forest initializations
if(flag_agri_init==1){
    agri_init <<- c(1,2) 
}else{
    agri_init <<- c(1,1,1,2) 
}

if(flag_agri_init==0){
    agri_init <<- 1 
}

if(flag_agri_init==3){
    agri_init <<- c(rep(2,15),rep(1,85))
}


non_forest_pos_init         <- which(world[,,1,1]!=0, arr.ind = T)                  # positions of non forest cells to be initialized

if(flag_clustered_agri!=1 & flag_clustered_agri!=2){                                # random assignment of sustainables
    j <- 1
    for (j in 1:nrow(non_forest_pos_init)) {
        if(mc>1){
            RDint[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- 1
            sales[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- 5
            agri[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]      <- sample(agri_init,1)
            revenues[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]  <- 5*food_price[1,1]
        } else{ #need a different code if you have only one MC
            RDint[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- 1
            sales[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- 5
            agri[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]     <- sample(agri_init,1)
            revenues[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1] <- 5*food_price[1,1]
        }
    }
}else{ # non-random sustainable
    if(flag_clustered_agri==1){                                                     # north sustainable
        # overwrite previous agri_init vector
        if(flag_agri_init==1){
            n_conv_aux <- round((x*y - init_forest_n)/2)
            n_sust_aux <- x*y - init_forest_n - n_conv_aux
            agri_init <<- c(rep(1,n_conv_aux),rep(2,n_sust_aux))
        }else{
            if(flag_agri_init==0){                                                  # no sustainables
                n_conv_aux <- round(x*y - init_forest_n)
                n_sust_aux <- 0
                agri_init <<- c(rep(1,n_conv_aux),rep(2,n_sust_aux))
            }else{
                n_conv_aux <- round((x*y - init_forest_n)*3/4)
                n_sust_aux <- x*y - init_forest_n - n_conv_aux
                agri_init <<- c(rep(1,n_conv_aux),rep(2,n_sust_aux))
            }
        }
        
        #create another vector for non-forest positions, this time ordered by column (sustainable goes in the south, not in the east)
        non_forest_agri_bycol <- non_forest_pos_init[order(non_forest_pos_init[,1],non_forest_pos_init[,2],decreasing=TRUE),]
              
        j <- 1
        for (j in 1:nrow(non_forest_agri_bycol)) {
            if(mc>1){
                RDint[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]     <- 1
                sales[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]     <- 5
                agri[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]      <- agri_init[j]
                revenues[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]  <- 5*food_price[1,1]
            } else{ #need a different code if you have only one MC
                RDint[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1]    <- 1
                sales[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1]    <- 5
                agri[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1]     <- agri_init[j]
                revenues[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1] <- 5*food_price[1,1]
            }
        }
    } else{                                                                         # central sustainable
        if(flag_agri_init==1){
            n_conv_aux <- round((x*y - init_forest_n)/2)
            n_sust_aux <- x*y - init_forest_n - n_conv_aux
            agri_init <<- c(rep(1,n_conv_aux),rep(2,n_sust_aux))
        }else{
            if(flag_agri_init==0){                                                  # no sustainables
                n_conv_aux <- round(x*y - init_forest_n)
                n_sust_aux <- 0
                agri_init <<- c(rep(1,n_conv_aux),rep(2,n_sust_aux))
            }else{
                n_conv_aux <- round((x*y - init_forest_n)*3/4)
                n_sust_aux <- x*y - init_forest_n - n_conv_aux
                agri_init <<- c(rep(1,n_conv_aux),rep(2,n_sust_aux))
            }
        }
        
        cr <- floor(x/2)                                                            # central row
        converter <- array(0, dim=c(x+2,2))
        converter[1,1] <- cr
        for(ci in 1:(x+1)){
            if(ci%%2==0){                                                           # if even round
                converter[ci+1,1] <- converter[ci,1] - ci 
                conv_mem <- converter[ci,1]
            }else{
                converter[ci+1,1] <- converter[ci,1]  + ci 
                conv_mem <- converter[ci,1]
            }
        }
        
        converter <- converter[!(converter[,1]<1 |converter[,1]>15),]
        converter[,2] <- 1:x
        nf <- as.data.frame(non_forest_pos_init)
        ci <- 1
        nf$priority <- 0
        for(ci in 1:nrow(converter)){
            nf[nf[,1]==converter[ci,1],"priority"] <- converter[ci,2]
        }

        non_forest_agri_bycol <- nf[order(nf[,3],nf[,2],decreasing=T),]
        
        j <- 1
        for (j in 1:nrow(non_forest_agri_bycol)) {
            if(mc>1){
                RDint[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]     <- 1
                sales[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]     <- 5
                agri[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]      <- agri_init[j]
                revenues[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,]  <- 5*food_price[1,1]
            } else{ #need a different code if you have only one MC
                RDint[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1]    <- 1
                sales[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1]    <- 5
                agri[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1]     <- agri_init[j]
                revenues[non_forest_agri_bycol[j,1],non_forest_agri_bycol[j,2],1,1] <- 5*food_price[1,1]
            }
        }
        
    }
    
}


conv_pos_init  <- which(agri[,,1,1]==1, arr.ind = T) 
sust_pos_init  <- which(agri[,,1,1]==2, arr.ind = T) 

non_forest_pos_init <- cbind(non_forest_pos_init,agri[,,1,1][non_forest_pos_init])
colnames(non_forest_pos_init)[3] <- "agri"

#### Productivities 
if(flag_prod==1){                                                                   # random initial productivity
    j <- 1
    for (j in 1:nrow(non_forest_pos_init)) {
        if(mc>1){
            if(non_forest_pos_init[j,"agri"]==1){
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rtruncnorm(1,1.5,4,theta_init,var_theta_init)
            }else{
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rtruncnorm(1,1.5,4,theta_init*theta_init_pen_sust,var_theta_init*theta_init_pen_sust)#*theta_init_pen_sust)
            }
        } else{ # need a different code if you have only one MC
            if(non_forest_pos_init[j,"agri"]==1){
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rtruncnorm(1,1.5,4,theta_init,var_theta_init)
            }else{
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rtruncnorm(1,1.5,4,theta_init*theta_init_pen_sust,var_theta_init*theta_init_pen_sust)#*theta_init_pen_sust)
            }
        }
    }
}


if(flag_prod==2){                                                                   # productivities located by quadrants
    cut_quad_x <- median(1:x)
    cut_quad_y <- median(1:y)
    j <- 1
    for (j in 1:nrow(non_forest_pos_init)) {
        if(mc>1){
            if(non_forest_pos_init[j,1] <=cut_quad_x & non_forest_pos_init[j,2]<=cut_quad_y){                             # upper left quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rnorm(1,theta_init*0.75,0)
            }
            if(non_forest_pos_init[j,1] >cut_quad_x & non_forest_pos_init[j,2]<=cut_quad_y){                              # upper right quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rnorm(1,theta_init*0.85,0)
            }
            if(non_forest_pos_init[j,1] <=cut_quad_x & non_forest_pos_init[j,2]>cut_quad_y){                              # upper left quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rnorm(1,theta_init*1.15,0)
            }
            if(non_forest_pos_init[j,1] >cut_quad_x & non_forest_pos_init[j,2]>cut_quad_y){                               # upper left quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rnorm(1,theta_init*1.25,0)
            }
        } else{ # need a different code if you have only one MC
            if(non_forest_pos_init[j,1] <=cut_quad_x & non_forest_pos_init[j,2]<=cut_quad_y){                             # upper left quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rnorm(1,theta_init*0.75,0)
            }
            if(non_forest_pos_init[j,1] >cut_quad_x & non_forest_pos_init[j,2]<=cut_quad_y){                              # upper right quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rnorm(1,theta_init*0.85,0)
            }
            if(non_forest_pos_init[j,1] <=cut_quad_x & non_forest_pos_init[j,2]>cut_quad_y){                              # upper left quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rnorm(1,theta_init*1.15,0)
            }
            if(non_forest_pos_init[j,1] >cut_quad_x & non_forest_pos_init[j,2]>cut_quad_y){                               # upper left quadrant
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rnorm(1,theta_init*1.25,var_theta_init)
            }
        }
    }
}


if(flag_prod==3){                                                                   # productivities located by quadrants
    cut_quad_x <- median(1:x)
    cut_quad_y <- median(1:y)
    j <- 1
    for (j in 1:nrow(non_forest_pos_init)) {
        if(mc>1){
            if(non_forest_pos_init[j,1] <=cut_quad_x){                             # lower half
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rnorm(1,theta_init*0.75,0)
            }
            if(non_forest_pos_init[j,1] >cut_quad_x){                               # upper half
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,]     <- rnorm(1,theta_init*1.25,0)
            }
        } else{ # need a different code if you have only one MC
            if(non_forest_pos_init[j,1] <=cut_quad_x){                             # lower half
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rnorm(1,theta_init*0.75,0)
            }
            if(non_forest_pos_init[j,1] >cut_quad_x){                               # upper half
                theta[non_forest_pos_init[j,1],non_forest_pos_init[j,2],1,1]    <- rnorm(1,theta_init*1.25,var_theta_init)
            }
        }
    }
}


if(flag_prod==4){                                                                   # productivities located by quadrants
    
    nfic <- cbind(non_forest_pos_init[order(non_forest_pos_init[,"row"]),], rep(0,nrow(non_forest_pos_init)) )
    colnames(nfic)[4] <- "quadrant"
    
    nfic[nfic[,"row"]>=6 & nfic[,"row"]<= 13 & nfic[,"col"]<=4, "quadrant"] <- 1
    nfic[(nfic[,"row"]>=14 & nfic[,"col"]<=4) | (nfic[,"row"]>=12 & nfic[,"col"]>=5) & nfic[,"col"]<=10, "quadrant"] <- 2
    nfic[nfic[,"row"]>=10 & nfic[,"col"]>=10, "quadrant"] <- 3
    nfic[nfic[,"row"]>=4 & nfic[,"row"]<=9 & nfic[,"col"]>=11, "quadrant"] <- 4
    nfic[(nfic[,"row"]<=3 & nfic[,"col"]>=11) | (nfic[,"row"]<=5 & nfic[,"col"]>=8 & nfic[,"col"]<=10) | (nfic[,"row"]>=3 & nfic[,"row"]<=5 & nfic[,"col"]==7), "quadrant"] <- 5
    nfic[(nfic[,"row"]<=5 & nfic[,"col"]<=6) | (nfic[,"row"]<=2 & nfic[,"col"]==7) , "quadrant"] <- 6
    
    j <- 1
    for (j in 1:nrow(nfic)) {
        if(mc>1){
            if(nfic[j,"quadrant"]==1){                             # lower half
                theta[nfic[j,1],nfic[j,2],1,]     <- rnorm(1,theta_init*0.75,0)
            }
            if(nfic[j,"quadrant"]==2){                             # lower half
                theta[nfic[j,1],nfic[j,2],1,]     <- rnorm(1,theta_init*0.85,0)
            }
            if(nfic[j,"quadrant"]==3){                             # lower half
                theta[nfic[j,1],nfic[j,2],1,]     <- rnorm(1,theta_init*0.95,0)
            }
            if(nfic[j,"quadrant"]==4){                             # lower half
                theta[nfic[j,1],nfic[j,2],1,]     <- rnorm(1,theta_init*1.05,0)
            }
            if(nfic[j,"quadrant"]==5){                             # lower half
                theta[nfic[j,1],nfic[j,2],1,]     <- rnorm(1,theta_init*1.15,0)
            }
            if(nfic[j,"quadrant"]==6){                             # lower half
                theta[nfic[j,1],nfic[j,2],1,]     <- rnorm(1,theta_init*1.25,0)
            }
        } else{ # need a different code if you have only one MC
            if(nfic[j,"quadrant"]==1){
                theta[nfic[j,1],nfic[j,2],1,1]    <- rnorm(1,theta_init*0.75,0)
            }
            if(nfic[j,"quadrant"]==2){
                theta[nfic[j,1],nfic[j,2],1,1]    <- rnorm(1,theta_init*0.85,0)
            }
            if(nfic[j,"quadrant"]==3){
                theta[nfic[j,1],nfic[j,2],1,1]    <- rnorm(1,theta_init*0.95,0)
            }
            if(nfic[j,"quadrant"]==4){
                theta[nfic[j,1],nfic[j,2],1,1]    <- rnorm(1,theta_init*1.05,0)
            }
            if(nfic[j,"quadrant"]==5){
                theta[nfic[j,1],nfic[j,2],1,1]    <- rnorm(1,theta_init*1.15,0)
            }
            if(nfic[j,"quadrant"]==6){
                theta[nfic[j,1],nfic[j,2],1,1]    <- rnorm(1,theta_init*1.25,0)
            }
        }
    }
}

#world[,,1,] <- readRDS("world_o.rds")
#theta[,,1,] <- readRDS("theta_o.rds")
