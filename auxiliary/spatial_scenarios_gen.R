library(reshape2)
library(tidyverse)

set.seed(1)

#### SCENARIO 1 cent_for_rand_sust ####
sust_pos_1 <- rbind(
    c(6,1),
    c(14,1),
    c(15,1),
    c(1,2),
    c(2,2),
    c(7,2),
    c(11,2),
    c(4,3),
    c(7,3),
    c(14,3),
    c(7,4),
    c(13,4),
    c(3,5),
    c(13,5),
    c(15,5),
    c(1,6),
    c(12,6),
    c(15,6),
    c(13,7),
    c(5,8),
    c(13,8),
    c(14,8),
    c(1,9),
    c(2,10), 
    c(13,10), 
    c(14,10), 
    c(15,10), 
    c(1,11),
    c(2,11),
    c(14,11), 
    c(4,12), 
    c(8,12), 
    c(11,12),  
    c(12,12),  
    c(1,13), 
    c(2,13), 
    c(7,13), 
    c(12,13),  
    c(2,14), 
    c(6,14),
    c(9,14),
    c(1,15),
    c(5,15),
    c(7,15),
    c(13,15), 
    c(14,15) 
)

for(uu in 6:11){
    for(aa in 5:10){
        ss <- c(uu,aa)
        if(uu==6 & aa==5){
            forest_pos_1 <- ss
        }else{
            forest_pos_1 <- rbind(forest_pos_1,ss)
        }
    }
}

colnames(sust_pos_1) <- c("lat", "lon")
colnames(forest_pos_1) <- c("lat", "lon")

### generate random positions of forests and conventional
aworld               <- array(1, dim = c(x,y))

for(j in 1:nrow(sust_pos_1)){
    aworld[sust_pos_1[j,"lat"],sust_pos_1[j,"lon"]] <- 2
}
for(j in 1:nrow(forest_pos_1)){
    aworld[forest_pos_1[j,"lat"],forest_pos_1[j,"lon"]] <- 0
}

conv_pos_1 <- which(aworld==1, arr.ind = T) 
colnames(conv_pos_1) <- c("lat", "lon")


#### SCENARIO 2 rand_for_rand_sust ####

sust_pos_2 <- sust_pos_1
aworld               <- array(1, dim = c(x,y))

for(j in 1:nrow(sust_pos_1)){
    aworld[sust_pos_1[j,"lat"],sust_pos_1[j,"lon"]] <- 2
}

non_sust_pos_init         <- which(aworld!=2, arr.ind = T)  
non_sust_pos_init <- cbind(non_sust_pos_init, sample(c(rep(0, 36), rep(1, nrow(non_sust_pos_init)-36))))

forest_pos_2 <- non_sust_pos_init[non_sust_pos_init[,3]==0,1:2]
conv_pos_2<- non_sust_pos_init[non_sust_pos_init[,3]==1,1:2]

colnames(forest_pos_2) <- c("lat", "lon")
colnames(conv_pos_2) <- c("lat", "lon")

















#### SCENARIO 3 cent_for_ring_sust ####
sust_pos_3 <- rbind(
    c(5,4),
    c(6,4),
    c(7,4),
    c(8,4),
    c(9,4),
    c(10,4),
    c(11,4),
    c(12,4),
    c(12,5),
    c(12,6),
    c(12,7),
    c(12,8),
    c(12,9),
    c(12,10),
    c(12,11),
    c(11,11),
    c(10,11),
    c(9,11),
    c(8,11),
    c(7,11),
    c(6,11),
    c(5,11),
    c(5,10),
    c(5,9),
    c(5,8),
    c(5,7),
    c(5,6),
    c(5,5),
    
    c(4,3),
    c(4,4),
    c(4,5),
    c(4,6),
    c(4,7),
    c(4,8),
    c(4,9),
    c(4,10),
    c(4,11),
    c(4,12),
    c(5,3),
    c(6,3),
    c(7,3),
    c(8,3),
    c(5,12),
    c(6,12),
    c(7,12),
    c(8,12)
)

colnames(sust_pos_3) <- c("lat", "lon")

forest_pos_3 <- forest_pos_1



### generate random positions of forests and conventional
aworld               <- array(1, dim = c(x,y))

for(j in 1:nrow(sust_pos_1)){
    aworld[sust_pos_3[j,"lat"],sust_pos_3[j,"lon"]] <- 2
}
for(j in 1:nrow(forest_pos_1)){
    aworld[forest_pos_3[j,"lat"],forest_pos_3[j,"lon"]] <- 0
}

conv_pos_3 <- which(aworld==1, arr.ind = T) 
colnames(conv_pos_3) <- c("lat", "lon")


#### SCENARIO 4 rand_for_ring_sust ####

sust_pos_4 <- sust_pos_3
aworld               <- array(1, dim = c(x,y))

for(j in 1:nrow(sust_pos_1)){
    aworld[sust_pos_4[j,"lat"],sust_pos_4[j,"lon"]] <- 2
}

non_sust_pos_init         <- which(aworld!=2, arr.ind = T)  
non_sust_pos_init <- cbind(non_sust_pos_init, sample(c(rep(0, 36), rep(1, nrow(non_sust_pos_init)-36))))

forest_pos_4 <- non_sust_pos_init[non_sust_pos_init[,3]==0,1:2]
conv_pos_4<- non_sust_pos_init[non_sust_pos_init[,3]==1,1:2]

colnames(forest_pos_4) <- c("lat", "lon")
colnames(conv_pos_4) <- c("lat", "lon")



















#### SCENARIO 5 ring_for_cent_sust ####
for(uu in 6:11){
    for(aa in 5:10){
        ss <- c(uu,aa)
        if(uu==6 & aa==5){
            sust_pos_5 <- ss
        }else{
            sust_pos_5 <- rbind(sust_pos_5,ss)
        }
    }
}
#add contour sust cells
sust_pos_5 <- rbind(sust_pos_5,
                        rbind(
                            c(5,4),
                            c(5,5),
                            c(5,6),
                            c(5,7),
                            c(5,8),
                            c(5,9),
                            c(5,10),
                            c(5,11),
                            c(6,4),
                            c(6,11)
                        )
)

rownames(sust_pos_5) <- NULL
colnames(sust_pos_5) <- c("lat", "lon")


forest_pos_5 <- rbind(
    c(7,4),
    c(8,4),
    c(9,4),
    c(10,4),
    c(11,4),
    c(12,4),
    
    c(12,5),
    c(12,6),
    c(12,7),
    c(12,8),
    c(12,9),
    c(12,10),
    
    c(12,11),
    c(11,11),
    c(10,11),
    c(9,11),
    c(8,11),
    c(7,11),
    
    c(4,3),
    c(4,4),
    c(4,5),
    c(4,6),
    c(4,7),
    c(4,8),
    c(4,9),
    c(4,10),
    c(4,11),
    c(4,12),
    
    c(5,3),
    c(6,3),
    c(7,3),
    c(8,3),
    
    c(5,12),
    c(6,12),
    c(7,12),
    c(8,12)
)

colnames(forest_pos_5) <- c("lat", "lon")
rownames(forest_pos_5) <- NULL


### generate random positions of forests and conventional
aworld               <- array(1, dim = c(x,y))

for(j in 1:nrow(sust_pos_1)){
    aworld[sust_pos_5[j,"lat"],sust_pos_5[j,"lon"]] <- 2
}
for(j in 1:nrow(forest_pos_1)){
    aworld[forest_pos_5[j,"lat"],forest_pos_5[j,"lon"]] <- 0
}

conv_pos_5 <- which(aworld==1, arr.ind = T) 
colnames(conv_pos_5) <- c("lat", "lon")


#### SCENARIO 6 rand_for_cent_sust ####

sust_pos_6 <- sust_pos_5
aworld               <- array(1, dim = c(x,y))

for(j in 1:nrow(sust_pos_1)){
    aworld[sust_pos_6[j,"lat"],sust_pos_6[j,"lon"]] <- 2
}

non_sust_pos_init         <- which(aworld!=2, arr.ind = T)  
non_sust_pos_init <- cbind(non_sust_pos_init, sample(c(rep(0, 36), rep(1, nrow(non_sust_pos_init)-36))))

forest_pos_6 <- non_sust_pos_init[non_sust_pos_init[,3]==0,1:2]
conv_pos_6<- non_sust_pos_init[non_sust_pos_init[,3]==1,1:2]

colnames(forest_pos_6) <- c("lat", "lon")
colnames(conv_pos_6) <- c("lat", "lon")












##### GENERATE RANDOMIZED THETAS ACROSS MC ####

#SCENARIO 1
theta_baseline      <- melt(theta[,,1,1],      varnames = c("lat", "lon"), value.name = "theta")   %>% as_tibble()
agri_baseline       <- melt(agri[,,1,1],      varnames = c("lat", "lon"), value.name = "agri")   %>% as_tibble()

theta_baseline %<>% filter(theta!=0) %>% #189 elements
    inner_join(agri_baseline, by=c("lat", "lon"))

theta_rand <- theta_baseline
theta_rand_conv <- theta_rand %>% filter(agri==1) #143 items
theta_rand_sust <- theta_rand %>% filter(agri==2) #46 items

#for_cent_sust_rand (baseline)
theta_rand$mc <- 1
theta_scen_1 <- theta_rand #189 items

for(i in 2:50){
    theta_conv <- theta_rand_conv
    theta_sust <- theta_rand_sust
    theta_conv$theta <- sample(theta_conv$theta, length(theta_conv$theta))
    theta_sust$theta <- sample(theta_sust$theta, length(theta_sust$theta))
    theta_conv$mc <- i
    theta_sust$mc <- i
    theta_aux <- rbind(theta_conv,theta_sust)
    theta_scen_1 <- rbind(theta_scen_1, theta_aux)
}


#OTHER SCENARIOS
for(k in 2:6){
    #SCENARIO 2
    theta_sust <- as_tibble(get(paste0("sust_pos_",k)))
    theta_conv <- as_tibble(get(paste0("conv_pos_",k)))
    theta_sust$theta <- theta_rand_sust$theta
    theta_conv$theta <- theta_rand_conv$theta
    theta_sust$mc <- 1
    theta_conv$mc <- 1
    
    
    assign(paste0("theta_scen_",k),rbind(theta_sust,theta_conv))
    
    for(i in 2:50){
        theta_sust_aux <- theta_sust
        theta_conv_aux <- theta_conv
        theta_sust_aux$theta <- sample(theta_sust$theta, length(theta_sust$theta))
        theta_conv_aux$theta <- sample(theta_conv$theta, length(theta_conv$theta))
        theta_sust_aux$mc <- i
        theta_conv_aux$mc <- i
        theta_aux <- rbind(theta_sust_aux, theta_conv_aux)
        assign(paste0("theta_scen_",k), rbind(get(paste0("theta_scen_",k)), theta_aux))
    }
}


save(
    sust_pos_1, sust_pos_2, sust_pos_3, sust_pos_4, sust_pos_5, sust_pos_6,
    conv_pos_1, conv_pos_2, conv_pos_3, conv_pos_4, conv_pos_5, conv_pos_6,
    forest_pos_1, forest_pos_2, forest_pos_3, forest_pos_4, forest_pos_5, forest_pos_6,
    theta_scen_1, theta_scen_2, theta_scen_3, theta_scen_4, theta_scen_5, theta_scen_6,
    file = "spatial_scenarios_initialization.RData"
)




