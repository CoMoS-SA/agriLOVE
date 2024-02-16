
###########
# Epsilon Market Plain-Vanilla
exp_eps    <- seq(0.1,1, by=0.1)
names_i <- paste0("eps_",as.character(exp_eps))
auto_i <- 1
for (auto_i in 1:length(exp_eps)) {
    save(exp_eps,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}

# Flag Yes/No
exp_flag <- c(0,1)
names_i <- paste0("flag_imit_",as.character(exp_flag))
auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    save(exp_flag,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


# Imitation/Bandwagon
exp_flag <- c(0,1)
exp_band <- c(0.01,0)
names_i <- paste0("flag_imit_",as.character(exp_flag))
names_j <- paste0("band_",as.character(exp_band))
auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_band)) {
        save(exp_flag, exp_band, names_i,auto_i, names_j, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}




# Imitation/Bandwagon and selected Epsilon
exp_flag <- c(0,1)
exp_band <- c(0.01,0)
names_i <- paste0("flag_imit_",as.character(exp_flag))
names_j <- paste0("band_",as.character(exp_band))
exp_eps <- c(0.2,0.5,0.8)
names_k <- paste0("eps_",as.character(exp_eps))

auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_band)) {
        auto_k <- 1
        for (auto_k in 1:length(exp_eps)) {
            save(exp_flag, exp_band, exp_eps, names_i,auto_i, names_j, auto_j, names_k, auto_k, file=("auto_aux.RData"))
            source("main.R")
            gc()
        }
    }
}



# Flag prod
exp_flag_prod <- c(3,4)
names_i <- paste0("flag_imit_",as.character(exp_flag_prod))
auto_i <- 1
for (auto_i in 1:length(exp_flag_prod)) {
    save(exp_flag_prod,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}



############


exp_start    <- c(10,30,60,90,120,170)
names_i <- paste0("eps_",as.character(exp_start))
auto_i <- 1
for (auto_i in 1:length(exp_start)) {
    save(exp_start,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


# Imitation/Bandwagon
exp_start    <- c(30,60,90,130,170)
exp_growth   <- c(0.025,0.035,0.045,0.055,0.065)
names_i <- paste0("start_",as.character(exp_start))
names_j <- paste0("growth_",as.character(exp_growth))
auto_i <- 1
for (auto_i in 1:length(exp_start)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_growth)) {
        save(exp_start, exp_growth, names_i,auto_i, names_j, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}


# Imitation/Bandwagon
exp_start    <- c(100,120)
exp_switch   <- c(0,1)
names_i <- paste0("start_",as.character(exp_start))
names_j <- paste0("switch_",as.character(exp_switch))
auto_i <- 1
for (auto_i in 1:length(exp_start)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_switch)) {
        save(exp_start, exp_switch, names_i,auto_i, names_j, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}



exp_switch    <- c(0,1)
exp_diff   <- c(0.15,0.2,0.25,0.3)
exp_sust <- c(2,3)

names_i <- paste0("switch_",as.character(exp_switch))
names_j <- paste0("diff_",as.character(exp_diff))
names_h <- paste0("sust_",as.character(exp_sust))
auto_i <- 1
for (auto_i in 1:length(exp_switch)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_diff)) {
        auto_h <- 1
            for (auto_h in 1:length(exp_sust)) {
            save(exp_switch, exp_diff, exp_sust, names_i,auto_i, names_j, auto_j, names_h, auto_h, file=("auto_aux.RData"))
            source("main.R")
            #rm(list = ls(all.names=TRUE))
            gc()
        }
    }
}




exp_flag    <- c(0,1)
exp_mc   <- c(100,50)
names_i <- paste0("soil_",as.character(exp_flag))
names_j <- paste0("mc_",as.character(exp_mc))
auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    #auto_j <- 1
    for (auto_j in auto_i:length(exp_mc)) {
        save(exp_flag, exp_mc, names_i,auto_i, names_j, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}








exp_flag    <- c(1,2,3,4,5)
names_i <- paste0("shock_flag_",as.character(exp_flag))
auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    save(exp_flag,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}



exp_flag    <- c(1,2,3,4,5)
exp_var    <- c(0.1,0.2,0.3)
names_i <- paste0("shock_flag_",as.character(exp_flag))
names_j <- paste0("var_",as.character(exp_var))
auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_var)) {
        save(exp_flag, exp_var, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}

################################## TRANSITIONS ##################




exp_tau    <- c(1)
exp_ray    <- c(7)
names_i <- paste0("shock_flag_",as.character(exp_tau))
names_j <- paste0("var_",as.character(exp_ray))
auto_i <- 1
for (auto_i in 1:length(exp_tau)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_ray)) {
        save(exp_tau, exp_ray, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}


#exp_start    <- c(60,80,100,120,140,160,180)
#exp_growth   <- c(0.018, 0.027, 0.036, 0.045, 0.054, 0.063, 0.072)
exp_start    <- c(80)
exp_growth   <- c(0.036, 0.054)
names_i <- paste0("start",as.character(exp_start))
names_j <- paste0("growth",as.character(exp_growth))
auto_i <- 1
for (auto_i in 1:length(exp_start)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_growth)) {
        save(exp_start, exp_growth, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}








flag_deg   <- c(0,1)
names_i <- paste0("output_flag_deg",as.character(flag_deg))
auto_i <- 1
for (auto_i in 1:length(flag_deg)) {
    save(flag_deg,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


aug <- seq(0.05, 0.3, 0.05)
c(rev(1-aug),1,1+aug)

exp_dem   <- c(rev(1-aug),1,1+aug)
names_i <- paste0("demand",as.character(exp_dem))
auto_i <- 1
for (auto_i in 1:length(exp_dem)) {
    save(exp_dem,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


exp_spat   <- c(1,2,3,4,5,6)
names_i <- paste0("spatial_scen",as.character(exp_spat))
auto_i <- 1
for (auto_i in 1:length(exp_spat)) {
    save(exp_spat,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


aug <- seq(0.05, 0.3, 0.05)
c(rev(1-aug),1,1+aug)

exp_dem   <- c(rev(1-aug),1,1+aug)
names_i <- paste0("demand",as.character(exp_dem))
#auto_i <- 1
for (auto_i in 4:length(exp_dem)) {
    save(exp_dem,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


#exp_pol   <- c(0.05, 0.1, 0.15,0.2,0.3,0.4,0.5)
exp_pol   <- c(0.1, 0.15)
names_i <- paste0("def_pol",as.character(exp_pol))
#auto_i <- 1
for (auto_i in 1:length(exp_pol)) {
    save(exp_pol,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


#exp_tax   <- c(0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.6, 0.8)
exp_tax   <- c( 0.1)
names_i <- paste0("tax_conv_prof",as.character(exp_tax))
auto_i <- 1
for (auto_i in 1:length(exp_tax)) {
    save(exp_tax,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


#exp_tax    <- c(0.05,0.1,0.15,0.2,0.3,0.4,0.5)
exp_tax    <- c(0.4,0.5)
exp_flag   <- c(5)
names_i <- paste0("baltax",as.character(exp_tax))
names_j <- paste0("flag",as.character(exp_flag))
auto_i <- 1
for (auto_i in 1:length(exp_tax)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_flag)) {
        save(exp_tax, exp_flag, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}



exp_switch <- c(50)
exp_mem    <- c(50)
names_i <- paste0("switch",as.character(exp_switch))
names_j <- paste0("mem",as.character(exp_mem))
auto_i <- 1
for (auto_i in 1:length(exp_switch)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_mem)) {
        save(exp_switch, exp_mem, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}



exp_flag_tau    <- c(1,2)
exp_tau_delta   <- c(0.4,0.5,0.6)
names_i <- paste0("flag_tau",as.character(exp_flag_tau))
names_j <- paste0("tau_delta",as.character(exp_tau_delta))
auto_i <- 1
for (auto_i in 1:length(exp_flag_tau)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_tau_delta)) {
        save(exp_flag_tau, exp_tau_delta, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}










# 2023 #






# Sust policy at different points in time
exp_tax    <- c(0.1,0.2,0.3, 0.4, 0.5)
exp_time   <- c(100, 150, 200, 250, 300)
names_i <- paste0("sussub",as.character(exp_tax))
names_j <- paste0("time",as.character(exp_time))
auto_i <- 4
for (auto_i in 4:length(exp_tax)) {
    print(auto_i)
    auto_j <- 1
    for (auto_j in 1:length(exp_time)) {
        save(exp_tax, exp_time, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}


#Deforestation policy at different tresholds
exp_tresh   <- seq(1,0.1,-0.1)
names_i <- paste0("def_pol",as.character(exp_tresh))
auto_i <- 1
for (auto_i in 1:length(exp_tresh)) {
    save(exp_tresh,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}

# new deforestation with 500 mc 
exp_tresh   <- seq(0,1,0.2)
names_i <- paste0("def_pol",as.character(exp_tresh))
auto_i <- 1
for (auto_i in 1:length(exp_tresh)) {
    save(exp_tresh,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}

# demand 

aug <- seq(0.05, 0.25, 0.05)

exp_dem   <- c(rev(1-aug),1,1+aug)
exp_dem <- c(0.7, 1.3)
names_i <- paste0("demand",as.character(exp_dem))
auto_i <- 1
for (auto_i in 1:length(exp_dem)) {
    save(exp_dem,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}



aug <- seq(0.1, 0.4, 0.1)

exp_dem   <- c(rev(1-aug),1,1+aug)
names_i <- paste0("demand",as.character(exp_dem))
auto_i <- 1
for (auto_i in 1:length(exp_dem)) {
    save(exp_dem,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


exp_start    <- c(60,80,100,120,140,160,180)
exp_growth   <- c(0.018, 0.027, 0.036, 0.045, 0.054, 0.063, 0.072)
names_i <- paste0("start",as.character(exp_start))
names_j <- paste0("growth",as.character(exp_growth))
auto_i <- 1
for (auto_i in 1:length(exp_start)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_growth)) {
        save(exp_start, exp_growth, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}


exp_tau    <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75)
exp_ray    <- c(1,2,3,4,5,6,7)
names_i <- paste0("shock_flag_",as.character(exp_tau))
names_j <- paste0("var_",as.character(exp_ray))
auto_i <- 1
for (auto_i in 1:length(exp_tau)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_ray)) {
        save(exp_tau, exp_ray, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}



exp_scen   <- c(0,1,2,3,4,5,6)
names_i <- paste0("scen",as.character(exp_scen))
auto_i <- 1
for (auto_i in 1:length(exp_scen)) {
    save(exp_scen,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    #rm(list = ls(all.names=TRUE))
    gc()
}


exp_flag_tau    <- c(1,2)
exp_tau_delta   <- c(0.1,0.2,0.3,0.4,0.5,0.6)
names_i <- paste0("flag_tau",as.character(exp_flag_tau))
names_j <- paste0("tau_delta",as.character(exp_tau_delta))
auto_i <- 1
for (auto_i in 1:length(exp_flag_tau)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_tau_delta)) {
        save(exp_flag_tau, exp_tau_delta, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}


exp_switch <- c(10,20,30,40,50,60,70)
exp_mem    <- c(10,20,30,40,50,60,70)
names_i <- paste0("switch",as.character(exp_switch))
names_j <- paste0("mem",as.character(exp_mem))
auto_i <- 1
for (auto_i in 1:length(exp_switch)) {
    auto_j <- 1
    for (auto_j in 1:length(exp_mem)) {
        save(exp_switch, exp_mem, names_i, names_j, auto_i, auto_j, file=("auto_aux.RData"))
        source("main.R")
        #rm(list = ls(all.names=TRUE))
        gc()
    }
}


