# Sample pipelines for automatic experiments

#### Epsilon Market Plain-Vanilla ####
exp_eps    <- seq(0.1,1, by=0.1)
names_i <- paste0("eps_",as.character(exp_eps))
auto_i <- 1
for (auto_i in 1:length(exp_eps)) {
    save(exp_eps,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    gc()
}

#### Flag Yes/No  ####
exp_flag <- c(0,1)
names_i <- paste0("flag_imit_",as.character(exp_flag))
auto_i <- 1
for (auto_i in 1:length(exp_flag)) {
    save(exp_flag,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    gc()
}


#### Imitation/Bandwagon  ####
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
        gc()
    }
}




#### Imitation/Bandwagon and selected Epsilon  ####
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



#### Flag prod  ####
exp_flag_prod <- c(3,4)
names_i <- paste0("flag_imit_",as.character(exp_flag_prod))
auto_i <- 1
for (auto_i in 1:length(exp_flag_prod)) {
    save(exp_flag_prod,names_i,auto_i, file=("auto_aux.RData"))
    source("main.R")
    gc()
}







#### Lateness/Speed Soil Degradation ####
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
        gc()
    }
}


#### Ray/Tau ####
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
        gc()
    }
}
