library(data.table)
library(tidyverse)

#path <- "/Volumes/Pietro/BALANCED/switch_vs_mem/data_" 
#path <- "/Users/matteo/Projects/agrilove_simulations/plain_vanilla_eps/single_data/" 
#path <- "/Users/matteo/Projects/agrilove_simulations/init_prod/single_data/" 
#path <- "/Users/matteo/Projects/new_simulations_agrilove/tax_100/single_data/" 
#path <- "/Users/matteo/Projects/agrilowe/high_mc_base/" 
#path <- "/Users/matteo/Projects/agrilowe/switch_vs_mem/" 
path <- "/Users/matteo/Projects/agrilowe/flag_tau/" 

macro_var <- 1                                          #set to 1 if only macro variables were saved

#exp <- c(                                               #end string for saved data
#    "eps_0.1",
#    "eps_0.2",
#    "eps_0.3",
#    "eps_0.4",
#    "eps_0.5",
#    "eps_0.6",
#    "eps_0.7",
#    "eps_0.8",
#    "eps_0.9",
#    "eps_1"
#)

#exp <- c(                                               #end string for saved data
#    "flag_imit_0_band_0_eps_0.2",
#    "flag_imit_0_band_0_eps_0.5",
#    "flag_imit_0_band_0_eps_0.8",
#    
#    "flag_imit_0_band_0.01_eps_0.2",
#    "flag_imit_0_band_0.01_eps_0.5",
#    "flag_imit_0_band_0.01_eps_0.8",
#
#    "flag_imit_1_band_0_eps_0.2",
#    "flag_imit_1_band_0_eps_0.5",
#    "flag_imit_1_band_0_eps_0.8",
#    
#    "flag_imit_1_band_0.01_eps_0.2",
#    "flag_imit_1_band_0.01_eps_0.5",
#    "flag_imit_1_band_0.01_eps_0.8"   
#    )

#exp <- c(
#    "mu_imit_0.005_mu_band_0.005",
#    "mu_imit_0.005_mu_band_0.0075",
#    "mu_imit_0.005_mu_band_0.01",
#    "mu_imit_0.005_mu_band_0.0125",
#    "mu_imit_0.005_mu_band_0.015",
#    
#    "mu_imit_0.0075_mu_band_0.005",
#    "mu_imit_0.0075_mu_band_0.0075",
#    "mu_imit_0.0075_mu_band_0.01",
#    "mu_imit_0.0075_mu_band_0.0125",
#    "mu_imit_0.0075_mu_band_0.015",
#    
#    "mu_imit_0.01_mu_band_0.005",
#    "mu_imit_0.01_mu_band_0.0075",
#    "mu_imit_0.01_mu_band_0.01",
#    "mu_imit_0.01_mu_band_0.0125",
#    "mu_imit_0.01_mu_band_0.015",
#    
#    "mu_imit_0.0125_mu_band_0.005",
#    "mu_imit_0.0125_mu_band_0.0075",
#    "mu_imit_0.0125_mu_band_0.01",
#    "mu_imit_0.0125_mu_band_0.0125",
#    "mu_imit_0.0125_mu_band_0.015",
#    
#    "mu_imit_0.015_mu_band_0.005",
#    "mu_imit_0.015_mu_band_0.0075",
#    "mu_imit_0.015_mu_band_0.01",
#    "mu_imit_0.015_mu_band_0.0125",
#    "mu_imit_0.015_mu_band_0.015",
#    
#    "mu_imit_0.02_mu_band_0.02",
#    "mu_imit_0.02_mu_band_0.03",
#    "mu_imit_0.03_mu_band_0.02",
#    "mu_imit_0.03_mu_band_0.03"
#
#)

#exp <- c(
#    "init_prod_random",
#    "init_prod_two",
#    "init_prod_quadrant",
#    "init_prod_six"
#)

#exp <- c(
#    "soil_deg",
#    "deforestation_onlyconv",
#    "deforestation_with_switch"
#)

#exp <- c(
#    "climate_baseline",
#    "shock_flag_1_var_0.1",
#    "shock_flag_2_var_0.1",
#    "shock_flag_3_var_0.1",
#    "shock_flag_4_var_0.1",
#    "shock_flag_5_var_0.1",
#    
#    "shock_flag_1_var_0.2",
#    "shock_flag_2_var_0.2",
#    "shock_flag_3_var_0.2",
#    "shock_flag_4_var_0.2",
#    "shock_flag_5_var_0.2",
#    
#    "shock_flag_1_var_0.3",
#    "shock_flag_2_var_0.3",
#    "shock_flag_3_var_0.3",
#    "shock_flag_4_var_0.3",
#    "shock_flag_5_var_0.3"
#)

#NOTE: for ray vs tau names are wrong. tau = shock_flag, ray = var_
#exp <- c(
#    "shock_flag_0.1_var_1",
#    "shock_flag_0.1_var_2",
#    "shock_flag_0.1_var_3",
#    "shock_flag_0.1_var_4",
#    "shock_flag_0.1_var_5",
#    "shock_flag_0.1_var_6",
#    "shock_flag_0.1_var_7",
#    
#    "shock_flag_0.25_var_1",
#    "shock_flag_0.25_var_2",
#    "shock_flag_0.25_var_3",
#    "shock_flag_0.25_var_4",
#    "shock_flag_0.25_var_5",
#    "shock_flag_0.25_var_6",
#    "shock_flag_0.25_var_7",
#    
#    "shock_flag_0.5_var_1",
#    "shock_flag_0.5_var_2",
#    "shock_flag_0.5_var_3",
#    "shock_flag_0.5_var_4",
#    "shock_flag_0.5_var_5",
#    "shock_flag_0.5_var_6",
#    "shock_flag_0.5_var_7",
#    
#    "shock_flag_0.75_var_1",
#    "shock_flag_0.75_var_2",
#    "shock_flag_0.75_var_3",
#    "shock_flag_0.75_var_4",
#    "shock_flag_0.75_var_5",
#    "shock_flag_0.75_var_6",
#    "shock_flag_0.75_var_7",
#    
#    "shock_flag_1_var_1",
#    "shock_flag_1_var_2",
#    "shock_flag_1_var_3",
#    "shock_flag_1_var_4",
#    "shock_flag_1_var_5",
#    "shock_flag_1_var_6",
#    "shock_flag_1_var_7",
#    
#    "shock_flag_1.25_var_1",
#    "shock_flag_1.25_var_2",
#    "shock_flag_1.25_var_3",
#    "shock_flag_1.25_var_4",
#    "shock_flag_1.25_var_5",
#    "shock_flag_1.25_var_6",
#    "shock_flag_1.25_var_7",
#    
#    "shock_flag_1.5_var_1",
#    "shock_flag_1.5_var_2",
#    "shock_flag_1.5_var_3",
#    "shock_flag_1.5_var_4",
#    "shock_flag_1.5_var_5",
#    "shock_flag_1.5_var_6",
#    "shock_flag_1.5_var_7",
#    
#    "shock_flag_1.75_var_1",
#    "shock_flag_1.75_var_2",
#    "shock_flag_1.75_var_3",
#    "shock_flag_1.75_var_4",
#    "shock_flag_1.75_var_5",
#    "shock_flag_1.75_var_6",
#    "shock_flag_1.75_var_7",
#    
#    "shock_flag_2_var_1",
#    "shock_flag_2_var_2",
#    "shock_flag_2_var_3",
#    "shock_flag_2_var_4",
#    "shock_flag_2_var_5",
#    "shock_flag_2_var_6",
#    "shock_flag_2_var_7"
#)

#exp <- c(
#    "start60_growth0.018",
#    "start60_growth0.027",
#    "start60_growth0.036",
#    "start60_growth0.045",
#    "start60_growth0.054",
#    "start60_growth0.063",
#    "start60_growth0.072",
#    
#    "start80_growth0.018",
#    "start80_growth0.027",
#    "start80_growth0.036",
#    "start80_growth0.045",
#    "start80_growth0.054",
#    "start80_growth0.063",
#    "start80_growth0.072",
#    
#    "start100_growth0.018",
#    "start100_growth0.027",
#    "start100_growth0.036",
#    "start100_growth0.045",
#    "start100_growth0.054",
#    "start100_growth0.063",
#    "start100_growth0.072",
#    
#    "start120_growth0.018",
#    "start120_growth0.027",
#    "start120_growth0.036",
#    "start120_growth0.045",
#    "start120_growth0.054",
#    "start120_growth0.063",
#    "start120_growth0.072",
#    
#    "start140_growth0.018",
#    "start140_growth0.027",
#    "start140_growth0.036",
#    "start140_growth0.045",
#    "start140_growth0.054",
#    "start140_growth0.063",
#    "start140_growth0.072",
#    
#    "start160_growth0.018",
#    "start160_growth0.027",
#    "start160_growth0.036",
#    "start160_growth0.045",
#    "start160_growth0.054",
#    "start160_growth0.063",
#    "start160_growth0.072",
#    
#    "start180_growth0.018",
#    "start180_growth0.027",
#    "start180_growth0.036",
#    "start180_growth0.045",
#    "start180_growth0.054",
#    "start180_growth0.063",
#    "start180_growth0.072"
#)

#exp <- c(
#    "output_flag_deg0",
#    "output_flag_deg1"
#)

#exp <- c(
#    "demand0.7",
#    "demand0.75",
#    "demand0.8",
#    "demand0.85",
#    "demand0.9",
#    "demand0.95",
#    "demand1",
#    "demand1.05",
#    "demand1.1",
#    "demand1.15",
#    "demand1.2",
#    "demand1.25",
#    "demand1.3"
#)

#exp <-c(
#    "spatial_scen1",
#    "spatial_scen2",
#    "spatial_scen3",
#    "spatial_scen4",
#    "spatial_scen5",
#    "spatial_scen6"
#)

#exp <- c(
#    "tax0_flag3",
#    "tax0_flag4",
#    
#    "tax0.05_flag3",
#    "tax0.05_flag4",
#    
#    "tax0.1_flag3",
#    "tax0.1_flag4",
#    
#    "tax0.15_flag3",
#    "tax0.15_flag4",
#    
#    "tax0.2_flag3",
#    "tax0.2_flag4",
#    
#    "tax0.3_flag3",
#    "tax0.3_flag4",
#    
#    "tax0.4_flag3",
#    "tax0.4_flag4"
#)

#exp <- c(
#    "baltax0_flag3",
#    "baltax0_flag4",
#    "baltax0_flag6",
#    
#    "baltax0.05_flag3",
#    "baltax0.05_flag4",
#    "baltax0.05_flag6",
#        
#    "baltax0.1_flag3",
#    "baltax0.1_flag4",
#    "baltax0.1_flag6",
#    
#    "baltax0.2_flag3",
#    "baltax0.2_flag4",
#    "baltax0.2_flag6",
#    
#    "baltax0.3_flag3",
#    "baltax0.3_flag4",
#    "baltax0.3_flag6",
#    
#    "baltax0.4_flag3",
#    "baltax0.4_flag4",
#    "baltax0.4_flag6",
#    
#    "baltax0.5_flag3",
#    "baltax0.5_flag4",
#    "baltax0.5_flag6"
#)


#exp <- c(
#    "baselineOK0-50",
#    "baselineOK50-100",
#    "baselineOK100-200",
#    "baselineOK200-300",
#    "baselineOK300-400",
#    "baselineOK400-500"
#)

#exp <- c(
#    "switch10_mem10",
#    
#    "switch20_mem10",
#    "switch20_mem20",
#    
#    "switch30_mem10",
#    "switch30_mem20",
#    "switch30_mem30",
#    
#    "switch40_mem10",
#    "switch40_mem20",
#    "switch40_mem30",
#    "switch40_mem40",
#    
#    "switch50_mem10",
#    "switch50_mem20",
#    "switch50_mem30",
#    "switch50_mem40",
#    "switch50_mem50",
#    
#    "switch60_mem10",
#    "switch60_mem20",
#    "switch60_mem30",
#    "switch60_mem40",
#    "switch60_mem50",
#    "switch60_mem60",
#    
#    "switch70_mem10",
#    "switch70_mem20",
#    "switch70_mem30",
#    "switch70_mem40",
#    "switch70_mem50",
#    "switch70_mem60",
#    "switch70_mem70"
#)

exp <- c(
    "flag_tau1_tau_delta0",
    "flag_tau1_tau_delta0.1",
    "flag_tau1_tau_delta0.2",
    "flag_tau1_tau_delta0.3",
    "flag_tau1_tau_delta0.4",
    "flag_tau1_tau_delta0.5",
    "flag_tau1_tau_delta0.6",
    
    "flag_tau2_tau_delta0",
    "flag_tau2_tau_delta0.1",
    "flag_tau2_tau_delta0.2",
    "flag_tau2_tau_delta0.3",
    "flag_tau2_tau_delta0.4",
    "flag_tau2_tau_delta0.5",
    "flag_tau2_tau_delta0.6"
)

############################################################################################################

if(macro_var == 1){                                     #grabbed variables
    vs <- c(
        "data",
        "data_t",
        "data_p",
        "data_market_conc"
    )
}else{
    vs <- c(
        "data",
        "data_p",
        "data_t"
    )
}

vsc <- c("sales_counter",                               #grabbed counters
         "bankrupt_counter",  
         "rebound_counter",   
         "switch_counter",    
         "abandon_counter")


for(simul in 1:length(exp)){
    load(paste(path,exp[simul],".RData", sep=""))
    print(paste("Loaded Simulation", simul))
    
    #create differently named objectes for different objects from different experiments
    for(i in 1:length(vs)){
        aux_simul<-as.data.frame(rep(simul,nrow(get(vs[i]))))
        colnames(aux_simul)[1] <- "simul"
        temp_data<-bind_cols(get(vs[i]),aux_simul)
        
        assign(paste(vs[i],"_",simul,sep = "") , temp_data)
        remove(list=vs[i]) #remove original object
    }
    
    #separate loop for counters
    for(k in 1:length(vsc)){
        assign(paste(vsc[k],"_",simul,sep = "") , cbind(get(vsc[k]),simul))
        remove(list=vsc[k]) #remove original object
    }
    
}
remove(temp_data)
remove(aux_simul)

#bind together object
i <- 1
for(i in 1:length(vs)){
    temp_list <- list()
    simul <- 1
    for(simul in 1:length(exp)){
        temp_list[[simul]] <- get(paste(vs[i],"_",simul,sep = "")) #transfer copied object into a list
        remove(list=paste(vs[i],"_",simul,sep = "")) #remove copied object
    }
    assign(vs[i],rbindlist(temp_list)) #collapse list into one dataframe
    
}

#bind together counters
k <- 1
for(k in 1:length(vsc)){
    simul <- 1
    for(simul in 1:length(exp)){
        if(simul==1){#if first simul, create object
            assign(vsc[k],get(paste(vsc[k],"_",simul,sep = "")) )
        }else{#otherwise, bind on it
            assign(vsc[k],rbind(get(vsc[k]), get(paste(vsc[k],"_",simul,sep = ""))))
        }
        remove(list=paste(vsc[k],"_",simul,sep = ""))
    }
}
