rm(list=ls())
#load("auto_aux.RData")                                          # load data for multiple experiments 
#### Libraries
library("purrr")
library("tictoc")
library("truncnorm")

tic("Total Model")

#### Load Functions
source("evol_market.R")
source("profit.R")
source("entry.R")
source("rd.R")
source("climate_box.R")
source("preliminary.R")
source("production.R")
source("p_profit.R")
source("functions.R")
source("switch.R")
source("mean_entry.R")
source("hire.R")
source("auction.R")
source("deforestation.R")

#### Analysis settings
experiment_naming<- 0                                           # set to 1 to retriece duynamic names for experiments, se to 2 for two levels experiemnts, otherewise use "last_run.RData".
save_data        <- 1                                           # set to 1 to save wrangled data
save_macro_var   <- 1                                           # set to 1 if only macro variables must be saved. Save all relevant variables otherwise
report           <- 1                                           # do you want automatic analysis to be permormed automatically at the end of the run? If yes, set to 1
transient        <- 1                                           # set transient: to be removed from final analysis

#### Global Settings ####
time <- 500                                                     # number of time steps
x    <- 15                                                      # horizontal dimension of land
y    <- 15                                                      # vertical dimension of land
mc   <- 10                                                     # number of montecarlo replications
                                                                # Dimensions are set here as they are used in declaring various objects
#### Flags
source("flags.R")

RNGkind(sample.kind = "Rounding")
set.seed(1)                                                     # it is used as a seed in initialization

#### Declarations
source("declarations.R")

#### Parameters
source("parameters.R")

##### Initialization
if(scen_flag==0){
    source("initialization.R")
}else{
    source("spatial_initialization.R")
}



##### Simulation ####
p <- 1
for (p in 1:mc) {                                               # open montecarlo simulation
    tic(paste("Ended MC",p))
    #set.seed(p+400)
    if(flag_climate==2 | flag_climate==4){
            set.seed(1)                                         # set seed for pseudo-random number generation
    }
    
    t <- 2
    for(t in 2:time) {                                          # open time loop
        
        if(flag_climate==2 | flag_climate==4){
            if(t==300){
                set.seed(p)                                     # set seed for pseudo-random number generation
            }
        }
        
        preliminary_f()                                         # preliminary module
        hire_f()
        i<-1
        for(i in 1:x){                                          # open row agents
            k<-1
            for(k in 1:y) {                                     # open column agents
                rd_f()                                        
            }                                                   #close column agents
        }                                                       #close row agents
        production_f()                                          # producer production module
        market_f()                                              # market module 
        i<-1                                                    # food price is determined, we re-open the land-cycle to assign profits to each cell
        for(i in 1:x){                                          # open row agents
            k<-1
            for(k in 1:y) {                                     # open column agents
                profit_f()                                      # profit module
            }
        }
        p_profit_f()                                            # producer profit module
        z <- 1                                                  # open producer loop
        for (z in 1:dim(existing_producers)[1]) {
            switch_f()                                          # switch agriculture module
            entry_f()                                           # entry/auction module
        }                                                       # close producer loop
        #weather_f()
        climate_box_f()
        deforestation_f()
    }                                                           # close time loop
    toc()
}                                                               # close mc loop

toc()

                                                                # Performs automatic analysis and creates html and md file

######################################################################################################################################

if(experiment_naming==1){
    data_path <- names_i[auto_i]
}else{
    if(experiment_naming==2){
        data_path <- paste(names_i[auto_i],names_j[auto_j], sep="_")
    }else{
        data_path <- "last_run"
    }
}


if(report==1){
    source("data_wrangling.R")
    rmarkdown::render("report.Rmd",
                      output_file = paste0(data_path,".html")
                      )
}else{
    if(save_data==1){
        source("data_wrangling.R")
    }
}
    

