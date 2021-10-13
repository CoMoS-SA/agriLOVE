#DECLARATIONS

#Four-dimensional arrays (cell level)
world               <- array(0, dim = c(x,y,time,mc))       # owners location
RDint               <- array(0, dim = c(x,y,time,mc))       # RD innovation expenses
cost                <- array(0, dim = c(x,y,time,mc))       # unit cost
profit              <- array(0, dim = c(x,y,time,mc))       # profits
output              <- array(0, dim = c(x,y,time,mc))       # output/yield
pot_output          <- array(0, dim = c(x,y,time,mc))       # output before flood
output_lost         <- array(0, dim = c(x,y,time,mc))       # output lost due to floof
sales               <- array(0, dim = c(x,y,time,mc))       # sales 
wealth              <- array(0, dim = c(x,y,time,mc))       # liquid assets
RD_scaling          <- array(0, dim = c(x,y,time,mc))       # RD innovation expenses compared to max spender
theta               <- array(0, dim = c(x,y,time,mc))       # land productivity
revenues            <- array(0, dim = c(x,y,time,mc))       # revenues
tot_cost            <- array(0, dim = c(x,y,time,mc))       # total costs
sales_growth        <- array(0, dim = c(x,y,time,mc))       
agri                <- array(0, dim = c(x,y,time,mc))       # type of agriculture employed
gain                <- array(0, dim = c(x,y,time,mc))       
loss                <- array(0, dim = c(x,y,time,mc))       
L                   <- array(0, dim = c(x,y,time,mc))       # labor
unfilled_demand     <- array(0, dim = c(x,y,time,mc))       # unfilled demand
IMM                 <- array(0, dim = c(x,y,time,mc))       # land productivity of imitated cell
IMint               <- array(0, dim = c(x,y,time,mc))       # RD imitation expenses
IM_scaling          <- array(0, dim = c(x,y,time,mc))       # RD imitation expenses compared to max spender
price_land          <- array(0, dim = c(x,y,time,mc))       # price of land
reborn              <- array(0, dim = c(x,y,time,mc))       
unshocked_price_land<- array(0, dim = c(x,y,time,mc))       # price of land before iid no-memory shock
lab_share_cost      <- array(0, dim = c(x,y,time,mc))       # labor share of total costs
land_share_cost     <- array(0, dim = c(x,y,time,mc))       # price of land share of total costs
pos_loss            <- array(0, dim = c(x,y,time,mc))       # position in the land degradation process
flood_shock         <- array(0, dim = c(x,y,time,mc))       # 1 if central position of flood
droughts_shock      <- array(0, dim = c(x,y,time,mc))
theta_lost          <- array(0, dim = c(x,y,time,mc))
theta_band          <- array(0, dim = c(x,y,time,mc))
tax          <- array(0, dim = c(x,y,time,mc))
subs          <- array(0, dim = c(x,y,time,mc))


mkt_share_growth<- array(0, dim = c(x,y,time,mc))

# Producer tri-dimensional arrays
p_pot_output        <- array(0, dim = c((x*y),time,mc))     # output before flood
p_output            <- array(0, dim = c((x*y),time,mc))     # output
p_cost              <- array(0, dim = c((x*y),time,mc))     # unit cost
p_sales             <- array(0, dim = c((x*y),time,mc))     # sales
p_revenues          <- array(0, dim = c((x*y),time,mc))     # revenues
p_tot_cost          <- array(0, dim = c((x*y),time,mc))     # total costs
p_profit            <- array(0, dim = c((x*y),time,mc))     # profits
p_wealth            <- array(0, dim = c((x*y),time,mc))     # liquid assets
existing_producers  <- array(0, dim = c((x*y),time,mc))     # equal to 1 if producer owns at least one property
p_sales_growth      <- array(0, dim = c((x*y),time,mc))     
mkt_share           <- array(0, dim = c((x*y),time,mc))     # market shares
p_unfilled_demand   <- array(0, dim = c((x*y),time,mc))     # unfilled demand
p_unfilled_demand_lab <- array(0, dim = c((x*y),time,mc))     # unfilled demand for labor
fitness             <- array(0, dim = c((x*y),time,mc))     # replicator dynamics fitness
cost_share_fitness  <- array(0, dim = c((x*y),time,mc))     # cost shares of fitness
n_property          <- array(0, dim = c((x*y),time,mc))     # number of owned cells
p_bankrupt          <- array(0, dim = c((x*y),time,mc))     # equal to 1 if producer goes bankrupt
p_L                 <- array(0, dim = c((x*y),time,mc))     # producer labor


#Vectors of lenght t
forests             <- array(0, dim = c(time,mc))           # number of forests
food_price          <- array(0, dim = c(time,mc))           # price of food bundle
active_producers    <- array(0, dim = c(time,mc))            
demand              <- array(0, dim = c(time,mc))           # demand
demand_noise        <- array(0, dim = c(time,mc))           # demand noise
wage                <- array(0, dim = c(time,mc))           # wage
bankrupt            <- array(0, dim = c(time,mc))           # number of bankrupts
mkt_iter            <- array(0, dim = c(time,mc))           # market algorithm iterations
supply              <- array(0, dim = c(time,mc))           # total supply
excess_demand       <- array(0, dim = c(time,mc))           # total demand minus total supply
innovators          <- array(0, dim = c(time,mc))           # number of cells succesfully innovating
imitators           <- array(0, dim = c(time,mc))           # number of cells succesfully imitating
num_floods          <- array(0, dim = c(time,mc))           # number of floods in each period
num_droughts        <- array(0, dim = c(time,mc)) 
forest_gone         <- array(0, dim = c(time,mc)) 
wastelands          <- array(0, dim = c(time,mc)) 
agri_share_def      <- array(0, dim = c(time,mc)) 
sum_tax             <- array(0, dim = c(time,mc))

#Counters
sales_counter       <- array(0, dim = mc)                   # sales counter
bankrupt_counter    <- array(0, dim = mc)                   # bankrupt counter
rebound_counter     <- array(0, dim = mc)                   # rebound counter
switch_counter      <- array(0, dim = mc)                   # switching agriculture counter
abandon_counter     <- array(0, dim = mc) 

#Miscellaneous
init_land                           <- array(0, dim = x*y)          # serves the initalizazion of ownerships
p_bidding                           <- array(0, dim = x*y)          # bidding producers in auctions
switcher                            <- array(0, dim = c((x*y),mc))
mkt_share_cell_on_sale              <- array(0, dim = c((x*y),2))   # market share moving matrix during auctions
colnames(mkt_share_cell_on_sale)    <- c("ms","sold")               # returns cell market share in first column, whether it has been sold (1) or must rebound (0) in the auction in the second column (local, not global environment) 
