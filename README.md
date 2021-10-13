# agriLOVE
Repository for agriLOVE model.

# Code structure
The file `main.R` contains the main loop structure of the model, and calls to several functions, each implementing a different task along the timeline of the events.

The file begins by running four auxiliary files, which contains preliminary operations that need to be performed before the simulation starts. The file `flags.R` contains the various flags which can be manipulated to turn on or off several features of the model (e.g. deforestation, conventional and sustainable farming, soil degradation, various types of initialization). In the file `declarations.R`, all the various arrays are created. For example, the array containing cell productivities is a 4-dimensional array: two dimensions indicating the position of each cell in the grid, one dimension indicating the point in time, one dimension indicating the Monte Carlo replication; the array containing producers wealth will then be, by the same logic, 3-dimensional. The file `parameters.R` contains a comprehensive list of all parameters in the model, where they can be tuned. Finally, the file `initialization.R` fill the various arrays with initial values, i.e. for `t=1`, along all Monte Carlo replications (initialization is constant across them). The file `spatial_initialization.R` contains special initialization procedures for several spatial scenarios, loaded from data present in the `aux_data` folder.

The main structure of the simulation is given by two nested loops: the inner one scrolls the instant of time, the outer one replicates each run of the model with a different seed for pseudo-random number generation, scrolling thus the various Monte Carlo replications. 
For each step in the time loop, the code replicate the following steps:

1. The function `preliminary_f()` (contained in the `preliminary.R` file) operates preliminary operations on arrays (e.g. update wages, land use arrays).
2. A loop is opened to scroll producers: for each of them, the function `hire_f()` (contained in the `hire.R` file) computes the desired labor force at the producer level and how to redistribute them across owned farms. The loop is closed.
3. Two nested loops are opened, one scrolling the horizontal dimension of land, one the vertical dimension, thus scrolling each cell in the grid. For each cell/farm, the function `rd_f()` (contained in the `rd.R` file) determines innovation and imitation outcomes, as well as within-farm learning. Soil degradation impact is computed if present. Soil productivities are updated, taking into account climate shocks generated in the previous time step, if present. Food production at cell is then level is computed. Both loops are closed. 
4. The function `production_f()` (contained in the `production.R` file) computes food production at the producer level, updates fitnesses and market shares for each producer. 
5. The function `market_f()` (contained in the `evol_mamrket.R` file) contains the market algorithm, through which sales are assigned to each producer given the current level of demand and their market shares. Food price is determined. 
6. Two nested loops scrolls the grid and the function `profit_f()` (contained in the `profit.R` file) computes profits and costs at the cell level. The loops are closed. Profits and costs are then computed at the producer level through the function `p_profit_f()` (contained in the `p_profit.R` file).
7. A loop is opened to scroll producers. For each of them, the function `switch_f()` (contained in the `switch.R` file) determines whether they change or not their agricultural technique. The function `entry_f()` (contained in the `entry.R` file) check whether each producer has negative wealth and, if any, perform auctions (see `auction.R` file) on each cell owned by them, reassigning new properties and updating net worths. Simpler entry/exit mechanisms are handled through the file `mean_ententry.R`. The loop is closed.
8. The function `weather_f()` (contained in the `climate_box.R` file) computes climate shocks output.
9. The function `deforestation_f()` (contained in the `deforestation.R` file) scroll existing forests and determines whether any of them is turned into arable land or not, updating all arrays relative to the newly established farm. Reforestation happens.

The file `data_wrangling.R` compresses and save data for further analysis. Automatic HTML reports are then generated through the markdown file `report.Rmd`. Finally, the folder `experiment analysis` contains codes for multiple experiments analysis, previously created through the automatic generator `auto.R`.

# Model's Feature
*Code Language* : R, C++ (under development)

*Compile* : R no need, C++ uses CMake

# Requirements
agriLOVE has no system specific requirements.