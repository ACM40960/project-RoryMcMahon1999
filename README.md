# Simulating Golf Tournaments


## Basic Overview
 - The Goal of this project is to build a model that will acurately predict golfers probabilities of winning events on the PGA Tour.
 - We will employ Monte Carlo Simulations to compute these probabilities.
 - The data we  use is scrapped from the [PGA Tour website](https://www.pgatour.com/stats.html)



## Instalation
 1) Install R studio (preferably, version >= 4.1.2)
 2) Install the necessary packages by running,
 ```sh
 install.packages("dplyr")
 install.packages("sn")
 ```
 
 3) Downlaod the R file "Golf_Monte_Carlo_Simulations.R" 
 4) Downlaod the "PGAtourdata.csv" file
 5) Set working Directory to to that of which the "PGAtourdata.csv" file is in 
 6) Now Run the "Golf_Monte_Carlo_Simulations.R" file 

## Overview of code

In our code we run 3 Simulations of Tournamnets. The first simulation involves simulating a tournament of 480 players at any course, the second involves simulating a tournament of 480 players at Augusta National and the third involves simulating The Memorial Tournament at Muirfield Golf Course with the specified 117 players in the field. 
 - The data is "set up" in order to run our Monte Carlo simulations from lines 4-116
 - The first Simulation is run from lines 142-275
 - The second Simulation is run from lines 289-435
 - Finally the third simulation is run from lines 444-600

Altough the simulations are ran for for 1000 simulations they are set to 25 simulations in the code so that it is quicker to run. In total the code takes ~ 22.6 seconds to run. For each of the 3 different scenarios the number of simulations to run can be changed on  lines 142,307 and 472 respectively for each scenario.

For the second simulation the course can be changed on line 290

For the Third Simulation the field/players can be changed on line 461

The results are ordered by probability of winning the tournament in the dataframes positionsordered,positionsordered_C and positionsordered_F

## Conclusion
 From the code in the R file one can Predict the probabilty of a player winning a tournament, finishing in the Top 5 and Finishing in the Top 10. The code can also be altered to add new factors into the simulations rather easily.
 
## Author 
 Rory Mcmahon(17371086)










