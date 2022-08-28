# Plu price script

Best run from the command line - output is one A4 PDF file

Bit of a vomit script and nothing in the way of error catching so it may break and this is not financial advice.

The script requires 14 input arguments:

`Rscript plutus.R START_YEAR START_MONTH START_DAY END_YEAR END_MONTH END_DAY OUTPUT_PATH Starting_Price_of_PLU Monthly_Spend Plu_Price_Scenario Scenario_Multiplier(if_relevant) SPREAD GAS WITHDRAW_FEE`

The allowed PLU price scenarios are:

`viable_scenarios <- c("constant_price","linear_decrease","linear_increase","price_drop_next_day","price_drop_next_day_then_decreases_99","decreases_then_increases","increases_then_decreases")
`

some of these respond to the scenario_modifier arguments

`constant_price` = PLU value stays the same (scenario_multiplier irrelevant)

`linear_decrease \ linear_increase` PLU linearly evolves over time to an end value (controlled by `Scenario_Multiplier`), so a start price of `6` and a scenario modifier of `3` under `linear_increase` (`linear_decrease`) will result in a end price of `18` (i.e. 6 * 3) (`2`, i.e. 6 / 3), reached at the end of the time given by `END_YEAR END_MONTH END_DAY`

`price_drop_next_day` = PLU drops in price by an amount controlled by the scenario_multiplier on Day 2 of the simulation and stays at that price throughout

`price_drop_next_day_then_decreases_99`= as above but PLU drops to 99% of the starting price over time after the day 2 collaspe 

`decreases_then_increases \ increases_then_decreases` PLU linearly decreases/increases to an amount controlled by the scenario_multiplier at halfway through the simulation then reverts to the base price by the end day

So to run a simulation for constant price, from 1st August 2022 to Dec 31st 2024, spending £1250 a month, assuming an exchange spread of 3%, £10 gas fee and £15 platform withdrawal fee, outputing the pdf in your downloads folder, change directory to where Plutus.R is and

`Rscript plutus.R 2022 8 1 2024 12 31 ~/Downloads 6 1250 constant_price 5 1.03 10 15`

(the scenario multiplier above is 5 but it does nothing for the constant price scenario). The spread is given as 1.03 == 3%, so 1.05 would be 5% etc... The gas fee might reduce when Polygon solution gets implemented, and the platform withdrawal fee you'd think would go when that happens too, but who knows. 

R dependencies have to be installed, which are:

`install.packages("imputeTS","ggpubr","cowplot","tidyverse")`

All subscription models and staking levels are assessed. 

Subscription costs, gas and platforms fees are taken into account for total P&L

e.g. Assess PLU increasing in price from 2 to 20 times 

`for i in {2..20}; do Rscript plutus.R 2022 8 1 2024 12 31 ~/Downloads 6 1250 linear_increase ${i} 1.03 10 15; done`

if you have GNU_Parallel, you could assess multiple things at once

`parallel Rscript plutus.R 2022 8 1 {1} 12 31 ~/Downloads 6 {2} linear_increase {3} 1.02 5 15 ::: {2023..2030} ::: {500..2500..500} ::: {2..10}`

would generate 360 files (8 years times * 5 price points * 9 modifiers)

