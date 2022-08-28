################################################################################
## Input Arguments
################################################################################

# these are the dependencies
##install.packages("imputeTS","ggpubr","cowplot","tidyverse")

in_args <- commandArgs(trailingOnly = T)
start_year <- as.numeric(in_args[1])
start_month <- as.numeric(in_args[2])
start_day <- as.numeric(in_args[3])
end_year <- as.numeric(in_args[4])
end_month <- as.numeric(in_args[5])
end_day <- as.numeric(in_args[6])
output_path <- as.character(in_args[7])
base_price <- as.numeric(in_args[8])
monthly_spend <- as.numeric(in_args[9])
scenario <- tolower(as.character(in_args[10]))
scenario_multiplier <- as.numeric(in_args[11])
spread <- as.numeric(in_args[12]) # % spread/margin on exchange buying and selling
gas <- as.numeric(in_args[13]) # exchange to wallet incurs a fee each way, £1.50 to £20 realistic
wfee <- as.numeric(in_args[14]) # Currently £15 fee to get PLU off app to wallet
library(tidyverse)
library(ggpubr)

#in_args <- c("2022","7","1","2023","12","31","~/Downloads","6","500","constant_price","8","1.03","10","15","15")
#in_args <- c("2022","7","1","2023","12","31","~/Downloads","6","750","increases_then_decreases","2","1.03","15","15")
#in_args <- c("2022","7","1","2024","12","31","~/Downloads","6","1250","price_drop_next_day_then_decreases_99","5","1.03","10","15")


################################################################################
## some constraints
################################################################################

if (scenario_multiplier <=0) {
  message("For the scenario multiplier, please enter a number >0 and 100\nFor example, 0.5 for a 50% decrease or 2 for a 50% increase")
  quit("no")}

if (spread < 1 | spread > 1.1) {
  message("For the exchange spread (their markup) realistically it's not going to be above a few percent,\nso please enter a value between 1 (no spread) and 1.10 (10%)")
  quit("no")}

if (monthly_spend <1 | monthly_spend > 22500) {
  message("Monthly spend must be between 1 and 22500 (cashback limit)")
  quit("no")}

if (base_price <= 0 | base_price > 100) {
  message("Monthly spend must be between 1 and 22500 (cashback limit)")
quit("no")}

viable_scenarios <- c("constant_price","linear_decrease","linear_increase","price_drop_next_day","price_drop_next_day_then_decreases_99","decreases_then_increases","increases_then_decreases")
if (!(scenario %in% viable_scenarios)) {message("The scenario you entered is not a viable option, please input one of:\n",paste(viable_scenarios,collapse = "\n"))
  quit("no")} 

################################################################################
## Setup Date
################################################################################

start_day <- lubridate::make_date(start_year,start_month,start_day)
end_day <- lubridate::make_date(end_year,end_month,end_day)
df <- data.frame(date=seq(as.Date(start_day), as.Date(end_day), by = "day"))
df$yr <- lubridate::year(df$date)
df$mo <- lubridate::month(df$date)
df$dy <- lubridate::day(df$date)

################################################################################
## Subscription Costs - every 30 days
################################################################################

thirty_day_period <- rep(seq(1,30),((end_day-start_day)/30)+1)
df$thirty_day_period <- c(0,thirty_day_period[1:nrow(df)-1])
df$subscription_everyday <- ifelse(df$thirty_day_period==30 | df$thirty_day_period==0,4.99,0)
df$subscription_premium <- ifelse(df$thirty_day_period==30 | df$thirty_day_period==0,14.99,0)
df$subscription_everday_cumsum <- cumsum(df$subscription_everyday)
df$subscription_premium_cumsum <- cumsum(df$subscription_premium)
rm(thirty_day_period)

################################################################################
## What Plu Price Scenario
################################################################################

if (scenario == "constant_price") {
  message("\nUsing a constant PLU price throughout of: £",base_price,"\n")
  df$plu_price[1] <- base_price
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("Using a constant PLU price throughout of: £%s",base_price)))  
} else if (scenario == "linear_decrease") {
  df$plu_price <- NA_real_
  df$plu_price[1] <- base_price
  df$plu_price[nrow(df)] <- base_price / ifelse(scenario_multiplier<1,1/scenario_multiplier,scenario_multiplier) ## forces desired scenario if multiplier incorrect
  df$plu_price <- imputeTS::na_interpolation(df$plu_price)
  message("\nPlu linearly decreases from £",base_price," on ",start_day," to £",df$plu_price[nrow(df)]," on ",end_day,"\n")
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("PLU linearly decreases from £%s to £%s",base_price,df$plu_price[nrow(df)])))
} else if (scenario == "linear_increase") {
  df$plu_price <- NA_real_
  df$plu_price[1] <- base_price
  df$plu_price[nrow(df)] <- base_price * ifelse(scenario_multiplier<1,1/scenario_multiplier,scenario_multiplier) ## forces desired scenario if multiplier incorrect
  df$plu_price <- imputeTS::na_interpolation(df$plu_price)
  message("\nPlu linearly increases from £",base_price," on ",start_day," to £",df$plu_price[nrow(df)]," on ",end_day,"\n")
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("PLU linearly increases from £%s to £%s",base_price,df$plu_price[nrow(df)])))
  } else if (scenario == "price_drop_next_day") {
  df$plu_price <- NA_real_
  df$plu_price[1] <- base_price
  df$plu_price[2:nrow(df)] <- base_price / scenario_multiplier
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("PLU drops from £%s to £%s the day after you buy and stays there",base_price,df$plu_price[nrow(df)])))
} else if (scenario == "price_drop_next_day_then_decreases_99") {
  df$plu_price <- NA_real_
  df$plu_price[1] <- base_price
  df$plu_price[2:nrow(df)] <- base_price / scenario_multiplier
  df$plu_price[3:nrow(df)] <- NA_real_
  df$plu_price[nrow(df)] <- base_price * 0.01
  df$plu_price <- imputeTS::na_interpolation(df$plu_price)
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("PLU drops from £%s to £%s the day after you buy and depreciates to 1%s (£%s)",base_price,df$plu_price[2],"%",df$plu_price[nrow(df)])))
} else if (scenario == "decreases_then_increases") {
  df$plu_price <- NA_real_
  df$plu_price[1] <- base_price
  df$plu_price[floor(nrow(df)/2)] <- base_price * ifelse(scenario_multiplier>1,1/scenario_multiplier,scenario_multiplier) ## forces desired scenario if multiplier incorrect
  df$plu_price[nrow(df)] <- base_price
  df$plu_price <- imputeTS::na_interpolation(df$plu_price)
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("PLU increases from £%s to £%s and then back down again",base_price,df$plu_price[floor(nrow(df)/2)])))
} else if (scenario == "increases_then_decreases") {
  df$plu_price <- NA_real_
  df$plu_price[1] <- base_price
  df$plu_price[floor(nrow(df)/2)] <- base_price * ifelse(scenario_multiplier<1,1/scenario_multiplier,scenario_multiplier) ## forces desired scenario if multiplier incorrect
  df$plu_price[nrow(df)] <- base_price
  df$plu_price <- imputeTS::na_interpolation(df$plu_price)
  scen_msg <- cowplot::get_title(ggplot() + labs(title=sprintf("PLU decreases from £%s to £%s and then back up again",base_price,df$plu_price[floor(nrow(df)/2)])))
  }

################################################################################
## Daily Spend - constant for now as a function of month
################################################################################

df$daily_spend <- monthly_spend / lubridate::days_in_month(df$date)

## Cashback required in £ depending on tier
df$cashback_in_pounds_3 <- (df$daily_spend / 100) * 3
df$cashback_in_pounds_4 <- (df$daily_spend / 100) * 4
df$cashback_in_pounds_5 <- (df$daily_spend / 100) * 5
df$cashback_in_pounds_6 <- (df$daily_spend / 100) * 6
df$cashback_in_pounds_8 <- (df$daily_spend / 100) * 8
cashback_month <- do.call (rbind, by( df,list(df$yr,df$mo),function(x){y <- data.frame(date=x$date,monthly_total_spend=cumsum(x$daily_spend))}))
df <- merge(df,cashback_month,by="date")

################################################################################
# add threshold limit for cashback - naive but close enough 
################################################################################

df$threshold_basic <- ifelse(df$monthly_total_spend > 250,0,1)
df$threshold_everyday <- ifelse(df$monthly_total_spend > 2000,0,1)
df$threshold_premium <- ifelse(df$monthly_total_spend > 22500,0,1)

################################################################################
## add plu earned via cashback for different staking tiers
################################################################################

df$plu_earned_3_premium <- cumsum((df$cashback_in_pounds_3 / df$plu_price) * df$threshold_premium)
df$plu_earned_4_premium <- cumsum((df$cashback_in_pounds_4 / df$plu_price) * df$threshold_premium)
df$plu_earned_5_premium <- cumsum((df$cashback_in_pounds_5 / df$plu_price) * df$threshold_premium)
df$plu_earned_6_premium <- cumsum((df$cashback_in_pounds_6 / df$plu_price) * df$threshold_premium)
df$plu_earned_8_premium <- cumsum((df$cashback_in_pounds_8 / df$plu_price) * df$threshold_premium)

df$plu_earned_3_everyday <- cumsum((df$cashback_in_pounds_3 / df$plu_price) * df$threshold_everyday)
df$plu_earned_4_everyday <- cumsum((df$cashback_in_pounds_4 / df$plu_price) * df$threshold_everyday)
df$plu_earned_5_everyday <- cumsum((df$cashback_in_pounds_5 / df$plu_price) * df$threshold_everyday)
df$plu_earned_6_everyday <- cumsum((df$cashback_in_pounds_6 / df$plu_price) * df$threshold_everyday)
df$plu_earned_8_everyday <- cumsum((df$cashback_in_pounds_8 / df$plu_price) * df$threshold_everyday)

df$plu_earned_3_basic <- cumsum((df$cashback_in_pounds_3 / df$plu_price) * df$threshold_basic)
df$plu_earned_4_basic <- cumsum((df$cashback_in_pounds_4 / df$plu_price) * df$threshold_basic)
df$plu_earned_5_basic <- cumsum((df$cashback_in_pounds_5 / df$plu_price) * df$threshold_basic)
df$plu_earned_6_basic <- cumsum((df$cashback_in_pounds_6 / df$plu_price) * df$threshold_basic)
df$plu_earned_8_basic <- cumsum((df$cashback_in_pounds_8 / df$plu_price) * df$threshold_basic)

################################################################################
## add the plu rebate - for simplicity gives it in middle of month
################################################################################

df$plu_rebate1 <- ifelse(df$dy==15,10/df$plu_price,0)
df$plu_rebate2 <- ifelse(df$dy==15,20/df$plu_price,0)
df$plu_rebate3 <- ifelse(df$dy==15,30/df$plu_price,0)
df$plu_rebate4 <- ifelse(df$dy==15,40/df$plu_price,0)
df$plu_rebate5 <- ifelse(df$dy==15,50/df$plu_price,0)
df$plu_rebate6 <- ifelse(df$dy==15,60/df$plu_price,0)
df$plu_rebate8 <- ifelse(df$dy==15,80/df$plu_price,0)

df$plu_rebate1_cumsum <- cumsum(df$plu_rebate1)
df$plu_rebate2_cumsum <- cumsum(df$plu_rebate2)
df$plu_rebate3_cumsum <- cumsum(df$plu_rebate3)
df$plu_rebate4_cumsum <- cumsum(df$plu_rebate4)
df$plu_rebate5_cumsum <- cumsum(df$plu_rebate5)
df$plu_rebate6_cumsum <- cumsum(df$plu_rebate6)
df$plu_rebate8_cumsum <- cumsum(df$plu_rebate8)

################################################################################
## how much PLU have we accumulated
################################################################################

df$total_accumulated_plu_nostake_basic <- df$plu_rebate1_cumsum + df$plu_earned_3_basic
df$total_accumulated_plu_nostake_everyday <- df$plu_rebate2_cumsum + df$plu_earned_3_everyday
df$total_accumulated_plu_nostake_premium <- df$plu_rebate3_cumsum + df$plu_earned_3_premium
df$total_accumulated_plu_hero_basic <- df$plu_rebate4_cumsum + df$plu_earned_4_basic
df$total_accumulated_plu_hero_everyday <- df$plu_rebate4_cumsum + df$plu_earned_4_everyday
df$total_accumulated_plu_hero_premium <- df$plu_rebate4_cumsum + df$plu_earned_4_premium
df$total_accumulated_plu_veteran_basic <- df$plu_rebate5_cumsum + df$plu_earned_5_basic
df$total_accumulated_plu_veteran_everyday <- df$plu_rebate5_cumsum + df$plu_earned_5_everyday
df$total_accumulated_plu_veteran_premium <- df$plu_rebate5_cumsum + df$plu_earned_5_premium
df$total_accumulated_plu_legend_basic <- df$plu_rebate6_cumsum + df$plu_earned_6_basic
df$total_accumulated_plu_legend_everyday <- df$plu_rebate6_cumsum + df$plu_earned_6_everyday
df$total_accumulated_plu_legend_premium <- df$plu_rebate6_cumsum + df$plu_earned_6_premium
df$total_accumulated_plu_goat_basic <- df$plu_rebate8_cumsum + df$plu_earned_8_basic
df$total_accumulated_plu_goat_everyday <- df$plu_rebate8_cumsum + df$plu_earned_8_everyday
df$total_accumulated_plu_goat_premium <- df$plu_rebate8_cumsum + df$plu_earned_8_premium

################################################################################
## what is the overall cost of the subscription over the time period
################################################################################

subscription_fiat_cost_basic <- 0
subscription_fiat_cost_everday <- df$subscription_everday_cumsum[nrow(df)]
subscription_fiat_cost_premium <- df$subscription_premium_cumsum[nrow(df)]

################################################################################
## cost of staking including exchange spread/margin and gas
## not assuming anything about polygon L2 solution/price
################################################################################

hero_entry_cost <- ((base_price * 250) * spread) + gas
hero_exit_cost <- ((df$plu_price[nrow(df)] * 250) * (2-spread)) - gas
veteran_entry_cost <- ((base_price * 500) * spread) + gas
veteran_exit_cost <- ((df$plu_price[nrow(df)] * 500) * (2-spread)) - gas
legend_entry_cost <- ((base_price * 1000) * spread) + gas
legend_exit_cost <- ((df$plu_price[nrow(df)] * 1000) * (2-spread)) - gas
goat_entry_cost <- ((base_price * 2000) * spread) + gas
goat_exit_cost <- ((df$plu_price[nrow(df)] * 2000) * (2-spread)) - gas

################################################################################
## total overall profit £

## first, what is the total amount of PLU we can withdraw from the ecosystem, 
## we have the value of the stake at the time of exit and
## Accumulated GBP equivalent = price of plu at end * number of plu (minus platform withdraw fee)
## then accumulated PLU is with the stake in your wallet and you'd pay 
## 1 * gas fee to withdraw this and the spread on everything
## this is put in the revenue variables 

## then we subtract the 'entry' cost of the initial stake and subscription fee
## then we have total profit/loss and % equivalent
################################################################################

total_revenue_nostake_basic <- round(((df$total_accumulated_plu_nostake_basic[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread) + gas,0)
total_revenue_nostake_everyday <- round(((df$total_accumulated_plu_nostake_everyday[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread) + gas,0)
total_revenue_nostake_premium <- round(((df$total_accumulated_plu_nostake_premium[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread) + gas,0)
total_revenue_hero_basic <- round(((df$total_accumulated_plu_hero_basic[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + hero_exit_cost,0)
total_revenue_hero_everyday <- round(((df$total_accumulated_plu_hero_everyday[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + hero_exit_cost,0)
total_revenue_hero_premium <- round(((df$total_accumulated_plu_hero_premium[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + hero_exit_cost,0)
total_revenue_veteran_basic <- round(((df$total_accumulated_plu_veteran_basic[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + veteran_exit_cost,0)
total_revenue_veteran_everyday <- round(((df$total_accumulated_plu_veteran_everyday[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread) + veteran_exit_cost,0)
total_revenue_veteran_premium <- round(((df$total_accumulated_plu_veteran_premium[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + veteran_exit_cost,0)
total_revenue_legend_basic <- round(((df$total_accumulated_plu_legend_basic[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + legend_exit_cost,0)
total_revenue_legend_everyday <- round(((df$total_accumulated_plu_legend_everyday[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + legend_exit_cost,0)
total_revenue_legend_premium <- round(((df$total_accumulated_plu_legend_premium[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + legend_exit_cost,0)
total_revenue_goat_basic <- round(((df$total_accumulated_plu_goat_basic[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + goat_exit_cost,0)
total_revenue_goat_everyday <- round(((df$total_accumulated_plu_goat_everyday[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread)  + goat_exit_cost,0)
total_revenue_goat_premium <- round(((df$total_accumulated_plu_goat_premium[nrow(df)] * df$plu_price[nrow(df)]) - wfee) * (2 - spread) + goat_exit_cost,0)

total_offset_nostake_basic <- 0
total_offset_nostake_everyday <- round(subscription_fiat_cost_everday,0)
total_offset_nostake_premium <- round(subscription_fiat_cost_premium,0)
total_offset_hero_basic <- round(subscription_fiat_cost_basic + hero_entry_cost,0)
total_offset_hero_everyday <- round(subscription_fiat_cost_everday + hero_entry_cost,0)
total_offset_hero_premium <- round(subscription_fiat_cost_premium + hero_entry_cost,0)
total_offset_veteran_basic <- round(subscription_fiat_cost_basic + veteran_entry_cost,0)
total_offset_veteran_everyday <- round(subscription_fiat_cost_everday + veteran_entry_cost,0)
total_offset_veteran_premium <- round(subscription_fiat_cost_premium + veteran_entry_cost,0)
total_offset_legend_basic <- round(subscription_fiat_cost_basic + legend_entry_cost,0)
total_offset_legend_everyday <- round(subscription_fiat_cost_everday + legend_entry_cost,0)
total_offset_legend_premium <- round(subscription_fiat_cost_premium + legend_entry_cost,0)
total_offset_goat_basic <- round(subscription_fiat_cost_basic + goat_entry_cost,0)
total_offset_goat_everyday <- round(subscription_fiat_cost_everday + goat_entry_cost,0)
total_offset_goat_premium <- round(subscription_fiat_cost_premium + goat_entry_cost,0)

total_profit_nostake_basic <- total_revenue_nostake_basic - total_offset_nostake_basic
total_profit_nostake_everyday <- total_revenue_nostake_everyday - total_offset_nostake_everyday
total_profit_nostake_premium <- total_revenue_nostake_premium - total_offset_nostake_premium
total_profit_hero_basic <- total_revenue_hero_basic - total_offset_hero_basic
total_profit_hero_everyday <- total_revenue_hero_everyday - total_offset_hero_everyday
total_profit_hero_premium <- total_revenue_hero_premium - total_offset_hero_premium
total_profit_veteran_basic <-  total_revenue_veteran_basic - total_offset_veteran_basic
total_profit_veteran_everyday <- total_revenue_veteran_everyday - total_offset_veteran_everyday
total_profit_veteran_premium <- total_revenue_veteran_premium - total_offset_veteran_premium
total_profit_legend_basic <-  total_revenue_legend_basic - total_offset_legend_basic
total_profit_legend_everyday <- total_revenue_legend_everyday- total_offset_legend_everyday
total_profit_legend_premium <- total_revenue_legend_premium - total_offset_legend_premium
total_profit_goat_basic <-  total_revenue_goat_basic - total_offset_goat_basic
total_profit_goat_everyday <- total_revenue_goat_everyday - total_offset_goat_everyday
total_profit_goat_premium <- total_revenue_goat_premium - total_offset_goat_premium

## total overall profit %

total_profit_nostake_basic_percent <- Inf
total_profit_nostake_everyday_percent <- round((total_revenue_nostake_everyday / total_offset_nostake_everyday) * 100,0)
total_profit_nostake_premium_percent <- round((total_revenue_nostake_premium / total_offset_nostake_premium) * 100,0)
total_profit_hero_basic_percent <- round((total_revenue_hero_basic / total_offset_hero_basic) * 100,0)
total_profit_hero_everyday_percent <- round((total_revenue_hero_everyday / total_offset_hero_everyday) * 100,0)
total_profit_hero_premium_percent <- round((total_revenue_hero_premium / total_offset_hero_premium) * 100,0)
total_profit_veteran_basic_percent <- round((total_revenue_veteran_basic / total_offset_veteran_basic) * 100,0)
total_profit_veteran_everyday_percent <- round((total_revenue_veteran_everyday / total_offset_veteran_everyday) * 100,0)
total_profit_veteran_premium_percent <- round((total_revenue_veteran_premium / total_offset_veteran_premium) * 100,0)
total_profit_legend_basic_percent <- round((total_revenue_legend_basic / total_offset_legend_basic) * 100,0)
total_profit_legend_everyday_percent <- round((total_revenue_legend_everyday / total_offset_legend_everyday) * 100,0)
total_profit_legend_premium_percent <-  round((total_revenue_legend_premium / total_offset_legend_premium) * 100,0)
total_profit_goat_basic_percent <- round((total_revenue_goat_basic / total_offset_goat_basic) * 100,0)
total_profit_goat_everyday_percent <- round((total_revenue_goat_everyday / total_offset_goat_everyday) * 100,0)
total_profit_goat_premium_percent <-  round((total_revenue_goat_premium / total_offset_goat_premium) * 100,0)
  
## generate output for dashboard, start with profit/loss table
prof_table_row1 <- c(sprintf("%s£%s (%s%s)",ifelse(total_profit_nostake_basic >0,"+","-"),abs(total_profit_nostake_basic),total_profit_nostake_basic_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_nostake_everyday >0,"+","-"),abs(total_profit_nostake_everyday),total_profit_nostake_everyday_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_nostake_premium >0,"+","-"),abs(total_profit_nostake_premium),total_profit_nostake_premium_percent,"%"))
prof_table_row2 <- c(sprintf("%s£%s (%s%s)",ifelse(total_profit_hero_basic >0,"+","-"),abs(total_profit_hero_basic),total_profit_hero_basic_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_hero_everyday >0,"+","-"),abs(total_profit_hero_everyday),total_profit_hero_everyday_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_hero_premium >0,"+","-"),abs(total_profit_hero_premium),total_profit_hero_premium_percent,"%"))
prof_table_row3 <- c(sprintf("%s£%s (%s%s)",ifelse(total_profit_veteran_basic >0,"+","-"),abs(total_profit_veteran_basic),total_profit_veteran_basic_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_veteran_everyday >0,"+","-"),abs(total_profit_veteran_everyday),total_profit_veteran_everyday_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_veteran_premium >0,"+","-"),abs(total_profit_veteran_premium),total_profit_veteran_premium_percent,"%"))
prof_table_row4 <- c(sprintf("%s£%s (%s%s)",ifelse(total_profit_legend_basic >0,"+","-"),abs(total_profit_legend_basic),total_profit_legend_basic_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_legend_everyday >0,"+","-"),abs(total_profit_legend_everyday),total_profit_legend_everyday_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_legend_premium >0,"+","-"),abs(total_profit_legend_premium),total_profit_legend_premium_percent,"%"))
prof_table_row5 <- c(sprintf("%s£%s (%s%s)",ifelse(total_profit_goat_basic >0,"+","-"),abs(total_profit_goat_basic),total_profit_goat_basic_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_goat_everyday >0,"+","-"),abs(total_profit_goat_everyday),total_profit_goat_everyday_percent,"%"),sprintf("%s£%s (%s%s)",ifelse(total_profit_goat_premium >0,"+","-"),abs(total_profit_goat_premium),total_profit_goat_premium_percent,"%"))
tab_out <- data.frame(prof_table_row1,prof_table_row2,prof_table_row3,prof_table_row4,prof_table_row5)
colnames(tab_out) <- c("No Stake","Hero","Veteran","Legend","Goat")
rownames(tab_out) <- c("Basic","Everyday","Premium")
t1 <- ggpubr::ggtexttable(tab_out) %>% tab_add_footnote(text = "Total Profit assumes withdrawing from the PLU ecosystem on final day, taking all accumulated tokens and stake back.\nThis action costs £15 (withdraw to wallet fee), gas fee (wallet to exchange) and the spread on the exchange.\nSubscription costs and spread on the intial purchase are subtracted.", size = 8, face = "italic") %>% tab_add_title(text=sprintf("Overall Profit / Loss Table | £%s monthly spend over the period %s to %s\nThe percent values are all relative to the initial investment",monthly_spend,start_day,end_day),size = "10")
## time series of earned plu
df_long <- reshape2::melt(df[,c(1,50:64)],id="date")
df_long$stake <- dplyr::case_when(grepl("nostake",df_long$variable) ~ "No Stake",
                                  grepl("hero",df_long$variable) ~ "Hero",
                                  grepl("veteran",df_long$variable) ~ "Veteran",
                                  grepl("legend",df_long$variable) ~ "Legend",
                                  grepl("goat",df_long$variable) ~ "GOAT")

df_long$stake <- factor(df_long$stake,levels = c("No Stake","Hero","Veteran","Legend","GOAT"))
df_long$subscription <- dplyr::case_when(grepl("basic",df_long$variable) ~ "Basic",
                                  grepl("everyday",df_long$variable) ~ "EveryDay",
                                  grepl("premium",df_long$variable) ~ "Premium")

p2 <- ggplot(df_long,aes(x=date,y=value,colour=stake)) + geom_line() + facet_wrap(~subscription) + labs(colour="Stake",lty="Subscription",x="",y="Acummulated Plu",title="Total Acquired PLU (ignores 'staked' amount)") + theme_pubclean() + theme(legend.position = "right")

p0 <- ggplot() + geom_line(aes(x=df$date,y=df$plu_price),col="DarkRed") + labs(x="Date",y="Plu Price GBP") + theme_pubclean()

tab_out2 <- data.frame(matrix(data=c(sprintf("£%s (£%s)",round(hero_entry_cost,0),round(base_price * 250,0)),sprintf("£%s (£%s)",round(veteran_entry_cost,0),round(base_price * 500,0)),sprintf("£%s (£%s)",round(legend_entry_cost,0),round(base_price * 1000,0)),sprintf("£%s (£%s)",round(goat_entry_cost,0),round(base_price * 2000,0)),sprintf("£%s (£%s)",round(hero_exit_cost,0),round(df$plu_price[nrow(df)] * 250,0)),sprintf("£%s (£%s)",round(veteran_exit_cost,0),round(df$plu_price[nrow(df)] * 500,0)),sprintf("£%s (£%s)",round(legend_exit_cost,0),round(df$plu_price[nrow(df)] * 1000,0)),sprintf("£%s (£%s)",round(goat_exit_cost,0),round(df$plu_price[nrow(df)] * 2000,0))),4,2))
colnames(tab_out2) <- c("Entry Cost","Exit Value")
rownames(tab_out2) <- c("Hero","Veteran","Legend","GOAT")
t2 <- ggtexttable(tab_out2) %>% tab_add_footnote(text = sprintf("Includes gas @ £%s and spread @ %s %s\n(True Value in Brackets)",gas,round(spread%%1*100,2),"%"), size = 8, face = "italic") 

################################################################################
## decided to add in a portfolio value over time graph to the output
################################################################################

df$portfolio_worth_nostake_basic <- df$total_accumulated_plu_nostake_basic * df$plu_price
df$portfolio_worth_nostake_everyday <- df$total_accumulated_plu_nostake_everyday * df$plu_price
df$portfolio_worth_nostake_premium <- df$total_accumulated_plu_nostake_premium * df$plu_price

df$portfolio_worth_hero_basic <- (df$total_accumulated_plu_hero_basic + 250) * df$plu_price
df$portfolio_worth_hero_everyday <- (df$total_accumulated_plu_hero_everyday + 250) * df$plu_price
df$portfolio_worth_hero_premium <- (df$total_accumulated_plu_hero_premium + 250) * df$plu_price

df$portfolio_worth_veteran_basic <- (df$total_accumulated_plu_veteran_basic + 500) * df$plu_price
df$portfolio_worth_veteran_everyday <- (df$total_accumulated_plu_veteran_everyday + 500) * df$plu_price
df$portfolio_worth_veteran_premium <- (df$total_accumulated_plu_veteran_premium + 500) * df$plu_price

df$portfolio_worth_legend_basic <- (df$total_accumulated_plu_legend_basic + 1000) * df$plu_price
df$portfolio_worth_legend_everyday <- (df$total_accumulated_plu_legend_everyday + 1000) * df$plu_price
df$portfolio_worth_legend_premium <- (df$total_accumulated_plu_legend_premium + 1000) * df$plu_price

df$portfolio_worth_goat_basic <- (df$total_accumulated_plu_goat_basic + 2000) * df$plu_price
df$portfolio_worth_goat_everyday <- (df$total_accumulated_plu_goat_everyday + 2000) * df$plu_price
df$portfolio_worth_goat_premium <- (df$total_accumulated_plu_goat_premium + 2000) * df$plu_price

df_long2 <- reshape2::melt(df[,c(1,65:79)],id="date")
df_long2$stake <- dplyr::case_when(grepl("nostake",df_long2$variable) ~ "No Stake",
                                  grepl("hero",df_long2$variable) ~ "Hero",
                                  grepl("veteran",df_long2$variable) ~ "Veteran",
                                  grepl("legend",df_long2$variable) ~ "Legend",
                                  grepl("goat",df_long2$variable) ~ "GOAT")

df_long2$stake <- factor(df_long2$stake,levels = c("No Stake","Hero","Veteran","Legend","GOAT"))
df_long2$subscription <- dplyr::case_when(grepl("basic",df_long2$variable) ~ "Basic",
                                         grepl("everyday",df_long2$variable) ~ "EveryDay",
                                         grepl("premium",df_long2$variable) ~ "Premium")

p3 <- ggplot(df_long2,aes(x=date,y=value,colour=stake)) + geom_line() + facet_wrap(~subscription) + labs(colour="Stake",lty="Subscription",x="Date",y="Portfolio Value (£)",title="Overall Portfolio Value (pure PLU-GBP equivalent - ignores gas etc)") + theme_pubclean() + theme(legend.position = "right")

##


output_filename <- sprintf("%s/PLU_start_date_%s_end_date_%s_baseprice_%s_endprice_%s_monthly_spend_%s_%s_scenario_multiplier_if_relevant_%s_spread_%s.pdf",output_path,start_day,end_day,base_price,df$plu_price[nrow(df)],monthly_spend,scenario,scenario_multiplier,spread)
ggsave(output_filename,device = "pdf",plot = cowplot::plot_grid(scen_msg,cowplot::plot_grid(p0,t2,nrow=1),t1,p2,p3,ncol=1,rel_heights = c(0.05,0.2,0.25,0.225,0.225)),width=8,height=11)

# 
# df_area1 <- data.frame(date=df$date,plu=df$plu_earned_3_everyday)
# df_area1b <- data.frame(date=df$date,plu=df$plu_rebate2_cumsum )
# df_area1b$tag <- "Rebate"
# df_area1$tag <- "Cashback"
# df_area <- dplyr::bind_rows(df_area1,df_area1b)
# ggplot(df_area,aes(x=date,y=plu,fill=tag)) + geom_area()
# 
# df_area1 <- data.frame(date=df$date,plu=df$plu_earned_4_everyday)
# df_area1b <- data.frame(date=df$date,plu=df$plu_rebate4_cumsum )
# df_area1b$tag <- "Rebate"
# df_area1$tag <- "Cashback"
# df_area <- dplyr::bind_rows(df_area1,df_area1b)
# ggplot(df_area,aes(x=date,y=plu,fill=tag)) + geom_area()
# 
# 
