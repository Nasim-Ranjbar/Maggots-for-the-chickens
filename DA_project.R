library(decisionSupport)

# Make variables

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("data/DA_project.csv")))

DA_function <- function(x, varnames) {
  
  ## Calculate number day-old chicks for 12 years 
  # buy 3 batch first year, 1 batch next year, and no purchase on third year
  yearly_chick <- numeric(n_year)
  
  i <- 1
  for (i in 1:n_year) {
    year_in_cycle <- (i - 1) %% 3 + 1
    
    if (year_in_cycle == 1) {
      yearly_chick[i] <- number_chicks_per_batch * 3
    } else if (year_in_cycle == 2) {
      yearly_chick[i] <- number_chicks_per_batch * 1
    } else {
      yearly_chick[i] <- 0
    }
  }
  
  yearly_chick 
  
  # Calculate yearly chick costs
  yearly_chick_cost <- yearly_chick * cost_per_day_old_chick
  
  # Calculate cost of yearly broiler chicks for festive season
  yearly_festive_chick_cost <- (number_yearly_festive_sale *
                                  number_broiler_chick_per_festive *
                                  cost_per_festive_broiler_chick)
  
  
  # Calculate yearly feed amount 
  # first 2 months is 2kg, 3rd month is 3 kg and starting from 4 kg for the next 32 months
  # Same cycle of chicks purchase per every 3-year cycle, so i will just use first 2 values
  yearly_feed <- numeric(n_year)
  for (i in 1:n_year) {
    year_in_cycle <- (i - 1) %% 4 + 1
    
    if (year_in_cycle == 1) {
      yearly_feed[i] <- ( ((yearly_chick[1]/3)*4*12) + # 1st batch purchase from year 4
                            ((yearly_chick[1]/3)*4*12) + # 2nd batch purchase from year 4
                            ((yearly_chick[1]/3)*2) + # 3rd batch purchase from year 4 (move-in Dec)
                            ((yearly_chick[1]/3)*3) + 
                            ((yearly_chick[1]/3)*4*10)) # 3rd batch mature here
      
    } else if (year_in_cycle == 2) {
      yearly_feed[i] <- ((yearly_chick[2]*2*2) + # new batch purchase in year 2
                           (yearly_chick[2]*3) +
                           (yearly_chick[2]*4*9) +
                           (yearly_chick[1]*4*12))
      
    } else if (year_in_cycle == 3) {
      yearly_feed[i] <- ((yearly_chick[1]*4*12) +
                           (yearly_chick[2]*4*12))
    } else {
      # First batch of year 1 is already harvested by Jan of year 4 (no feed)
      # second batch needs 4 months feed of 4kg
      # third batch needs 8 months feed of 4 kg
      yearly_feed[i] <- ((yearly_chick[2]*4*12) +
                           ((yearly_chick[1]/3)*4*4*2) + # x 2 coz two new batches buy here
                           ((yearly_chick[1]/3)*4*8*2) +
                           ((yearly_chick[1]/3)*2)) # third new batch purchase in year 4
    }
  }
  
  yearly_feed
  
  # Calculate cost per 1 kg
  baseline_feed_cost_1kg <- (cost_maize_feed_per_100kg +
                               cost_concentrate_per_100kg) / 100
  
  bsfl_feed_cost_1kg <- (cost_maize_feed_per_100kg +
                           cost_BSFL_powder_per_100kg) / 100
  
  # Calculate yearly feed cost
  yearly_baseline_feed_cost <- yearly_feed * baseline_feed_cost_1kg
  
  yearly_bsfl_feed_cost <- yearly_feed * bsfl_feed_cost_1kg
  
  # Calculate festive broiler chicks feed cost
  yearly_baseline_festive_feed_cost <- (feed_per_festive_chick_per_month *
                                          number_broiler_chick_per_festive *
                                          number_yearly_festive_sale *
                                          baseline_feed_cost_1kg * 2) # harvest in 2 months
  
  yearly_bsfl_festive_feed_cost <- (feed_per_festive_chick_per_month *
                                      number_broiler_chick_per_festive *
                                      number_yearly_festive_sale *
                                      bsfl_feed_cost_1kg * 2)
  
  # Calculate maintenance costs
  yearly_maintenance_cost <- (monthly_water_bill +
                                monthly_electricity_bill +
                                cost_monthly_disinfectant +
                                cost_materials_per_month +
                                cost_monthly_transportation +
                                cost_monthly_antibiotics) * 12
  
  # Calculate other yearly costs 
  yearly_other_cost_baseline <- (cost_building_maintenance_yearly +
                                   cost_yearly_vaccinations +
                                   (number_doctor_visit_yearly * cost_per_doctor_visit) +
                                   cost_yearly_medical_bill)
  
  yearly_other_cost_bsfl <- (cost_building_maintenance_yearly +
                               cost_yearly_vaccinations +
                               ((number_doctor_visit_yearly * cost_per_doctor_visit) +
                                  cost_yearly_medical_bill) * (1- bsfl_immune_boosting_effect))
  
  # Calculate employee salary yearly
  employee_salarly_yearly <- ((number_caretaker * monthly_salary_caretaker) +
                                (monthly_salary_for_manager+
                                   monthly_salary_assistant) +
                                ((number_eggs_crate_sale_per_new_cus *
                                    number_new_customer_monthly *
                                    price_per_crate_30egg) -
                                   (baseline_farm_cost_per_crate *
                                      number_eggs_crate_sale_per_new_cus *
                                      number_new_customer_monthly)) *
                                percent_sale_provision_manager) * 12
  
  # Yearly cost of advertisement
  yearly_cost_ad <- cost_per_advertisement * number_yearly_festive_sale
  
  # Cost of yearly bird loss
  yearly_loss_birds_death_baseline <- yearly_loss_birds_death
  
  yearly_loss_birds_death_bsfl <- (yearly_loss_birds_death *
                                     (1- bsfl_immune_boosting_effect))
  
  # Calculate yearly egg production
  yearly_egg <- numeric(n_year)
  for (i in 1:n_year) {
    year_in_cycle <- (i - 1) %% 4 + 1
    
    if (year_in_cycle == 1) {
      yearly_egg[i] <- (((yearly_chick[1]/3)*12*number_egg_per_bird_per_month) + # 1st batch from year 4
                          ((yearly_chick[1]/3)*12*number_egg_per_bird_per_month) + # 2nd batch from year 4
                          ((yearly_chick[1]/3)*9 * number_egg_per_bird_per_month)) # 3rd batch from year 4
      
    } else if (year_in_cycle == 2) {
      yearly_egg[i] <- ((yearly_chick[2]*8*number_egg_per_bird_per_month) + # the new batch of year 2
                          (yearly_chick[1]*12*number_egg_per_bird_per_month))
      
    } else if (year_in_cycle == 3) {
      yearly_egg[i] <- ((yearly_chick[1]*12*number_egg_per_bird_per_month) +
                          (yearly_chick[2]*12*number_egg_per_bird_per_month))
    } else {
      yearly_egg[i] <- ((yearly_chick[2]*12*number_egg_per_bird_per_month) + 
                          ((yearly_chick[1]/3)*4*number_egg_per_bird_per_month) + # 1st batch from last cycle is harvested in Jan
                          ((yearly_chick[1]/3)*8*number_egg_per_bird_per_month) + # so only two batches from last cycle are left
                          ((yearly_chick[1]/3)*8*number_egg_per_bird_per_month) + # new purchase
                          ((yearly_chick[1]/3)*4*number_egg_per_bird_per_month)) # new purchase
    }
  }
  
  yearly_egg <- ceiling(yearly_egg)
  
  # Calculate yearly egg profit including festive sales boost
  # Expert estimates no change in yield with BSFL 
  # According to farm owner, all eggs are sold every year
  # Calculate total egg sales on festive months (including regular sales)
  festive_egg_sale <- ceiling((number_yearly_festive_sale *festive_egg_sale_increase * 
                                 (yearly_egg/12)) + ((yearly_egg/12)*number_yearly_festive_sale)) # festive eggs per year (all egg sales in festive months of the year)
  
  # Calculate extra yearly festive eggs
  yearly_festive_egg_extra <- ceiling((number_yearly_festive_sale *festive_egg_sale_increase * 
                                         (yearly_egg/12)))
  
  # Yearly egg sale with festive sale
  yearly_egg_with_festive <- yearly_egg + yearly_festive_egg_extra
  
  # New eggs will be purchased if sales increased
  # No purchase if extra demand is small 
  yearly_egg_demand_limit <- ceiling((percent_demand_increase * yearly_egg) + yearly_egg)
  
  # Calculate yearly egg with extra demand limit for egg purchase 
  # Cynthia doesn't do this-- instead of buying eggs, she would buy extra layer hens
  # But, to keep the model simple (our goal is to compare between intervention and baseline,
  # and any extra profit or cost would be the same), we will buy eggs instead.
  # We do this step, to keep the model sensible (answer to where do extra eggs come from)
  yearly_egg_profit <- numeric(n_year)
  festive_cost <- numeric(n_year)
  packaging_cost <- numeric(n_year)
  yearly_crates_sold <- numeric(n_year)
  
  for (i in 1:n_year) {
    regular_sale <- ceiling((yearly_egg[i] - festive_egg_sale[i]) / 30)
    festive_sale <- ceiling(festive_egg_sale[i] / 30)
    
    gross_profit <- (regular_sale * price_per_crate_30egg) + 
      (festive_sale * (price_per_crate_30egg * (1 + festive_price_increase)))
    
    yearly_crates_sold[i] <- regular_sale + festive_sale
    packaging_cost[i] <- (yearly_crates_sold[i] / 100) * cost_packaging_per_100_crates
    
    if (yearly_egg_with_festive[i] <= yearly_egg_demand_limit[i]) {
      festive_cost[i] <- 0
    } else { 
      extra_crates_needed <- ceiling((yearly_egg_with_festive[i] - yearly_egg_demand_limit[i]) / 30)
      festive_cost[i] <- extra_crates_needed * price_per_crate_30egg # assume to buy at regular price
    }
    
    yearly_egg_profit[i] <- gross_profit - festive_cost[i]
  }
  
  
  yearly_egg_profit 
  festive_cost 
  packaging_cost 
  yearly_crates_sold 
  
  # Manure yearly profit
  yearly_manure_profit <- number_manure_bag_per_month * price_per_manure_bag * 12
  
  # Yearly festive bird profit 
  yearly_festive_meat_profit <- (number_broiler_chick_per_festive *
                                   number_yearly_festive_sale *
                                   price_per_festive_bird)
  
  # Spent layer hens sale
  layer_meat_sale <- numeric(n_year)
  for (i in 1:n_year) {
    year_in_cycle <- (i - 1) %% 4 + 1
    
    if (year_in_cycle == 1) {
      layer_meat_sale[i] <- 0 # no spent layer hens
      
    } else if (year_in_cycle == 2) {
      layer_meat_sale[i] <- 0 # no spent layer hens
      
    } else if (year_in_cycle == 3) {
      layer_meat_sale[i] <- 0 # no spent layer hens
    } else {
      layer_meat_sale[i] <- yearly_chick[1] * price_per_bird_meat # all spent hens from last cycle
    }
  }
  
  layer_meat_sale
  
  # Calculate total feed cost
  
  total_baseline_feed_cost <- vv(yearly_baseline_feed_cost +
                                   yearly_baseline_festive_feed_cost,
                                 var_CV, n_year)
  
  total_bsfl_feed_cost <- vv(yearly_bsfl_feed_cost +
                               yearly_bsfl_festive_feed_cost,
                             var_CV, n_year)
  
  
  # Calculate total baseline cost
  
  total_baseline_cost <- vv(yearly_festive_chick_cost +
                              yearly_maintenance_cost +
                              yearly_other_cost_baseline +
                              employee_salarly_yearly +
                              yearly_loss_birds_death_baseline +
                              packaging_cost +
                              festive_cost +
                              yearly_cost_ad +
                              yearly_chick_cost +
                              yearly_baseline_feed_cost +
                              yearly_baseline_festive_feed_cost,
                            var_CV, n_year) 
  
  # Calculate total baseline benefit
  
  total_baseline_benefit <- vv(yearly_manure_profit +
                                 yearly_festive_meat_profit +
                                 yearly_egg_profit +
                                 layer_meat_sale,
                               var_CV, n_year) 
  
  
  # With extra advertisement, the BSFL sales may increase.
  # So, we will adjust the cost and benefit here, instead of creating more loops.
  # We will use very uncertain values here, but lower ranges for cost because
  # cost may not increase much if sale boosts is low (supply still meets demand).
  # Cost will only increase with expansion.
  
  # Add chance_event
  
  chance_bsfl_ad_success_true <- chance_event(chance_bsfl_ad_success) 
  
  chance_taste_color_change_accepted_true <- chance_event(chance_taste_color_change_accepted)
  
  total_bsfl_cost <- if (chance_bsfl_ad_success_true == 1) {
    vv(yearly_other_cost_bsfl +
         yearly_loss_birds_death_bsfl +
         (yearly_festive_chick_cost * (1 + percent_cost_increase)) +
         yearly_maintenance_cost +
         employee_salarly_yearly +
         (packaging_cost * (1 + percent_cost_increase)) +
         festive_cost +
         yearly_cost_ad +
         (yearly_bsfl_feed_cost * (1 + percent_cost_increase)) +
         (yearly_chick_cost * (1 + percent_cost_increase)) +
         (yearly_bsfl_festive_feed_cost * (1 + percent_cost_increase)) +
         yearly_extra_bsfl_ad_cost,
       var_CV, n_year) 
    
  } else {
    
    vv(yearly_other_cost_bsfl +
         yearly_loss_birds_death_bsfl +
         yearly_festive_chick_cost +
         yearly_maintenance_cost +
         employee_salarly_yearly +
         packaging_cost +
         festive_cost +
         yearly_cost_ad +
         yearly_bsfl_feed_cost +
         yearly_chick_cost +
         yearly_bsfl_festive_feed_cost,
       var_CV, n_year) 
    
  }
  
  
  # Calculate total benefit for bsfl
  # We add increase benefits only if both advertisements and the changed traits of eggs 
  # are well accepted.
  total_bsfl_benefit <- if (chance_bsfl_ad_success_true == 1 &&
                            chance_taste_color_change_accepted_true ==1)  {
    vv(((yearly_manure_profit +
           yearly_egg_profit +
           layer_meat_sale +
           yearly_festive_meat_profit) * (1 + percent_benefit_increase)),
       var_CV, n_year) 
    
  } else {
    
    vv(yearly_manure_profit +
         yearly_festive_meat_profit +
         yearly_egg_profit +
         layer_meat_sale,
       var_CV, n_year) 
  }
  
  
  # Calculate results
  
  baseline_result <- total_baseline_benefit - total_baseline_cost
  
  bsfl_result <- total_bsfl_benefit - total_bsfl_cost
  
  decision_result <- bsfl_result - baseline_result
  
  Yearly_cost_saved_with_intervention <- total_baseline_cost - total_bsfl_cost
  
  Yearly_feed_cost_saved_with_intervention <- total_baseline_feed_cost - total_bsfl_feed_cost
  
  
  # Calculate NPV
  
  NPV_baseline <- discount (x = baseline_result,
                            discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  
  NPV_bsfl <- discount (x = bsfl_result,
                        discount_rate = discount_rate,
                        calculate_NPV = TRUE)
  
  NPV_decision <- discount (x = decision_result,
                            discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  
  
  return(list(NPV_baseline = NPV_baseline,
              NPV_bsfl = NPV_bsfl,
              NPV_decision = NPV_decision,
              Cashflow_baseline = baseline_result,
              Cashflow_bsfl = bsfl_result,
              Cashflow_feed_cost_saved = 
                Yearly_feed_cost_saved_with_intervention,
              total_bsfl_cost = sum(total_bsfl_cost),
              total_baseline_cost = sum(total_baseline_cost),
              total_cost_saved_with_intervention = 
                sum(Yearly_cost_saved_with_intervention),
              total_feed_cost_saved_with_intervention = 
                sum(Yearly_feed_cost_saved_with_intervention),
              total_baseline_feed_cost = sum(total_baseline_feed_cost),
              total_bsfl_feed_cost = sum(total_bsfl_feed_cost)))
}

DA_simulation_results <- mcSimulation(
  estimate = estimate_read_csv("data/DA_project.csv"),
  model_function = DA_function,
  numberOfModelRuns = 1000, 
  functionSyntax = "plainNames")



# Plot distribution
plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = c("NPV_baseline", 
                            "NPV_bsfl"),
                   old_names = c("NPV_baseline", "NPV_bsfl"),
                   new_names = c("NPV baseline concentrate feed", "NPV BSFL feed"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7, 
                   x_axis_name = "Net Present Value (NPV) outcomes (GHC)")

# Bar plot
plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = c("NPV_baseline","NPV_bsfl"),
                   old_names = c("NPV_baseline", "NPV_bsfl"),
                   new_names = c("NPV baseline concentrate feed", "NPV BSFL feed"),
                   method = "boxplot", 
                   base_size = 11, 
                   x_axis_name = "Net Present Value (NPV) outcomes (GHC)")

# Plot distribution with boxplot
plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = "NPV_decision",
                   old_names = "NPV_decision",
                   new_names = "NPV decision",
                   method = "boxplot_density", 
                   base_size = 10, 
                   x_axis_name = "Net Present Value (NPV) outcomes (GHC)")

plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = c("total_baseline_cost","total_bsfl_cost"),
                   old_names = c("total_baseline_cost", "total_bsfl_cost"),
                   new_names = c("Total cost at baseline", "Total cost with BSFL intervention"),
                   method = "boxplot_density", 
                   base_size = 10, 
                   x_axis_name = "Ghanaian Cedi (GHC)")

plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = "total_cost_saved_with_intervention",
                   old_names = "total_cost_saved_with_intervention",
                   new_names = "Total cost saved with intervention",
                   method = "boxplot_density", 
                   base_size = 10, 
                   x_axis_name = "Ghanaian Cedi (GHC)")


plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = c("total_baseline_feed_cost","total_bsfl_feed_cost"),
                   old_names = c("total_baseline_feed_cost", "total_bsfl_feed_cost"),
                   new_names = c("Total feed cost at baseline", 
                                 "Total feed cost with BSFL intervention"),
                   method = "boxplot_density", 
                   base_size = 10, 
                   x_axis_name = "Ghanaian Cedi (GHC)")

plot_distributions(mcSimulation_object = DA_simulation_results, 
                   vars = "total_feed_cost_saved_with_intervention",
                   old_names = "total_feed_cost_saved_with_intervention",
                   new_names = "Total feed cost saved with intervention",
                   method = "boxplot_density", 
                   base_size = 10, 
                   x_axis_name = "Ghanaian Cedi (GHC)")

# Plot cash flow
plot_cashflow(mcSimulation_object = DA_simulation_results,
              cashflow_var_name = c("Cashflow_baseline", "Cashflow_bsfl",
                                    "Cashflow_feed_cost_saved"),
              x_axis_name = "Year",
              y_axis_name = "Cashflow in Ghanaian Cedi (GHC)") 

#Find EVPI 
mcSimulation_table <- data.frame(DA_simulation_results$x,
                                 DA_simulation_results$y[, "NPV_decision"])

# colnames(mcSimulation_table)
colnames(mcSimulation_table)[colnames(mcSimulation_table) == 
                               "DA_simulation_results.y....NPV_decision.."] <- "NPV_decision"

evpi_decision <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
save(evpi_decision,file="data/data_evpi_decision.Rda")

# Plot evpi
list.files("data")
load("data/data_evpi_decision.Rda")
plot_evpi(evpi_decision, decision_vars = "NPV_decision")

#Find PLS result
input_table <- read.csv("DA_project.csv")
names(DA_simulation_results$y)

pls_result <- plsr.mcSimulation(object = DA_simulation_results,
                                resultName = names
                                (DA_simulation_results$y)[1], 
                                ncomp = 1) # baseline

plot_pls(pls_result, input_table, threshold = 0.9)


pls_result <- plsr.mcSimulation(object = DA_simulation_results,
                                resultName = names
                                (DA_simulation_results$y)[2], 
                                ncomp = 1) # bsfl

plot_pls(pls_result, input_table, threshold = 0.9)

pls_result <- plsr.mcSimulation(object = DA_simulation_results,
                                resultName = names
                                (DA_simulation_results$y)[3], 
                                ncomp = 1) # decision

plot_pls(pls_result, input_table, threshold = 0.9)

# Summary
library(gtExtras)
library(svglite)

nrow(input_table)
mcSimulation_summary <- data.frame(DA_simulation_results$x[1:49], 
                                   DA_simulation_results$y[, c(1:3, 28:30)])

gt_plt_summary(mcSimulation_summary) 

# Table
library(gtsummary)
library(kableExtra)
library(dplyr)

# Create summary table
summary_table <- mcSimulation_summary %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ± {sd}"),
    digits = all_continuous() ~ 2
  )

# Convert to data frame
summary_df <- as_tibble(summary_table$table_body) %>%
  select(label, stat_0) %>%
  rename(`Variable` = label,`Mean ± SD` = stat_0)%>%
  filter(
    !is.na(`Mean ± SD`),              
    !grepl("^[0-9.,%()\\s]+$", Variable)          
  )
# Display table
summary_df %>%
  kable("html", escape = FALSE, align = "l") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


