##############################
#API 209, Fall 2021
#Team Maharjan
#Final project
#Due on Tuesday, Dec 2, 2021, 8:30 am.
##############################
# ---------
# Loading packages
# ---------

## install packages
#install.packages("haven")
#install.packages("ggpubr")
#install.packages("modelsummary")
#install.packages("ggrepel")
#install.packages("rsample")
#install.packages("glmnet", dependencies=TRUE)

## load packages
library(foreign)
library(readxl)
library(tidyverse)
library(scales)
library(haven)
library(gmodels)
library(ggrepel)
library(forcats)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(HistogramTools)
library(fastDummies)
library(dineq)
library(lawstat)
library(broom)
library(stargazer)
library(randomizr)
library(modelsummary)
library(rsample)
library(glmnet)
library(hdm)
library(randomForest)
library(AmesHousing)
library(dplyr)

setwd("/Users/lourentiusdimas/OneDrive - UNIVERSITAS INDONESIA/HKS/api 209/final exercise/Option 1 - Educate Girls India/data")

##################################################################
## PART 1: Use compiled data V2

idinsight_data <- read.csv("compiled_data_v2.csv")

## vars to remove
rm_vars <- c("VillageCode", 
             "District_D2D",
             "District.x",
             "State.x",
             "Block.x",
             "Village.x",
             "pincode_DISE.x",
             "is_duplicate",
             "D2D_year",
             "index",
             "missing_census",
             "match_dise_census",
             "District.y",
             "State.y",
             "Block.y",
             "Village.y",
             "pincode_DISE.y",
             "cluster_name_DISE",
             "village_1314",
             "village_1617",
             "village_1718",
             "StateDistrictCode",
             "iden1",
             "iden2",
             "idenm",
             "village_name_CENSUS",
             "village_code_CENSUS",
             "shrid",
             "year",
             "oos_g_5to14")

long_vars  <- setdiff(colnames(idinsight_data), rm_vars)
f_long_rhs <- str_c(long_vars, collapse = " + ")
f_long_c   <- as.formula(str_c("oos_g_5to14 ~ ", f_long_rhs))

#districts 
#district_names  <- c("ALIRAJPUR", "BANDA", "BANSWARA", "CHITRAKOOT", "CHITTOR", "DHAR", "JHABUA", 
#                    "JHALAWAR", "KAUSHAMBI", "KHANDWA", "KHARGONE", "SHIVPURI", "SINGRAULI", 
#                     "UDAIPUR")

district_names  <- c("ALIRAJPUR", "BANDA")

##################################################################
#PART 2: fit models, calculate MSEs + Loop

#create holders for various models
mse_holder_train_S <- c()
mse_holder_test_S <- c()
mse_holder_train_D <- c()
mse_holder_test_D <- c()
mse_holder_train_S_D <- c()
mse_holder_test_S_D <- c()
mse_holder_train_L <- c()
mse_holder_test_L <- c()
mse_holder_train_KS <- c()
mse_holder_test_KS <- c()
mse_holder_train_RF <- c()
mse_holder_test_RF <- c()


#loop within the list of districts
for(i in 1:length(district_names)){
  
  #Create training and holdout/test dataset for each district
  idinsight_data_train <- idinsight_data %>% filter(District.x != district_names[i])
  idinsight_data_test <- idinsight_data %>% filter(District.x == district_names[i])
  #----------------------------------------------------
  #model 1: OLS Supply
  
  #OLS formula - 1) Supply
  OLS_supply_formula <- oos_g_5to14 ~ approachbyroad1718 + cce1718 + electric1718 + medinstr_english1718 +
    midday_meals1718 + numschools1718 + student_teacher_ratio1718 + 
    toilet_g1718 + txtbkrecd1718 + water_none1718
  
  #OLS model - 1) Supply
  ols_supply_model <- lm(OLS_supply_formula, idinsight_data_train)
  summary(ols_supply_model)
  
  #Predict on TRAINING dataset 
  ols_supply_predict_train <- idinsight_data_train %>%
    mutate(ols_supply_predict_train = as.vector(predict(ols_supply_model, idinsight_data_train))
    ) 
  
  #Predict on TEST dataset 
  ols_supply_predict_test <- idinsight_data_test %>%
    mutate(ols_supply_predict_test = as.vector(predict(ols_supply_model, idinsight_data_test))
    ) 
  #calculate mse for TRAINING dataset
  mse_holder_train_S[i] <- ols_supply_predict_train %>%
    summarise(ols_supply_train = mean((oos_g_5to14 - ols_supply_predict_train)^2, na.rm = TRUE)
    )
  #calculate mse for TEST dataset
  mse_holder_test_S[i] <- ols_supply_predict_test %>%
    summarise(ols_supply_test = mean((oos_g_5to14 - ols_supply_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_S[i] 
  mse_holder_test_S[i] 
  
  #----------------------------------------------------
  #model 2: OLS Demand
  #Demand side OLS formula
  OLS_demand_formula <- oos_g_5to14 ~ asset_3c11 + asset_7c11 + banking_servicesc11 +
    fuel_cooking_7c11 + house_structure_1c11 + households_w_latrine_1c11 +
    households_w_latrine_2c11 + ownership_status_1c11 + roof_material_4c11 +
    source_drinking_water_1c11 + source_lighting_1c11 + wall_material_9c11
  
  #Demand side OLS model
  ols_demand_model <- lm(OLS_demand_formula, idinsight_data_train)
  summary(ols_demand_model)
  
  #Predict on TRAINING dataset 
  ols_demand_predict_train <- idinsight_data_train %>%
    mutate(ols_demand_predict_train = as.vector(predict(ols_demand_model, idinsight_data_train))
    ) 
  
  #Predict on TEST dataset 
  ols_demand_predict_test <- idinsight_data_test %>%
    mutate(ols_demand_predict_test = as.vector(predict(ols_demand_model, idinsight_data_test))
    ) 
  
  #calculate mse for TRAINING dataset
  mse_holder_train_D[i] <- ols_demand_predict_train %>%
    summarise(ols_demand_train = mean((oos_g_5to14 - ols_demand_predict_train)^2, na.rm = TRUE)
    )
  #calculate mse for TEST dataset
  mse_holder_test_D[i] <- ols_demand_predict_test %>%
    summarise(ols_demand_test = mean((oos_g_5to14 - ols_demand_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_D[i] 
  mse_holder_test_D[i] 
  
  #----------------------------------------------------
  #model 3: OLS Demand and Supply
  #Supply & Demand OLS formula
  ols_S_D_formula <- oos_g_5to14 ~ asset_3c11 + asset_7c11 + banking_servicesc11 +
    fuel_cooking_7c11 + house_structure_1c11 + households_w_latrine_1c11 +
    households_w_latrine_2c11 + ownership_status_1c11 + roof_material_4c11 +
    source_drinking_water_1c11 + source_lighting_1c11 + wall_material_9c11+
    approachbyroad1718 + cce1718 + electric1718 + medinstr_english1718 +
    midday_meals1718 + numschools1718 + student_teacher_ratio1718 + 
    toilet_g1718 + txtbkrecd1718 + water_none1718
  
  #Supply & Demand  OLS model
  ols_S_D_model <- lm(ols_S_D_formula, idinsight_data_train)
  summary(ols_S_D_model)
  
  #Predict on TRAINING dataset 
  ols_S_D_predict_train <- idinsight_data_train %>%
    mutate(ols_S_D_predict_train = as.vector(predict(ols_S_D_model, idinsight_data_train))
    ) 
  
  #Predict on TEST dataset 
  ols_S_D_predict_test <- idinsight_data_test %>%
    mutate(ols_S_D_predict_test = as.vector(predict(ols_S_D_model, idinsight_data_test))
    ) 

  #calculate mse for TRAINING dataset
  mse_holder_train_S_D[i] <- ols_S_D_predict_train %>%
    summarise(ols_S_D_train = mean((oos_g_5to14 - ols_S_D_predict_train)^2, na.rm = TRUE)
    )
  #calculate mse for TEST dataset
  mse_holder_test_S_D[i] <- ols_S_D_predict_test %>%
    summarise(ols_S_D_test = mean((oos_g_5to14 - ols_S_D_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_S_D[i] 
  mse_holder_test_S_D[i] 
  
  #----------------------------------------------------
  #model 4: OLS Kitchen sink
  ksink_model <- lm(f_long_c, idinsight_data_train)
  summary(ksink_model)
  
  #predict train data
  ksink_predict_train <- idinsight_data_train %>% 
    mutate(ksink_predict_train = as.vector(predict(ksink_model,idinsight_data_train))
    )
  
  #Predict using the test dataset
  ksink_predict_test <- idinsight_data_test %>% 
    mutate(ksink_predict_test = as.vector(predict(ksink_model,idinsight_data_test))
    )
 
   #Calculate mse for training data
  mse_holder_train_KS[i] <- ksink_predict_train %>%
    summarise(ksink_train = mean((oos_g_5to14 - ksink_predict_train)^2, na.rm = TRUE)
    )

  #Calculate mse for test data 
  mse_holder_test_KS[i] <- ksink_predict_test %>%
    summarise(ksink_test = mean((oos_g_5to14 - ksink_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_KS[i] 
  mse_holder_test_KS[i] 
  
  #----------------------------------------------------
  #model 5: Lasso
  y_c_train <- idinsight_data_train$oos_g_5to14
  X_train   <- as.matrix(select(idinsight_data_train, !!!long_vars))
  X_test <- as.matrix(select(idinsight_data_test, !!!long_vars))

  # estimating predictive models on the training set
  lasso <- cv.glmnet(x = X_train, y = y_c_train)
  plot(lasso)
  coef(lasso, s = "lambda.min")
  log(lasso$lambda.min)
  coef_lasso <- enframe(coef(lasso, s = "lambda.min")[,1])
  coef_lasso

  #Predict on TRAINING dataset
  lasso_predict_train <- idinsight_data_train %>%
    mutate(lasso_predict_train = as.vector(predict(lasso,
                                             newx = X_train,
                                             s = "lambda.min")))
  #Predict on TEST dataset
  lasso_predict_test <- idinsight_data_test %>%
    mutate(lasso_predict_test = as.vector(predict(lasso,
                                             newx = X_test,
                                             s = "lambda.min")))

   #Calculate mse for training data
  mse_holder_train_L[i] <- lasso_predict_train %>%
    summarise(lasso_train = mean((oos_g_5to14 - lasso_predict_train)^2, na.rm = TRUE)
    )

  #Calculate mse for test data
  mse_holder_test_L[i] <- lasso_predict_test %>%
    summarise(lasso_test = mean((oos_g_5to14 - lasso_predict_test)^2, na.rm = TRUE)
    )

  mse_holder_train_L[i]
  mse_holder_test_L[i]

  #----------------------------------------------------
  #model 6: random forest
  forest <- randomForest(x = X_train, y = y_c_train)

  #Predict on TRAINING dataset
  forest_predict_train <- idinsight_data_train %>%
    mutate(forest_predict_train = as.vector(predict(forest,
                                              newx = X_train)))
  #Predict on TEST dataset
  forest_predict_test <- idinsight_data_test %>%
    mutate(forest_predict_test = as.vector(predict(forest, X_test)))

  #Calculate mse for training data
  mse_holder_train_RF[i] <- forest_predict_train %>%
    summarise(forest_train = mean((oos_g_5to14 - forest_predict_train)^2, na.rm = TRUE)
    )

  #Calculate mse for test data
  mse_holder_test_RF[i] <- forest_predict_test %>%
    summarise(forest_test = mean((oos_g_5to14 - forest_predict_test)^2, na.rm = TRUE)
    )

  mse_holder_train_RF[i]
  mse_holder_test_RF[i]
  
}

write.csv(mse_holder_train_S,"mse_holder_train_S.csv", row.names = FALSE)
write.csv(mse_holder_test_S,"mse_holder_test_S.csv", row.names = FALSE)
write.csv(mse_holder_train_D,"mse_holder_train_D.csv", row.names = FALSE)
write.csv(mse_holder_test_D,"mse_holder_test_D.csv", row.names = FALSE)
write.csv(mse_holder_train_S_D,"mse_holder_train_S_D.csv", row.names = FALSE)
write.csv(mse_holder_test_S_D,"mse_holder_test_S_D.csv", row.names = FALSE)
write.csv(mse_holder_train_KS,"mse_holder_train_KS.csv", row.names = FALSE)
write.csv(mse_holder_test_KS,"mse_holder_test_KS.csv", row.names = FALSE)
write.csv(mse_holder_train_L,"mse_holder_train_L.csv", row.names = FALSE)
write.csv(mse_holder_test_L,"mse_holder_test_L.csv", row.names = FALSE)
write.csv(mse_holder_train_RF,"mse_holder_train_RF.csv", row.names = FALSE)
write.csv(mse_holder_test_RF,"mse_holder_test_RF.csv", row.names = FALSE)

######################################################################################################################################
######################################################################################################################################

#TWO-TIERED PARENT-CHILD MODEL

##################################################################
## PART 1: prepare existing datasets
dise_census_shrug <- read.csv("~/MPA-ID/API-209_Quantitative Methods I-Statistics/Final Exercise/Team Folder/data/compiled_data_v2.csv")

##################################################################
#PART 2: fit models, calculate MSEs + Loop

#create holders for various models
mse_holder_train_mini_p <- c()
mse_holder_test_mini_p <- c()
mse_holder_train_mini_c <- c()
mse_holder_test_mini_c <- c()
mse_holder_train_mini_o <- c()
mse_holder_test_mini_o <- c()
mse_holder_train_mini_a <- c()
mse_holder_test_mini_a <- c()
mse_holder_train_mini_h <- c()
mse_holder_test_mini_h <- c()
mse_holder_train_mini_s <- c()
mse_holder_test_mini_s <- c()
mse_holder_train_mini_t <- c()
mse_holder_test_mini_t <- c()
mse_holder_train_mini_r <- c()
mse_holder_test_mini_r <- c()
mse_holder_train_mini_x <- c()
mse_holder_test_mini_x <- c()

#loop within the list of districts
for(i in 1:length(district_names)){
  
  #Create training and holdout/test dataset for each district
  dise_census_shrug_train <- dise_census_shrug %>% filter(District.x != district_names[i])
  dise_census_shrug_test <- dise_census_shrug %>% filter(District.x == district_names[i])
  #----------------------------------------------------
  #model 1: OLS Parent's literacy
  
  #OLS formula - 1) Parent's literacy
  OLS_mini_p_formula <- oos_g_5to14 ~ f_illc11 + m_illc11 + f_litc11 + m_litc11 + pc11_pca_p_lit
  
  #OLS model - 1) Parent's literacy
  OLS_mini_p_model <- lm(OLS_mini_p_formula, dise_census_shrug_train)
  summary(OLS_mini_p_model)
  
  #Predict on TRAINING dataset 
  OLS_mini_p_predict_train <- dise_census_shrug_train %>%
    mutate(OLS_mini_p_predict_train = as.vector(predict(OLS_mini_p_model, dise_census_shrug_train))
    ) 
  
  #Predict on TEST dataset 
  OLS_mini_p_predict_test <- dise_census_shrug_test %>%
    mutate(OLS_mini_p_predict_test = as.vector(predict(OLS_mini_p_model, dise_census_shrug_test))
    ) 
  #calculate mse for TRAINING dataset
  mse_holder_train_mini_p[i] <- OLS_mini_p_predict_train %>%
    summarise(OLS_mini_p_train = mean((oos_g_5to14 - OLS_mini_p_predict_train)^2, na.rm = TRUE)
    )
  #calculate mse for TEST dataset
  mse_holder_test_mini_p[i] <- OLS_mini_p_predict_test %>%
    summarise(OLS_mini_p_test = mean((oos_g_5to14 - OLS_mini_p_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_mini_p[i] 
  mse_holder_test_mini_p[i] 
  
  #----------------------------------------------------
  #model 2: OLS Caste and religion
  
  #OLS formula - 2) Caste and religion
  OLS_mini_c_formula <- sc_b1314 + sc_b1516 + sc_b1718 + sc_g1314 + sc_g1516 + sc_g1718 + st_b1314 + 
    st_b1516 + st_b1718 + st_g1314 + st_g1516 + st_g1718 + rel_age06_fe_buddhist_rurc11 + 
    rel_pop_fe_buddhist_rurc11 + rel_age06_fe_christian_rurc11 + rel_pop_fe_christian_rurc11 + 
    rel_age06_fe_hindu_rurc11 + rel_pop_fe_hindu_rurc11 + rel_age06_fe_jain_rurc11 + rel_pop_fe_jain_rurc11 + 
    rel_age06_fe_muslim_rurc11 + rel_pop_fe_muslim_rurc11 + rel_age06_fe_notstated_rurc11 + 
    rel_pop_fe_notstated_rurc11 + rel_age06_fe_other_rurc11 + rel_pop_fe_other_rurc11 + rel_age06_fe_sikh_rurc11 + 
    rel_pop_fe_sikh_rurc11 + rel_age06_ma_buddhist_rurc11 + rel_pop_ma_buddhist_rurc11 + rel_age06_ma_christian_rurc11 + 
    rel_pop_ma_christian_rurc11 + rel_age06_ma_hindu_rurc11 + rel_pop_ma_hindu_rurc11 + rel_age06_ma_jain_rurc11 + 
    rel_pop_ma_jain_rurc11 + rel_age06_ma_muslim_rurc11 + rel_pop_ma_muslim_rurc11 + rel_age06_ma_notstated_rurc11 + 
    rel_pop_ma_notstated_rurc11 + rel_age06_ma_other_rurc11 + rel_pop_ma_other_rurc11 + rel_age06_ma_sikh_rurc11 + 
    rel_pop_ma_sikh_rurc11 + f_scc11 + m_scc11 + f_stc11 + m_stc11 + pc11_pca_p_sc + pc11_pca_p_st
  
  
  #OLS model - 2) Caste and religion
  OLS_mini_c_model <- lm(OLS_mini_c_formula, dise_census_shrug_train)
  summary(OLS_mini_c_model)
  
  #Predict on TRAINING dataset 
  OLS_mini_c_predict_train <- dise_census_shrug_train %>%
    mutate(OLS_mini_c_predict_train = as.vector(predict(OLS_mini_c_model, dise_census_shrug_train))
    ) 
  
  #Predict on TEST dataset 
  OLS_mini_c_predict_test <- dise_census_shrug_test %>%
    mutate(OLS_mini_c_predict_test = as.vector(predict(OLS_mini_c_model, dise_census_shrug_test))
    ) 
  #calculate mse for TRAINING dataset
  mse_holder_train_mini_c[i] <- OLS_mini_c_predict_train %>%
    summarise(OLS_mini_c_train = mean((oos_g_5to14 - OLS_mini_c_predict_train)^2, na.rm = TRUE)
    )
  #calculate mse for TEST dataset
  mse_holder_test_mini_c[i] <- OLS_mini_c_predict_test %>%
    summarise(OLS_mini_c_test = mean((oos_g_5to14 - OLS_mini_c_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_mini_c[i] 
  mse_holder_test_mini_c[i] 
  
  #----------------------------------------------------
  #model 3: OLS Occupation
  
  #OLS formula - 3) Occupation
  OLS_mini_o_formula <- ecYY_emp_all + marg_al_0_3_fc11 + marg_cl_0_3_fc11 + marg_hh_0_3_fc11 + marg_ot_0_3_fc11 + 
    margwork_0_3_fc11 + marg_al_3_6_fc11 + marg_cl_3_6_fc11 + marg_hh_3_6_fc11 + marg_ot_3_6_fc11 + margwork_3_6_fc11 + 
    main_al_fc11 + tot_work_fc11 + main_cl_fc11 + mainwork_fc11 + non_work_fc11 + main_ot_fc11 + main_hh_fc11 + 
    marg_al_0_3_mc11 + marg_cl_0_3_mc11 + marg_hh_0_3_mc11 + marg_ot_0_3_mc11 + margwork_0_3_mc11 + marg_al_3_6_mc11 + 
    marg_cl_3_6_mc11 + marg_hh_3_6_mc11 + marg_ot_3_6_mc11 + margwork_3_6_mc11 + main_al_mc11 + tot_work_mc11 + 
    main_cl_mc11 + mainwork_mc11 + non_work_mc11 + main_ot_mc11 + main_hh_mc11 + secc_inc_cultiv_share + nco2d_cultiv_share
  
  #OLS model - 3) Occupation
  OLS_mini_o_model <- lm(OLS_mini_o_formula, dise_census_shrug_train)
  summary(OLS_mini_o_model)
  
  #Predict on TRAINING dataset 
  OLS_mini_o_predict_train <- dise_census_shrug_train %>%
    mutate(OLS_mini_o_predict_train = as.vector(predict(OLS_mini_o_model, dise_census_shrug_train))
    ) 
  
  #Predict on TEST dataset 
  OLS_mini_o_predict_test <- dise_census_shrug_test %>%
    mutate(OLS_mini_o_predict_test = as.vector(predict(OLS_mini_o_model, dise_census_shrug_test))
    ) 
  #calculate mse for TRAINING dataset
  mse_holder_train_mini_o[i] <- OLS_mini_o_predict_train %>%
    summarise(OLS_mini_o_train = mean((oos_g_5to14 - OLS_mini_o_predict_train)^2, na.rm = TRUE)
    )
  #calculate mse for TEST dataset
  mse_holder_test_mini_o[i] <- OLS_mini_o_predict_test %>%
    summarise(OLS_mini_o_test = mean((oos_g_5to14 - OLS_mini_o_predict_test)^2, na.rm = TRUE)
    )
  mse_holder_train_mini_o[i] 
  mse_holder_test_mini_o[i] 
  
}