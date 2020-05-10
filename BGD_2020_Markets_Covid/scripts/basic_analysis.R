rm(list=ls())

# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(tidyverse)
library(readr)
library(stringr)
library(srvyr)
library(survey)
library(readxl)
library(AMR)

create_csv <- c("yes","no")[1]

# data_read ---------------------------------------------------------------

cleaned_df <- read.csv("inputs/02_cleaned_data/market_assessment_recoding.csv", stringsAsFactors = FALSE,
                 na.strings = c("", " ", NA))
# indv <- read.csv("inputs/01_daily_data/indv.csv", stringsAsFactors = FALSE,
                 # na.strings = c("", " ", NA))

analysis_indicator <-  read.csv("inputs/dap/Analysis_indicators.csv", stringsAsFactors = FALSE,
                                na.strings = c("", " ", NA))

data_for_analysis <- cleaned_df


col_not_to_analyze <- c("days_of_stock_of_rice", "restocking_time_of_rice", "days_of_stock_of_cooking_oil",
                        "restocking_time_of_cooking_oil", "days_of_stock_of_lentils",
                        "days_of_stock_of_chicken","restocking_time_of_chicken",
                        "restocking_time_of_lentils", "days_of_stock_of_leafy_greens",
                        "restocking_time_of_leafy_greens", "days_of_stock_of_bananas",
                        "restocking_time_of_bananas", "days_of_stock_of_eggs", "restocking_time_of_eggs",
                        "days_of_stock_of_fish", "restocking_time_of_fish", "days_of_stock_of_soap",
                        "restocking_time_of_soap", "days_of_stock_of_washing_powder",
                        "restocking_time_of_washing_powder", "rice_sale_in_past_week","chicken_sale_in_past_week",
                        "oil_sale_in_past_week", "lentils_sale_in_past_week", "leafy_greens_sale_in_past_week",
                        "bananas_sale_in_past_week", "eggs_sale_in_past_week", "fish_sale_in_past_week",
                        "soap_sale_in_past_week", "washing_powder_sale_in_past_week",
                        "paracetamol_sale_in_past_week", "tarpaulin_sale_in_past_week", "ki_code"
                        ,"X","X_uuid","survey_date", "survey_start","end_survey")

col_to_analyze <- data_for_analysis %>% select(-col_not_to_analyze) %>% dplyr::select(-contains("_other"))   %>% colnames()


# analysis ----------------------------------------------------------------
df_strata <- "upazilla"
dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = data_for_analysis)

# dfsvy$variables<- butteR::questionnaire_factorize_categorical(data = dfsvy$variables,questionnaire = assessment,return_full_data = T)

is_not_empty<-function(x){ all(is.na(x))==FALSE}
cols_to_analyze<-data_for_analysis[col_to_analyze] %>% select(-ends_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% colnames()


dfsvy$variables$upazilla<- forcats::fct_expand(dfsvy$variables$upazilla,c( "ukhiya", "teknaf"))

dfsvy$variables$sell_tarpaulin<- forcats::fct_expand(dfsvy$variables$sell_tarpaulin,c( "no", "yes"))
dfsvy$variables$sell_paracetamol<- forcats::fct_expand(dfsvy$variables$sell_paracetamol,c( "no", "yes"))

cols_to_sell <- c("sell_tarpaulin","sell_paracetamol")

dfsvy$variables<-dfsvy$variables %>%
  mutate_at(.vars=cols_to_sell, .funs=forcats::fct_expand,c("no","yes")
  )

dfsvy$variables$income_changed_to_4_weeks<- forcats::fct_expand(dfsvy$variables$income_changed_to_4_weeks,c( "it_decreased", "it_increased","it_stayed_the_same","dontknow"))
dfsvy$variables$customer_visits_change<- forcats::fct_expand(dfsvy$variables$customer_visits_change,c( "it_decreased", "it_increased","it_stayed_the_same","dontknow"))

cols_to_times <- c("i.restocking_time_of_lentils","i.days_of_stock_of_leafy_greens",
                   "i.restocking_time_of_bananas","i.days_of_stock_of_bananas",
                   "i.restocking_time_of_eggs","i.restocking_time_of_fish",
                   "i.restocking_time_of_soap","i.days_of_stock_of_chicken",
                   "i.restocking_time_of_chicken")

dfsvy$variables<-dfsvy$variables %>%
  mutate_at(.vars=cols_to_times, .funs=forcats::fct_expand,c("0_3_days","4_7_days","7_and_more")
  )

# basic analysis ----------------------------------------------------------

basic_analysis_overall<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze)



# median ------------------------------------------------------------------

median_col <- c("vendors_operational",
                "price_of_1kg",
                "cheapest_price_for_12_of_chicken",
                "cheapest_price_for_cooking_oil",
                "cheapest_price_for_1kg_of_lentils",
                "cheapest_price_for_0.5kg_of_leafy_greens",
                "cheapest_price_for_1kg_of_bananas",
                "cheapest_price_for_12__of_eggs",
                "cheapest_price_for_1kg__of_fish",
                "cheapest_price_for_100g_soap_bar_of_soap",
                "cheapest_price_for_0_5l_of_bleachwashing_powder",
                "cheapest_price_for_12_of_paracetamol",
                "restocking_time_of_paracetamol",
                "cheapest_price_for_4mx5m_of_tarpaulin",
                "vendors_colsed",
                "days_of_stock_of_rice", "days_of_stock_of_cooking_oil", "days_of_stock_of_lentils",
                "days_of_stock_of_leafy_greens", "days_of_stock_of_bananas",
                "days_of_stock_of_eggs", "days_of_stock_of_fish", "days_of_stock_of_soap",
                "days_of_stock_of_washing_powder", "days_of_stock_of_paracetamol",
                "days_of_stock_of_chicken", "days_of_stock_of_tarpaulin")

for(i in median_col){
  print(i)
  col3 <- paste0("i.",i,"_median")
  basic_analysis_overall[[col3]] <- median(data_for_analysis[[i]],na.rm = T)
}


# reordering (need to be adjusted once the tool is updated)--------------------------------------------------------------

# cols_order <- read.csv("inputs/dap/dataset_arrage.csv") %>%  colnames()
# cols_order <- cols_order %>% str_replace_all("0.3.days","0_3_days")
# cols_order <- cols_order %>% str_replace_all("4.7.days","4_7_days")
#
#
# cols_order <- cols_order %>% as.data.frame()
# cols_order<- cols_order %>% dplyr::filter(. != "customer_visits_change.no")
# cols_order <- cols_order$. %>% dput
#
# # cols_or <- basic_analysis_overall %>% colnames()
# #
# # cols_oder1 <- cols_order %>% dplyr::mutate(
# #   t = cols_order$. %in% cols_or
# # )
#
# basic_analysis_overall_2 <- basic_analysis_overall %>% dplyr::select(cols_order)

# write csv ---------------------------------------------------------------

if (create_csv =="yes"){
  output_location <- "outputs/02_butter_analysis/"
  write.csv(basic_analysis_overall,paste0(output_location,str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_overall.csv"))
  }
