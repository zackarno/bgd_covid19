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

koboquest <- list.files("scrap/koboquest/R",full.names = T)

for (i in koboquest ){
  source(i)
}

create_csv <- c("yes","no")[1]

# data_read ---------------------------------------------------------------

cleaned_df <- read.csv("inputs/02_cleaned_data/market_assessment_recoding.csv", stringsAsFactors = FALSE,
                 na.strings = c("", " ", NA))
# indv <- read.csv("inputs/01_daily_data/indv.csv", stringsAsFactors = FALSE,
                 # na.strings = c("", " ", NA))

analysis_indicator <-  read.csv("inputs/dap/Analysis_indicators.csv", stringsAsFactors = FALSE,
                                na.strings = c("", " ", NA))

data_for_analysis <- cleaned_df

assess_survey<- readxl::read_xls("inputs/03_tool/BGD_covid_19_market_monitoring.xls",sheet = "survey")
assess_choices<-readxl::read_xls("inputs/03_tool/BGD_covid_19_market_monitoring.xls",sheet = "choices")


assessment<-load_questionnaire(data = data_for_analysis,questions = assess_survey,
                               choices = assess_choices,choices.label.column.to.use = "label::english")

# main_df_colnames<- main_df %>% colnames()
# analysis_indicator_2 <- analysis_indicator %>% dplyr::mutate(
                         # colum =analysis_indicator$column.header %in% main_df_colnames)
col_not_to_analyze <- c("days_of_stock_of_rice", "restocking_time_of_rice", "days_of_stock_of_cooking_oil",
                        "restocking_time_of_cooking_oil", "days_of_stock_of_lentils","X.1",
                        "days_of_stock_of_chicken","restocking_time_of_chicken",
                        "restocking_time_of_lentils", "days_of_stock_of_leafy_greens",
                        "restocking_time_of_leafy_greens", "days_of_stock_of_bananas",
                        "restocking_time_of_bananas", "days_of_stock_of_eggs", "restocking_time_of_eggs",
                        "days_of_stock_of_fish", "restocking_time_of_fish", "days_of_stock_of_soap",
                        "restocking_time_of_soap", "days_of_stock_of_washing_powder","camp",
                        "restocking_time_of_washing_powder", "rice_sale_in_past_week","chicken_sale_in_past_week",
                        "oil_sale_in_past_week", "lentils_sale_in_past_week", "leafy_greens_sale_in_past_week",
                        "bananas_sale_in_past_week", "eggs_sale_in_past_week", "fish_sale_in_past_week",
                        "soap_sale_in_past_week", "washing_powder_sale_in_past_week", "enumerator_id" ,
                        "paracetamol_sale_in_past_week", "tarpaulin_sale_in_past_week","audit", "ki_code" ,
                        "X_id","X_uuid","X_submission_time","X_validation_status","end_survey","intro_text" ,
                        "X_index","X","survey_date","survey_start","deviceid","instance_name", "informed_consent"
)

col_to_analyze <- data_for_analysis %>% select(-col_not_to_analyze) %>% dplyr::select(-contains("_other"))   %>% colnames()


# analysis ----------------------------------------------------------------
df_strata <- "upazilla"
dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = data_for_analysis)

dfsvy$variables<- butteR::questionnaire_factorize_categorical(data = dfsvy$variables,questionnaire = assessment,return_full_data = T)

is_not_empty<-function(x){ all(is.na(x))==FALSE}
cols_to_analyze<-data_for_analysis[col_to_analyze] %>% select(-ends_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% colnames()


dfsvy$variables$upazilla<- forcats::fct_expand(dfsvy$variables$upazilla,c( "ukhiya", "teknaf"))
cols_to_refactor_yn<- c("rice_unit" , "lentils_unit" , "leafy_greens_unit" ,"bananas_unit"," paracetamol_unit" , "soap_unit","sell_tarpaulin")
dfsvy$variables<-dfsvy$variables %>%
    mutate_at(.vars=cols_to_refactor_yn, .fun=forcats::fct_expand(.,c("yes","no"))



dfsvy$variables$income_changed_to_4_weeks<- forcats::fct_expand(dfsvy$variables$income_changed_to_4_weeks,c( "it_decreased", "it_increased"))
dfsvy$variables$customer_visits_change<- forcats::fct_expand(dfsvy$variables$customer_visits_change,c( "it_decreased", "no"))
dfsvy$variables$round<- forcats::fct_expand(dfsvy$variables$round,c( "round", "no"))
dfsvy$variables$i.restocking_time_of_lentils<- forcats::fct_expand(dfsvy$variables$i.restocking_time_of_lentils,c( "0-3 days", "no"))
dfsvy$variables$i.days_of_stock_of_leafy_greens<- forcats::fct_expand(dfsvy$variables$i.days_of_stock_of_leafy_greens,c( "0-3 days", "no"))
dfsvy$variables$i.restocking_time_of_bananas<- forcats::fct_expand(dfsvy$variables$i.restocking_time_of_bananas,c( "0-3 days", "no"))
dfsvy$variables$i.days_of_stock_of_bananas<- forcats::fct_expand(dfsvy$variables$i.days_of_stock_of_bananas,c( "0-3 days", "no"))
dfsvy$variables$i.restocking_time_of_eggs<- forcats::fct_expand(dfsvy$variables$i.restocking_time_of_eggs,c( "0-3 days", "no"))
dfsvy$variables$i.restocking_time_of_fish<- forcats::fct_expand(dfsvy$variables$i.restocking_time_of_fish,c( "0-3 days", "no"))
dfsvy$variables$i.restocking_time_of_soap<- forcats::fct_expand(dfsvy$variables$i.restocking_time_of_soap,c( "0-3 days", "no"))



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
                "days_of_stock_of_paracetamol",
                "restocking_time_of_paracetamol",
                "cheapest_price_for_4mx5m_of_tarpaulin",
                "vendors_colsed")

for(i in median_col){
  print(i)
  col3 <- paste0("i.",i,"_median")
  basic_analysis_overall[[col3]] <- median(data_for_analysis[[i]],na.rm = T)
}
# write csv ---------------------------------------------------------------

if (create_csv =="yes"){
  output_location <- "outputs/02_butter_analysis/"
  write.csv(basic_analysis_overall,paste0(output_location,str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_overall.csv"))
  }
