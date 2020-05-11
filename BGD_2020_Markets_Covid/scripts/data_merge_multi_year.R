# rm(list=ls())

# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(tidyverse)
library(readr)
library(stringr)
library(srvyr)
library(survey)
library(readxl)
# library(AMR)


refactor_to_xlsform<-function(data,kobo_survey,kobo_choices ,label_column = "label::english" ){
  xls_lt<-make_xlsform_lookup_table(kobo_survey ,kobo_choices,label_column )
  xls_lt_select_questions<-xls_lt %>%
    filter(str_detect(question_type,"select"))
  for(i in 1: length(unique(xls_lt_so$question_name))){
    col_temp <- unique(xls_lt_so$question_name)[i]
    print(col_temp)
    choices_temp<-xls_lt_so %>% filter(question_name==col_temp) %>% pull(choice_name)
    data<-data %>%
      mutate(!!col_temp:= forcats::fct_expand(as.factor(!!sym(col_temp)), choices_temp))
  }
  return(data)

}



current_round$days_of_stock_of_dry_fish
current_round$cheapest_price_for_12__of_eggs
name_changes<-tibble::tibble(round_1=c("sell_fish",
                                       "cheapest_price_for_1kg__of_fish" ,
                                       "days_of_stock_of_fish" ,
                                       "cheapest_price_for_12_of_chicken"),
               round_2= c("sell_dry_fish",
                          "cheapest_price_for_1kg__of_dry_fish",
                          "days_of_stock_of_dry_fish",
                          "cheapest_price_for_4mx5m_of_chicken"))

# as_tibble(name_changes) %>% View()

create_csv <- c("yes","no")[1]

# # data_read ---------------------------------------------------------------
# ks<-readxl::read_xls(path = "BGD_2020_Markets_Covid/inputs/kobo_tool/BGD_covid_19_market_monitoring.xls",sheet = "survey")


xlsform_paths<-list.files("BGD_2020_Markets_Covid/inputs/kobo_tool/",full.names = T) %>% sort()



clean_data_file_paths<-list.files("BGD_2020_Markets_Covid/inputs/clean_data",full.names = T) %>% sort()
round_number<-length(clean_data_file_paths)

current_round <- read.csv(clean_data_file_paths[round_number], stringsAsFactors = FALSE,
                       na.strings = c("", " ", NA))

prev_round <- read.csv(clean_data_file_paths[round_number-1], stringsAsFactors = FALSE,
                       na.strings = c("", " ", NA))

ks<-readxl::read_xlsx(path = xlsform_paths[round_number],sheet = "survey")
kc<-readxl::read_xlsx(path = xlsform_paths[round_number],sheet = "choices")


current_round<-refactor_to_xlsform(data = current_round,kobo_survey = ks,kobo_choices = kc,label_column = "label::english" )



analysis_indicator <-  read.csv("inputs/dap/Analysis_indicators.csv", stringsAsFactors = FALSE,
                                na.strings = c("", " ", NA))


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
col_not_to_analyze<-colnames(current_round)[colnames(current_round) %in% col_not_to_analyze]

col_to_analyze <- current_round %>% select(-col_not_to_analyze) %>% dplyr::select(-contains("_other"))   %>% colnames()


# analysis ----------------------------------------------------------------
df_strata <- "upazilla"
dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = current_round)

# dfsvy$variables<- butteR::questionnaire_factorize_categorical(data = dfsvy$variables,questionnaire = assessment,return_full_data = T)

is_not_empty<-function(x){ all(is.na(x))==FALSE}
cols_to_analyze<-current_round[col_to_analyze] %>% select(-ends_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% colnames()



# basic analysis ----------------------------------------------------------

basic_analysis_overall<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze)



# median ------------------------------------------------------------------

# median_col <- c("vendors_operational",
                # "price_of_1kg",
food_prices<-c("cheapest_price_for_12_of_chicken",
                "cheapest_price_for_cooking_oil",
                "cheapest_price_for_1kg_of_lentils",
                "cheapest_price_for_0.5kg_of_leafy_greens",
                "cheapest_price_for_1kg_of_bananas",
                "cheapest_price_for_12__of_eggs",
                "cheapest_price_for_1kg__of_fish")
nfi_prices<-c("cheapest_price_for_100g_soap_bar_of_soap",
                "cheapest_price_for_0_5l_of_bleachwashing_powder",
                "cheapest_price_for_12_of_paracetamol",
                "cheapest_price_for_4mx5m_of_tarpaulin")

# "restocking_time_of_paracetamol",
                # "vendors_colsed",
                # "days_of_stock_of_rice", "days_of_stock_of_cooking_oil", "days_of_stock_of_lentils",
                # "days_of_stock_of_leafy_greens", "days_of_stock_of_bananas",
                # "days_of_stock_of_eggs", "days_of_stock_of_fish", "days_of_stock_of_soap",
                # "days_of_stock_of_washing_powder", "days_of_stock_of_paracetamol",
                # "days_of_stock_of_chicken", "days_of_stock_of_tarpaulin")

median_df<-data.frame()
item_price_cols<-tibble::tibble(items=c(food_prices,nfi_prices),
                                item_type=ifelse(items %in% food_prices,"food","nfi"))


item_price_cols_fixed<-item_price_cols %>%
  left_join(name_changes, by=c("items"="round_1")) %>%
  mutate(round_2_items=ifelse(is.na(round_2),items, round_2)) %>% select(-round_2)

prev_round$cheapest_price_for_12_of_chicken
?setNames
set_names()
names(df2) = median_col_df$r1_median[match(names(df), variable_match$old)]
purrr::set_names(df, old = median_col_df$r1_median, new = median_col_df$r2_median)


current_round$cheapest_price_for_4mx5m_of_chicken
# median_col<- ifelse(median_col==name_changes$round_1,median_col,name_changes$round_2)

# loop --------------------------------------------------------------------

current_round %>% select(item_price_cols_fixed$round_2_items)
current_round %>%
  mutate_at(.vars = median_col, median,na.rm=T) %>%
  select(median_col)

for(i in median_col){
  print(i)
  col3 <- paste0("i.",i,"_median")
  basic_analysis_overall[[col3]] <- median(current_round[[i]],na.rm = T)
}



basic_analysis_overall %>% select(i.cheapest_price_for_0.5kg_of_leafy_greens_median)



if (create_csv =="yes"){
  output_location <- "outputs/02_butter_analysis/"
  write.csv(basic_analysis_overall,paste0(output_location,str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_overall.csv"))
}
