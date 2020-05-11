

# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(tidyverse)
library(readr)
library(stringr)
library(srvyr)
library(survey)
library(readxl)
library(matrixStats) # needed for row medians
source("BGD_2020_Markets_Covid/scripts/functions/utils.R")





# Markets team needs to start tracking tool changes -----------------------

# for now i will just keep changes i notice here
name_changes<-tibble::tibble(round_1=c("sell_fish",
                                       "cheapest_price_for_1kg__of_fish" ,
                                       "days_of_stock_of_fish" ,
                                       "cheapest_price_for_12_of_chicken"),
               round_2= c("sell_dry_fish",
                          "cheapest_price_for_1kg__of_dry_fish",
                          "days_of_stock_of_dry_fish",
                          "cheapest_price_for_4mx5m_of_chicken"))





# automatically load current and previous round data

xlsform_paths<-list.files("BGD_2020_Markets_Covid/inputs/kobo_tool/",full.names = T) %>% sort()
clean_data_file_paths<-list.files("BGD_2020_Markets_Covid/inputs/clean_data",full.names = T) %>% sort()


round_number<-length(clean_data_file_paths)

current_round <- read.csv(clean_data_file_paths[round_number], stringsAsFactors = FALSE,
                       na.strings = c("", " ", NA))

prev_round <- read.csv(clean_data_file_paths[round_number-1], stringsAsFactors = FALSE,
                       na.strings = c("", " ", NA))






# automatically load tool------------------------


ks<-readxl::read_xlsx(path = xlsform_paths[round_number],sheet = "survey")
kc<-readxl::read_xlsx(path = xlsform_paths[round_number],sheet = "choices")



# adds factors to tool data from tool so you dont have to -----------------
# will make this better and add to butteR

current_round<-refactor_to_xlsform(data = current_round,kobo_survey = ks,kobo_choices = kc,label_column = "label::english" )



# get column names for p 3 of FS
safety_measures<-current_round %>% select(starts_with("prevent_the_spread_of_COVID19.")) %>% colnames()
community_barriers<-current_round %>% select(starts_with("community_members_accessing_markets.")) %>% colnames()
security_threats<-current_round %>% select(starts_with("witnessed_safety_or_security.")) %>% colnames()

cols_to_analyze<-c(safety_measures,community_barriers,security_threats)



# set up survye objects for current & preious -----------------------------


dfsvy<-as_survey(current_round)
dfsvy_prev<-as_survey(prev_round)

current_analysis<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze)
prev_analysis<-butteR::mean_proportion_table(design = dfsvy_prev,list_of_variables = cols_to_analyze)





# get datamerge values for table 1 in FS ----------------------------------


page_3_fs<-percentage_pt_change(x = current_analysis, y=prev_analysis)
page_3_fs_dm<-cbind(page_3_fs$percent,page_3_fs$triangle_cols)





# median analysis - prev & current round comparison -----------------------

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




# this is a very annoying thing that needed doing because of chang --------

item_price_cols<-tibble::tibble(items=c(food_prices,nfi_prices),
                                item_type=ifelse(items %in% food_prices,"food","nfi"))


item_price_cols_fixed<-item_price_cols %>%
  left_join(name_changes, by=c("items"="round_1")) %>%
  mutate(round_2_items=ifelse(is.na(round_2),items, round_2)) %>% select(-round_2)

food_prices<- item_price_cols_fixed %>% filter(item_type=="food") %>% pull(round_2_items)
nfi_prices<- item_price_cols_fixed %>% filter(item_type=="nfi") %>% pull(round_2_items)

#adjust prev round item - rename where we can
prev_round_names_fix<-prev_round %>% rename_at(.vars = item_price_cols_fixed$items,function(x){x<-item_price_cols_fixed$round_2_items})

# add medians of medians for nfi and food items
current_round<-current_round %>% add_rowwise_nfi_food_medians()
prev_round_names_fix<-prev_round_names_fix %>% add_rowwise_nfi_food_medians()

#make list of vars to calc median
item_prices_full<-c(item_price_cols_fixed$round_2_items,"i.food_item_median" ,"i.nfi_item_median")


current_round_median_prices<-current_round %>%
  summarise_at(.vars = item_prices_full, median,na.rm=T) %>%
  select(item_prices_full)
previous_round_median_prices<-prev_round_names_fix %>%
  summarise_at(.vars = item_prices_full, median,na.rm=T) %>%
  select(item_prices_full)

price_differences<-percent_diff_dm(x = current_round_median_prices,y=previous_round_median_prices)
price_differences_dm<-cbind(price_differences$current_round,price_differences$percent_diff, price_differences$triangle)

#combine wo datamerge outputs
dm1<-cbind(price_differences_dm,page_3_fs_dm)

# dm1 %>% write.csv("BGD_2020_Markets_Covid/outputs/datamerge/datamerge_20200511.csv", na="",row.names = F)





