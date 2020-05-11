rm(list = ls())

library(dplyr)

basic_analysis<- read.csv("outputs/02_butter_analysis/basic_analysis_overall.csv",stringsAsFactors = FALSE,
                          na.strings = c("", " ", NA))

#remove cols, which value is 0
basic_analysis<- basic_analysis[,-(which(colSums(basic_analysis)==0))]


# difficulties_faced_in_replenishing-top 5 --------------------------------

data_for_dfcults_fcd_in_repl <- basic_analysis %>% dplyr::select(starts_with("difficulties_faced_in_replenishing."))

sort_for_dfcults_fcd_in_repl <- sort(data_for_dfcults_fcd_in_repl[1,], decreasing = TRUE)


if( ncol(sort_for_dfcults_fcd_in_repl) > 5 ){
  top5_dfcults_fcd_in_repl<- sort_for_dfcults_fcd_in_repl[1:5] %>% colnames()
}

if( ncol(sort_for_dfcults_fcd_in_repl) < 6 ){
  top5_dfcults_fcd_in_repl<- sort_for_dfcults_fcd_in_repl %>% colnames()
}


# assistance_items_if_yes-top 5  ------------------------------------------

data_for_assistance_items_if_yes <- basic_analysis %>% dplyr::select(starts_with("assistance_items_if_yes."))

sort_assistance_items_if_yes <- sort(data_for_assistance_items_if_yes[1,], decreasing = TRUE)

if( ncol(sort_assistance_items_if_yes) > 5 ){
  top5_assistance_items_if_yes<- sort_assistance_items_if_yes[1:5] %>% colnames()
}

if( ncol(sort_assistance_items_if_yes) < 6 ){
  top5_assistance_items_if_yes<- sort_assistance_items_if_yes %>% colnames()
}


# items_are_most_affected  ------------------------------------------------

data_for_items_are_most_affected <- basic_analysis %>% dplyr::select(starts_with("items_are_most_affected."))




