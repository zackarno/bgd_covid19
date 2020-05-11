

df<-read_csv("BGD_2020_Markets_Covid/inputs/clean_data/01_reach_bgd_markets_cleaned_round1.csv")
ks<-readxl::read_xls(path = "BGD_2020_Markets_Covid/inputs/kobo_tool/BGD_covid_19_market_monitoring.xls",sheet = "survey")
kc<-readxl::read_xls(path = "BGD_2020_Markets_Covid/inputs/kobo_tool/BGD_covid_19_market_monitoring.xls",sheet = "choices")
?rename_all

# FUNCTION 1
make_xlsform_lookup_table<- function(kobo_survey, kobo_choices, label_column){
  question_label_col<- paste0("question_",label_column)
  choice_label_col<- paste0("choice_",label_column)
  kobo_survey %>%
    select(type, name,all_of(label_column)) %>%
    rename_all(,.funs = function(x){paste0("question_",x)}) %>%
    filter(str_detect(question_type, "select")) %>%
    select(1:3) %>%
    mutate(question_list_name=str_replace_all(question_type,
                                              c("select_one"="", "select_multiple"="")) %>% trimws()) %>%
    right_join(kobo_choices %>% select(1:3) %>%
                 rename_all(,.funs = function(x){paste0("choice_",x)}),
               by=c("question_list_name"= "choice_list_name")) %>%
    mutate(xml_format=paste0(question_name,".",choice_name))
  }

xls_lookup<-make_xlsform_lookup_table(kobo_survey = ks,kobo_choices = kc,label_column = "label::english")
library(srvyr)
dfsvy<-srvyr::as_survey(df)

df %>%
  select(starts_with("prevent_the_spread_of_COVID19")) %>%# rownames_to_column("gg") %>%
  summarise_all(.funs=mean) %>%
  pivot_longer(cols = 1:ncol(.),names_to="option", values_to = "value") %>%
  arrange(desc(value)) %>%
  pivot_wider(names_from = "option",values_from = "value")

df$prevent_the_spread_of_COVID19.facemasks



asdf<-return_top_sm(dfsvy,rank_col="prevent_the_spread_of_COVID19" ,rank_n = 2,
                    xlsform_lookup = xls_lookup)

check_dot<-function(x){
  stringr::str_ends(string = x,pattern ="\\." )
}



return_top_sm<-function(df,
                        rank_col,
                        rank_n,
                        tie_break_remove,
                        aggregation_level=NULL, xlsform_lookup =NULL){

  result<-list()
  if(check_dot(rank_col)==F){
    rank_col<-paste0(rank_col,".")
  }
  cols_to_rank<-df %>%
    select(starts_with(rank_col)) %>% colnames()
  rank_cols_analyzed<- butteR::mean_proportion_table(design = df,list_of_variables = cols_to_rank, aggregation_level=aggregation_level)

  rank_table<-rank_cols_analyzed %>%
    pivot_longer(cols = 1:ncol(.),names_to="option", values_to = "value") %>%
    arrange(desc(value)) %>%
    mutate(rank_last=rank(desc(value),ties.method = "last")
    ) %>%
    group_by(value) %>%
    mutate(num_tie=n()) %>%
    arrange(rank_last)
  if(!is.null(xlsform_lookup)){
    rank_table<-rank_table %>% left_join(xlsform_lookup %>% select(xml_format, question_name,starts_with("choice_label")), by=c("option" ="xml_format"))}


  if(rank_table%>% filter(rank_last==rank_n) %>% pull(num_tie)>1){
    nth_rank_value<-rank_table %>% filter(rank_last==rank_n) %>% pull(value)
    number_records_with_tie<-rank_table %>% filter(value>=nth_rank_value) %>% nrow()
    number_records_gt_rank<-number_records_with_tie - rank_n
    tie_records<- rank_table %>%
      filter(value==nth_rank_value) %>%
      mutate(number_to_remove=number_records_gt_rank)


    result[["ranks_with_ties"]]<- rank_table
    result[["ranks_filtered"]]<-rank_table%>% filter(rank_last<=rank_n)
    result[["tied_records"]]<- tie_records
    print(paste("top",rank_n, "in", sm,"were tied", sep = " "))
    interactive_title<-paste0("You need to remove",number_records_gt_rank, "record(s)", "from the tie_records table")
    tie_break_remove <- select.list(tie_records$option,multiple = T, title = interactive_title)

    if(!is.null(tie_break_remove)){
      tie<-result$ranks_with_ties %>%
        group_by(value) %>%
        filter(!option%in% tie_break_remove) %>% ungroup() %>%
        mutate(rank_final= 1:n()) %>% filter(rank_final<=rank_n)
      result[["ranks_long_for_dm"]]<-tie
    }else{
      result[["ranks_long_for_dm"]]<-rank_table%>%
        filter(rank_last<=rank_n) %>%
        rename(rank_final="rank_last")

    }


    results_long<-result$ranks_long_for_dm %>%
      mutate(question_rank_lab=paste0(question_name,"_lab_",rank_final),
             question_rank_val=paste0(question_name,"_val_",rank_final)) %>% ungroup() %>%
      select(question_rank_lab,question_rank_val, value,starts_with("choice"))

     labels_wide<- results_long %>% pivot_wider(-c(question_rank_val,value), names_from=c("question_rank_lab"), values_from = c("choice_label::english"))
     values_wide<- results_long %>% pivot_wider(-c(question_rank_lab,"choice_label::english"), names_from=c("question_rank_val"), values_from = c("value"))
     results_wide<- cbind(labels_wide, values_wide)
    result[["wide"]]<-results_wide


      result$ranks_long_for_dm %>%  pivot_wider(names_from = option, values_from = value)

    }
  return(result)
}

