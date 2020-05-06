remove_kobo_grouper<-function(colname){
  sub(".*?\\.", '', colname)
}



maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}


filter_answer_by_rank<- function(df,unique_answer_col, rank){
  answer_name<-paste0(unique_answer_col,"_", rank)
  answer_val<-paste0(answer_name,"_val")

  answer_df<-df %>% select(A.base, contains(unique_answer_col)) %>% group_by(A.base)
  answer_df_long<-answer_df %>% pivot_longer(cols=2:ncol(.), names_to=answer_name, values_to = answer_val)
  answer_df_long_filtered<-answer_df_long %>% filter(!!sym(answer_val)==maxN(!!sym(answer_val),rank))
  return(answer_df_long_filtered)
}



return_top_n_choices<- function(df,unique_answer_col,rank){
  answers_ranked_list<-list()
  for (i in 1:rank){
    answers_ranked_list[[i]]<-filter_answer_by_rank(df=basic_analysis1,unique_answer_col ,rank = i)

  }
answer_ranked_df<-purrr::reduce(answers_ranked_list,left_join)
return(answer_ranked_df)
}

return_top_n_choices<- function(df,unique_answer_col,rank){
  answers_ranked_list<-list()
  for (i in 1:rank){
    answers_ranked_temp<-filter_answer_by_rank(df=basic_analysis1,unique_answer_col ,rank = i)
    dup_value<-answers_ranked_temp[[3]][duplicated(answers_ranked_temp[[3]])]
    dup_df<-answers_ranked_temp[answers_ranked_temp[[3]]==dup_value,]
    if(nrow(dup_df)>0){
      print(paste0("There are ", nrow(dup_df) ," duplicated values in rank number ",i))
      names_of_dup_vals<-paste(unique(dup_df[[2]]), sep="", collapse="  and ")
      print(paste0(names_of_dup_vals, " both have values of ", unique(dup_df[[3]])))
    }
    answers_ranked_list[[i]]<-answers_ranked_temp


  }
  answer_ranked_df<-purrr::reduce(answers_ranked_list,left_join)
  return(answer_ranked_df)
}






df_list<-list()
for (i in 1:3){
  df_list[[i]]<-filter_answer_by_rank(df=basic_analysis1,unique_answer_col = "push_factors",rank = i)
}
purrr::reduce(df_list, left_join)
