
make_composite_indicators_bgd_msna_2019 <- function(hh_data, individual_data, population) {


# Individual to individual_both_refugee_and_host_community ----------------

ind_to_ind <- individual_data %>%
  mutate(
    I.INDV_CHAR.age_groups_adult_child.INDV = if_else(ind_age < 18,"child",
                                                      if_else (ind_age >= 18,"adult","999",NULL)),
    I.INDV_CHAR.age_groups_demographics.INDV = if_else(ind_age < 5, "0-4",
                                                       if_else(ind_age %in% 5:11,"5-11",
                                                               if_else(ind_age %in% 12:17,"12-17",
                                                                       if_else( ind_age %in% 18:24, "18-24",
                                                                                if_else(ind_age %in% 25:59 ,"25-59",
                                                                                        if_else(ind_age >= 60,"60+","999",NULL)))))),
    I.HEALTH.age_groups_medical.INDV = if_else(ind_age < 18,"0-17",
                                               if_else(ind_age %in% 18:59,"18-59",
                                                       if_else(ind_age >= 60,"60+","999",NULL))),
    ind_gender=if_else(ind_gender== "male", "male", "female", missing=NULL)


    )



# Individual to Household_both_refugee_and_host_community -------------------------------------------------

 if (population == "Refugee"){
    HEALTH.safety_concerns_access_health <- individual_data[,c("ind_why_notreatment.safety_concerns","ind_why_notreatment.safety_concerns_night")]
    atleast_one_child_hazardous <- individual_data[,c("ind_work_hazardous.machine_heavy_objects", "ind_work_hazardous.more_40_hours",
                                                 "ind_work_hazardous.chemicals", "ind_work_hazardous.heat",
                                                 "ind_work_hazardous.work_night", "ind_work_hazardous.sharp_objects")]

  }#for Refugee


  if (population == "Host"){
    HEALTH.safety_concerns_access_health <- individual_data[,c("ind_why_notreatment.safety_concerns","ind_why_notreatment.night_safety")]

    atleast_one_child_hazardous <- individual_data[,c("ind_work_situation.machine_heavy_objects", "ind_work_situation.more_40_hours",
                                                      "ind_work_situation.chemicals", "ind_work_situation.heat",
                                                      "ind_work_situation.work_night", "ind_work_situation.sharp_objects")]

    }#for host

indiv_to_hh <- individual_data %>%
    mutate(
      atleast_one_child_hazardous_rowsum = rowSums(atleast_one_child_hazardous, na.rm = T),
      HEALTH.safety_concerns_access_health_sum = rowSums(HEALTH.safety_concerns_access_health,na.rm = T ),

      #   age_under_5 = if_else(ind_age <5 , "yes","no"),
        adult_yes = if_else(ind_age >=18 , 1,0)
      #   age_over_60= if_else(ind_age >= 60 , "yes","no")
    ) %>%
    group_by(X_submission__uuid) %>%
    summarise(
      dependents = (length(ind_age[ind_age<15]) + length(ind_age[ind_age>64])),
      non_dependent = (length(ind_age[ind_age %in% 15:64])),
      I.HH_CHAR.dependency_ratio.INDVHH  = (dependents/non_dependent),
      I.HH_CHAR.dependency_ratio_classification.HH = if_else(I.HH_CHAR.dependency_ratio.INDVHH  >= 1.5, "High", "Low", NULL),
      I.CP.sep_unaccom_minor_atleast_one.INDVHH = if_else(any(ind_new_hh_member == "yes"),true = "yes",false = "no",missing = "no"),

      I.HEALTH.indillness_atleast.INDVHH = if_else(any(ind_illness == "yes"), "yes","no"),
      I.HEALTH.indhelp_atleast.INDVHH = if_else(any(ind_help_daily == "yes" & ind_age >= 5 ),"yes","no"),
      I.HEALTH.indhelp_atleast_elderly.INDVHH = if_else(any(ind_help_daily == "yes" & ind_age >= 60),"yes","no"),
      I.FSL.livelihoods_atleast_one.INDVHH = if_else(any(ind_work == "yes" & ind_age >= 5),"yes","no","no"),
      I.FSL.livelihoods_atleast_one_child.INDVHH = if_else(any(ind_work == "yes" & ind_age < 18),"yes","no","no"),
      I.HEALTH.safety_concerns_access_health_any.INDVHH = if_else(any(HEALTH.safety_concerns_access_health_sum == 1),"yes","no"),
      I.P.safety_issues_barriers_health_facilities.INDVHH =  if_else(any(HEALTH.safety_concerns_access_health_sum == 1),"Health_unsafe","no"),
      number_people_over18= sum(adult_yes,na.rm = TRUE),
      I.CP.livelihoods_atleast_one_child_hazardous.INDVHH= if_else(any(atleast_one_child_hazardous_rowsum >= 1),"yes","no",NULL),
      I.FSL.livelihoods_atleast_one_adult.INDVHH = if_else(any(ind_age>18 & ind_work == "yes"),"yes","no","no")


    )

# Household to Household_both_regfugee and host community  ---------------------------------------------------------------

  enough_water_without_drinking <- hh_data[,c("enough_water.cooking", "enough_water.washing_bathing",
                                         "enough_water.domestic_purpose")]
  medical_expenses_grater_than_zero <- c("501_1000_bdt","1_500_bdt","1001_2000_bdt","2001_5000_bdt","5001_plus_bdt")

  marital_status <- c("single","widow","separated","divorced")

  hh_to_hh<- mutate(hh_data,
                    I.HH_CHAR.size.HH = if_else(hh_size < 6, "small","large", missing = NULL),
                    I.HH_CHAR.gender_hoh.HH = if_else(respondent_hoh == "yes",hh_data$respondent_gender, hh_data$hoh_gender, NULL ),
                    I.HH_CHAR.age_hoh.HH = if_else(respondent_hoh == "yes", true =hh_data$respondent_age, false = hh_data$hoh_age, missing = NULL ),
                    I.HH_CHAR.singe_female_hoh.HH = if_else(I.HH_CHAR.gender_hoh.HH == "female" & hoh_marital %in% marital_status ,"yes","no"),
                    I.P.married_women_no_work_market_alone.HH = if_else(married_work_mobility == "never_alone" & married_market_mobility == "never_alone","yes","no"),
                    I.P.unmarried_women_no_work_market_alone.HH =  if_else(unmarried_work_mobility == "never_alone" & unmarried_market_mobility == "never_alone" , "yes","no"),
                    enough_water_without_drinking_rowsum = rowSums(enough_water_without_drinking),

                    I.WASH.enough_water_all_needs.HH = if_else( enough_water_without_drinking_rowsum + enough_water.drinking == 4 , "yes","no",NULL),
                    # I.WASH.enough_drinking_only_andnot_oneother.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==2 ,"yes","no",NULL),
                    I.WASH.enough_drinking_and_two_other.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==2 ,"yes","no",NULL),
                    I.WASH.enough_drinking_and_one_other.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==1 ,"yes","no",NULL),
                    I.WASH.enough_drinking_only.HH= if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum == 0 ,"yes","no",NULL),

                    # I.WASH.enough_water_all_needs.HH = if_else( enough_water_without_drinking_rowsum + enough_water.drinking == 4 , "yes","no",NULL),
                    # I.WASH.enough_drinking_only_andnot_oneother.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==2 ,"yes","no",NULL),
                    # I.WASH.enough_drinking_only_andnot_twoothers.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==1 ,"yes","no",NULL),
                    # I.WASH.enough_drinking_only_andnot_threeothers.HH= if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum == 0 ,"yes","no",NULL),
                    I.WASH.visible_faeces_stagwater_waste_accom.HH =if_else(stagnant_water == "yes" & visible_faeces == "yes" & visible_waste == "yes", "yes","no",NULL),
                    I.FCS_score = (cereals_tubers*2 + pulses_nuts_seeds*3 +
                                     vegetables*1 + fruits*1 +dairy*4 +
                                     meat_fish*4 + oil_fats*.5 +sweets*.5 +
                                     spices_condiments*0),
                    I.FSL.food_consumption_score.HH = if_else(I.FCS_score > 42, "Acceptable",
                                                              if_else (I.FCS_score >28 ,  "Borderline",
                                                                       if_else(I.FCS_score >= 21 , "Poor",
                                                                               if_else( I.FCS_score < 21, "Extreme","no" )))),
                    I.HH_CHAR.childheaded_households.HH = if_else(informed_consent == "child_head", "yes","no"),
                    I.HEALTH.health_expenses_reported.HH = if_else(exp_medical %in% medical_expenses_grater_than_zero | health_coping.pay_care == 1, "yes","no","no" ),
                    I.HEALTH.debt_pay_health.HH = if_else(health_coping.ghealth_debt == 1 | debt_reason.pay_health_expenses ==1 , "yes","no","no"),
                    I.HEALTH.atleast_oneunder5_diarrhea.HH = if_else(under5_diarrhea > 0,"yes","no",NULL),
                    I.HEALTH.atleast_oneover5_diarrhea.HH = if_else(over5_diarrhea > 0, "yes","no",NULL)

  )
  hh_with_individual_level_data<-hh_to_hh %>% left_join(indiv_to_hh, by= c("X_uuid"="X_submission__uuid"))

  if (population == "Refugee"){

    Some_primary<- c ("elementary_1", "elementary_2", "elementary_3", "elementary_4","kindergarten")
    Primary_and_above<- c("elementary_5","middle_6", "middle_7", "middle_8", "middle_9", "high_10", "high_11", "tertiary")
    no_formal_education <- c("noedu","madrassa", "dntknow_no_answer")

    Language <- hh_with_individual_level_data[,c("language.chittagonian","language.burmese" ,"language.english","language.rohingya","language.bangla","language.arabic","language.other")]

    firewood_womenchildren <- hh_with_individual_level_data[,c("wood_access.adult_female","wood_access.girls","wood_access.boys")]


    FSL.begging_borrowing_bartering_support_food_included <- hh_with_individual_level_data[, c("food_source.support_relatives","food_source.exchange", "food_source.borrowed",
                                                                          "food_source.begging")]
    FSL.begging_borrowing_bartering_support_food_excluded <- hh_with_individual_level_data[, c("food_source.own_production","food_source.gather", "food_source.hunt_fish",
                                                                          "food_source.assistance_distribution", "food_source.assistance_voucher",
                                                                          "food_source.army" )]
    SNFI.no_shelter_improvements_reason <-c("no_money","no_labor","dnk_items","market_unreachable")
    FSL.refugee_food_assistance_only_included <- hh_with_individual_level_data[,c("food_source.army",
                                                             "food_source.assistance_distribution",
                                                             "food_source.assistance_voucher")]
    FSL.refugee_food_assistance_only_excluded <- hh_with_individual_level_data[,c("food_source.purchase","food_source.support_relatives", "food_source.exchange", "food_source.borrowed",
                                                             "food_source.begging", "food_source.gather", "food_source.hunt_fish",
                                                             "food_source.own_production")]
    cooking_fuel_1 <- hh_with_individual_level_data[,c("cooking_fuel.purchased_firewood", "cooking_fuel.collected_firewood",
                                  "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove",
                                  "cooking_fuel.dung_cakes", "cooking_fuel.other", "cooking_fuel.dntknow_prefer")]
    safety_issues_barriers_key_facilities_included_education <- hh_with_individual_level_data[,c("education_barrier.center_unsafe",
                                                                      "education_barrier.way_unsafe")]
    safety_issues_barriers_key_facilities_included_market <- hh_with_individual_level_data[,c("market_problems.market_unsafe_way",
                                                                   "market_problems.market_unsafe")]
    P.awareness_community_protection_atleast_two <- hh_with_individual_level_data[,c("comm_mechanisms.health",
                                                          "comm_mechanisms.education", "comm_mechanisms.safety", "comm_mechanisms.community_mob",
                                                          "comm_mechanisms.natural_disasters", "comm_mechanisms.disabilities",
                                                          "comm_mechanisms.child_protect", "comm_mechanisms.dispute_resolution", "comm_mechanisms_other")]

    exp_shelter_materials_greater_than_zero <- c("1_500_bdt", "501_1000_bdt","1001_2000_bdt","2001_5000_bdt","5001_plus_bdt") #excluding dntknow_prefer

    hh_distress <- hh_with_individual_level_data[,c("HH_distress.angry_outburst", "HH_distress.changes_eating",
                       "HH_distress.violence", "HH_distress.substance_abuse", "HH_distress.sleep_disruptions")]


hh_to_hh <- hh_with_individual_level_data %>%
      mutate(
        language_rowsum = rowSums(Language,na.rm = TRUE),
        I.HH_CHAR.language_fluency.HH= if_else(language_rowsum==1, "Monolingual",
                                               if_else(language_rowsum == 2,"Bilingual",
                                                       if_else(language_rowsum>2,"Multilingual","999", missing = NULL))),

        I.HH_CHAR.datearrival_bgd.HH = if_else(anydate(datearrival_bgd) < dmy("01/01/2016")  , true = "Before 2016",
                                               if_else(anydate(datearrival_bgd) %in%  dmy("01/01/2016"):dmy("31/07/2017"),"Jan 2016- July 2017",
                                                       if_else(anydate(datearrival_bgd) > dmy("31/07/2017")  , true = "After July 2017", false = "999", missing = NULL))),

        I.HH_CHAR.datearrival_shelter.HH = if_else(anydate(datearrival_shelter) < dmy("01/08/2017")  , true = "pre_Aug_2017",
                                                   if_else(anydate(datearrival_shelter) %in%  dmy("01/08/2017"):dmy("30/06/2018"), true = "Aug2017-June2018",
                                                           if_else(anydate(datearrival_shelter) %in%  dmy("01/07/2018"):dmy("31/12/2018"), true = "July-Dec 2018",
                                                                   if_else(anydate(datearrival_shelter) %in%  dmy("01/01/2019"):dmy("31/03/2019"), true = "Jan-March 2019 ",
                                                                           if_else(anydate(datearrival_shelter) > dmy("31/03/2019")  , true = "April 2019_Present", false = "999" , missing = NULL))))),


        I.HH_CHAR.education_level.HH = if_else(edu_highest %in% Some_primary, "Some primary",
                                               if_else( edu_highest %in% Primary_and_above, "Primary and above",
                                                        if_else(edu_highest %in% no_formal_education, "No formal Education","999",missing = NULL ))),

        firewood_womenchildren_rowsum = rowSums(firewood_womenchildren),
        I.SNFI.firewood_womenchildren.HH = if_else(firewood_womenchildren_rowsum >=1, "yes", "no",NULL),


        I.SNFI.min_floormat.HH =if_else(floormat >= hh_size, true = "yes",false = "no", missing = NULL),
        I.SNFI.min_blanket.HH = if_else( blanket>= hh_size, true = "yes",false = "no", missing = NULL ),

        cooking_fuel_rowsum = rowSums(cooking_fuel_1,na.rm = T),
        I.SNFI.lpg_only.HH = if_else(cooking_fuel_rowsum == 1 & cooking_fuel.lpg_gas_cylinder == 1, "yes","no",NULL),

        I.SNFI.use_firewood_any.HH = if_else(cooking_fuel.purchased_firewood == 1 | cooking_fuel.collected_firewood == 1, "yes","no" ),

        I.SNFI.no_shelter_improvements_reason.HH =  if_else(improvement_reason %in% SNFI.no_shelter_improvements_reason ,"yes","no",NULL),

        I.SNFI.pay_shelter_materials_improvements.HH= if_else(shelter_purchased == "yes" | exp_shelter_materials %in% exp_shelter_materials_greater_than_zero, "yes","no"),

        FSL.refugee_food_assistance_only_excluded_rowsum = rowSums(FSL.refugee_food_assistance_only_excluded,na.rm = T),
        FSL.refugee_food_assistance_only_included_rowsum = rowSums(FSL.refugee_food_assistance_only_included, na.rm = T),

        I.FSL.refugee_food_assistance_only.HH = if_else(FSL.refugee_food_assistance_only_included_rowsum >= 1 &
                                                          FSL.refugee_food_assistance_only_excluded_rowsum == 0 , "yes","no",NULL),

        FSL.begging_borrowing_bartering_support_food_included_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_included,na.rm = T),
        FSL.begging_borrowing_bartering_support_food_excluded_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_excluded,na.rm = T),
        I.FSL.begging_borrowing_bartering_support_food.HH = if_else(FSL.begging_borrowing_bartering_support_food_included_rowsum >=1 &
                                                                      FSL.begging_borrowing_bartering_support_food_excluded_rowsum == 0, "yes","no",NULL),

        safety_issues_barriers_key_facilities_included_education_rowsum = rowSums(safety_issues_barriers_key_facilities_included_education,na.rm = T),
        safety_issues_barriers_key_facilities_included_market_rowsum = rowSums(safety_issues_barriers_key_facilities_included_market,na.rm = T),

        I.P.safety_issues_barriers_education_facilities.HH = if_else(safety_issues_barriers_key_facilities_included_education_rowsum >=1 , "Education_unsafe","no",NULL),
        I.P.safety_issues_barriers_markets.HH = if_else(safety_issues_barriers_key_facilities_included_market_rowsum >=1 , "Market_unsafe","no"),

        #below pb, have to check
        P.awareness_community_protection_atleast_two_rowsum = rowSums(P.awareness_community_protection_atleast_two),

        I.P.awareness_community_protection_atleast_two.HH = if_else(P.awareness_community_protection_atleast_two_rowsum >= 2,"yes","no"),



        I.FSL.debt_pay_food.HH = if_else(food_source.borrowed == 1 | debt_reason.buy_food == 1, "yes", "no", NULL),

        I.SNFI.portable_lamps_atleast_2.HH = if_else(portable_light >= 2, "yes","no",NULL),



        I.SNFI.pay_shelter_materials_improvements.HH = if_else(exp_shelter_materials %in% exp_shelter_materials_greater_than_zero | shelter_purchased == "yes" , "yes","no", NULL),


        hh_distress_rowsum = rowSums(hh_distress,na.rm = T),

        I.P.atleast_one_risk_member.HH = if_else( I.HH_CHAR.singe_female_hoh.HH == "yes" | I.CP.sep_unaccom_minor_atleast_one.INDVHH == "yes" |
                                                    child_marriage == "yes" | I.CP.livelihoods_atleast_one_child_hazardous.INDVHH == "yes" |
                                                    I.HEALTH.indhelp_atleast.INDVHH == "yes"| hh_distress_rowsum > 0,"yes","no"),

        I.P.safety_issues_barriers_key_facilities_any.HH= if_else(I.P.safety_issues_barriers_health_facilities.INDVHH == "Health_unsafe" |
                                                                     I.P.safety_issues_barriers_markets.HH == "Market_unsafe" |
                                                                     I.P.safety_issues_barriers_education_facilities.HH == "Education_unsafe","yes","no")



      ) %>% select("X_uuid",starts_with("I."))


# individual to individual ------------------------------------------------

    Ind_to_Ind_final <- mutate( ind_to_ind,

                          I.EDU.both_formal_informal_edu.INDV= if_else(ind_ed_TLC == "yes" | ind_ed_madrassa == "yes", "yes", "no",NULL),

                          I.HEALTH.ind_smoke_some_all_days.INDV = if_else( ind_smoke == "everyday" |ind_smoke == "some_days", "yes", "no",NULL),
                          I.EDU.age_group_education.INDV = if_else(ind_age %in% 3:4,"3-4",
                                                                   if_else(ind_age %in% 5:11,"5-11",
                                                                           if_else(ind_age %in% 12:17,"12-17",
                                                                                   if_else(ind_age %in% 18:24,"18-24",NULL,NULL))))

    ) %>%  select(X_index,starts_with("I."))

  }


  if (population == "Host") {

    Some_primary<- c ("1", "2", "3", "4")
    Primary_and_above<- c("5","6", "7", "8", "9", "10", "11", "12", "above_12_tertiary_edu")
    no_formal_education <- c("none","madrasah_only", "dntknow_prefer")

    safety_issues_barriers_key_facilities_included_education <- hh_with_individual_level_data[,c("education_barrier.school_unsafe",
                                                                            "education_barrier.way_unsafe")]
    safety_issues_barriers_key_facilities_included_market <- hh_with_individual_level_data[,c("market_problems.market_unsafe_way",
                                                                         "market_problems.market_unsafe")]

    SNFI.no_shelter_improvements_reason <-hh_with_individual_level_data[,c("improvement_reason.no_money",
                                                      "improvement_reason.no_labour",
                                                      "improvement_reason.no_item_acess")]

    firewood_womenchildren <- c("adult_female","girls","boys")

    FSL.begging_borrowing_bartering_support_food_included <- hh_with_individual_level_data[, c("food_source.borrow","food_source.support_friends_relatives",
                                                                          "food_source.begging_scavenging","food_source.barter_exchange")]

    FSL.begging_borrowing_bartering_support_food_excluded <- hh_with_individual_level_data[, c("food_source.purchase_cash", "food_source.purchase_credit",
                                                                          "food_source.food_assistance_food_card","food_source.gathering",
                                                                          "food_source.hunting_fishing", "food_source.own_production")]

    FSL.host_community_food_assistance_only_included <- hh_with_individual_level_data[,c("food_source.army_distributing","food_source.food_assistance_food_card")]

    FSL.host_community_food_assistance_only_excluded <- hh_with_individual_level_data[,c("food_source.purchase_cash", "food_source.purchase_credit",
                                                             "food_source.support_friends_relatives", "food_source.barter_exchange",
                                                             "food_source.borrow", "food_source.begging_scavenging", "food_source.gathering",
                                                             "food_source.hunting_fishing", "food_source.own_production","food_source.other")]

    cooking_fuel <- hh_with_individual_level_data[,c("cooking_fuel.purchased_firewood", "cooking_fuel.collected_firewood",
                                "cooking_fuel.biogas","cooking_fuel.induction","cooking_fuel.dried_leaf_hay",
                                "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove",
                                "cooking_fuel.dung_cakes", "cooking_fuel.other")]

    cooking_fuel_rowsum <- rowSums(cooking_fuel,na.rm = T)

    P.awareness_community_protection_atleast_two <- hh_with_individual_level_data[,c("comm_mechanisms.health", "comm_mechanisms.education",
                                                                "comm_mechanisms.safety_security", "comm_mechanisms.community_mobilization",
                                                                "comm_mechanisms.drr_preparation", "comm_mechanisms.support_disable",
                                                                "comm_mechanisms.protect_children", "comm_mechanisms.resolve_dispute")]

    # hh_to_hh<-hh_to_hh %>% left_join(indiv_to_hh, by=c("X_uuid"="X_submission__uuid"))

hh_to_hh <- hh_with_individual_level_data %>% mutate(

      I.SNFI.lpg_only.HH = if_else( cooking_fuel_rowsum == 1 & cooking_fuel.lpg_gas_cylinder == 1, "yes","no",NULL),

      safety_issues_barriers_key_facilities_included_education_rowsum = rowSums(safety_issues_barriers_key_facilities_included_education,na.rm = T),
      safety_issues_barriers_key_facilities_included_market_rowsum = rowSums(safety_issues_barriers_key_facilities_included_market,na.rm = T),

      I.P.safety_issues_barriers_education_facilities.HH  = if_else(safety_issues_barriers_key_facilities_included_education_rowsum >=1 , "Education_unsafe","no",NULL),
      I.P.safety_issues_barriers_markets.HH = if_else(safety_issues_barriers_key_facilities_included_market_rowsum >=1 , "Market_unsafe","no"),

      SNFI.no_shelter_improvements_reason_rowsum = rowSums(SNFI.no_shelter_improvements_reason),
      I.SNFI.no_shelter_improvements_reason.HH = if_else( SNFI.no_shelter_improvements_reason_rowsum >= 1 ,"yes","no",NULL),

      P.awareness_community_protection_atleast_two_rowsum = rowSums(P.awareness_community_protection_atleast_two,na.rm = T),
      I.P.awareness_community_protection_atleast_two.HH = if_else(P.awareness_community_protection_atleast_two_rowsum >= 2,"yes",
                                                                  if_else(P.awareness_community_protection_atleast_two_rowsum == 0,"None","no","no")),

      FSL.begging_borrowing_bartering_support_food_included_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_included,na.rm = T),
      FSL.begging_borrowing_bartering_support_food_excluded_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_excluded,na.rm = T),
      I.FSL.begging_borrowing_bartering_support_food.HH = if_else(FSL.begging_borrowing_bartering_support_food_included_rowsum >=1  &
                                                                    FSL.begging_borrowing_bartering_support_food_excluded_rowsum == 0, "yes","no",NULL),

      FSL.host_community_food_assistance_only_excluded_rowsum = rowSums(FSL.host_community_food_assistance_only_excluded,na.rm = T),
      FSL.host_community_food_assistance_only_included_rowsum = rowSums(FSL.host_community_food_assistance_only_included,na.rm = T),
      I.FSL.host_community_food_assistance.HH = if_else(FSL.host_community_food_assistance_only_included_rowsum >= 1, "yes","no",NULL),

      I.HH_CHAR.education_level.HH = if_else(edu_highest %in% Some_primary, "Some primary",
                                             if_else( edu_highest %in% Primary_and_above, "Primary and above",
                                                      if_else(edu_highest %in% no_formal_education, "No formal Education","999",missing = NULL ))),


      I.FSL.debt_pay_food.HH = if_else(food_source.borrow == 1 | debt_reason.buy_food == 1, "yes", "no"),


      I.SNFI.firewood_womenchildren.HH = if_else(wood_access %in% firewood_womenchildren, "yes", "no",NULL),

      I.P.min_national_IDcard.HH = if_else(valid_id >= number_people_over18 ,"yes","no",NULL), #age must be included 18, need to check

      I.SNFI.portable_lamps_atleast_2.HH = if_else(solar_light >=2 , "yes","no",NULL),


      I.FSL.without_livelihood_assets.HH = if_else(livestock =="yes" | agricultural_land =="yes" | fishing_gear =="yes","no","yes", NULL),

      I.P.atleast_one_risk_member.HH = if_else( I.HH_CHAR.singe_female_hoh.HH == "yes" | I.CP.sep_unaccom_minor_atleast_one.INDVHH == "yes" |
                                                  child_marriage == "yes" | I.CP.livelihoods_atleast_one_child_hazardous.INDVHH == "yes" |
                                                  I.HEALTH.indhelp_atleast.INDVHH == "yes","yes","no"),

      I.P.safety_issues_barriers_key_facilities_any.HH = if_else(I.P.safety_issues_barriers_health_facilities.INDVHH == "Health_unsafe" |
                                                                   I.P.safety_issues_barriers_markets.HH == "Market_unsafe" |
                                                                   I.P.safety_issues_barriers_education_facilities.HH == "Education_unsafe","yes","no"),



    )%>% select("X_uuid",starts_with("I."))


# Individual to Individual ------------------------------------------------
formal_education <- c ("govt_school","alia_madrasah","private_school","college","university","tech_college","dev_program ")
Informal_education <- individual_data[,c("ind_nonformal_learn.ngo_school", "ind_nonformal_learn.madrasah", "ind_nonformal_learn.vocational_training")]

Ind_to_Ind_final <- mutate(ind_to_ind,

                      Informal_education_rowsum = rowSums(Informal_education),
                      I.EDU.both_formal_informal_edu.INDV= if_else(ind_formal_learning %in% formal_education
                                                                   | Informal_education_rowsum >0 ,"yes","no",NULL),
                      I.HEALTH.ind_smoke_some_all_days.INDV = if_else( ind_smoke == "everyday" |ind_smoke == "some_days", "yes", "no",NULL),
                      I.EDU.formal_2018_not_2019.INDV = if_else( ind_formal_learning_prev %in% formal_education & ind_formal_learning == "none", "yes","no","no"),
                      I.EDU.madrassah_only.INDV = if_else( Informal_education_rowsum == 1 & ind_nonformal_learn.madrasah ==1 & ind_formal_learning == "none","yes","no", NULL),
                      I.EDU.any_formal_learning.INDV = if_else(ind_formal_learning %in% formal_education, "yes","no"),
                      I.EDU.any_informal_learning.INDV = if_else(Informal_education_rowsum > 0, "yes","no"),
                      I.EDU.age_group_formal_education.INDV = if_else(ind_age ==4,"4",
                                                               if_else(ind_age %in% 5:11,"5-11",
                                                                       if_else(ind_age %in% 12:17,"12-17",
                                                                               if_else(ind_age %in% 18:24,"18-24",NULL,NULL)))),
                      I.EDU.age_group_nonformal_education.INDV =if_else(ind_age %in% 5:11,"5-11",
                                                                       if_else(ind_age %in% 12:17,"12-17",
                                                                               if_else(ind_age %in% 18:24,"18-24",NULL,NULL)))

) %>%  select(X_index,starts_with("I."))
    }


  # INDV_to_HH <- indiv_to_hh
  combined_results<- list(household_composites=hh_to_hh, individual_composites=Ind_to_Ind_final)

 }


