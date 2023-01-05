
lag_var_name <- function(x){
  
  x_n<- parse_number(x)
  y_n <- x_n-1
  y <- gsub(as.character(x_n),
            as.character(y_n),
            x)
  y
}


# Inclusion criteria
# age 19-75
# regular visits prior to pandemic  (reg_visits_before_covid_2)
# No dental  pain at the baseline

get_analytic <- function(dat){
  
  dat %>% 
    # age 19-75 (xxx dropped)
    filter(w_age>18 & w_age<76 ) %>%
    # filter(reg_visits_0==2) %>%
    # No dental  pain at the baseline (643 dropped)
    filter(pain_0==2) %>% 
    filter(visits_interupted_0!=3) %>% 
    mutate(q_age=case_when(w_age<35~ 1,
                           w_age>34 & w_age<55~ 2,
                           w_age>55~ 3)) %>% 
    mutate(c1= ifelse(rowSums(is.na(select(.,ends_with(c("1","2","3")))))== 
                        ncol(select(.,ends_with(c("1","2","3")))), 0,1),
           c2= ifelse(rowSums(is.na(select(.,ends_with(c("2","3")))))== 
                        ncol(select(.,ends_with(c("2","3")))), 0,1),
           c3= ifelse(rowSums(is.na(select(.,ends_with("3"))))== 
                        ncol(select(.,ends_with("3"))), 0,1)) %>% 
    mutate(
      # create exposure
      a0= case_when(is.na(visits_interupted_0)~ NA_real_,
                    visits_interupted_0==1 ~ 1,
                    TRUE~ 0),
      a1= case_when(is.na(visits_interupted_1)~ NA_real_,
                    visits_interupted_1==1 ~ 1,
                    TRUE~ 0),
      a2= case_when(is.na(rx_visits_interupted_2) & is.na(cx_visits_interupted_2)~ NA_real_,
                    rx_visits_interupted_2==1|cx_visits_interupted_2==1 ~ 1,
                    TRUE~ 0),
      a3= case_when(is.na(rx_visit_interupted_3) & is.na(cx_visit_interupted_3)~ NA_real_,
                    rx_visit_interupted_3==1|cx_visit_interupted_3==1 ~ 1,
                    TRUE~ 0),
      # create pain status
      y0= case_when(is.na(pain_0) ~ NA_real_,
                    pain_0==2~ 0,
                    TRUE~ 1),
      y1= case_when(is.na(pain_1) ~ NA_real_,
                    pain_1==2~ 0,
                    TRUE~ 1),
      y2= case_when(is.na(pain_caries_2) & is.na(pain_gum_2) ~ NA_real_,
                    pain_caries_2==1~ 1,
                    pain_gum_2==1~ 1,
                    TRUE~ 0),
      # y2= if_else(is.na(y2), y1, y2),
      y3= case_when(is.na(pain_3)~ NA_real_,
                    pain_3== 1~ 0,
                    # pain_3== 2~ 0,
                    TRUE~ 1)
    ) %>%
    # covariates
    mutate(
      # Clean num of family members
      across(contains("_members"), ~ifelse(.x>6,6,.x)),
      across(contains("inc"), ~ifelse(.x==19|.x==20,NA_real_,.x)),
      across(contains("inc"), ~ifelse(is.na(.x),median(.x,na.rm = T),.x)),
      across(contains("members"), sqrt, .names = "sqrt_{col}"),
      across(contains("inc"), ~case_when(
        .x==1~ 0, .x==2~ 0.25, .x==3~ 0.75, .x==4~ 1.5, .x==5~ 2.5, 
        .x==6~ 3.5, .x==7~ 4.5, .x==8~ 5.5, .x==9~ 6.5, .x==10~ 7.5, 
        .x==11~ 8.5, .x==12~ 9.5, .x==13~ 11, .x==14~ 13, .x==15~ 15, 
        .x==16~ 17, .x==17~ 19, .x==18~ 20
      ), .names = "l{c(0:3)}_inc"),
      # create eqincome 
      across(paste0("l",0:3,"_inc"))/ across(paste0("sqrt_hh_members_",0:3)),
      l0_mari= case_when(
        l0_mari==1~ "married",
        l0_mari==2~ "single",
        l0_mari==3|l0_mari==4 ~ "widowed",
        l0_mari==5|l0_mari==6 ~ "divorsed"),
      across(paste0("l",1:3,"_mari"), ~case_when(
        is.na(.x)~ NA_character_,
        .x<4 ~ "married",
        .x==4 ~ "single",
        .x==5|.x==6|.x== 7 ~ "widowed",
        .x==8|.x==9|.x==10 ~ "divorsed")),
      across(contains(c("smok","alco")), ~case_when(
        is.na(.x)~ NA_character_,                                           
        .x==1~ "non",
        .x==2~ "ex_light",
        .x==3~ "ex_habitual",
        .x==4~ "current_light",
        .x==5~ "current_habitual")),
      w_reg_visits= reg_visits_0) %>% 
    
    mutate(across(contains(c("mari","smok","alco")),as.factor),
           w_education= case_when(w_education<4 | w_education==10   ~ "high_school",
                                  w_education==4 | w_education==5 ~ "technical",
                                  w_education>5 & w_education<9  ~ "uni",
                                  w_education==9 ~ "postgrad"
                                  )) %>% 
    fastDummies::dummy_cols(c("w_sex","w_perio","w_caries",
                              "w_reg_visits","w_education",
                              paste0("l",0:3,"_mari"),
                              paste0("l",0:3,"_smok"),
                              paste0("l",0:3,"_alco")),
                            remove_first_dummy = T,
                            ignore_na = T,
                            remove_selected_columns = T ) %>%
    mutate(across(where(is.integer), as.numeric)) %>% 
    select(starts_with(c("w","q","l0","a0","y0","c1",
                         "l1", "a1","y1","c2",
                         "l2","a2","y2","c3","y3","q_age")), 
           visit_hx=reg_visits_0)  %>%
    mutate(visit_hx=ifelse(visit_hx==1, "regular", "non_regular"),
           sex= ifelse(w_sex_2==1, "female", "male")) %>% 
    mutate(across(.cols = contains("1"),
                  .fns = ~ifelse(is.na(get(glue::glue("{cur_column()}"))),
                                 get(lag_var_name(cur_column())), .x))) %>% 
    mutate(across(.cols = contains("2"),
                  .fns = ~ifelse(is.na(get(glue::glue("{cur_column()}"))),
                                 get(lag_var_name(cur_column())), .x))) %>% 
    select(-y0) %>% 
    lmtp::event_locf(c(paste0("y",1:3)))
  
}


# initiate other super learner candidates
get_learners <- function(lrnr_names){
  
  lasso <- sl3::Lrnr_glmnet$new(alpha = 1)
  ridge <- sl3::Lrnr_glmnet$new(alpha = 0)
  enet <- sl3::Lrnr_glmnet$new(alpha = 0.5)
  mean <- sl3::Lrnr_mean$new()
  xgb <- sl3::Lrnr_xgboost$new()
  
  learners <- unlist(list(
    lasso= lasso, ridge=ridge, enet=enet, mean=mean, xgb=xgb
  )
  , recursive = T 
  ) 
  
  names <- lrnr_names
  selected_learners <- learners[names] %>% unname()
  
  sl3::make_learner(sl3::Stack, selected_learners)
  
}



## -----------------------------------------------------------------------------------------


## make results table for dental pain incidence rate 

get_sdr_estimates <- function(results){
  imap_dfr(results, # a named list of results
           function(x,y){
             x %>%
               tidy() %>%
               mutate(intervention = y,
                      estimate=1-estimate,
                      conf_low = 1-conf.high,
                      conf_high=1-conf.low,
               ) %>%
               select(intervention, estimate, conf_low, conf_high)
             
           })
}



get_diff <- function(x){ 
  
  exposed<- filter(x, 
                   a=="interupted")$mod %>%  .[[1]]
  non_exposed<- filter(x, 
                       a=="not_interupted")$mod %>%  .[[1]]
  lmtp_contrast(exposed, ref=non_exposed)$vals %>%
    mutate(estimate = -theta,
           conf_low = -conf.high,
           conf_high= -conf.low,
           intervention = "Difference")  %>%
    select(intervention, estimate, conf_low, conf_high) %>% 
    add_column(grp= x$grp %>% unique(), .before = 1 )
}




