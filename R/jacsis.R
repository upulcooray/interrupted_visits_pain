library(tidyverse) # load tidyverse package
library(lmtp) # note that this code uses sl3 version, load lmtp package
library(future) # load future package
library(sl3) # load sl3 package


plan(sequential)


sis_2020 <- read.csv("~/Desktop/JACSIS/JACSIS2020_raw_mod.csv")
tis_2021 <- read.csv("~/Desktop/JACSIS/JASTIS2021_raw_mod.csv")
sis_2021 <- read.csv("~/Desktop/JACSIS/JACSIS2021_raw_mod.csv")
tis_2022 <- read.csv("~/Desktop/JACSIS/JASTIS2022_raw_mod.csv")

# df20_21 <- read_dta("~/Desktop/JACSIS/JACSIS2020_JASTIS2021.dta")

# Q57 of 2020 1-yes 2-no
# [Q34.4] of 2021
# [Q34.4]
# Q34.3]

base<- sis_2020 %>% 
  select(USER_ID,
         reg_visits_0=Q57_8,  
         # 1.	Yes, 2.	no
         visits_interupted_0=Q19_8,
         # 1.	Yes,2.	no, 3.	Not applicable (e.g., no plans to do so)
         pain_0=Q17_7, 
         #1.	yes 2.	no
         w_perio=Q75_6,  
         #2.	Not in the present, but in the past
         w_caries=Q75_7,
         #2.	Not in the present, but in the past
         w_age=AGE,
         w_sex=SEX,
         hhinc_0= Q69,
         # 1.	0 yen , 2. 0.25m yen, 3.	0.75m yen
         # 4.	1.5m yen,  5.	2.5m yen,  6.	3.5m yen
         # 7.	4.5m yen, 8.	5.5m yen,  9.	6.5m yen, 10. 7.5m yen
         # 11. 8.5m yen, 12. 9.5m yen, 13.	11m yen, 14.	13m yen
         # 15.	15m yen, 16.	17m yen, 17.	19m yen, 18.	20m yen 
         # 19.	IDK 20.	Don't understand  <- make them 0?
         hh_members_0=SC1_1 ,
         # if >10 make them NA?
         l0_mari= Q8,
         # 1.	With spouse,  2.	unmarried
         # 3.	widowed before March 2020, 4.	widowed after April 2020
         # 5.	Divorced before March 2020, 6.	Divorced after April 2020
         l0_smok= Q65_1,
         # 1.	Never before,  2.	used- not habitually
         # 3.	used- habitually, 4.	current- not habitual
         # 5.	current- habitual
         l0_alco= Q62_1,
         # 1.	Never before,  2.	used- not habitually
         # 3.	used- habitually, 4.	current- not habitual
         # 5.	current- habitual
         CHIIKI, NAGE
         )  


w2<- tis_2021 %>% 
  select(USER_ID,
         reg_visits_1=Q34_4,
         # same as 2020 jacsis
         visits_interupted_1=Q20_10,
         # same as 2020 jacsis
         pain_1=Q23_7, 
         # same as 2020 jacsis
         hhinc_1= Q10,
         # same as 2020 jacsis
         hh_members_1=Q7_1 ,
         # if >10 make them NA?
         l1_mari= Q8,
         # 1:3= married, 4= unmaried, 5:7= widowed
         # 8:10= divorsed
         l1_smok= Q45_1,
         # same as 2020 jacsis
         l1_alco=Q39_1,
         # same as 2020 jacsis
         CHIIKI
         ) 

w3<- sis_2021 %>% 
  select(USER_ID,
         reg_visits_2=Q29_3, 
         # 1=visited, 2= no(non-covid reason), 3=no(covid reason)
         # 4= no(non-covid reason will go in future)
         # 5= no(covid reason, no plan to attend in the  future)
         reg_visits_before_covid_2=Q28_2,  
         # 1= regular 2=irregular 3=never
         rx_visits_interupted_2=Q27_8,
         # 1.	Yes,2.	no, 3.	Not applicable (e.g., no plans to do so)
         cx_visits_interupted_2=Q27_9,
         # 1.	Yes,2.	no, 3.	Not applicable (e.g., no plans to do so)
         pain_past_wk_2= Q25_9,
         pain_caries_2=Q27S5_1,  
         # 1.	Yes,2.	no
         pain_gum_2=Q27S5_2, 
         # 1.	Yes,2.	no
         hhinc_2= Q75_1,
         # same as 2020 jacsis
         hh_members_2=SC1_1 ,
         # same as 2020 jacsis
         l2_mari= Q13,
         # 1:3= married, 4= unmaried, 5:7= widowed
         # 8:10= divorsed
         l2_smok= Q70_1,
         # 1.	Never before,  2.	used- not habitually
         # 3.	used- habitually, 4.	current- not habitual
         # 5.	current- habitual
         l2_alco=Q68_1,
         # 1.	Never before,  2.	used- not habitually
         # 3.	used- habitually, 4.	current- not habitual
         # 5.	current- habitual
         CHIIKI
         ) 

w4<- tis_2022 %>% 
  select(USER_ID,
         reg_visits_3=Q40_3,
         # 1.	Yes,2.	no
         rx_visit_interupted_3= Q27_8,
         # 1.	Yes,2.	no, 3.	Not applicable (e.g., no plans to do so)
         cx_visit_interupted_3= Q27_9,
         # 1.	Yes,2.	no, 3.	Not applicable (e.g., no plans to do so)
         pain_3=Q29_9, 
         # 1.	Not bothered at all, 2.	Slightly annoyed
         # 3.	Slightly annoyed, 4.	Quite annoyed, 5.	Very annoyed
         hhinc_3= Q81_1,
         # same as 2020 jacsis
         hh_members_3=Q6_1 ,
         l3_mari= Q7,
         l3_smok= Q8,
         l3_alco=Q44_1,
         CHIIKI
  ) 


lag_var_name <- function(x){
  
  x_n<- parse_number(x)
  y_n <- x_n-1
  y <- gsub(as.character(x_n),
            as.character(y_n),
            x)
  y
}

lag_var_name("a1")


key <- "USER_ID"

dat <- left_join(base,w2,by=key) %>% 
  left_join(w3,by=key) %>% left_join(w4,by=key)

saveRDS(dat, "left_joined_4waves.rds")

# Inclusion criteria
# age 19-75
# regular visits prior to pandemic  (reg_visits_before_covid_2)
# No dental  pain at the baseline

analytic<- dat %>% 
  # age 19-75 (2045 dropped)
  filter(w_age>18 & w_age<76 ) %>%
  # regular visits prior to pandemic (19063 dropped)
  filter(reg_visits_0==2) %>%
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
      .x==5~ "current_habitual"))) %>% 
  
  mutate(across(contains(c("mari","smok","alco")),as.factor)) %>% 
  fastDummies::dummy_cols(c("w_sex","w_perio","w_caries",
                            paste0("l",0:3,"_mari"),
                            paste0("l",0:3,"_smok"),
                            paste0("l",0:3,"_alco")),
                          remove_first_dummy = T,
                          ignore_na = T,
                          remove_selected_columns = T ) %>%
  mutate(across(where(is.integer), as.numeric)) %>% 
  select(starts_with(c("w","q","l0","a0","y0","c1",
                       "l1", "a1","y1","c2",
                       "l2","a2","y2","c3","y3")))  %>% 
  mutate(across(.cols = contains("1"),
                .fns = ~ifelse(is.na(get(glue::glue("{cur_column()}"))),
                               get(lag_var_name(cur_column())), .x))) %>% 
  mutate(across(.cols = contains("2"),
                .fns = ~ifelse(is.na(get(glue::glue("{cur_column()}"))),
                               get(lag_var_name(cur_column())), .x))) %>% 
  select(-y0) %>% 
  lmtp::event_locf(c(paste0("y",1:3)))



men_df <- analytic %>% filter(q_age==3)



visits_interupted  <- function(dat, trt) {
  
  trt_letter <- str_split(trt,"",simplify = T)[[1]]
  trt_waves <- paste0(trt_letter,0:2)
  
  sum <- 0
  for(i in trt_waves){
    add <- ifelse(dat[[i]] == 1, 1, 0)
    sum <- sum + add
  }
  
  return(ifelse(sum > 0, 1, 0)) # otherwise, return 0
  
}  




visits_not_interrupted <- function(dat, trt) {
  
  dat[[trt]] <- 0 # entire treatment vector of interest to 0
  dat[[trt]] # return
  
}
  
# initiate other super learner candidates
lrn_lasso <- Lrnr_glmnet$new(alpha = 1)
lrn_ridge <- Lrnr_glmnet$new(alpha = 0)
lrn_enet <- Lrnr_glmnet$new(alpha = 0.5)
lrn_mean <- Lrnr_mean$new()

learners <- unlist(list(
  lrn_lasso,
  lrn_ridge,
  lrn_enet,
  lrn_mean
),
recursive = TRUE
)

lrnrs <- make_learner(Stack, learners) 

# set LMTP parameters of intervention,outcome and confounders

a <- analytic %>% select(starts_with("a")) %>% names() # names of intervention cols

b_cov <- analytic %>% select(starts_with("w_")) %>% 
  names() # names of baseline covariates cols

y <- analytic %>% select(starts_with("y")) %>% names() # names of outcome cols
  

# tv <- map(1:2, ~l_vars[[str_detect(l_vars, as.character(.x))]])
l0_vars <- analytic %>% select(starts_with("l0")) %>% names()
l1_vars <- analytic %>% select(starts_with("l1")) %>% names()
l2_vars <- analytic %>% select(starts_with("l2")) %>% names()
l3_vars <- analytic %>% select(starts_with("l2")) %>% names()

# tv <- map(1:2, ~l_vars[[str_detect(l_vars, as.character(.x))]])
tv <- list(l0_vars,l1_vars,l2_vars)


trim <- .995 # Quantile range for trimming ranges, if necessary

folds <- 5 # cross-fitting folds (5 for time, 10 in paper)
SL_folds <- 5 # cross-validation folds for superlearning (5 for time, 10 in paper)
k <- 1 # we assume that data from k days previously is sufficient for the patient's covariate history
# this is 2 in the paper

## -----------------------------------------------------------------------------------------

## fit LMTP results objects -- note that estimate is for survival, not incidence rate

# intervention 1
progressr::with_progress(
  res_interupted <-lmtp_sdr(
    data=analytic,
    trt = a,
    outcome = y,
    cens = c(paste0("c",1:3)),
    baseline = b_cov,
    time_vary = tv,
    shift = visits_interupted,
    outcome_type = "survival",
    learners_outcome = lrnrs,
    learners_trt = lrnrs,
    folds = folds,
    .SL_folds = SL_folds,
    .trim = trim,
    k=k,
    intervention_type = "dynamic"
  )
)

progressr::with_progress(
  res_not_interupted <-lmtp_sdr(
    data=analytic,
    trt = a,
    outcome = y,
    cens = c(paste0("c",1:3)),
    baseline = b_cov,
    time_vary = tv,
    shift = visits_not_interrupted,
    outcome_type = "survival",
    learners_outcome = lrnrs,
    learners_trt = lrnrs,
    folds = folds,
    .SL_folds = SL_folds,
    .trim = trim,
    k=k,
    intervention_type = "static"
  )
)


progressr::with_progress(
  res_obs <-lmtp_sdr(
    data=analytic,
    trt = a,
    outcome = y,
    cens = c(paste0("c",1:3)),
    baseline = b_cov,
    time_vary = tv,
    shift = NULL,
    outcome_type = "survival",
    learners_outcome = lrnrs,
    learners_trt = lrnrs,
    folds = folds,
    .SL_folds = SL_folds,
    .trim = trim,
    k=k,
    intervention_type = "static"
  )
)


library(gt)


## make results table for dental pain incidence rate 

res_obs %>% tidy()

ests <- imap_dfr(list("Not interupted" = res_not_interupted, 
                      "Interupted" = res_interupted),
                 function(x,y){
                   x %>%
                     tidy() %>%
                     mutate(intervention = y,
                            estimate=1-estimate,
                            conf_low = 1-conf.high,
                            conf_high=1-conf.low) %>%
                     select(intervention, estimate, conf_low, conf_high)
                   
                 })

diff <- lmtp_contrast(res_interupted, ref=res_not_interupted)$vals %>%
  mutate(estimate = -theta,
         conf_low = -conf.high,
         conf_high= -conf.low,
         intervention = "Difference")  %>%
  select(intervention, estimate, conf_low, conf_high)


ests %>%
  bind_rows(diff) %>%
  gt() %>%
  fmt_number(columns = 2:4, decimals = 1, scale_by=100) %>%
  cols_merge(columns = 2:4, pattern = c("{1} ({2}, {3})")) %>%
  cols_label(intervention = "Intervention",
             estimate = "Estimated Dental Pain Incidence Rate\n(95% CI)")








analytic %>% 
  mutate(across(starts_with("a"), ~dental_visits_interupted(analytic,.x))) %>% 
  select(starts_with("a"))







  
  
  

  
  