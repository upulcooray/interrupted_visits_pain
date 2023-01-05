library(targets)
library(tarchetypes)
library(tidyverse) # load tidyverse package
library(lmtp) # note that this code uses sl3 version, load lmtp package
library(sl3) # load sl3 package



# Define custom functions and other global objects -----------------------------
source("R/functions.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse",
                            "lmtp" ,
                            "sl3",
                            "future",
                            "gt", "rlang"
                            ))

# Shift functions------------
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


visits_not_interupted <- function(dat, trt) {
  
  dat[[trt]] <- 0 # entire treatment vector of interest to 0
  dat[[trt]] # return
  
}



library(future.callr)
plan(callr)
set.seed(198511110)

list(
  # load working data
  tar_target(df_file,
             "data/left_joined_4waves.rds",
             format = "file")
  ,

  # Working data----------------------------------------------------------------
  tar_target(working_df,
             readRDS(file=df_file))
  ,

  tar_target(analytic_df,
             get_analytic(working_df))
  ,
  
  tar_target(lrnrs, # select from c("lasso","ridge","enet","xgb","mean")
             get_learners(c("lasso","ridge","enet","xgb","mean"))
                          )
  ,
  
  tar_target(a, paste0("a",0:2)) # exposure: Visits interrupted or not
                          
  ,
  
  tar_target(y, paste0("y",1:3)) # Outcome: Oral pain + or -
                          
  ,
  tar_target(b_cov,
             analytic_df %>% select(starts_with("w_")) %>%  names()) 
                          
  ,
  
  tar_target(l_vars,
             analytic_df %>% select(starts_with("l")) %>%  names()) 
                          
  ,
  
  tar_target(tv,
             map(0:2, ~l_vars[str_detect(l_vars,as.character(.x))])
             )
                          
  ,
  
  tar_target(cens, paste0("c",1:3))
                          
  ,
  
  tar_target(params,
             list(
               trt = a,
               outcome = y,
               baseline = b_cov,
               time_vary = tv,
               cens = cens,
               outcome_type = "survival",
               learners_outcome = lrnrs,
               learners_trt = lrnrs,
               folds = 5,
               .SL_folds = 5,
               .trim = 0.995,
               k=1))
                          
  ,
  
  tar_group_by(grouped_df,
             analytic_df,
             visit_hx 
             )
  ,
  
 
  tar_target(branched_df,
             grouped_df,
             map(grouped_df))
  ,
  
  tar_group_by(grouped_sex,
             analytic_df,
             sex 
             )
  ,
  
  tar_target(branched_sex,
             grouped_sex,
             map(grouped_sex))
  ,
  

  tar_target(sdr_interupted,

             tibble(
               grp=branched_df$visit_hx %>% unique(),
               a= "interupted",
               mod= rlang::exec(lmtp_sdr, rlang::splice(params),
                                     data=branched_df,
                                     shift=visits_interupted,
                                     intervention_type = "dynamic") %>% list()
               ),
             map(branched_df))
  ,
  

  
  tar_target(sdr_not_interupted,

             tibble(
               grp=branched_df$visit_hx %>% unique(),
               a= "not_interupted",
               mod= rlang::exec(lmtp_sdr,
                                     rlang::splice(params),
                                     data=branched_df,
                                     shift=visits_not_interupted,
                                     intervention_type = "static")%>% list()
               ),
             map(branched_df))
  ,
  
  tar_target(sdr_interupted_sex,

             tibble(
               grp=branched_sex$sex %>% unique(),
               a= "interupted",
               mod= rlang::exec(lmtp_sdr, rlang::splice(params),
                                     data=branched_df,
                                     shift=visits_interupted,
                                     intervention_type = "dynamic") %>% list()
               ),
             map(branched_sex))
  ,
  
  tar_target(sdr_not_interupted_sex,

             tibble(
               grp=branched_sex$sex %>% unique(),
               a= "not_interupted",
               mod= rlang::exec(lmtp_sdr,
                                     rlang::splice(params),
                                     data=branched_df,
                                     shift=visits_not_interupted,
                                     intervention_type = "static")%>% list()
               ),
             map(branched_sex))
  ,

  tar_target(mod_combined,
             bind_rows(sdr_interupted,
                       sdr_not_interupted,
                       sdr_interupted_sex,
                       sdr_not_interupted_sex
                       ))
  ,

  tar_target(res_sdr_combined,
            setNames(as.list(mod_combined$mod), paste0(mod_combined$a,"_",
                                                       mod_combined$grp) ) %>%
            get_sdr_estimates() 
           
             )
  ,
  
  tar_group_by(grouped_sdr,
               mod_combined,
               grp 
  )
  ,
  
  tar_target(branched_sdr,
             grouped_sdr,
             map(grouped_sdr))
  ,
  
  tar_target(diff_branched,
             get_diff(branched_sdr),
             map(branched_sdr))
  ,
  
  tar_target(diff_res,
             bind_rows(diff_branched) %>% 
               mutate(intervention= str_c(intervention, "_" ,grp)) %>%
               select(-grp))
  ,
  
  tar_target(tab,
             bind_rows(res_sdr_combined,diff_res) %>% 
               gt() %>%
               fmt_number(columns = 2:4, decimals = 1, scale_by=100) %>%
               cols_merge(columns = 2:4, pattern = c("{1} ({2}, {3})")) %>%
               cols_label(intervention = "Intervention",
                          estimate = "Estimated Dental Pain Incidence Rate\n(95% CI)")
             
  )
  
  
  
  
             
  )
  





