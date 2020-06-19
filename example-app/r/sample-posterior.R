# Posterior draws

library(tidyverse)
library(brms)

dt <- read_csv("brms/data/model-data.csv")
country_list <- unique(dt$country)

a <- sample(dt$age_s, 1000) # random age draws

dat_out <- data.frame()

for(cty in country_list) {
  print(cty)
  # Load models
  filename <- paste("brms/r/models", cty, "bayes-models.rda", sep = "/")
  load(filename)
  # Draw samples
  dat <- filter(dt, country == cty)
  post_samps <- expand.grid(uniq_region = unique(dat$uniq_region), 
                               urban = unique(dat$urban), 
                               age_s = a) %>% 
    as.tbl() %>% 
    mutate(prev = predict(prev_model, ., 
                         re_formula = ~(1 + urban + age_s| uniq_region), 
                         type = 'response')[,1], 
           sens = predict(sens_model, ., 
                          re_formula = ~(1 + urban + age_s| uniq_region), 
                          type = 'response')[,1],
           spec = predict(spec_model, ., 
                          re_formula = ~(1 + urban + age_s| uniq_region), 
                          type = 'response')[,1]
    )
  # Export
  filepath = paste("brms/data/bayes-predict/", cty, "post-samples.csv", sep = "/")
  write_csv(post_samps, filepath)
  # Append
  dat_out <- rbind(dat_out, post_samps)
  
}

reg_code <- read_csv("brms/data/reg_code.csv")

dat_out %>% 
  left_join(distinct(dt, age, age_s)) %>%
  left_join(reg_code) %>% 
  write_csv("brms/data/post-samps-all.csv")

