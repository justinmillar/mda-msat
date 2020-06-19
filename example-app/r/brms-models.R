# Bayesian mixed model with brms package

## Load data and packages ----
library(tidyverse)
library(brms)

# Run Stan faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dt <- read_csv("example/data/glmer-data.csv")

dt <- filter(dt, age < 60, !country == "ML6") # remove Mali, select correct ages

# Fix region mis-match issue
dt[dt$country == "GN6","region"] = dt[dt$country == "GN6","region2"] # Fix Guinea region field

# Organize data from regression
rescale_age <- unique(data.frame(age = dt$age, age_s = scale(dt$age)))
dt$age_s <- scale(dt$age) 
#dtsex<−factor(dtsex)
#levels(dt$sex) <- c("Male", "Female")
dt$urban <- factor(dt$urban)
levels(dt$urban) <- c("Urban", "Rural")

# Create unique regions for each country
dt <- dt %>% 
  unite(uniq_region, country, region, sep = "_", remove = F) %>% 
  mutate(cluster = as.character(cluster))

# final_out <- tidyr::expand(dt, country, uniq_region, urban, age)

## Run loop ----
country_list <- unique(dt$country)

start <- Sys.time()
for(cty in country_list) {
  
  # Track progress
  print(paste("Evaluating:", cty))
  
  # Subset data
  dat <- filter(dt, country == cty)
  
  # Create models
  ncores <- parallel::detectCores()
  
  print("Modelling prevalence")
  prev_model <- brm(micro_mala ~ (1 | cluster) + urban + age_s + 
                         (1 + urban + age_s| uniq_region),
                    data    = dat, 
                    family  = bernoulli(link = "logit"), 
                    cores   = ncores, 
                    control = list(adapt_delta = 0.999, max_treedepth = 15))
  
  print("Modelling senstivity")
  sens_model<- brm(rapid_mala ~ (1 | cluster) + urban + age_s + 
                        (1 + urban + age_s| uniq_region),
                   data    =  filter(dat, micro_mala == 1), 
                   family  = bernoulli(link = "logit"), 
                   cores   = ncores, 
                   control = list(adapt_delta = 0.999, max_treedepth = 15))
  
  print("Modelling specificity")
  spec_model<- brm(rapid_mala ~ (1 | cluster) + urban + age_s + 
                        (1 + urban + age_s| uniq_region),
                   data    =  filter(dat, micro_mala == 0), 
                   family  = bernoulli(link = "logit"), 
                   cores   = ncores, 
                   control = list(adapt_delta = 0.999, max_treedepth = 15))
  
  # Export models
  print("Exporting models")
  filename <- paste("brms/r/models", cty, "bayes-models.rda", sep = "/")
  save(prev_model, sens_model, spec_model, file = filename)
  
  # Make predictions
  print("Making predictions")
  variable_grid <- tidyr::expand(dat, age_s, urban, uniq_region)
  
  prev <- fitted(prev_model,variable_grid, 
                 re_formula = ~(1 + urban + age_s| uniq_region), 
                 type = 'response')
  
  sens <- fitted(sens_model,variable_grid, 
                 re_formula = ~(1 + urban + age_s| uniq_region), 
                 type = 'response')
  
  spec <- fitted(spec_model,variable_grid, 
                 re_formula = ~(1 + urban + age_s| uniq_region), 
                 type = 'response')
  
  brm_out <- data.frame(variable_grid, 
                           prevalence  = prev[,1],
                           prev_lwr    = prev[,3],
                           prev_upr    = prev[,4],
                           sensitivity = sens[,1], 
                           sens_lwr    = sens[,3],
                           sens_upr    = sens[,4],
                           specificity = spec[,1],
                           spec_lwr    = spec[,3],
                           spec_upr    = spec[,4]) %>% 
    left_join(rescale_age) %>% 
    separate(col = uniq_region, into = c("country", "region2"), 
             sep = "_", remove = FALSE)
  
  # Save results
  print("Saving results")
  
  outpath <- paste("brms/data/bayes-predict", cty, "brm-out.csv", sep = "/")
  write_csv(brm_out, outpath)
  
  # Compile outputs
  # print("Saving results")
  # final_out <- left_join(final_out, brm_out)
}
end <- Sys.time()
runtime <- end - start

# Merge model predictions into single file ----

# Get uniq_reg_code for mapping
glmer <- read_csv("update/data/app-data.csv") %>% 
  select(uniq_reg_code, survey, region) %>% 
  unite(uniq_region, survey, region, sep = "_") %>% 
  distinct()


df <- list.files("brms/data/bayes-predict/", 
                 pattern = "*.csv", 
                 recursive = T, 
                 full.names = T) %>% 
  map_dfr(read_csv) %>% 
  left_join(glmer)

write_csv(df, "brms/data/app-data.csv")
