# Which are the first and last country-rounds?

first <- contacts.dw %>% filter(first == T) %>% reframe(first = unique(cntry.round))
last <- contacts.dw %>% filter(last == T) %>% reframe(last = unique(cntry.round)) 



model.dem.sim1 <- formula(interview ~ gndr_su + age_cat)

model.full.sim1 <- formula(interview ~ multi.unit + neighb + no.access + gndr_su + age_cat)

model.dem.sim2 <- formula(interview ~ gndr_su + age_cat + attempt)

model.full.sim2 <- formula(interview ~ multi.unit * neighb +  no.access + gndr_su + age_cat + attempt)


models <- c("model.dem.sim1", "model.full.sim1", "model.dem.sim2", "model.full.sim2")

## Model fits 
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html

summary.model.dem.sim1 <- contacts.early.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.dem.sim1, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, glance), 
         model = "Model 1")   %>% 
  unnest(tidy.resp) %>% 
  select(model, cntry.round, nobs, logLik, AIC) 


summary.model.full.sim1 <- contacts.early.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.full.sim1, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, glance), 
         model = "Model 2")   %>% 
  unnest(tidy.resp) %>% 
  select(model, cntry.round, nobs, logLik, AIC) 


summary.model.dem.sim2 <- contacts.late.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.dem.sim2, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, glance), 
         model = "Model 3")   %>% 
  unnest(tidy.resp) %>% 
  select(model, cntry.round, nobs, logLik, AIC) 


summary.model.full.sim2 <- contacts.late.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.full.sim2, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, glance), 
         model = "Model 4")   %>% 
  unnest(tidy.resp) %>% 
  select(model, cntry.round, nobs, logLik, AIC) 


bind_rows(summary.model.dem.sim1, summary.model.full.sim1, summary.model.dem.sim2, summary.model.full.sim2) %>% 
  mutate(cntry.round = str_replace_all(cntry.round, c("BE" = "Belgium",
                                                      "CH" = "Switzerland",
                                                      "DK" = "Denmark",
                                                      "EE" = "Estonia",
                                                      "FI" = "Finland",
                                                      "SI" = "Slovenia"))) %>%
  gt::gt() %>% 
  cols_label(.list = c("model" = "Model", "cntry.round" = "Country-round", "nobs" = "N")) %>% 
  gtsave(filename = "model_summaries.docx", inline_css = TRUE)







coefs.model.dem.sim1 <- contacts.early.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.dem.sim1, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, tidy),
         model = "Model 1")   %>% 
  unnest(tidy.resp) 


coefs.model.full.sim1 <- contacts.early.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.full.sim1, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, tidy),
         model = "Model 2")   %>% 
  unnest(tidy.resp) 


coefs.model.dem.sim2 <- contacts.early.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.dem.sim2, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, tidy),
         model = "Model 3")   %>% 
  unnest(tidy.resp) 


coefs.model.full.sim2 <- contacts.early.attempts %>% 
  nest(data = -cntry.round) %>% 
  mutate(resp = map(data, ~ glm(formula = model.full.sim2, 
                                data = .x, 
                                family = "binomial")),
         tidy.resp = map(resp, tidy),
         model = "Model 4")   %>% 
  unnest(tidy.resp) 



bind_rows(coefs.model.dem.sim1, coefs.model.full.sim1, coefs.model.dem.sim2, coefs.model.full.sim2) %>% 
  mutate(estimate  = round(estimate, 3),
         p.value   = round(p.value, 3),
         statistic = round(statistic, 3),
         std.error = round(std.error, 3)) %>% 
  mutate(term = case_when(term == "(Intercept)" ~ "Constant",
                          term == "multi.unitMulti-unit" ~ "Multi-unit",
                          term == "neighbOne or more" ~ "One or more undesirable neighb. char.",
                          term == "no.accessBarriers" ~ "One or more barriers to access",
                          term == "gndr_suMale" ~ "Male",
                          term == "age_cat35-49" ~ "Age 35-49",
                          term == "age_cat50-64" ~ "Age 50-64",
                          term == "age_cat65+" ~ "Age 65+",
                          term == "attempt" ~ "Number of contact attempts",
                          term == "multi.unitMulti-unit:neighbOne or more" ~ "Multi-unit*Neighbourhood characteristics",
                          term == "gndr_suMale:age_cat35-49" ~ "Male*Age 35-49",
                          term == "gndr_suMale:age_cat50-64" ~ "Male*Age 50-64",
                          term == "gndr_suMale:age_cat65+" ~ "Male*Age 65+")) %>% 
  mutate(signif = gtools::stars.pval(p.value)) %>% 
  mutate(cntry.round = str_replace_all(cntry.round, c("BE" = "Belgium",
                                                      "CH" = "Switzerland",
                                                      "DK" = "Denmark",
                                                      "EE" = "Estonia",
                                                      "FI" = "Finland",
                                                      "SI" = "Slovenia"))) %>% 
  filter(p.value < 0.05) %>%
  select(-data, -resp) %>%
  relocate(model) %>% 
  gt::gt() %>% 
  cols_label(.list = c(
    "model" = "Model",
    "cntry.round" = "Country-round",
    "term" = "Term",
    "estimate" = "Estimate",
    "std.error" = "SE",
    "statistic" = "Statistic",
    "p.value" = "p",
    "signif" = "")) %>% 
  gtsave(filename = "coefs_regression_models.docx", inline_css = TRUE)


