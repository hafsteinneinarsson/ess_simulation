# Summary of R-indicators 


model.interview.full <- formula(interview ~ multi.unit * neighb + no.access + 
                                  gndr_su + age_cat)
model.interview.dem <- formula(interview ~ gndr_su + age_cat)

model.status.70pct.full <- formula(status.70pct ~ multi.unit * neighb + no.access + 
                                     gndr_su + age_cat)
model.status.70pct.dem <- formula(status.70pct ~ gndr_su + age_cat)


interview.full <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.interview.full, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

interview.dem <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.interview.dem, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

r.summary.interview.full <- summariseR(interview.full, 
                                       response.model = "Full dataset", 
                                       selected = "Full dataset",
                                       Rindicator = "Full")


r.summary.interview.dem <- summariseR(interview.dem, 
                                      response.model = "Full dataset", 
                                      selected = "Full dataset",
                                      Rindicator = "Dem")





status70pct.full <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.status.70pct.full, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

status70pct.dem <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.status.70pct.dem, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

r.summary.status70pct.full <- summariseR(status70pct.full, 
                                         response.model = "Early fieldwork", 
                                         selected = "Early fieldwork",
                                         Rindicator = "Full")


r.summary.status70pct.dem <- summariseR(status70pct.dem, 
                                        response.model = "Early fieldwork", 
                                        selected = "Early fieldwork",
                                        Rindicator = "Dem")




r.summary.nosim  <- bind_rows(r.summary.status70pct.full, r.summary.status70pct.dem,
                              r.summary.interview.full, r.summary.interview.dem) |> 
  mutate(CV.CI95.lower = CV - CVSE * 1.96, CV.CI95.upper = CV + CVSE * 1.96) %>% 
  mutate(cntry = substr(cntry.round, start = 1, stop = 2), 
         round = substr(cntry.round, start = 4, stop = 5),
         cntry.full = case_when(cntry == "BE" ~ "Belgium",
                                cntry == "CH" ~ "Switzerland",
                                cntry == "DK" ~ "Denmark",
                                cntry == "EE" ~ "Estonia",
                                cntry == "FI" ~ "Finland",
                                cntry == "IT" ~ "Italy",
                                cntry == "PL" ~ "Poland",
                                cntry == "SI" ~ "Slovenia")) %>% 
  mutate(cntry.round.full = str_c(cntry.full, round, sep = "-"))  %>% 
  mutate(cntry.round.full = factor(cntry.round.full,
                                   levels = c("Belgium-7", "Belgium-8", "Belgium-9",
                                              "Denmark-7", "Estonia-7", "Estonia-10",
                                              "Finland-7", "Finland-8", "Finland-9", "Finland-10", 
                                              "Slovenia-7", "Slovenia-8","Slovenia-9", "Slovenia-10",
                                              "Switzerland-7", "Switzerland-9", "Switzerland-10"))) 





## Table 1 Average coefficient of variation of response propensities and response rate across country-rounds by simulation scenarios and response propensity models (CV calculated using Model 2)

r.summary |> 
  bind_rows(r.summary.nosim) |> 
  group_by(response.model, selected, Rindicator) |> 
  summarise(mean.cv = round(mean(CV), 3),
            sd.cv = round(sd(CV), 3),
            mean.rr = round(mean(response.rate), 3),
            sd.rr = round(sd(response.rate), 3)
  ) |> 
  filter(Rindicator == "Full") |> 
  select(-Rindicator) |> 
  ungroup() |> 
  gt(
    groupname_col = "selected") |> 
  fmt_percent(columns = mean.rr, decimals = 1)|> 
  fmt_percent(columns = sd.rr, decimals = 1)|> 
  cols_label(mean.cv = "CV (mean)",
             sd.cv = "CV (SD)",
             mean.rr = "Response rate (mean)",
             sd.rr = "Response rate (SD)",
             response.model = "Response model") |>  
  gt::gtsave(filename = "cv_rr_comparison_table1.rtf")



## Appendix Table 4. Average coefficient of variation of response propensities and response rate across country-rounds by simulation scenarios and response propensity models (CV calculated using Model 1)



r.summary |> 
  bind_rows(r.summary.nosim) |> 
  group_by(response.model, selected, Rindicator) |> 
  summarise(mean.cv = round(mean(CV), 3),
            sd.cv = round(sd(CV), 3),
            mean.rr = round(mean(response.rate), 3),
            sd.rr = round(sd(response.rate), 3)
  ) |> 
  filter(Rindicator == "Dem") |> 
  select(-Rindicator) |> 
  ungroup() |> 
  gt(
    groupname_col = "selected") |> 
  fmt_percent(columns = mean.rr, decimals = 1)|> 
  fmt_percent(columns = sd.rr, decimals = 1)|> 
  cols_label(mean.cv = "CV (mean)",
             sd.cv = "CV (SD)",
             mean.rr = "Response rate (mean)",
             sd.rr = "Response rate (SD)",
             response.model = "Response model") |>  
  gt::gtsave(filename = "cv_rr_comparison_dem_appendix_table3.rtf")



# A version of table 1 broken down by country-round



r.summary |> 
  bind_rows(r.summary.nosim) |> 
  filter(Rindicator == "Full") |> 
  select(cntry.round.full, CV, CVSE, response.rate, response.model, selected) |> 
  mutate(cntry.round.full = factor(cntry.round.full,
                                   levels = c("Belgium-7", "Belgium-8", "Belgium-9",
                                              "Denmark-7", "Estonia-7", "Estonia-10",
                                              "Finland-7", "Finland-8", "Finland-9", "Finland-10", 
                                              # "Netherlands-10",
                                              "Slovenia-7", "Slovenia-8","Slovenia-9", "Slovenia-10",
                                              "Switzerland-7", "Switzerland-9", "Switzerland-10"))) |> 
  mutate(selected = factor(selected,
                           levels = c("Early fieldwork", "Sim1 Bottom", "Sim2 Top", "Sim3 Rank",
                                      "Sim4 Random", "Full dataset"))) |> 
  mutate(CV = round(mean(CV), 3),
         CVSE = round(mean(CVSE), 3),
         response.rate = round(mean(response.rate), 3)
  ) |> 
  arrange(cntry.round.full, selected) |>
  gt() |> 
  fmt_percent(columns = response.rate, decimals = 1) |> 
  cols_label(response.rate = "Response rate",
             response.model = "Covariates",
             selected = "Simulation",
             cntry.round.full = "Country-round")  |>  
  gt::gtsave(filename = "cv_rr_comparison_full_bycr_appendix_table5.rtf")


