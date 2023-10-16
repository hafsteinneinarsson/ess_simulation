
# Select the country-rounds which meet the criteria


contacts.selected <- contacts %>% 
  filter(cntry == "BE" | cntry == "CH" | cntry == "DK" | cntry == "EE" | cntry == "FI" | cntry == "SI") %>% 
  filter(cntry.round != "EE-8") %>% #
  filter(cntry.round != "EE-9") %>% 
  filter(cntry.round != "CH-8") %>% 
  mutate(cntry = droplevels(cntry),
         cntry.round = droplevels(cntry.round)) %>% 
  mutate(cntry.full = case_when(cntry == "BE" ~ "Belgium",
                                cntry == "CH" ~ "Switzerland",
                                cntry == "DK" ~ "Denmark",
                                cntry == "EE" ~ "Estonia",
                                cntry == "FI" ~ "Finland",
                                cntry == "PL" ~ "Poland",
                                cntry == "SI" ~ "Slovenia")) %>% 
  mutate(cntry.round.full = as.factor(str_c(cntry.full, round.x, sep = "-")))  %>% 
  mutate(cntry.round.full = fct_relevel(cntry.round.full,
                                        "Belgium-6","Belgium-7", "Belgium-8", "Belgium-9",
                                        "Denmark-6","Denmark-7", "Estonia-6", "Estonia-7", "Estonia-10",
                                        "Finland-6","Finland-7", "Finland-8", "Finland-9", "Finland-10",
                                        "Slovenia-6", "Slovenia-7", "Slovenia-8","Slovenia-9", "Slovenia-10",
                                        "Switzerland-6","Switzerland-7", "Switzerland-9", "Switzerland-10"))   %>% 
  mutate(cntry.round = fct_relevel(cntry.round,
                                   "BE-6","BE-7", "BE-8", "BE-9",
                                   "DK-6", "DK-7", "EE-6", "EE-7", "EE-10",
                                   "FI-6","FI-7", "FI-8", "FI-9", "FI-10", 
                                   "SI-6",  "SI-7", "SI-8","SI-9", "SI-10",
                                   "CH-6",  "CH-7", "CH-9", "CH-10"))

# Only include the row which includes the last contact attempt, and remove incomplete cases

contacts.dw <- contacts.selected %>% 
  filter(!is.na(interview)) %>% 
  filter(!is.na(gndr_su)) %>% 
  filter(!is.na(multi.unit)) %>% 
  filter(!is.na(no.access)) %>% 
  filter(!is.na(age_cat)) %>% 
  filter(!is.na(neighb))  %>% 
  group_by(cntry.round) %>%
  arrange(cntry.round, time) %>%
  mutate(attempt.number = row_number(),
         n = n(),
         attempt.pct = attempt.number / n,
         attempt.first50pct = attempt.pct < .50,
         attempt.first60pct = attempt.pct < .60,
         attempt.first70pct = attempt.pct < .70,
         attempt.first80pct = attempt.pct < .80,
         status.final = success <= attempt,
         status.final = replace_na(status.final, replace = F),
         status.50pct = case_when(attempt.first50pct == T & status.final == T ~ TRUE, TRUE ~ FALSE),
         status.60pct = case_when(attempt.first60pct == T & status.final == T ~ TRUE, TRUE ~ FALSE),
         status.70pct = case_when(attempt.first70pct == T & status.final == T ~ TRUE, TRUE ~ FALSE),
         status.80pct = case_when(attempt.first80pct == T & status.final == T ~ TRUE, TRUE ~ FALSE),
         first = if_else(str_detect(cntry.round, "6"), TRUE, FALSE),
         last = case_when(str_detect(cntry.round, "10") ~ TRUE, 
                          cntry.round == "BE-9" ~ TRUE,
                          cntry.round == "DK-7" ~ TRUE,
                          TRUE ~ FALSE),
         cntry.round.tminus1 = str_replace_all(cntry.round, 
                                               c("6" = "5", "7" = "6", "8" = "7", "9" = "8", "10" = "9"))
  ) %>% 
  ungroup() 


# find out how many attempts were made at 70pct

contacts.attempt.number.70pct  <- contacts.dw %>% 
  filter(attempt.first70pct == T) %>% 
  mutate(cntry = droplevels(cntry),
         cntry.round = droplevels(cntry.round),
         attempt.number.70pct = attempt) %>% 
  arrange(cntry, essround, idno, desc(time)) %>% 
  group_by(cntry, essround, idno) %>%
  slice(1) %>%  # Keep only last attempt
  ungroup() %>%
  select(cntry.round, idno, attempt.number.70pct) 


contacts.no.nas <- contacts.dw %>%
  arrange(cntry, essround, idno, desc(time)) %>% 
  group_by(cntry, essround, idno) %>%
  slice(1) %>% # Keep only last attempt
  ungroup() %>% 
  arrange(cntry, essround, idno) |> 
  left_join(contacts.attempt.number.70pct, 
            by = c("cntry.round", "idno")) %>% 
  mutate(attempt.number.70pct = replace_na(attempt.number.70pct, replace = 0))





# Create response propensity models for full dataset

model.dem.sim1 <- formula(interview ~ gndr_su + age_cat)

model.full.sim1 <- formula(interview ~ multi.unit * neighb + 
                             no.access + 
                             gndr_su + age_cat)

model.dem.sim2 <- formula(interview ~ gndr_su + age_cat + attempt)

model.full.sim2 <- formula(interview ~ multi.unit * neighb + 
                             no.access +
                             gndr_su + age_cat + attempt)


# Create datasets of late and early contact attempts

contacts.late.attempts <- contacts.no.nas %>% 
  filter(attempt.first70pct == F)

contacts.early.attempts <- contacts.no.nas %>% 
  filter(attempt.first70pct == T)



# Produce response propensity models for each country-round using demographics only

pred.mod.dem.sim1 <- contacts.early.attempts %>% 
  group_split(.$cntry.round) %>% 
  map(~ glm(formula = model.dem.sim1, 
            data = .x, 
            family = "binomial"))   %>% 
  map_dfr(~ predict.glm(object = .,
                        newdata = contacts.late.attempts,
                        type = "response")) %>% 
  t() %>% as_tibble()


colnames(pred.mod.dem.sim1) <- levels(contacts.late.attempts$cntry.round)

#  unique(str_c(contacts.early.attempts$cntry, contacts.early.attempts$round.x,  sep = "-"))

pred.mod.dem.sim2 <- contacts.late.attempts %>% 
  group_split(.$cntry.round) %>% 
  map(~ glm(formula = model.dem.sim2, 
            data = .x, 
            family = "binomial")) %>% 
  map_dfr(~ predict.glm(object = .,
                        newdata = contacts.late.attempts,
                        type = "response")) %>% 
  t() %>% as_tibble()

colnames(pred.mod.dem.sim2) <- levels(contacts.late.attempts$cntry.round)

#unique(str_c(contacts.late.attempts$cntry, contacts.late.attempts$round.x, sep = "-"))


# Produce response propensity models for each country-round using demographics and interviewer observations

pred.mod.full.sim1 <- contacts.early.attempts %>% 
  group_split(.$cntry.round) %>% 
  map(~ glm(formula = model.full.sim1, 
            data = .x, 
            family = "binomial")) %>% 
  map_dfr(~ predict.glm(object = .,
                        newdata = contacts.late.attempts,
                        type = "response")) %>% 
  t() %>% as_tibble()


colnames(pred.mod.full.sim1) <- levels(contacts.late.attempts$cntry.round)


pred.mod.full.sim2 <- contacts.late.attempts %>% 
  group_split(.$cntry.round) %>% 
  map(~ glm(formula = model.full.sim2, 
            data = .x, 
            family = "binomial"))  %>% 
  map_dfr(~ predict.glm(object = .,
                        newdata = contacts.late.attempts,
                        type = "response")) %>% 
  t() %>% as_tibble()

colnames(pred.mod.full.sim2) <-  levels(contacts.late.attempts$cntry.round)


# Combine models, DEM + SIM1 

contacts.pred.dem.sim1 <- cbind(contacts.late.attempts, 
                                pred.mod.dem.sim1) %>% 
  select(cntry.round, idno, contains("-")) %>% 
  mutate(dem.predicted.response.propensity.sim1 = case_when(cntry.round == "BE-7" ~ `BE-7`,
                                                            cntry.round == "BE-8" ~ `BE-8`,
                                                            cntry.round == "BE-9" ~ `BE-9`,
                                                            cntry.round == "CH-7" ~ `CH-7`,
                                                            cntry.round == "CH-9" ~ `CH-9`,
                                                            cntry.round == "CH-10" ~ `CH-10`,
                                                            cntry.round == "DK-7" ~ `DK-7`,
                                                            cntry.round == "EE-7" ~ `EE-7`,
                                                            cntry.round == "EE-10" ~ `EE-10`,
                                                            cntry.round == "FI-7" ~ `FI-7`,
                                                            cntry.round == "FI-8" ~ `FI-8`,
                                                            cntry.round == "FI-9" ~ `FI-9`,
                                                            cntry.round == "FI-10" ~ `FI-10`,
                                                            cntry.round == "SI-7" ~ `SI-7`,
                                                            cntry.round == "SI-8" ~ `SI-8`,
                                                            cntry.round == "SI-9" ~ `SI-9`,
                                                            cntry.round == "SI-10" ~ `SI-10`,
                                                            TRUE ~ NA_real_),
         rand = rnorm(n = nrow(.))) %>% 
  arrange(rand) %>% 
  group_by(cntry.round) %>% 
  mutate(rank.dem.sim1 = rank(dem.predicted.response.propensity.sim1,
                              ties.method = "random"),
         dem.sim1.top50 = rank.dem.sim1 > median(rank.dem.sim1),
         dem.sim1.bottom50 = rank.dem.sim1 <= median(rank.dem.sim1)) %>% 
  select(-contains("-"))


contacts.status.dem.sim1 <- contacts.no.nas %>% 
  left_join(contacts.pred.dem.sim1, by = c("cntry.round", "idno")) %>% 
  mutate(dem.sim1.top50selected = attempt.first70pct == F & dem.sim1.top50 == T,
         dem.sim1.bottom50selected = attempt.first70pct == F & dem.sim1.bottom50 == T,
         dem.sim1.resp.bottom50 = case_when(interview == T & dem.sim1.bottom50 == T ~ TRUE,
                                            interview == T & is.na(dem.sim1.bottom50) == T ~ TRUE,
                                            interview == T & dem.sim1.bottom50 == F ~ FALSE,
                                            interview == F ~ FALSE),
         dem.sim1.resp.top50 = case_when(interview == T & dem.sim1.top50 == T ~ TRUE,
                                         interview == T & is.na(dem.sim1.top50) == T ~ TRUE,
                                         interview == T & dem.sim1.top50 == F ~ FALSE,
                                         interview == F ~ FALSE)) 



# Combine models, DEM + SIM2 with preceding round used to predict for next round


contacts.pred.dem.sim2 <- cbind(contacts.late.attempts, 
                                pred.mod.dem.sim2) %>% 
  select(cntry.round, idno, contains("-")) %>% 
  mutate(dem.predicted.response.propensity.sim2 = case_when(cntry.round == "BE-7" ~ `BE-6`,
                                                            cntry.round == "BE-8" ~ `BE-7`,
                                                            cntry.round == "BE-9" ~ `BE-8`,
                                                            cntry.round == "CH-7" ~ `CH-6`,
                                                            cntry.round == "CH-9" ~ `CH-7`,
                                                            cntry.round == "CH-10" ~ `CH-9`,
                                                            cntry.round == "DK-7" ~ `DK-6`,
                                                            cntry.round == "EE-7" ~ `EE-6`,
                                                            cntry.round == "EE-10" ~ `EE-7`,
                                                            cntry.round == "FI-7" ~ `FI-6`,
                                                            cntry.round == "FI-8" ~ `FI-7`,
                                                            cntry.round == "FI-9" ~ `FI-8`,
                                                            cntry.round == "FI-10" ~ `FI-9`,
                                                            cntry.round == "SI-7" ~ `SI-6`,
                                                            cntry.round == "SI-8" ~ `SI-7`,
                                                            cntry.round == "SI-9" ~ `SI-8`,
                                                            cntry.round == "SI-10" ~ `SI-9`,
                                                            TRUE ~ NA_real_),
         rand = rnorm(n = nrow(.))) %>% 
  arrange(rand) %>% 
  group_by(cntry.round) %>% 
  mutate(rank.dem.sim2 = rank(dem.predicted.response.propensity.sim2,
                              ties.method = "random"),
         dem.sim2.top50 = rank.dem.sim2 > median(rank.dem.sim2),
         dem.sim2.bottom50 = rank.dem.sim2 <= median(rank.dem.sim2)) %>% 
  select(-contains("-"))

contacts.status.dem <- contacts.status.dem.sim1 %>% 
  left_join(contacts.pred.dem.sim2, by = c("cntry.round", "idno")) %>% 
  mutate(dem.sim2.top50selected = attempt.first70pct == F & dem.sim2.top50 == T,
         dem.sim2.bottom50selected = attempt.first70pct == F & dem.sim2.bottom50 == T,
         dem.sim2.resp.bottom50 = case_when(interview == T & dem.sim2.bottom50 == T ~ TRUE,
                                            interview == T & is.na(dem.sim2.bottom50) == T ~ TRUE,
                                            interview == T & dem.sim2.bottom50 == F ~ FALSE,
                                            interview == F ~ FALSE),
         dem.sim2.resp.top50 = case_when(interview == T & dem.sim2.top50 == T ~ TRUE,
                                         interview == T & is.na(dem.sim2.top50) == T ~ TRUE,
                                         interview == T & dem.sim2.top50 == F ~ FALSE,
                                         interview == F ~ FALSE)) 

tabyl(contacts.status.dem, age_cat, dem.sim2.resp.top50, cntry.round)




# Combine models, full + SIM1 

contacts.pred.full.sim1 <- cbind(contacts.late.attempts, 
                                 pred.mod.full.sim1) %>% 
  select(cntry.round, idno, contains("-")) %>% 
  mutate(full.predicted.response.propensity.sim1 = case_when(cntry.round == "BE-7" ~ `BE-7`,
                                                             cntry.round == "BE-8" ~ `BE-8`,
                                                             cntry.round == "BE-9" ~ `BE-9`,
                                                             cntry.round == "CH-7" ~ `CH-7`,
                                                             cntry.round == "CH-9" ~ `CH-9`,
                                                             cntry.round == "CH-10" ~ `CH-10`,
                                                             cntry.round == "DK-7" ~ `DK-7`,
                                                             cntry.round == "EE-7" ~ `EE-7`,
                                                             cntry.round == "EE-10" ~ `EE-10`,
                                                             cntry.round == "FI-7" ~ `FI-7`,
                                                             cntry.round == "FI-8" ~ `FI-8`,
                                                             cntry.round == "FI-9" ~ `FI-9`,
                                                             cntry.round == "FI-10" ~ `FI-10`,
                                                             cntry.round == "SI-7" ~ `SI-7`,
                                                             cntry.round == "SI-8" ~ `SI-8`,
                                                             cntry.round == "SI-9" ~ `SI-9`,
                                                             cntry.round == "SI-10" ~ `SI-10`,
                                                             TRUE ~ NA_real_),
         rand = rnorm(n = nrow(.))) %>% 
  arrange(rand) %>% 
  group_by(cntry.round) %>% 
  mutate(rank.full.sim1 = rank(full.predicted.response.propensity.sim1,
                               ties.method = "random"),
         full.sim1.top50 = rank.full.sim1 > median(rank.full.sim1),
         full.sim1.bottom50 = rank.full.sim1 <= median(rank.full.sim1)) %>% 
  select(-contains("-"))


contacts.status.full.sim1 <- contacts.status.dem %>% 
  left_join(contacts.pred.full.sim1, by = c("cntry.round", "idno")) %>% 
  mutate(full.sim1.top50selected = attempt.first70pct == F & full.sim1.top50 == T,
         full.sim1.bottom50selected = attempt.first70pct == F & full.sim1.bottom50 == T,
         full.sim1.resp.bottom50 = case_when(interview == T & full.sim1.bottom50 == T ~ TRUE,
                                             interview == T & is.na(full.sim1.bottom50) == T ~ TRUE,
                                             interview == T & full.sim1.bottom50 == F ~ FALSE,
                                             interview == F ~ FALSE),
         full.sim1.resp.top50 = case_when(interview == T & full.sim1.top50 == T ~ TRUE,
                                          interview == T & is.na(full.sim1.top50) == T ~ TRUE,
                                          interview == T & full.sim1.top50 == F ~ FALSE,
                                          interview == F ~ FALSE)) 



# Combine models, full + SIM2 with preceding round used to predict for next round


contacts.pred.full.sim2 <- cbind(contacts.late.attempts, 
                                 pred.mod.full.sim2) %>% 
  select(cntry.round, idno, contains("-")) %>% 
  mutate(full.predicted.response.propensity.sim2 = case_when(cntry.round == "BE-7" ~ `BE-6`,
                                                             cntry.round == "BE-8" ~ `BE-7`,
                                                             cntry.round == "BE-9" ~ `BE-8`,
                                                             cntry.round == "CH-7" ~ `CH-6`,
                                                             cntry.round == "CH-9" ~ `CH-7`,
                                                             cntry.round == "CH-10" ~ `CH-9`,
                                                             cntry.round == "DK-7" ~ `DK-6`,
                                                             cntry.round == "EE-7" ~ `EE-6`,
                                                             cntry.round == "EE-10" ~ `EE-7`,
                                                             cntry.round == "FI-7" ~ `FI-6`,
                                                             cntry.round == "FI-8" ~ `FI-7`,
                                                             cntry.round == "FI-9" ~ `FI-8`,
                                                             cntry.round == "FI-10" ~ `FI-9`,
                                                             cntry.round == "SI-7" ~ `SI-6`,
                                                             cntry.round == "SI-8" ~ `SI-7`,
                                                             cntry.round == "SI-9" ~ `SI-8`,
                                                             cntry.round == "SI-10" ~ `SI-9`,
                                                             TRUE ~ NA_real_),
         rand = rnorm(n = nrow(.)),
         rand2 = rnorm(n = nrow(.))) %>% 
  arrange(rand) %>% 
  group_by(cntry.round) %>% 
  mutate(rank.full.sim2 = rank(full.predicted.response.propensity.sim2,
                               ties.method = "random"),
         full.sim2.top50 = rank.full.sim2 > median(rank.full.sim2),
         full.sim2.bottom50 = rank.full.sim2 <= median(rank.full.sim2),
         rand2.rank = rank(rand2, ties.method = "random"),
         rand2.selected = rand2.rank <= median(rand2.rank),
         rand.rank = rank(rand, ties.method = "random"),
         rand.selected = rand.rank <= median(rand.rank)) %>% 
  select(-contains("-"))



contacts.status.full <- contacts.status.full.sim1 %>% 
  left_join(contacts.pred.full.sim2, by = c("cntry.round", "idno")) %>% 
  mutate(full.sim2.top50selected = attempt.first70pct == F & full.sim2.top50 == T,
         full.sim2.bottom50selected = attempt.first70pct == F & full.sim2.bottom50 == T,
         full.sim2.resp.bottom50 = case_when(interview == T & full.sim2.bottom50 == T ~ TRUE,
                                             interview == T & is.na(full.sim2.bottom50) == T ~ TRUE,
                                             interview == T & full.sim2.bottom50 == F ~ FALSE,
                                             interview == F ~ FALSE),
         full.sim2.resp.top50 = case_when(interview == T & full.sim2.top50 == T ~ TRUE,
                                          interview == T & is.na(full.sim2.top50) == T ~ TRUE,
                                          interview == T & full.sim2.top50 == F ~ FALSE,
                                          interview == F ~ FALSE),
         rand.resp = case_when(interview == T & rand.selected == T ~ TRUE,
                               interview == T & is.na(rand.selected) == T ~ TRUE,
                               interview == T & rand.selected == F ~ FALSE,
                               interview == F ~ FALSE),
         rand2.resp = case_when(interview == T & rand2.selected == T ~ TRUE,
                                interview == T & is.na(rand2.selected) == T ~ TRUE,
                                interview == T & rand2.selected == F ~ FALSE,
                                interview == F ~ FALSE)
  )



## Create Rank response variable for Sim3

contacts.sim3 <- contacts.status.full %>% 
  mutate(rank.dem.sim2.rev = rank.dem.sim2 * -1,
         rank.full.sim2.rev = rank.full.sim2 * -1) %>% 
  select(cntry.round, idno, rank.dem.sim1, rank.dem.sim2.rev, 
         rank.full.sim1, rank.full.sim2.rev) %>% 
  filter(!is.na(rank.dem.sim2.rev))



cr <- unique(contacts.sim3$cntry.round)
crs <- setNames(vector('list', length = length(cr)), cr)
columns_to_choose = c("rank.dem.sim2.rev", "rank.dem.sim1")

for (co in cr) {
  next_col = "rank.dem.sim2.rev"
  df_country = contacts.sim3  %>%  filter(cntry.round == co)
  n = floor(nrow(df_country) / 2)
  out = vector('integer', n)
  out[1] = df_country$idno[[which.min(df_country[[next_col]])]] 
  for (i in seq_len(n)) {
    if (i == 1L) next
    next_col = columns_to_choose[i %% 2 + 1]
    df_country = df_country |> anti_join(data.frame(idno = out[seq_len(i)]), by = "idno")
    out[i] = df_country$idno[[which.min(df_country[[next_col]])]]     
  }
  crs[[co]] = out
}



cr <- unique(contacts.sim3$cntry.round)
crs2 <- setNames(vector('list', length = length(cr)), cr)
columns_to_choose = c("rank.full.sim2.rev", "rank.full.sim1")

for (co in cr) {
  next_col = "rank.full.sim2.rev"
  df_country = contacts.sim3  %>%  filter(cntry.round == co)
  n = floor(nrow(df_country) / 2)
  out = vector('integer', n)
  out[1] = df_country$idno[[which.min(df_country[[next_col]])]] 
  for (i in seq_len(n)) {
    if (i == 1L) next
    next_col = columns_to_choose[i %% 2 + 1]
    df_country = df_country |> anti_join(data.frame(idno = out[seq_len(i)]), by = "idno")
    out[i] = df_country$idno[[which.min(df_country[[next_col]])]]     
  }
  crs2[[co]] = out
}


ra.dem <- tibble::enframe(crs, name = "cntry.round", value = "idno") %>% 
  unnest(cols = c(idno)) %>% 
  mutate(rank.selected.dem = T)

ra.full <- tibble::enframe(crs2, name = "cntry.round", value = "idno") %>% 
  unnest(cols = c(idno)) %>% 
  mutate(rank.selected.full = T)


sim3 <- contacts.sim3 %>% 
  left_join(ra.full, by = c("cntry.round", "idno")) %>% 
  left_join(ra.dem, by = c("cntry.round", "idno")) %>% 
  mutate(rank.selected.dem = replace_na(rank.selected.dem, replace = F),
         rank.selected.full = replace_na(rank.selected.full, replace = F)) 





contacts.status.all <- contacts.status.full %>% 
  left_join(sim3, by = c("cntry.round", "idno")) %>% 
  group_by(cntry.round) %>%
  mutate(rank.resp.dem = case_when(interview == T & rank.selected.dem == T ~ TRUE,
                                   interview == T & is.na(rank.selected.dem) == T ~ TRUE,
                                   interview == T & rank.selected.dem == F ~ FALSE,
                                   interview == F ~ FALSE)) %>% 
  mutate(rank.resp.full = case_when(interview == T & rank.selected.full == T ~ TRUE,
                                    interview == T & is.na(rank.selected.full) == T ~ TRUE,
                                    interview == T & rank.selected.full == F ~ FALSE,
                                    interview == F ~ FALSE)) %>% 
  mutate(rand.selected = replace_na(rand.selected, replace = F),
         rand2.selected = replace_na(rand2.selected, replace = F),
         rank.selected.dem = replace_na(rank.selected.dem, replace = F),
         rank.selected.full = replace_na(rank.selected.full, replace = F),
         na.round = sum(!is.na(full.predicted.response.propensity.sim2))) %>%
  filter(na.round > 0) %>% # Remove first round for each country
  ungroup() %>%
  arrange(cntry, essround, idno) %>% 
  mutate(cntry.full = case_when(cntry == "BE" ~ "Belgium",
                                cntry == "CH" ~ "Switzerland",
                                cntry == "DK" ~ "Denmark",
                                cntry == "EE" ~ "Estonia",
                                cntry == "FI" ~ "Finland",
                                cntry == "SI" ~ "Slovenia")) %>% 
  mutate(cntry.round.full = str_c(cntry.full, essround, sep = "-"))

contacts.status.all$cntry.round <- droplevels(as.factor(contacts.status.all$cntry.round))

included.cntry.rounds <- unique(contacts.status.all$cntry.round)


contacts.status.all <- contacts.status.all %>% 
  mutate(cntry.round = fct_relevel(cntry.round,
                                   "BE-7", "BE-8", "BE-9",
                                   "DK-7",
                                   "EE-7", "EE-10",
                                   "FI-7", "FI-8", "FI-9", "FI-10", 
                                   "SI-7", "SI-8","SI-9", "SI-10",
                                   "CH-7", "CH-9", "CH-10"))  %>% 
  mutate(cntry.round.full = factor(cntry.round.full,
                                   levels = c("Belgium-7", "Belgium-8", "Belgium-9",
                                              "Denmark-7", "Estonia-7", "Estonia-10",
                                              "Finland-7", "Finland-8", "Finland-9", "Finland-10", 
                                              "Slovenia-7", "Slovenia-8","Slovenia-9", "Slovenia-10",
                                              "Switzerland-7", "Switzerland-9", "Switzerland-10"))) 


contacts.status.all %>% 
  filter(attempt.first70pct == F) %>% 
  tabyl(rank.selected.full)

# Produce all combinations of simulations * models

model.dem.sim2.top50 <- formula(dem.sim2.resp.top50 ~ gndr_su + age_cat)
model.dem.sim1.bottom50 <- formula(dem.sim1.resp.bottom50 ~ gndr_su + age_cat)

model.full.dem.sim2.top50 <- formula(dem.sim2.resp.top50 ~ multi.unit * neighb + no.access +
                                       gndr_su + age_cat)
model.full.dem.sim1.bottom50 <- formula(dem.sim1.resp.bottom50 ~ multi.unit * neighb +
                                          no.access + 
                                          gndr_su + age_cat)

model.dem.top50 <- formula(full.sim2.resp.top50 ~  gndr_su + age_cat)
model.dem.bottom50 <- formula(full.sim1.resp.bottom50 ~ gndr_su + age_cat)
model.full.top50 <- formula(full.sim2.resp.top50 ~ multi.unit * neighb + no.access + 
                              gndr_su + age_cat)
model.full.bottom50 <- formula(full.sim1.resp.bottom50 ~ multi.unit * neighb + no.access + 
                                 gndr_su + age_cat)

model.dem.rand <- formula(rand.resp ~ gndr_su + age_cat)
model.full.rand <- formula(rand.resp ~ multi.unit * neighb + no.access + 
                             gndr_su + age_cat)

model.dem.rand2 <- formula(rand.resp ~ gndr_su + age_cat)
model.full.rand2 <- formula(rand.resp ~ multi.unit * neighb + no.access + 
                              gndr_su + age_cat)

model.dem.rank.dem <- formula(rank.resp.dem ~ gndr_su + age_cat)
model.dem.rank.full <- formula(rank.resp.full ~ gndr_su + age_cat)
model.full.rank.dem <- formula(rank.resp.dem ~ multi.unit * neighb + no.access + 
                                 gndr_su + age_cat)
model.full.rank.full <- formula(rank.resp.full ~ multi.unit * neighb + no.access + 
                                  gndr_su + age_cat)


model.interview.full <- formula(interview ~ multi.unit * neighb + no.access + 
                                  gndr_su + age_cat)





summariseR <- function(data, response.model, selected, Rindicator) {
  cntry.round <- levels(contacts.status.all$cntry.round)
  cntry.round.full <- levels(contacts.status.all$cntry.round.full)
  response.rate <- sapply(data$country_indicator, `[[`, 'propMean' )
  CV <- sapply(data$country_indicator, `[[`, 'CV' )
  CVSE <- sapply(data$country_indicator, `[[`, 'CVSE' )
  tibble(cntry.round, CV, CVSE, response.rate, response.model, selected, Rindicator)
}



###### CV Demographics + Targeting on demographics

dem.sim1.bottom50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.sim1.bottom50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

dem.sim2.top50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.sim2.top50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


###### CV full model + Targeting on demographics

full.dem.sim1.bottom50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.dem.sim1.bottom50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

full.dem.sim2.top50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.dem.sim2.top50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


##### R-indicator demographics + Targeting on full model

dem.bottom50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.bottom50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


dem.top50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.top50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


##### R-indicator full model + Targeting on full model


full.bottom50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.bottom50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


full.top50 <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.top50, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


###### R-indicator demographics + Targeting on random selection 


rand.dem <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.rand, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


rand2.dem <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.rand2, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

###### R-indicator full model + Targeting on random selection 


rand.full <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.rand, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))


rand2.full <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.rand2, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

###### R-indicator demographics + Targeting on rank selection 

dem.dem.rank <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.rank.dem, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

dem.full.rank <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.dem.rank.full, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))




###### R-indicator full model + Targeting on rank selection 

full.dem.rank <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.rank.dem, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))

full.full.rank <- contacts.status.all %>% 
  group_by(cntry.round) %>%
  do(country_indicator = getRIndicator(formula = model.full.rank.full, 
                                       sampleData = ., 
                                       family = "binomial",
                                       withPartials = TRUE))








r.summary.dem.bottom <- summariseR(dem.sim1.bottom50, 
                                   response.model = "Demographics", 
                                   selected = "Sim1 Bottom",
                                   Rindicator = "Dem")
r.summary.dem.top <- summariseR(dem.sim2.top50, 
                                response.model = "Demographics", 
                                selected = "Sim2 Top",
                                Rindicator = "Dem")

r.summary.full.dem.bottom <- summariseR(full.dem.sim1.bottom50, 
                                        response.model = "Demographics", 
                                        selected = "Sim1 Bottom",
                                        Rindicator = "Full")
r.summary.full.dem.top <- summariseR(full.dem.sim2.top50, 
                                     response.model = "Demographics", 
                                     selected = "Sim2 Top",
                                     Rindicator = "Full")



r.summary.dem.dem.bottom50 <- summariseR(dem.bottom50, 
                                         response.model = "Demographics + Interviewer obs.", 
                                         selected = "Sim1 Bottom",
                                         Rindicator = "Dem")
r.summary.dem.full.top50 <- summariseR(full.top50, 
                                       response.model = "Demographics + Interviewer obs.", 
                                       selected = "Sim2 Top",
                                       Rindicator = "Dem")



r.summary.full.bottom <- summariseR(full.bottom50, 
                                    response.model = "Demographics + Interviewer obs.", 
                                    selected = "Sim1 Bottom",
                                    Rindicator = "Full")
r.summary.full.top <- summariseR(full.top50, 
                                 response.model = "Demographics + Interviewer obs.", 
                                 selected = "Sim2 Top",
                                 Rindicator = "Full")





r.summary.rank.dem.dem <- summariseR(dem.dem.rank, 
                                     response.model = "Demographics", 
                                     selected = "Sim3 Rank",
                                     Rindicator = "Dem")
r.summary.rank.dem.full <- summariseR(dem.full.rank, 
                                      response.model = "Demographics + Interviewer obs.", 
                                      selected = "Sim3 Rank",
                                      Rindicator = "Dem")

r.summary.rank.full.dem <- summariseR(full.dem.rank, 
                                      response.model = "Demographics", 
                                      selected = "Sim3 Rank",
                                      Rindicator = "Full")
r.summary.rank.full.full <- summariseR(full.full.rank, 
                                       response.model = "Demographics + Interviewer obs.", 
                                       selected = "Sim3 Rank",
                                       Rindicator = "Full")




r.summary.rand.dem.dem <- summariseR(rand.dem, 
                                     response.model = "Demographics", 
                                     selected = "Sim4 Random",
                                     Rindicator = "Dem")
r.summary.rand.dem.full <- summariseR(rand2.dem, 
                                      response.model = "Demographics + Interviewer obs.", 
                                      selected = "Sim4 Random",
                                      Rindicator = "Dem")

r.summary.rand.full.dem <- summariseR(rand.full, 
                                      response.model = "Demographics", 
                                      selected = "Sim4 Random",
                                      Rindicator = "Full")
r.summary.rand.full.full <- summariseR(rand2.full, 
                                       response.model = "Demographics + Interviewer obs.", 
                                       selected = "Sim4 Random",
                                       Rindicator = "Full")






r.summary <- rbind(r.summary.dem.bottom, r.summary.dem.top, r.summary.full.dem.bottom, r.summary.full.dem.top,
                   r.summary.dem.dem.bottom50, r.summary.dem.full.top50, r.summary.full.bottom, r.summary.full.top,
                   r.summary.rank.dem.dem, r.summary.rank.dem.full, r.summary.rank.full.dem, r.summary.rank.full.full,
                   r.summary.rand.dem.dem, r.summary.rand.dem.full, r.summary.rand.full.dem, r.summary.rand.full.full) %>% 
  arrange(cntry.round)  %>% 
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

# Figure 2 

r.summary %>% 
  filter(Rindicator == "Full") %>% 
  ggplot(aes(x = fct_rev(cntry.round.full), y = CV, color = response.model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CV.CI95.lower, ymax = CV.CI95.upper), 
                position = position_dodge(width = 0.5), width = 0) + 
  coord_flip() +
  theme_bw() +
  scale_color_viridis_d(option = "A", begin = 0.20, end = 0.8) +
  facet_wrap(~selected, ncol = 4) +
  ylim(-0.06, 0.35) +
  labs(x = "Country-round", y = "CV", color = "Covariates") + 
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        axis.text = element_text(size = 14))




ggsave(here("Figures/Figure2_CVs.tiff"), height = 11, width = 11, dpi = 300, device = "tiff", compression = "lzw") 



# Difference between bottom and random selection


r.summary %>% 
  filter(Rindicator == "Full") |> 
  filter(response.model == "Demographics + Interviewer obs." ) |> 
  select(cntry.round, CV, selected, response.model) |> 
  filter(selected == "Sim1 Bottom" | selected == "Sim4 Random") |> 
  pivot_wider(names_from = selected, values_from = CV) |> 
  mutate(sim1.sim4.CV.diff = `Sim4 Random` -  `Sim1 Bottom`) |> 
  arrange(sim1.sim4.CV.diff)




# Appendix Figure 2 

r.summary %>% 
  filter(Rindicator == "Dem") %>% 
  ggplot(aes(x = fct_rev(cntry.round.full), y = CV, color = response.model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CV.CI95.lower, ymax = CV.CI95.upper), 
                position = position_dodge(width = 0.5), width = 0) + 
  coord_flip() +
  theme_bw() +
  scale_color_viridis_d(option = "A", begin = 0.20, end = 0.8) +
  facet_wrap(~selected, ncol = 4) +
  labs(x = "Country-round", y = "CV", color = "Covariates") + 
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        axis.text = element_text(size = 14))



ggsave(here("Figures/Appendix_figure2_CVs_dem.tiff"), height = 14, width = 11, dpi = 300, device = "tiff", compression = "lzw") 


# Figure 3 Response Rates 

r.summary %>% 
  filter(Rindicator == "Full") %>% 
  ggplot(aes(x = fct_rev(cntry.round.full), y = response.rate, fill = response.model, width = .7)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  coord_flip(ylim = c(0.35, 0.65)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0.35, 0.65, by = 0.1), labels = scales::percent) +
  scale_fill_viridis_d(option = "A", begin = 0.20, end = 0.8) +
  facet_wrap(~selected, ncol = 4,) +
  labs(x = "Country-round", y = "Response rate", fill = "Covariates") + 
  theme(legend.position = "bottom",
        panel.spacing = unit(1, "lines"),
        text = element_text(size = 20),
        axis.text = element_text(size = 14))


ggsave(here("Figures/Figure3_response_rates.tiff"), 
       height = 11, width = 11, dpi = 300, device = "tiff", compression = "lzw") 




r.summary %>% 
  filter(Rindicator == "Full") %>% 
  pivot_wider(names_from = c("selected"), values_from = ("CV")) %>% 
  group_by(cntry.round, response.model) %>% 
  summarise(top.bottom.diff = mean(`Sim2 Top`, na.rm = T) - mean(`Sim1 Bottom`, na.rm = T)) %>% 
  arrange(response.model, top.bottom.diff) %>% 
  ungroup() %>% 
  group_by(response.model) %>% 
  summarise(mean.diff.top.bottom = mean(top.bottom.diff),
            sd.diff.top.bottom = sd(top.bottom.diff)
  )





### Appendix table A2


r.summary %>% 
  filter(Rindicator == "Dem") %>% 
  pivot_wider(names_from = c("selected"), values_from = ("CV")) %>% 
  group_by(response.model) %>% 
  summarise(mean.bottom  = mean(`Sim1 Bottom`, na.rm = T),
            sd.bottom  = sd(`Sim1 Bottom`, na.rm = T),
            mean.top = mean(`Sim2 Top`, na.rm = T),
            sd.top = sd(`Sim2 Top`, na.rm = T),
            mean.rank = mean(`Sim3 Rank`, na.rm = T),
            sd.rank = sd(`Sim3 Rank`, na.rm = T),
            mean.random = mean(`Sim4 Random`, na.rm = T),
            sd.random = sd(`Sim4 Random`, na.rm = T),
  ) %>% 
  gt::gt()






r.summary %>% 
  # filter(Rindicator == "Full") %>% 
  pivot_wider(names_from = c("selected"), values_from = ("response.rate")) %>% 
  group_by(response.model) %>% 
  summarise(mean.bottom  = mean(`Sim1 Bottom`, na.rm = T),
            sd.bottom  = sd(`Sim1 Bottom`, na.rm = T),
            mean.top = mean(`Sim2 Top`, na.rm = T),
            sd.top = sd(`Sim2 Top`, na.rm = T),
            mean.rank = mean(`Sim3 Rank`, na.rm = T),
            sd.rank = sd(`Sim3 Rank`, na.rm = T),
            mean.random = mean(`Sim4 Random`, na.rm = T),
            sd.random = sd(`Sim4 Random`, na.rm = T),
  ) %>% 
  gt::gt()





r.summary %>% 
  #  filter(Rindicator == "Full") %>% 
  pivot_wider(names_from = c("selected"), values_from = ("response.rate")) %>% 
  group_by(cntry.round, response.model) %>% 
  summarise(top.bottom.diff = round(mean(`Sim2 Top`, na.rm = T) - mean(`Sim1 Bottom`, na.rm = T), 3)) %>% 
  arrange(response.model, top.bottom.diff) %>% print(n = 100)


contacts.status.all %>% 
  select(cntry.round, attempt, attempt.number.70pct, rand.selected, rank.selected.dem, 
         rank.selected.full, starts_with("full"), starts_with("dem")) %>% 
  group_by(cntry.round) %>% 
  mutate(poss = attempt - attempt.number.70pct) 




### Number of contact attempts saved


contacts.status.all %>% 
  select(cntry.round, attempt, attempt.number.70pct, rand.selected, rank.selected.dem, 
         rank.selected.full, starts_with("full"), starts_with("dem")) %>% 
  group_by(cntry.round) %>% 
  mutate(poss = attempt - attempt.number.70pct,
         
         saved.rand.selected = if_else(rand.selected == F,
                                       attempt - attempt.number.70pct, 0),
         
         saved.rank.selected.dem = if_else(rank.selected.dem == F,
                                           attempt - attempt.number.70pct, 0),
         
         saved.rank.selected.full = if_else(rank.selected.full == F,
                                            attempt - attempt.number.70pct, 0),
         
         saved.full.sim2.top50selected = if_else(full.sim2.top50selected == F,
                                                 attempt - attempt.number.70pct, 0),
         
         saved.full.sim1.bottom50selected = if_else(full.sim1.bottom50selected == F,
                                                    attempt - attempt.number.70pct, 0),
         
         saved.dem.sim2.top50selected = if_else(dem.sim2.top50selected == F,
                                                attempt - attempt.number.70pct, 0),
         
         saved.dem.sim1.bottom50selected = if_else(dem.sim1.bottom50selected == F,
                                                   attempt - attempt.number.70pct, 0)) %>% 
  group_by(cntry.round) %>% 
  summarise(attempts = sum(attempt),
            poss = sum(poss),
            prop.poss = percent(poss / attempts, accuracy = 0.1),
            
            saved.dem.sim1.bottom50selected = sum(saved.dem.sim1.bottom50selected),
            prop.dem.bottom = percent(saved.dem.sim1.bottom50selected / attempts, accuracy = 0.1),
            
            saved.dem.sim2.top50selected = sum(saved.dem.sim2.top50selected),
            prop.dem.top = percent(saved.dem.sim2.top50selected / attempts, accuracy = 0.1),
            
            saved.full.sim1.bottom50selected = sum(saved.full.sim1.bottom50selected),
            prop.full.bottom = percent(saved.full.sim1.bottom50selected / attempts, accuracy = 0.1),
            
            saved.full.sim2.top50selected = sum(saved.full.sim2.top50selected),
            prop.full.top = percent(saved.full.sim2.top50selected / attempts, accuracy = 0.1),
            
            
            saved.rank.selected.dem = sum(saved.rank.selected.dem),
            prop.rank.dem = percent(saved.rank.selected.dem / attempts, accuracy = 0.1),
            
            saved.rank.selected.full = sum(saved.rank.selected.full),
            prop.rank.full = percent(saved.rank.selected.full / attempts, accuracy = 0.1),
            
            saved.rand.selected = sum(saved.rand.selected),
            prop.rand = percent(saved.rand.selected / attempts, accuracy = 0.1)
            
  ) %>% 
  gt::gt() %>%
  gt::gtsave(filename = "attempts_saved.rtf")


contacts.status.all %>% 
  select(cntry.round.full, attempt, attempt.number.70pct, rand.selected, 
         rank.selected.dem, 
         rank.selected.full, starts_with("full"), starts_with("dem")) %>% 
  group_by(cntry.round.full) %>% 
  mutate(poss = attempt - attempt.number.70pct,
         saved.rand.selected = if_else(rand.selected == F,
                                       attempt - attempt.number.70pct, 0),
         saved.rank.selected.dem = if_else(rank.selected.dem == F,
                                           attempt - attempt.number.70pct, 0),
         
         saved.rank.selected.full = if_else(rank.selected.full == F,
                                            attempt - attempt.number.70pct, 0),
         
         saved.full.sim2.top50selected = if_else(full.sim2.top50selected == F,
                                                 attempt - attempt.number.70pct, 0),
         
         saved.full.sim1.bottom50selected = if_else(full.sim1.bottom50selected == F,
                                                    attempt - attempt.number.70pct, 0),
         
         saved.dem.sim2.top50selected = if_else(dem.sim2.top50selected == F,
                                                attempt - attempt.number.70pct, 0),
         
         saved.dem.sim1.bottom50selected = if_else(dem.sim1.bottom50selected == F,
                                                   attempt - attempt.number.70pct, 0)) %>% 
  summarise(attempts = sum(attempt),
            poss = sum(poss),
            prop.poss = percent(poss / attempts, accuracy = 0.1),
            
            saved.dem.sim1.bottom50selected = sum(saved.dem.sim1.bottom50selected),
            prop.dem.bottom = percent(saved.dem.sim1.bottom50selected / attempts, accuracy = 0.1),
            
            
            saved.dem.sim2.top50selected = sum(saved.dem.sim2.top50selected),
            prop.dem.top = percent(saved.dem.sim2.top50selected / attempts, accuracy = 0.1),
            
            saved.full.sim1.bottom50selected = sum(saved.full.sim1.bottom50selected),
            prop.full.bottom = percent(saved.full.sim1.bottom50selected / attempts, accuracy = 0.1),
            
            saved.full.sim2.top50selected = sum(saved.full.sim2.top50selected),
            prop.full.top = percent(saved.full.sim2.top50selected / attempts, accuracy = 0.1),
            
            
            saved.rank.selected.dem = sum(saved.rank.selected.dem),
            prop.rank.dem = percent(saved.rank.selected.dem / attempts, accuracy = 0.1),
            
            saved.rank.selected.full = sum(saved.rank.selected.full),
            prop.rank.full = percent(saved.rank.selected.full / attempts, accuracy = 0.1),
            
            saved.rand.selected = sum(saved.rand.selected),
            prop.rand = percent(saved.rand.selected / attempts, accuracy = 0.1),
            prop.rand2 = percent(saved.rand.selected / attempts, accuracy = 0.1)
            
  ) %>% 
  select(cntry.round.full, starts_with("prop")) %>% 
  pivot_longer(starts_with("prop"), 
               names_to = "sim",
               values_to = "prop") %>% 
  filter(sim != "prop.poss") %>% 
  mutate(sim = recode(sim, 
                      prop.dem.bottom = "Mod1+Sim1",
                      prop.dem.top = "Mod1+Sim2",
                      prop.full.bottom = "Mod2+Sim1",
                      prop.full.top = "Mod2+Sim2",
                      prop.rank.dem = "Mod1+Sim3",
                      prop.rank.full = "Mod2+Sim3",
                      prop.rand = "Mod1+Sim4",
                      prop.rand2 = "Mod2+Sim4"
  ),
  prop = as.numeric(str_replace(prop, "%", ""))/100) %>% 
  mutate(model = str_sub(sim, 1, 4),
         simulation = str_sub(sim, -4, -1)) %>% 
  mutate(model = recode(model, "Mod1" = "Demographics only", 
                        "Mod2" = "Demographics + Interviewer obs."),
         simulation = recode(simulation, "Sim1" = "Sim1 Bottom",
                             "Sim2" = "Sim2 Top",
                             "Sim3" = "Sim3 Rank",
                             "Sim4" = "Sim4 Rand"
         )) %>% 
  ggplot(aes(x = fct_rev(cntry.round.full), y = prop, fill = fct_rev(model), width = .7)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  facet_wrap(~simulation, ncol = 4) +
  scale_fill_viridis_d(option = "A", begin = 0.20, end = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(x = "",
       y = "Proportion of contact attempts saved",
       fill = "Covariates") +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 6))

ggsave(here("Figures/Figure4_attepmts_saved.tiff"), height = 11, width = 11, dpi = 300, device = "tiff", compression = "lzw", bg = "white") 




#### Predicted probabilities by response propensity model - Model 2 dem + int - Figure 1

contacts.status.all %>% 
  filter(attempt.first70pct == F) |> 
  group_by(cntry.round) %>% 
  mutate(minx = min(full.predicted.response.propensity.sim2[full.sim2.top50selected == T], na.rm = T),
         maxy = max(full.predicted.response.propensity.sim1[full.sim1.bottom50selected == T], na.rm = T),
         rank.selected.full = if_else(rank.selected.full == T, "Selected", "Not selected"),
         rand.selected = if_else(rand.selected == T, "Selected", "Not selected"),
  ) %>% 
  mutate(cntry.round.full.x = factor(cntry.round.full,
                                     levels = c("Belgium-7", "Belgium-8", "Belgium-9"," ",
                                                "Denmark-7", "Estonia-7", "Estonia-10", "  ", 
                                                "Finland-7", "Finland-8", "Finland-9", "Finland-10", 
                                                "Slovenia-7", "Slovenia-8","Slovenia-9", "Slovenia-10",
                                                "Switzerland-7", "Switzerland-9", "Switzerland-10"))) %>% 
  ggplot(aes(y = full.predicted.response.propensity.sim1,
             x = full.predicted.response.propensity.sim2,
             color = rank.selected.full,
  )) +
  geom_jitter(width = 0.002, height = 0.002, alpha = 0.75) +
  geom_vline(aes(xintercept = minx)) + 
  geom_hline(aes(yintercept = maxy)) + 
  facet_wrap(~cntry.round.full.x,  ncol = 4, drop = FALSE) +
  theme_minimal() + 
  scale_color_viridis_d(option = "A", begin = 0.15, end = 0.85) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(size = 14)) +
  labs(y = "pr(R early = 1)",
       x = "pr(R late = 1 | R early = 0)",
       color = "Sim3 Rank"
  ) 



ggsave(here("Figures/Figure1_selectionmodels.tiff"), height = 14, width = 11, dpi = 300, device = "tiff", compression = "lzw", bg = "white") 



#### Predicted probabilities by response propensity model - Model 1 Dem only - Appendix Figure 1


contacts.status.all %>% 
  filter(attempt.first70pct == F) |> 
  group_by(cntry.round) %>% 
  mutate(minx = min(dem.predicted.response.propensity.sim2[dem.sim2.top50selected == T], na.rm = T),
         maxy = max(dem.predicted.response.propensity.sim1[dem.sim1.bottom50selected == T], na.rm = T),
         rank.selected.dem = if_else(rank.selected.dem == T, "Selected", "Not selected"),
         rand.selected = if_else(rand.selected == T, "Selected", "Not selected"),
  ) %>% 
  mutate(cntry.round.full.x = factor(cntry.round.full,
                                     levels = c("Belgium-7", "Belgium-8", "Belgium-9"," ",
                                                "Denmark-7", "Estonia-7", "Estonia-10", "  ", 
                                                "Finland-7", "Finland-8", "Finland-9", "Finland-10", 
                                                "Slovenia-7", "Slovenia-8","Slovenia-9", "Slovenia-10",
                                                "Switzerland-7", "Switzerland-9", "Switzerland-10"))) %>% 
  ggplot(aes(y = dem.predicted.response.propensity.sim1,
             x = dem.predicted.response.propensity.sim2,
             color = rank.selected.dem,
  )) +
  geom_jitter(width = 0.002, height = 0.002, alpha = 0.75) +
  geom_vline(aes(xintercept = minx)) + 
  geom_hline(aes(yintercept = maxy)) + 
  facet_wrap(~cntry.round.full.x, ncol = 4, drop = FALSE) +
  theme_minimal() + 
  scale_color_viridis_d(option = "A", begin = 0.15, end = 0.85) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 20),
        axis.text = element_text(size = 14)) +
  labs(y = "pr(R early)",
       x = "pr(R late | NR early)",
       color = "Sim3 Rank") 


ggsave(here("Figures/Appendix_figure1_selectionmodels_model1.tiff"), height = 14, width = 11, dpi = 300, device = "tiff", compression = "lzw", bg = "white") 



# How well do models predict RR?


contacts.status.all |> 
  group_by(cntry.round) |> 
  mutate(simgroups = santoku::chop_equally(full.predicted.response.propensity.sim2, groups = 20)) |> 
  group_by(cntry.round, simgroups) |> 
  summarise(rr = mean(interview)) |> 
  filter(!is.na (simgroups)) |> 
  ggplot(aes(x = simgroups, y = rr)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Sim2+Model4",
       x = "Sim2+M4 predicted RR",
       y = "Actual Response Rate") +
  theme(text = element_text(size = 10)) +
  coord_flip() + 
  facet_wrap(cntry.round ~ ., scales="free", nrow = 5)  +
  scale_y_continuous(labels = scales::percent)

ggsave(here("Figures/predictionsSim2Model4.tiff"), height = 14, width = 11) 



contacts.status.all |> 
  group_by(cntry.round) |> 
  mutate(simgroups = santoku::chop_equally(dem.predicted.response.propensity.sim2, groups = 20)) |> 
  group_by(cntry.round, simgroups) |> 
  summarise(rr = mean(interview)) |> 
  filter(!is.na (simgroups)) |> 
  ggplot(aes(x = simgroups, y = rr)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Sim2+Model2",
       x = "Sim2+M2 predicted RR",
       y = "Actual Response Rate") +
  theme(text = element_text(size = 10)) +
  coord_flip() + 
  facet_wrap(cntry.round ~ ., scales="free", nrow = 5) +
  scale_y_continuous(labels = scales::percent)

ggsave(here("Figures/predictionsSim2Model2.tiff"), height = 14, width = 11) 



contacts.status.all |> 
  group_by(cntry.round) |> 
  mutate(simgroups = santoku::chop_equally(full.predicted.response.propensity.sim1, groups = 20)) |> 
  group_by(cntry.round, simgroups) |> 
  summarise(rr = mean(interview)) |> 
  filter(!is.na (simgroups)) |> 
  ggplot(aes(x = simgroups, y = rr)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Sim1+Model3",
       x = "Sim1+M3 predicted RR",
       y = "Actual Response Rate") +
  theme(text = element_text(size = 10)) +
  coord_flip() + 
  facet_wrap(cntry.round ~ ., scales="free", nrow = 5)  +
  scale_y_continuous(labels = scales::percent)

ggsave(here("Figures/predictionsSim1Model3.tiff"), height = 14, width = 11) 



contacts.status.all |> 
  group_by(cntry.round) |> 
  mutate(simgroups = santoku::chop_equally(dem.predicted.response.propensity.sim2, groups = 20)) |> 
  group_by(cntry.round, simgroups) |> 
  summarise(rr = mean(interview)) |> 
  filter(!is.na (simgroups)) |> 
  ggplot(aes(x = simgroups, y = rr)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Sim1+Model1",
       x = "Sim1+M1 predicted RR",
       y = "Actual Response Rate") +
  theme(text = element_text(size = 10)) +
  coord_flip() + 
  facet_wrap(cntry.round ~ ., scales="free", nrow = 5) +
  scale_y_continuous(labels = scales::percent)

ggsave(here("Figures/predictionsSim1Model1.tiff"), height = 14, width = 11) 






