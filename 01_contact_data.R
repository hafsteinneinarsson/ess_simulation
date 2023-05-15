# 01_contact_data
# Cleaning of ESS contact data


# Read contact attempt data for ESS - Round 6

ESS6 <- read_sav(here::here("Data/ESS6CFe02.sav")) %>%
  mutate(typesamp = as_factor(typesamp),
         cntry = as.factor(cntry),
         interview = interva == 1,
         typesamp2 = case_when(typesamp == "Individual person" ~ "Individual person",
                               typesamp == "Household" ~ "Household or Address",
                               typesamp == "Address" ~ "Household or Address"),
         telephone.present = telnum == 1,
         gndr_su = case_when(gndr_su == 1 ~ "Male",
                             gndr_su == 2 ~ "Female"),
         age_cat = case_when(age_su >= 14 & age_su <= 34~ "Under 35",
                             age_su >= 35 & age_su <= 50 ~ "35-49",
                             age_su >= 50 & age_su <= 64 ~ "50-64",
                             age_su >= 65  ~ "65+"),
         age50 = case_when(age_su < 34 ~ "Under 50",
                           age_su >= 35 & age_su < 50 ~ "Under 50",
                           age_su >= 50 & age_su < 64 ~ "50+",
                           age_su >= 65 ~ "50+"),
         age_cat = fct_relevel(age_cat, "Under 35", after = 0),
         housetype = case_when(type < 5 ~ "Single unit",
                               type >= 6 & type <= 8 ~ "Multi-unit",
                               type == 5  ~ "Other",
                               type >= 9 & type <= 10 ~ "Other"),
         housetype = fct_relevel(housetype, "Other", after = Inf),
         single.unit = case_when(type < 5 ~ "Single unit",
                                 type >= 5 & type <= 10 ~ "Other"),
         single.unit = fct_relevel(single.unit, "Other", after = Inf),
         multi.unit = case_when(housetype ==  "Multi-unit"  ~ "Multi-unit",
                                housetype != "Multi-unit"  ~"Not multi-unit"),
         physical.assesment = fct_lump(as.factor(physa)),
         physa.3 = fct_collapse(as.factor(physa),
                                Good = c("1", "2"),
                                Satisfactory = c("3"),
                                Bad = c("4", "5")),
         physa.2 = physa.3 == "Bad",
         no.access = case_when(access == 4 ~ "No barriers",
                               access != 4 ~ "Barriers"), 
         vandalism = vandaa != 4,
         litter = littera != 4,
         neighb.assessment = physa.2 + vandalism + litter,
         neighb = case_when(neighb.assessment == 0 ~ "None",
                            neighb.assessment >= 1 ~ "One or more"),
         neighb3 = case_when(neighb.assessment == 0 ~ "None",
                             neighb.assessment == 1 ~ "One",
                             neighb.assessment >= 2 ~ "Two or more"),
         interview_fct = as.factor(interview),
         interview_fct = if_else(interview_fct == "TRUE", 
                                 "Interview", "No interview"),
         interva789 = interva == 7 |  interva == 8 | interva == 9,
         interview_na = ifelse(interva789 == T | is.na(interva), TRUE, FALSE)) %>% 
  filter(typesamp == "Individual person") %>% 
  mutate_at(vars(contains("minv")), 
            as.numeric) %>% 
  mutate_at(vars(starts_with("minv")), 
            round) 


yr.mon.date <- map2_dfc(select(ESS6, starts_with("monv")),
                        select(ESS6, starts_with("date")),
                        function(x, y){
                          str_c("2012-", x, "-", y)
                        }) 

hr.min <- map2_dfc(select(ESS6, starts_with("hourv")),
                   select(ESS6, starts_with("minv")),
                   function(x, y){
                     str_c(x, "-", y)
                   })

yr.mon.date.hr.min <- map2_dfc(yr.mon.date,
                               hr.min,
                               function(x, y){
                                 str_c(x, "-", y)
                               })

# rename columns
colnames(yr.mon.date.hr.min) <- str_replace(colnames(yr.mon.date.hr.min),
                                            pattern = "monv",
                                            "contact")

ESS6 %>% filter(typesamp == "Individual person") %>% tabyl(cntry)

# Fix dates based on https://www.europeansocialsurvey.org/data/deviations_6.html
ESS6x <- bind_cols(ESS6, yr.mon.date.hr.min) %>% 
  mutate_at(vars(starts_with("contact")), ymd_hm) %>% 
  mutate(across(starts_with("contact"),  # Some countries conducted fieldwork in both 2012 and 2013, have to add a year
                ~ case_when(month(.) < 8 & cntry == "CH" ~ . + years(1), 
                            cntry == "CZ" ~ . + years(1), 
                            cntry == "DE" ~ . + years(1), 
                            cntry == "DK" ~ . + years(1), 
                            month(.) < 8 & cntry == "EE" ~ . + years(1), 
                            cntry == "ES" ~ . + years(1), 
                            month(.) < 8 & cntry == "FI" ~ . + years(1), 
                            cntry == "FR" ~ . + years(1), 
                            month(.) < 9 & cntry == "HU" ~ . + years(1), 
                            cntry == "IT" ~ . + years(1), 
                            month(.) < 7 & cntry == "NO" ~ . + years(1), 
                            month(.) < 8 & cntry == "PL" ~ . + years(1), 
                            month(.) < 8 & cntry == "PT" ~ . + years(1), 
                            month(.) < 8 & cntry == "SE" ~ . + years(1), 
                            TRUE ~ .))) %>% 
  mutate_at(vars(starts_with("resulb")), 
            to_factor) %>% 
  mutate(across(starts_with("resulb"), 
                .fns = list(result = 
                              ~case_when(. == "Completed interview" ~ TRUE, 
                                         . == "Complete interview" ~ TRUE, 
                                         TRUE ~ FALSE)), 
                .names = "{fn}{col}"))  %>%
  rename_all(~str_replace(., 
                          "resultresulb", paste0("result"))) %>% 
  select(-starts_with("resulb"), 
         -starts_with("outnic"), 
         -starts_with("modev"), 
         -starts_with("totcin"), 
         -starts_with("refvis"), 
         -starts_with("rersb"), 
         -starts_with("agea"),
         -starts_with("ageb"),  
         -starts_with("gendera"),  
         -starts_with("genderb"),  
         -starts_with("coop"),  
         -starts_with("inwyyspt"),  
         -starts_with("intnum"), 
         -starts_with("resulb"))  %>% 
  select(-starts_with("monv"),
         -starts_with("date"),
         -starts_with("hourv"),
         -starts_with("minv"),
         -starts_with("yearv"),
         -starts_with("dayv")) 


ESS6x$success <- ESS6x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(x) 


ESS6x$sum <- ESS6x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(sum) 

# In the case that two results are marked as interviews, the later contact attemt takes presedence
# ESS6x %>% filter(sum > 1) %>% select(idno, cntry, success, starts_with("resul")) %>% View()




saveRDS(ESS6x, 
        here::here("Data/ESS6_contacts.Rdata"))


tabyl(ESS6x, success, result2)  

tabyl(ESS6x, interva, sum)  


contacts6 <- ESS6x %>% 
  pivot_longer(cols = starts_with("contact"),
               names_to = "attempt",
               values_to = "time") %>% 
  filter(!is.na(time)) %>% 
arrange(cntry, idno) 


saveRDS(contacts6, 
        here::here(
          "Data/contacts6.Rdata"))


contacts6 %>% 
  arrange(cntry, idno, rev(time)) %>% 
  group_by(idno) %>% 
  slice(1) %>% 
  tabyl(interva)






# Read contact attempt data for ESS - Round 7


ESS7 <- read_sav(here::here("Data/ess7CFe02_1.sav")) %>%
  mutate(typesamp = as_factor(typesamp),
         cntry = as.factor(cntry),
         interview = interva == 1,
         typesamp2 = case_when(typesamp == "Individual person" ~ "Individual person",
                               typesamp == "Household" ~ "Household or Address",
                               typesamp == "Address" ~ "Household or Address"),
         telephone.present = telnum == 1,
         gndr_su = case_when(gndr_su == 1 ~ "Male",
                             gndr_su == 2 ~ "Female"),
         age_cat = case_when(age_su >= 14 & age_su <= 34~ "Under 35",
                             age_su >= 35 & age_su <= 50 ~ "35-49",
                             age_su >= 50 & age_su <= 64 ~ "50-64",
                             age_su >= 65  ~ "65+"),
         age50 = case_when(age_su < 34 ~ "Under 50",
                           age_su >= 35 & age_su < 50 ~ "Under 50",
                           age_su >= 50 & age_su < 64 ~ "50+",
                           age_su >= 65 ~ "50+"),
         age_cat = fct_relevel(age_cat, "Under 35", after = 0),
         housetype = case_when(type < 5 ~ "Single unit",
                               type >= 6 & type <= 8 ~ "Multi-unit",
                               type == 5  ~ "Other",
                               type >= 9 & type <= 10 ~ "Other"),
         housetype = fct_relevel(housetype, "Other", after = Inf),
         single.unit = case_when(type < 5 ~ "Single unit",
                                 type >= 5 & type <= 10 ~ "Other"),
         single.unit = fct_relevel(single.unit, "Other", after = Inf),
         multi.unit = case_when(housetype ==  "Multi-unit" ~ "Multi-unit",
                                housetype != "Multi-unit" ~"Not multi-unit"),
         physa.3 = fct_collapse(as.factor(physa),
                                Good = c("1", "2"),
                                Satisfactory = c("3"),
                                Bad = c("4", "5")),
         physa.2 = physa.3 == "Bad",
         no.access = case_when(access == 4 ~ "No barriers",
                               access != 4 ~ "Barriers"), 
         vandalism = vandaa != 4,
         litter = littera != 4,
         neighb.assessment = physa.2 + vandalism + litter,
         neighb = case_when(neighb.assessment == 0 ~ "None",
                            neighb.assessment >= 1 ~ "One or more"),
         neighb3 = case_when(neighb.assessment == 0 ~ "None",
                             neighb.assessment == 1 ~ "One",
                             neighb.assessment >= 2 ~ "Two or more"),
         interview_fct = as.factor(interview),
         interview_fct = if_else(interview_fct == "TRUE", 
                                 "Interview", "No interview"),
         interva789 = interva == 7 |  interva == 8 | interva == 9,
         interview_na = ifelse(interva789 == T | is.na(interva), TRUE, FALSE)) %>% 
  filter(typesamp == "Individual person") %>% 
  mutate_at(vars(contains("minv")), 
            as.numeric) %>% 
  mutate_at(vars(starts_with("minv")), 
            round) 


yr.mon.date <- map2_dfc(select(ESS7, starts_with("monv")),
                        select(ESS7, starts_with("date")),
                        function(x, y){
                          str_c("2014-", x, "-", y)
                        }) 

hr.min <- map2_dfc(select(ESS7, starts_with("hourv")),
                   select(ESS7, starts_with("minv")),
                   function(x, y){
                     str_c(x, "-", y)
                   })

yr.mon.date.hr.min <- map2_dfc(yr.mon.date,
                               hr.min,
                               function(x, y){
                                 str_c(x, "-", y)
                               })

# rename columns
colnames(yr.mon.date.hr.min) <- str_replace(colnames(yr.mon.date.hr.min),
                                            pattern = "monv",
                                            "contact")

# Fix dates based on https://www.europeansocialsurvey.org/data/deviations_7.html
ESS7x <- bind_cols(ESS7, yr.mon.date.hr.min) %>% 
  filter(cntry != "GB") %>%  # remove GB which has two fieldwork periods and is not included in the analysis anyway
  mutate_at(vars(starts_with("contact")), ymd_hm) %>% 
  mutate(across(starts_with("contact"),  # Some countries conducted fieldwork in both 2014 and 2015, have to add a year
                ~ case_when(month(.) < 9 & cntry == "AT" ~ . + years(1),
                            month(.) < 9 & cntry == "BE" ~ . + years(1), 
                            month(.) < 8 & cntry == "CH" ~ . + years(1), 
                            month(.) < 9 & cntry == "CZ" ~ . + years(1), 
                            month(.) < 8 & cntry == "DE" ~ . + years(1), 
                            month(.) < 9 & cntry == "DK" ~ . + years(1), 
                            month(.) < 9 & cntry == "EE" ~ . + years(1), 
                            cntry == "ES" ~ . + years(1), 
                            month(.) < 9 & cntry == "FI" ~ . + years(1), 
                            month(.) < 9 & cntry == "FR" ~ . + years(1), 
                            cntry == "HU" ~ . + years(1), 
                            month(.) < 9 & cntry == "IE" ~ . + years(1), 
                            month(.) < 9 & cntry == "IE" ~ . + years(1), 
                            cntry == "IL" ~ . + years(1), 
                            cntry == "LT" ~ . + years(1), 
                            month(.) < 9 & cntry == "NL" ~ . + years(1), 
                            month(.) < 8 & cntry == "NO" ~ . + years(1), 
                            cntry == "PL" ~ . + years(1), 
                            cntry == "PT" ~ . + years(1), 
                            month(.) < 8 & cntry == "SE" ~ . + years(1), 
                            month(.) < 8 & cntry == "SI" ~ . + years(1), 
                            TRUE ~ .))) %>% 
  mutate_at(vars(starts_with("resulb")), 
            to_factor) %>% 
  mutate(across(starts_with("resulb"), 
                .fns = list(result = 
                              ~case_when(. == "Completed interview" ~ TRUE, 
                                         . == "Complete interview" ~ TRUE, 
                                         TRUE ~ FALSE)), 
                .names = "{fn}{col}"))  %>%
  rename_all(~str_replace(., 
                          "resultresulb", paste0("result"))) %>% 
  select(-starts_with("resulb"), 
         -starts_with("outnic"), 
         -starts_with("modev"), 
         -starts_with("totcin"), 
         -starts_with("refvis"), 
         -starts_with("rersb"), 
         -starts_with("agea"),
         -starts_with("ageb"),  
         -starts_with("gendera"),  
         -starts_with("genderb"),  
         -starts_with("coop"),  
         -starts_with("coop"),  
         -starts_with("intnum"), 
         -starts_with("resulb"))  %>% 
  select(-starts_with("monv"),
         -starts_with("date"),
         -starts_with("hourv"),
         -starts_with("minv"),
         -starts_with("yearv"),
         -starts_with("dayv")) 

ESS7x$success <- ESS7x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(x) 


ESS7x$sum <- ESS7x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(sum) 

# In the case that two results are marked as interviews, the later contact attemt takes presedence
# ESS7x %>% filter(sum > 1) %>% select(idno, cntry, success, starts_with("resul")) %>% View()




saveRDS(ESS7x, 
        here::here("Data/ESS7_contacts.Rdata"))


tabyl(ESS7x, success, result2)  

tabyl(ESS7x, interva, sum)  


contacts7 <- ESS7x %>% 
  pivot_longer(cols = starts_with("contact"),
               names_to = "attempt",
               values_to = "time") %>% 
  filter(!is.na(time))%>% 
arrange(cntry, idno) 

saveRDS(contacts7, 
        here::here(
          "Data/contacts7.Rdata"))




# Read contact attempt data for ESS - Round 8

ESS8 <- read_sav(here::here("Data/ESS8CFe03.sav")) %>%
  mutate(typesamp = as_factor(typesamp),
         cntry = as.factor(cntry),
         interview = interva == 1,
         typesamp2 = case_when(typesamp == "Individual person" ~ "Individual person",
                               typesamp == "Household" ~ "Household or Address",
                               typesamp == "Address" ~ "Household or Address"),
         telephone.present = telnum == 1,
         gndr_su = case_when(gndr_su == 1 ~ "Male",
                             gndr_su == 2 ~ "Female"),
         age_cat = case_when(age_su >= 14 & age_su <= 34~ "Under 35",
                             age_su >= 35 & age_su <= 50 ~ "35-49",
                             age_su >= 50 & age_su <= 64 ~ "50-64",
                             age_su >= 65  ~ "65+"),
         age50 = case_when(age_su < 34 ~ "Under 50",
                           age_su >= 35 & age_su < 50 ~ "Under 50",
                           age_su >= 50 & age_su < 64 ~ "50+",
                           age_su >= 65 ~ "50+"),
         age_cat = fct_relevel(age_cat, "Under 35", after = 0),
         housetype = case_when(type < 5 ~ "Single unit",
                               type >= 6 & type <= 8 ~ "Multi-unit",
                               type == 5  ~ "Other",
                               type >= 9 & type <= 10 ~ "Other"),
         housetype = fct_relevel(housetype, "Other", after = Inf),
         single.unit = case_when(type < 5 ~ "Single unit",
                                 type >= 5 & type <= 10 ~ "Other"),
         single.unit = fct_relevel(single.unit, "Other", after = Inf),
         multi.unit = case_when(housetype ==  "Multi-unit"  ~ "Multi-unit",
                                housetype != "Multi-unit"  ~"Not multi-unit"),
         physa.3 = fct_collapse(as.factor(physa),
                                Good = c("1", "2"),
                                Satisfactory = c("3"),
                                Bad = c("4", "5")),
         physa.2 = physa.3 == "Bad",
         no.access = case_when(access == 4 ~ "No barriers",
                               access != 4 ~ "Barriers"), 
         vandalism = vandaa != 4,
         litter = littera != 4,
         neighb.assessment = physa.2 + vandalism + litter,
         neighb = case_when(neighb.assessment == 0 ~ "None",
                            neighb.assessment >= 1 ~ "One or more"),
         neighb3 = case_when(neighb.assessment == 0 ~ "None",
                             neighb.assessment == 1 ~ "One",
                             neighb.assessment >= 2 ~ "Two or more"),
         interview_fct = as.factor(interview),
         interview_fct = if_else(interview_fct == "TRUE", 
                                 "Interview", "No interview"),
         interva789 = interva == 7 |  interva == 8 | interva == 9,
         interview_na = ifelse(interva789 == T | is.na(interva), TRUE, FALSE)) %>% 
  filter(typesamp == "Individual person") %>% 
  mutate_at(vars(contains("minv")), 
            as.numeric) %>% 
  mutate_at(vars(starts_with("minv")), 
            round) 


yr.mon.date <- map2_dfc(select(ESS8, starts_with("monv")),
                        select(ESS8, starts_with("date")),
                        function(x, y){
                          str_c("2016-", x, "-", y)
                        }) 

hr.min <- map2_dfc(select(ESS8, starts_with("hourv")),
                   select(ESS8, starts_with("minv")),
                   function(x, y){
                     str_c(x, "-", y)
                   })

yr.mon.date.hr.min <- map2_dfc(yr.mon.date,
                               hr.min,
                               function(x, y){
                                 str_c(x, "-", y)
                               })

# rename columns
colnames(yr.mon.date.hr.min) <- str_replace(colnames(yr.mon.date.hr.min),
                                            pattern = "monv",
                                            "contact")

ESS8 %>% filter(typesamp == "Individual person") %>% tabyl(cntry)

# Fix dates based on https://www.europeansocialsurvey.org/data/deviations_8.html
ESS8x <- bind_cols(ESS8, yr.mon.date.hr.min) %>% 
  mutate_at(vars(starts_with("contact")), ymd_hm) %>% 
  mutate(across(starts_with("contact"),  # Some countries conducted fieldwork in both 2016 and 2017, have to add a year
                ~ case_when(month(.) < 8 & cntry == "BE" ~ . + years(1), 
                            month(.) < 8 & cntry == "CH" ~ . + years(1), 
                            month(.) < 7 & cntry == "DE" ~ . + years(1), 
                            month(.) < 8 & cntry == "EE" ~ . + years(1), 
                            cntry == "ES" ~ . + years(1), 
                            month(.) < 8 & cntry == "FI" ~ . + years(1), 
                            month(.) < 8 & cntry == "FR" ~ . + years(1), 
                            cntry == "HU" ~ . + years(1), 
                            cntry == "IT" ~ . + years(1), 
                            month(.) < 7 & cntry == "NO" ~ . + years(1), 
                            month(.) < 8 & cntry == "PL" ~ . + years(1), 
                            month(.) < 8 & cntry == "PT" ~ . + years(1), 
                            month(.) < 7 & cntry == "SE" ~ . + years(1), 
                            month(.) < 8 & cntry == "SI" ~ . + years(1), 
                            TRUE ~ .))) %>% 
  mutate_at(vars(starts_with("resulb")), 
            to_factor) %>% 
  mutate(across(starts_with("resulb"), 
                .fns = list(result = 
                              ~case_when(. == "Completed interview" ~ TRUE, 
                                         . == "Complete interview" ~ TRUE, 
                                         TRUE ~ FALSE)), 
                .names = "{fn}{col}"))  %>%
  rename_all(~str_replace(., 
                          "resultresulb", paste0("result"))) %>% 
  select(-starts_with("resulb"), 
         -starts_with("outnic"), 
         -starts_with("modev"), 
         -starts_with("totcin"), 
         -starts_with("refvis"), 
         -starts_with("rersb"), 
         -starts_with("agea"),
         -starts_with("ageb"),  
         -starts_with("gendera"),  
         -starts_with("genderb"),  
         -starts_with("coop"),  
         -starts_with("inwyyspt"),  
         -starts_with("intnum"), 
         -starts_with("resulb"))  %>% 
  select(-starts_with("monv"),
         -starts_with("date"),
         -starts_with("hourv"),
         -starts_with("minv"),
         -starts_with("yearv"),
         -starts_with("dayv")) 


ESS8x$success <- ESS8x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(x) 


ESS8x$sum <- ESS8x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(sum) 

saveRDS(ESS8x, 
        here::here("Data/ESS8_contacts.Rdata"))


tabyl(ESS8x, success, result2)  

tabyl(ESS8x, interva, sum)  


contacts8 <- ESS8x %>% 
  pivot_longer(cols = starts_with("contact"),
               names_to = "attempt",
               values_to = "time") %>% 
  filter(!is.na(time)) %>% 
arrange(cntry, idno) 


saveRDS(contacts8, 
        here::here(
          "Data/contacts8.Rdata"))

# Read contact attempt data for ESS - Round 9


ESS9 <- read_sav(here::here("Data/ESS9CFe02.sav")) %>%
  mutate(typesamp = as_factor(typesamp),
         cntry = as.factor(cntry),
         interview = interva == 1,
         typesamp2 = case_when(typesamp == "Individual person" ~ "Individual person",
                               typesamp == "Household" ~ "Household or Address",
                               typesamp == "Address" ~ "Household or Address"),
         telephone.present = telnum == 1,
         gndr_su = case_when(gndr_su == 1 ~ "Male",
                             gndr_su == 2 ~ "Female"),
         age_cat = case_when(age_su >= 14 & age_su <= 34~ "Under 35",
                             age_su >= 35 & age_su <= 50 ~ "35-49",
                             age_su >= 50 & age_su <= 64 ~ "50-64",
                             age_su >= 65  ~ "65+"),
         age50 = case_when(age_su < 34 ~ "Under 50",
                           age_su >= 35 & age_su < 50 ~ "Under 50",
                           age_su >= 50 & age_su < 64 ~ "50+",
                           age_su >= 65 ~ "50+"),
         age_cat = fct_relevel(age_cat, "Under 35", after = 0),
         housetype = case_when(type < 5 ~ "Single unit",
                               type >= 6 & type <= 8 ~ "Multi-unit",
                               type == 5  ~ "Other",
                               type >= 9 & type <= 10 ~ "Other"),
         housetype = fct_relevel(housetype, "Other", after = Inf),
         single.unit = case_when(type < 5 ~ "Single unit",
                                 type >= 5 & type <= 10 ~ "Other"),
         single.unit = fct_relevel(single.unit, "Other", after = Inf),
         multi.unit = case_when(housetype ==  "Multi-unit"  ~ "Multi-unit",
                                housetype != "Multi-unit"  ~"Not multi-unit"),
         physa.3 = fct_collapse(as.factor(physa),
                                Good = c("1", "2"),
                                Satisfactory = c("3"),
                                Bad = c("4", "5")),
         physa.2 = physa.3 == "Bad",
         no.access = case_when(access == 4 ~ "No barriers",
                               access != 4 ~ "Barriers"), 
         vandalism = vandaa != 4,
         litter = littera != 4,
         neighb.assessment = physa.2 + vandalism + litter,
         neighb = case_when(neighb.assessment == 0 ~ "None",
                            neighb.assessment >= 1 ~ "One or more"),
         neighb3 = case_when(neighb.assessment == 0 ~ "None",
                             neighb.assessment == 1 ~ "One",
                             neighb.assessment >= 2 ~ "Two or more"),
         interview_fct = as.factor(interview),
         interview_fct = if_else(interview_fct == "TRUE", 
                                 "Interview", "No interview"),
         interva789 = interva == 7 |  interva == 8 | interva == 9,
         interview_na = ifelse(interva789 == T | is.na(interva), TRUE, FALSE)) %>% 
  filter(typesamp == "Individual person") %>% 
  mutate_at(vars(contains("minv")), 
            as.numeric) %>% 
  mutate_at(vars(starts_with("minv")), 
            round) 


yr.mon.date <- map2_dfc(select(ESS9, starts_with("monv")),
                        select(ESS9, starts_with("date")),
                        function(x, y){
                          str_c("2018-", x, "-", y)
                        }) 

hr.min <- map2_dfc(select(ESS9, starts_with("hourv")),
                   select(ESS9, starts_with("minv")),
                   function(x, y){
                     str_c(x, "-", y)
                   })

yr.mon.date.hr.min <- map2_dfc(yr.mon.date,
                               hr.min,
                               function(x, y){
                                 str_c(x, "-", y)
                               })

# rename columns
colnames(yr.mon.date.hr.min) <- str_replace(colnames(yr.mon.date.hr.min),
                                            pattern = "monv",
                                            "contact")

ESS9 %>% filter(typesamp == "Individual person") %>% tabyl(cntry)

# Fix dates based on https://www.europeansocialsurvey.org/data/deviations_9.html
ESS9x <- bind_cols(ESS9, yr.mon.date.hr.min) %>% 
  mutate_at(vars(starts_with("contact")), ymd_hm) %>% 
  mutate(across(starts_with("contact"),  # Some countries conducted fieldwork in both 2016 and 2017, have to add a year
                ~ case_when(month(.) < 8 & cntry == "BE" ~ . + years(1), 
                            month(.) < 8 & cntry == "CH" ~ . + years(1), 
                            month(.) < 7 & cntry == "DE" ~ . + years(1), 
                            month(.) < 8 & cntry == "EE" ~ . + years(1), 
                            month(.) < 8 & cntry == "ES" ~ . + years(1), 
                            month(.) < 8 & cntry == "FI" ~ . + years(1), 
                            cntry == "HU" ~ . + years(1), 
                            month(.) < 8 & cntry == "IT" ~ . + years(1), 
                            month(.) < 7 & cntry == "NO" ~ . + years(1), 
                            month(.) < 8 & cntry == "PL" ~ . + years(1), 
                            month(.) < 7 & cntry == "SE" ~ . + years(1), 
                            month(.) < 8 & cntry == "SI" ~ . + years(1), 
                            TRUE ~ .))) %>% 
  mutate_at(vars(starts_with("resulb")), 
            to_factor) %>% 
  mutate(across(starts_with("resulb"), 
                .fns = list(result = 
                              ~case_when(. == "Completed interview" ~ TRUE, 
                                         . == "Complete interview" ~ TRUE, 
                                         TRUE ~ FALSE)), 
                .names = "{fn}{col}"))  %>%
  rename_all(~str_replace(., 
                          "resultresulb", paste0("result"))) %>% 
  select(-starts_with("resulb"), 
         -starts_with("outnic"), 
         -starts_with("modev"), 
         -starts_with("totcin"), 
         -starts_with("refvis"), 
         -starts_with("rersb"), 
         -starts_with("agea"),
         -starts_with("ageb"),  
         -starts_with("gendera"),  
         -starts_with("genderb"),  
         -starts_with("coop"),  
         -starts_with("inwyyspt"),  
         -starts_with("intnum"), 
         -starts_with("resulb"))  %>% 
  select(-starts_with("monv"),
         -starts_with("date"),
         -starts_with("hourv"),
         -starts_with("minv"),
         -starts_with("yearv"),
         -starts_with("dayv")) 


ESS9x$success <- ESS9x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(x) 


ESS9x$sum <- ESS9x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(sum) 

saveRDS(ESS9x, 
        here::here("Data/ESS9_contacts.Rdata"))


tabyl(ESS9x, success, result2)  

tabyl(ESS9x, interva, sum)  


contacts9 <- ESS9x %>% 
  pivot_longer(cols = starts_with("contact"),
               names_to = "attempt",
               values_to = "time") %>% 
  filter(!is.na(time)) %>% 
arrange(cntry, idno) 


saveRDS(contacts9, 
        here::here(
          "Data/contacts9.Rdata"))



# Read contact attempt data for ESS - Round 10


ESS10 <- read_sav(here::here("Data/ESS10CF.sav")) %>%
  mutate(typesamp = as_factor(typesamp),
         cntry = as.factor(cntry),
         typesamp2 = case_when(typesamp == "Individual person" ~ "Individual person",
                               typesamp == "Household" ~ "Household or Address",
                               typesamp == "Address" ~ "Household or Address"),
         telephone.present = telnum == 1,
         gndr_su = case_when(gndr_su == 1 ~ "Male",
                             gndr_su == 2 ~ "Female"),
         age_cat = case_when(age_su >= 14 & age_su <= 34~ "Under 35",
                             age_su >= 35 & age_su <= 50 ~ "35-49",
                             age_su >= 50 & age_su <= 64 ~ "50-64",
                             age_su >= 65  ~ "65+"),
         age50 = case_when(age_su < 34 ~ "Under 50",
                           age_su >= 35 & age_su < 50 ~ "Under 50",
                           age_su >= 50 & age_su < 64 ~ "50+",
                           age_su >= 65 ~ "50+"),
         age_cat = fct_relevel(age_cat, "Under 35", after = 0),
         housetype = case_when(type < 5 ~ "Single unit",
                               type >= 6 & type <= 8 ~ "Multi-unit",
                               type == 5  ~ "Other",
                               type >= 9 & type <= 10 ~ "Other"),
         housetype = fct_relevel(housetype, "Other", after = Inf),
         single.unit = case_when(type < 5 ~ "Single unit",
                                 type >= 5 & type <= 10 ~ "Other"),
         single.unit = fct_relevel(single.unit, "Other", after = Inf),
         multi.unit = case_when(housetype ==  "Multi-unit"  ~ "Multi-unit",
                                housetype != "Multi-unit"  ~"Not multi-unit"),
         physical.assesment = fct_lump(as.factor(physa)),
         physa.3 = fct_collapse(as.factor(physa),
                                Good = c("1", "2"),
                                Satisfactory = c("3"),
                                Bad = c("4", "5")),
         physa.2 = physa.3 == "Bad",
         no.access = case_when(access == 4 ~ "No barriers",
                               access != 4 ~ "Barriers"), 
         vandalism = vandaa != 4,
         litter = littera != 4,
         neighb.assessment = physa.2 + vandalism + litter,
         neighb = case_when(neighb.assessment == 0 ~ "None",
                            neighb.assessment >= 1 ~ "One or more"),
         neighb3 = case_when(neighb.assessment == 0 ~ "None",
                             neighb.assessment == 1 ~ "One",
                             neighb.assessment >= 2 ~ "Two or more"),
         interview = interva == 1,
         interview_fct = as.factor(interview),
         interview_fct2 = if_else(interview_fct == "TRUE", 
                                  "Interview", "No interview"),
         interva789 = interva == 7 |  interva == 8 | interva == 9,
         interview_na = ifelse(interva789 == T | is.na(interva), TRUE, FALSE)) %>% 
  filter(typesamp == "Individual person") %>% 
  mutate_at(vars(contains("minv")), 
            as.numeric) %>% 
  mutate_at(vars(starts_with("minv")), 
            round) 

ESS10 %>% filter(typesamp == "Individual person") %>% tabyl(cntry)

ESS10x <- ESS10 %>% 
  rename_all(~str_replace(., 
                          "dateca", paste0("contact"))) %>% 
mutate_at(vars(starts_with("resulb")), 
          to_factor) %>% 
  mutate(across(starts_with("resulb"), 
                .fns = list(result = 
                              ~case_when(. == "Completed interview" ~ TRUE, 
                                         . == "Complete interview" ~ TRUE, 
                                         TRUE ~ FALSE)), 
                .names = "{fn}{col}"))  %>%
  rename_all(~str_replace(., 
                          "resultresulb", paste0("result"))) %>% 
  select(-starts_with("resulb"), 
         -starts_with("outnic"), 
         -starts_with("modev"), 
         -starts_with("totcin"), 
         -starts_with("refvis"), 
         -starts_with("rersb"), 
         -starts_with("agea"),
         -starts_with("ageb"),  
         -starts_with("gendera"),  
         -starts_with("genderb"),  
         -starts_with("coop"),  
         -starts_with("inwyyspt"),  
         -starts_with("intnum"), 
         -starts_with("resulb"))  %>% 
  select(-starts_with("monv"),
         -starts_with("date"),
         -starts_with("hourv"),
         -starts_with("minv"),
         -starts_with("yearv"),
         -starts_with("dayv")) 


ESS10x$success <- ESS10x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(x) 


ESS10x$sum <- ESS10x %>% 
  select(starts_with("result")) %>% 
  mutate_if(is.logical, 
            as.numeric) %>% 
  mutate(across(starts_with("result"), 
                ~replace_na(.x, 0))) %>% 
  mutate(
    sum = rowSums(across(where(is.numeric))),
    x = max.col(.),
    x = if_else(sum != 0, 
                x, 
                NA_integer_),
  ) %>% 
  pull(sum) 

saveRDS(ESS10x, 
        here::here("Data/ESS10_contacts.Rdata"))


tabyl(ESS10x, success, result2)  

tabyl(ESS10x, interva, sum)  


contacts10 <- ESS10x%>% 
  pivot_longer(cols = starts_with("contact"),
               names_to = "attempt",
               values_to = "time") %>% 
  filter(!is.na(time)) %>% 
arrange(cntry, idno) 


saveRDS(contacts10, 
        here::here(
          "Data/contacts10.Rdata"))



contacts10x <- contacts10 %>% 
  select(essround, idno, cntry, typesamp, starts_with("result"), success, sum, 
         starts_with("attempt"), starts_with("status"),
         time, interview, multi.unit, neighb, gndr_su, age_cat, telnum, no.access) %>% 
  mutate(multi.unit = fct_relevel(as.factor(multi.unit), "Multi-unit", after = Inf),
         no.access = fct_relevel(no.access, "Barriers", after = Inf),
         gndr_su = as.factor(gndr_su),
         neighb = as.factor(neighb),
         round.x = as.character(essround),
         cntry = droplevels(cntry),
         cntry.round = as.factor(str_c(cntry, round.x, sep = "-")),
         cntry.round = droplevels(cntry.round)
  ) %>% 
  arrange(essround, cntry, idno)



saveRDS(contacts10x, 
        here::here(
          "Data/contacts10x.Rdata"))




# Combine contact data for all rounds

contacts <- bind_rows(contacts6, contacts7, contacts8, contacts9, contacts10x) %>%
  select(essround, idno, cntry, typesamp, starts_with("result"), success, sum,
         starts_with("attempt"), starts_with("status"),
         time, interview, multi.unit, neighb, gndr_su, age_cat, telnum, no.access) %>%
  mutate(attempt = readr::parse_number(attempt)) %>%
  mutate(multi.unit = fct_relevel(as.factor(multi.unit), "Multi-unit", after = Inf),
         no.access = fct_relevel(no.access, "Barriers", after = Inf),
         gndr_su = as.factor(gndr_su),
         neighb = as.factor(neighb),
         round.x = as.character(essround),
         cntry = droplevels(cntry),
         cntry.round = as.factor(str_c(cntry, round.x, sep = "-")),
         cntry.round = droplevels(cntry.round)
  ) %>%
  arrange(essround, cntry, idno) %>% 
  filter(!is.na(time))

glimpse(contacts)



saveRDS(contacts,
        here::here(
          "Data/contacts.Rdata"))



## Structure of the dataset

ESS10sample <- ESS10 |> filter(cntry == "EE" | cntry == "FI") |> 
  sample_n(size = 10)

ESS10sample |> 
  select(essround, cntry, idno, typesamp, age_su, gndr_su, 
         type, access, physa, littera, vandaa
  ) |>
  mutate(gndr_su = to_factor(gndr_su),
         typesamp = to_factor(typesamp),
         type = to_factor(type),
         access = to_factor(access),
         physa = to_factor(physa),
         littera = to_factor(littera),
         vandaa = to_factor(vandaa)
  ) |> 
  gt() |> 
  gt::gtsave(filename = "dataset_structure.rtf")




ESS10sample |> 
  select(essround, cntry, idno, 
         dateca1, dateca2, dateca3, 
         resulb1, resulb2, resulb3,
         outnic1, outnic2, outnic3,
  ) |>
  mutate(
    resulb1 = to_factor(resulb1),
    resulb2 = to_factor(resulb2),
    resulb3 = to_factor(resulb3),
    outnic1 = to_factor(outnic1),
    outnic2 = to_factor(outnic2),
    outnic3 = to_factor(outnic3),
  ) |> 
  filter(cntry == "EE" | cntry == "FI") |> 
  gt() |> 
  gt::gtsave(filename = "dataset_structure2.rtf")







