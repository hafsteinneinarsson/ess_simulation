ESS.survey.contact <- readRDS(here::here("Data", "ESS_survey_and_contact_data_analysed.Rdata")) %>% 
  filter(cntry.round %in% included.cntry.rounds)


ESS.targetvars <- ESS.survey.contact  %>%
  filter(essround > 5) %>% 
  filter(analysed == "Interviewer observations and Demographics") %>% 
  filter(cntry == "BE" | cntry == "CH" | cntry == "DK" | cntry == "NL" | cntry == "EE" | cntry == "FI" | cntry == "SI") %>% 
  filter(cntry.round != "EE-8") %>% 
  filter(cntry.round != "EE-9") %>% 
  filter(cntry.round != "CH-8") 



n.cntries <- ESS.targetvars %>% 
  ungroup() %>% 
  # mutate(cntry9 = droplevels(cntry.round)) %>% 
  summarise(unique(cntry.round)) %>% 
  nrow()

n.comparisons <- 8



### Income

inc.neighb <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = hinctnta.n ~ neighb, 
           var.equal = FALSE, data = .) %>% 
  mutate(comp = paste("Neighbourhood characteristics:\n", group1, "-", group2))


inc.multi <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = hinctnta.n ~ multi.unit, 
           var.equal = FALSE, data = .) %>% 
  mutate(comp = paste("Type of building:\n", group1, "-", group2))


inc.access <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = hinctnta.n ~ no.access, 
           var.equal = FALSE, data = .) %>% 
  mutate(comp = paste("Barriers to access:\n", group1, "-", group2))


### Education

edu.neighb <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = eduyrs.n ~ neighb, 
           var.equal = FALSE, data = .) %>% 
  mutate(comp = paste("Neighbourhood characteristics:\n", group1, "-", group2))


edu.multi <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = eduyrs.n ~ multi.unit, 
           var.equal = FALSE, data = .) %>% 
  mutate(comp = paste("Type of building:\n", group1, "-", group2))

edu.access <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = eduyrs.n ~ no.access, 
           var.equal = FALSE, data = .) %>% 
  mutate(comp = paste("Barriers to access:\n", group1, "-", group2))

# Happiness

happy.neighb <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = happy.n ~ neighb, 
           var.equal = FALSE, data = .)  %>% 
  mutate(comp = paste("Neighbourhood characteristics:\n", group1, "-", group2))


happy.multi <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = happy.n ~ multi.unit, 
           var.equal = FALSE, data = .)  %>% 
  mutate(comp = paste("Type of building:\n", group1, "-", group2))

happy.access <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = happy.n ~ no.access, 
           var.equal = FALSE, data = .)  %>% 
  mutate(comp = paste("Barriers to access:\n", group1, "-", group2))



# Trust

ppltrst.neighb <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = ppltrst.n ~ neighb, 
           var.equal = FALSE, data = .)  %>% 
  mutate(comp = paste("Neighbourhood characteristics:\n", group1, "-", group2))


ppltrst.multi <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = ppltrst.n ~ multi.unit, 
           var.equal = FALSE, data = .)  %>% 
  mutate(comp = paste("Type of building:\n", group1, "-", group2))

ppltrst.access <- ESS.targetvars %>% 
  group_by(cntry) %>%   
  cohens_d(formula = ppltrst.n ~ no.access, 
           var.equal = FALSE, data = .)  %>% 
  mutate(comp = paste("Barriers to access:\n", group1, "-", group2))






combined <- bind_rows(inc.multi, edu.multi, happy.multi, ppltrst.multi, 
                      inc.neighb, edu.neighb, happy.neighb, ppltrst.neighb, 
                      inc.access, edu.access, happy.access, ppltrst.access)


combined  %>%  
  mutate(y.lab = case_when(.y. == "hinctnta.n" ~ "HH income",
                           .y. == "happy.n" ~ "Happy",
                           .y. == "eduyrs.n" ~ "Education (yrs)",
                           .y. == "ppltrst.n" ~ "Trust",
                           TRUE ~ "Other"
                           
  )) %>% 
  ggplot(aes(x = fct_rev(cntry), y = effsize, fill = magnitude)) +
  geom_col() +
  coord_flip() +
  facet_grid(y.lab ~ comp) +
  theme_bw() +
  labs(x = "Country",
       y = "Cohen's d effect size",
       fill = "Magnitude") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(legend.position="bottom") 



ggsave(here("Figures/Appendix_figure5_cohens_adaptive.tiff"), height = 9.5, width = 7, dpi = 300, device = "tiff", compression = "lzw") 


