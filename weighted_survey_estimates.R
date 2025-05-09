install.packages("readxl")
library(readxl)
project <- read_excel("C:/Users/saran/Downloads/Group5 (1).xlsx")
View(project)
View(table(project$Mi))


library(dplyr)
summary_table <- project %>%
  group_by(loc) %>%
  summarise(mi = n(), Mi_ = mean(Mi))
#we know what the Mi is for each cluster
#Final weights
library(dplyr)



summary_table <- summary_table %>%
  mutate(finalwt = (66/65) * (Mi_ / mi))
View(summary_table)

project <- project %>%
  left_join(summary_table %>% select(loc, mi, finalwt), by = "loc")
View(project)



library(survey)
stress_design <- svydesign(id = ~loc, weights = ~finalwt, data = project)
#CSSAS
svymean(~CSSAS, stress_design)
confint(svymean(~CSSAS, stress_design), df = degf(stress_design))
#FI
svymean(~FI, stress_design)
confint(svymean(~FI, stress_design), df = degf(stress_design))

#to compute without replacement variance
project$ID = 1:nrow(project)
proj_design2 <- svydesign(id = ~loc+ID, 
                          fpc = ~rep(66, nrow(project))+Mi, 
                          data = project)
#CSSAS
svymean(~CSSAS, proj_design2)
confint(svymean(~CSSAS, proj_design2))
#FI
svymean(~FI, proj_design2)
confint(svymean(~FI, proj_design2), df = degf(proj_design2))


undergrad <- project %>% filter(year %in% 1:5)
undergrad %>% 
  group_by(year) %>% 
  summarise(
    count = n(), 
    mean_CSSAS = mean(CSSAS), 
    sd_CSSAS = sd(CSSAS)
  )
pairwise.t.test(undergrad$CSSAS, undergrad$year, p.adjust.method = "bonferroni")


undergrad %>% 
  group_by(year) %>% 
  summarise(
    count = n(), 
    mean_fi = mean(FI), 
    sd_fi = sd(FI)
  )
pairwise.t.test(undergrad$FI, undergrad$year, p.adjust.method = "bonferroni")
