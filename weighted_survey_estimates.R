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
    sd_CSSAS = (CSSAS - mean_CSSAS),
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



project$year1 = as.numeric(project$year == 1)
project$year2 = as.numeric(project$year == 2)
project$year3 = as.numeric(project$year == 3)
project$year4 = as.numeric(project$year == 4)
project$year5 = as.numeric(project$year == 5)
n1 = sum(project$year1)
n2 = sum(project$year2)
n3 = sum(project$year3)
n4 = sum(project$year4)
n5 = sum(project$year5)
ybar1 = sum(project$CSSAS*project$year1)/sum(project$year1)
ybar2 = sum(project$CSSAS*project$year2)/sum(project$year2)
ybar3 = sum(project$CSSAS*project$year3)/sum(project$year3)
ybar4 = sum(project$CSSAS*project$year4)/sum(project$year4)
ybar5 = sum(project$CSSAS*project$year5)/sum(project$year5)
ssqy1 = sum((project$CSSAS[project$year1] - ybar1)^2)
ssqy2 = sum((project$CSSAS[project$year2] - ybar2)^2)
ssqy3 = sum((project$CSSAS[project$year3] - ybar3)^2)
ssqy4 = sum((project$CSSAS[project$year4] - ybar4)^2)
ssqy5 = sum((project$CSSAS[project$year5] - ybar5)^2)
SE1 = sqrt(1-5462/43300)*sqrt(5462/n1^2)*sqrt(ssqy1/(5462-1))
SE2 = sqrt(1-5462/43300)*sqrt(5462/n2^2)*sqrt(ssqy2/(5462-1))
SE3 = sqrt(1-5462/43300)*sqrt(5462/n3^2)*sqrt(ssqy3/(5462-1))
SE4 = sqrt(1-5462/43300)*sqrt(5462/n4^2)*sqrt(ssqy4/(5462-1))
SE5 = sqrt(1-5462/43300)*sqrt(5462/n5^2)*sqrt(ssqy5/(5462-1))
ybar1
SE1
ybar2
SE2
ybar3
SE3
ybar4
SE4
ybar5
SE5
anova_res <- aov(CSSAS~as.factor(year), data = project)
anova_res


x1 = as.numeric(project$year1 == 1)
y = project$FI
phat1 = sum(x1*y)/sum(x1)
ssqy_1 = sum((y[x1 == 1] - phat1)^2)
ssqy_1 = (sum(x1) - 1)*var(y[x1 == 1])
SE_1 = sqrt(1-5462/43300)*sqrt(5462/sum(x1)^2)*sqrt(ssqy_1/(5462-1))
phat1
SE_1

x2 = as.numeric(project$year2 == 1)
y = project$FI
phat2 = sum(x2*y)/sum(x2)
ssqy_2 = sum((y[x2 == 1] - phat2)^2)
ssqy_2 = (sum(x2) - 1)*var(y[x2 == 1])
SE_2 = sqrt(1-5462/43300)*sqrt(5462/sum(x2)^2)*sqrt(ssqy_2/(5462-1))
phat2
SE_2

x3 = as.numeric(project$year3 == 1)
y = project$FI
phat3 = sum(x3*y)/sum(x3)
ssqy_3 = sum((y[x3 == 1] - phat3)^2)
ssqy_3 = (sum(x3) - 1)*var(y[x3 == 1])
SE_3 = sqrt(1-5462/43300)*sqrt(5462/sum(x3)^2)*sqrt(ssqy_3/(5462-1))
phat3
SE_3

x4 = as.numeric(project$year4 == 1)
y = project$FI
phat4 = sum(x4*y)/sum(x4)
ssqy_4 = sum((y[x4 == 1] - phat4)^2)
ssqy_4 = (sum(x4) - 1)*var(y[x4 == 1])
SE_4 = sqrt(1-5462/43300)*sqrt(5462/sum(x4)^2)*sqrt(ssqy_4/(5462-1))
phat4
SE_4

x5 = as.numeric(project$year5 == 1)
y = project$FI
phat5 = sum(x5*y)/sum(x5)
ssqy_5 = sum((y[x5 == 1] - phat5)^2)
ssqy_5 = (sum(x5) - 1)*var(y[x5 == 1])
SE_5 = sqrt(1-5462/43300)*sqrt(5462/sum(x5)^2)*sqrt(ssqy_5/(5462-1))
phat5
SE_5


table(project$year, project$FI) %>% chisq.test()
