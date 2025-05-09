library(readxl)
library(dplyr)

# Load the data
data <- read_excel("C:/Users/rishi/Desktop/Group5.xlsx")

set.seed(123)

#Grad student, 1. full time and 2. part time
data$fulltimegradstud<-ifelse(data$graduate==1 & data$full==1, 1, 0)
data$parttimegradstud<-ifelse(data$graduate==1 & data$full==0, 1, 0)

n1 = sum(data$fulltimegradstud)
n2 = sum(data$parttimegradstud)

ybar1 = sum(data$CSSAS*data$fulltimegradstud)/n1
Bhat1 = sum(data$CSSAS*data$fulltimegradstud)/sum(data$fulltimegradstud)
ybar2 = sum(data$CSSAS*data$parttimegradstud)/n2
Bhat2 = sum(data$CSSAS*data$parttimegradstud)/sum(data$parttimegradstud)
ssqy1 = sum((data$CSSAS[data$fulltimegradstud==1]-ybar1)^2)
ssqy2 = sum((data$CSSAS[data$parttimegradstud==0]-ybar2)^2)

###Computing the SE for Domains 1 and 2
SE1 = sqrt(1 - 5462/43300)*sqrt(5462/n1^2)*sqrt(ssqy1/(5462 -1))
SE2 = sqrt(1 - 5462/43300)*sqrt(5462/n2^2)*sqrt(ssqy2/(5462 -1))

###Computing CIs
print(c("CI for Domain 1 ",round(ybar1-qt(.975,n1-1)*SE1,1),
        round(ybar1+qt(.975,n1-1)*SE1,1)))  
print(c("CI for Domain 2 ",round(ybar2-qt(.975,n2-1)*SE2,1),
        round(ybar2+qt(.975,n2-1)*SE2,1)))

# FI
x = data$fulltimegradstud
y = data$FI
phat1 = sum(x*y)/sum(x)
##Using SE formula
ssqy = sum((y[x==1] - phat1)^2)
ssqy = (sum(x)-1)*var(y[x==1])
SE1 = sqrt(1-5462/43300)*sqrt(5462/sum(x)^2)*sqrt(ssqy/(5462-1))
phat1 - 1.96*SE1
phat1 + 1.96*SE1

# FI
x = data$parttimegradstud
y = data$FI
phat2 = sum(x*y)/sum(x)
##Using SE formula
ssqy = sum((y[x==1] - phat2)^2)
ssqy = (sum(x)-1)*var(y[x==1])
SE2 = sqrt(1-5462/43300)*sqrt(5462/sum(x)^2)*sqrt(ssqy/(5462-1))
phat2 - 1.96*SE2
phat2 + 1.96*SE2


#Grad student, 3. employed and 4. not employed
data$empgradstud<-ifelse(data$graduate==1 & data$employ==1, 1, 0)
data$unempgradstud<-ifelse(data$graduate==1 & data$employ==0, 1, 0)

n3 = sum(data$empgradstud)
n4 = sum(data$unempgradstud)

ybar3 = sum(data$CSSAS*data$empgradstud)/n3
Bhat3 = sum(data$CSSAS*data$empgradstud)/sum(data$empgradstud)
ybar4 = sum(data$CSSAS*data$unempgradstud)/n4
Bhat4 = sum(data$CSSAS*data$unempgradstud)/sum(data$unempgradstud)
ssqy3 = sum((data$CSSAS[data$empgradstud==1]-ybar3)^2)
ssqy4 = sum((data$CSSAS[data$unempgradstud==0]-ybar4)^2)

###Computing the SE for Domains 3 and 4
SE3 = sqrt(1 - 5462/43300)*sqrt(5462/n3^2)*sqrt(ssqy3/(5462 -1))
SE4 = sqrt(1 - 5462/43300)*sqrt(5462/n4^2)*sqrt(ssqy4/(5462 -1))

###Computing CIs
print(c("CI for Domain 3 ",round(ybar3-qt(.975,n1-1)*SE3,1),
        round(ybar3+qt(.975,n3-1)*SE3,1)))  
print(c("CI for Domain 4 ",round(ybar4-qt(.975,n4-1)*SE4,1),
        round(ybar4+qt(.975,n4-1)*SE4,1)))

# FI
x = data$empgradstud
y = data$FI
phat3 = sum(x*y)/sum(x)
##Using SE formula
ssqy = sum((y[x==1] - phat3)^2)
ssqy = (sum(x)-1)*var(y[x==1])
SE3 = sqrt(1-5462/43300)*sqrt(5462/sum(x)^2)*sqrt(ssqy/(5462-1))
phat3 - 1.96*SE3
phat3 + 1.96*SE3

# FI
x = data$unempgradstud
y = data$FI
phat4 = sum(x*y)/sum(x)
##Using SE formula
ssqy = sum((y[x==1] - phat4)^2)
ssqy = (sum(x)-1)*var(y[x==1])
SE4 = sqrt(1-5462/43300)*sqrt(5462/sum(x)^2)*sqrt(ssqy/(5462-1))
phat4 - 1.96*SE4
phat4 + 1.96*SE4