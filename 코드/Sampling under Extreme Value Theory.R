#install.packages('evd')
#library(evd)
install.packages('EnvStats')
library(EnvStats)


vec = c(0,2,110,20,10,14,60,0,0,1,4)
mean = mean(vec)
sd = sd(vec)
theta = (6)^(1/2)*sd/(pi)
epsilon = -digamma(1)
eta = mean - epsilon*theta


exval = revd(100, location = eta, scale = theta)
exval[exval>0]

#cdf
pevd(vec, location = eta, scale = theta) > 0.95


############################################################################################
Gangwon = read.csv('day_Kangwon.csv')[-1]
Gangwon
as.vector(Gangwon[1,3:12])
c(as.vector(Gangwon[1,3:12])['X12'], as.vector(Gangwon[1,3:12])['X13'])
c('X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19', 'X20', 'X21')[1]
as.vector(unlist(Gangwon[1,3:12]))


################
Gangwon = read.csv('day_Kangwon.csv')[-1]
Busan = read.csv('day_Busan.csv')[-1]
Seoul = read.csv('day_Seoul.csv')[-1]

epsilon = -digamma(1)
worst_case = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Gangwon[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  worst_case[i] = qevd(0.95, location = eta, scale = theta)
}
plot(worst_case)

barplot(worst_case)

#################
# adjusted sampling
#강원
epsilon = -digamma(1)
Gangwon_worst_case1 = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Gangwon[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  if (length(day[day>4.5])<2){
    Gangwon_worst_case1[i] = median(day)
  }else{
    Gangwon_worst_case1[i] = qevd(0.95, location = eta, scale = theta)
  }
}
barplot(Gangwon_worst_case1)

Gangwon_worst_case2 = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Gangwon[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  if (length(day[day>10])<2){
    Gangwon_worst_case2[i] = median(day)
  }else{
    Gangwon_worst_case2[i] = qevd(0.95, location = eta, scale = theta)
  }
}
barplot(Gangwon_worst_case2)


write.csv(Gangwon_worst_case1,'Gangwon_rain ver1.csv')
write.csv(Gangwon_worst_case2,'Gangwon_rain ver2.csv')


# 서울
epsilon = -digamma(1)
Seoul_worst_case1 = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Seoul[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  if (length(day[day>4.5])<2){
    Seoul_worst_case1[i] = median(day)
  }else{
    Seoul_worst_case1[i] = qevd(0.95, location = eta, scale = theta)
  }
}
barplot(Seoul_worst_case1)

Seoul_worst_case2 = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Seoul[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  if (length(day[day>10])<2){
    Seoul_worst_case2[i] = median(day)
  }else{
    Seoul_worst_case2[i] = qevd(0.95, location = eta, scale = theta)
  }
}
barplot(Seoul_worst_case2)


write.csv(Seoul_worst_case1,'Seoul_rain ver1.csv')
write.csv(Seoul_worst_case2,'Seoul_rain ver2.csv')


# 부산 
epsilon = -digamma(1)
Busan_worst_case1 = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Busan[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  if (length(day[day>4.5])<2){
    Busan_worst_case1[i] = median(day)
  }else{
    Busan_worst_case1[i] = qevd(0.95, location = eta, scale = theta)
  }
}
barplot(Busan_worst_case1)

Busan_worst_case2 = rep(NA, 366)
for (i in 1:366){
  day = as.vector(unlist(Busan[i,3:12]))
  daily_mean = mean(day)
  daily_sd = sd(day)
  theta = ((6)^(1/2))*daily_sd/(pi)
  eta - daily_mean - epsilon*theta
  if (length(day[day>10])<2){
    Busan_worst_case2[i] = median(day)
  }else{
    Busan_worst_case2[i] = qevd(0.95, location = eta, scale = theta)
  }
}
barplot(Busan_worst_case2)


write.csv(Busan_worst_case1,'Busan_rain ver1.csv')
write.csv(Busan_worst_case2,'Busan_rain ver2.csv')
