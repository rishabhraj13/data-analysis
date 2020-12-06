library(MASS)
library(ISLR)
data(Boston)
attach(Boston)
auto <- data.frame(lstat,rm,medv)


set.seed(0111)
train <- sample(1:nrow(Boston),nrow(Boston)/2)
traindata <- auto[train, ]
testdata <- auto[-train, ]

x1 <- traindata$lstat
y1 <- traindata$rm
z1 <- traindata$medv

x2 <- testdata$lstat
y2 <- testdata$rm
z2 <- testdata$medv


#To calculate regions and find RSS for feature LSTAT
findRSS <- function(s){
  sum1 <- num1 <- sum2 <- num2 <- mean1 <-mean2 <- 0
  for (i in 1:length(x1)) {
    if(x1[i] < s)
    {
      sum1 = sum1 + z1[i]
      num1 = num1 + 1
    }
    if(x1[i] >= s)
    {
      sum2 = sum2 + z1[i]
      num2 = num2 + 1
    }
  }
  
  mean1 = sum1/num1
  mean2 = sum2/num2
  
  sumsquare1 <- sumsquare2 <- 0
  
  for(i in 1:length(x1)) {
    if(x1[i] < s)
    {sumsquare1 = sumsquare1 + ((z1[i] - mean1)**2)}
    if(x1[i] >= s)
    {sumsquare2 = sumsquare2 + ((z1[i] - mean2)**2)}
  }
  
  RSS <- sumsquare1 + sumsquare2
  return(RSS)
}

#to generate the sequence for s (threshold values)
rsses <- 0
s = seq(1.8 , 37.9 , by = 0.1)     
for (a in 1:length(s)) {
  rsses[a] = findRSS(s[a])  
}

#To find the minimum RSS and the value of s 
loc <- 0
minrss <- Inf
for (a in 1:length(s)){
  if(minrss > rsses[a])
  {minrss = rsses[a]
  loc = a
  }
}
min_s = s[loc]
print('minimum RSS for lstat for training set :')
print(minrss)
print('Best S for lstat :')
print(min_s)

# To find the Test MSE
sumt1 <- numt1 <- sumt2 <- numt2 <- meant1 <-meant2 <- 0
for (i in 1:length(x2)) {
  if(x2[i] < min_s)
  {
    sumt1 = sumt1 + z2[i]
    numt1 = numt1 + 1
  }
  if(x2[i] >= min_s)
  {
    sumt2 = sumt2 + z2[i]
    numt2 = numt2 + 1
  }
}

meant1 = sumt1/numt1
meant2 = sumt2/numt2

sumsquaret1 <- sumsquaret2 <- 0

for(i in 1:length(x2)) {
  if(x2[i] < min_s)
  {sumsquaret1 = sumsquaret1 + ((z2[i] - meant1)**2)}
  if(x2[i] >= min_s)
  {sumsquaret2 = sumsquaret2 + ((z2[i] - meant2)**2)}
}

RSS_test_lstat <- sumsquaret1 + sumsquaret2

MSE_test_lstat <- RSS_test_lstat/length(x2)
print('Test MSE for lstat :')
print(MSE_test_lstat)

#--------------------------------------------------------------------------

#To calculate regions and find RSS for feature RM
findRSS1 <- function(s1){
  sum1 <- num1 <- sum2 <- num2 <- mean1 <-mean2 <- 0
  for (i in 1:length(y1)) {
    if(y1[i] < s1)
    {
      sum1 = sum1 + z1[i]
      num1 = num1 + 1
    }
    if(y1[i] >= s1)
    {
      sum2 = sum2 + z1[i]
      num2 = num2 + 1
    }
  }
  
  mean1 = sum1/num1
  mean2 = sum2/num2
  
  sumsquare1 <- sumsquare2 <- 0
  
  for(i in 1:length(y1)) {
    if(y1[i] < s1)
    {sumsquare1 = sumsquare1 + ((z1[i] - mean1)**2)}
    if(y1[i] >= s1)
    {sumsquare2 = sumsquare2 + ((z1[i] - mean2)**2)}
  }
  
  RSS1 <- sumsquare1 + sumsquare2
  return(RSS1)
}

#to generate the sequence for s (threshold values)
s1 = seq(3.6 , 8.7 , by = 0.1)
rsses1 = 0
for (a in 1:length(s1)) {
  rsses1[a] = findRSS1(s1[a])  
}

#To find the minimum RSS and the value of s 
loc1 <- 0
minrss1 <- Inf
for (a in 1:length(s1)){
  if(minrss1 > rsses1[a])
  {minrss1 = rsses1[a]
  loc1 = a
  }
}
min_s1 = s1[loc1]
print('minimum RSS for RM for training set :')
print(minrss1)
print('Best S for RM :')
print(min_s1)

# To find the Test MSE
sumt1 <- numt1 <- sumt2 <- numt2 <- meant1 <-meant2 <- 0
for (i in 1:length(y2)) {
  if(y2[i] < min_s1)
  {
    sumt1 = sumt1 + z2[i]
    numt1 = numt1 + 1
  }
  if(y2[i] >= min_s1)
  {
    sumt2 = sumt2 + z2[i]
    numt2 = numt2 + 1
  }
}

meant1 = sumt1/numt1
meant2 = sumt2/numt2

sumsquaret1 <- sumsquaret2 <- 0

for(i in 1:length(y2)) {
  if(y2[i] < min_s1)
  {sumsquaret1 = sumsquaret1 + ((z2[i] - meant1)**2)}
  if(y2[i] >= min_s1)
  {sumsquaret2 = sumsquaret2 + ((z2[i] - meant2)**2)}
}

RSS_test_rm <- sumsquaret1 + sumsquaret2

MSE_test_rm <- RSS_test_rm/length(y2)
print('Test MSE for RM :')
print(MSE_test_rm)

#_______________________________________________________________________________#_______________________________________________________________________________

#CODE FOR BDS


#decision stump for RM is (rm,6.9)

r <- z1
b = 1:100
ita = 0.01
mean1 <-mean2 <- 0

for(k in 1:length(b)){
  sum1 <- num1 <- sum2 <- num2 <- 0
  
  for(i in 1:length(y1)) {
    if(y1[i] < 6.9)
    {
      sum1 = sum1 + r[i]
      num1 = num1 + 1
    }
    if(y1[i] >= 6.9)
    {
      sum2 = sum2 + r[i]
      num2 = num2 + 1
    }
  }
  
  mean1[k] = sum1/num1
  mean2[k] = sum2/num2
  
  for(i in 1:length(y1)) {
    if(y1[i] < 6.9){
      r[i] <- r[i] - (ita*mean1[k])
    }
    if(y1[i] >= 6.9){
      r[i] <- r[i] - (ita*mean2[k])
    }
  }
}

# To compute the MSE test

MSE <- 0
r2 <- z2
b = 1:1000
ita = 0.01
mean3 <-mean4 <- 0

for(k in 1:length(b)){
  sum1 <- num1 <- sum2 <- num2 <- 0
  
  for(i in 1:length(y2)) {
    if(y2[i] < 6.9)
    {
      sum1 = sum1 + r2[i]
      num1 = num1 + 1
    }
    if(y2[i] >= 6.9)
    {
      sum2 = sum2 + r2[i]
      num2 = num2 + 1
    }
  }
  
  mean3[k] = sum1/num1
  mean4[k] = sum2/num2
  
  for(i in 1:length(y2)) {
    if(y2[i] < 6.9){
      r2[i] <- r2[i] - (ita*mean3[k])
    }
    if(y2[i] >= 6.9){
      r2[i] <- r2[i] - (ita*mean4[k])
    }
  }
  
  
  ita_sum_mean3 <- (ita*sum(mean3))
  ita_sum_mean4 <- (ita*sum(mean4))
  
  
  ab <- 0
  x1 <- 0
  for (i in 1:length(y1)){
    if(y2[i] < 6.9){
      x1[i] <- ((z2[i] - ita_sum_mean3)**2)
    }
    if(y2[i] >= 6.9){
      x1[i] <- ((z2[i] - ita_sum_mean4)**2)
    }
  }
  
  TEST_MSE = sum(x1)/length(y1)
  
  MSE[k] = TEST_MSE
  
}  

print('The TEST MSE of the SYSTEM is : ')
print(TEST_MSE)


#Question 3

#Plot graph betweem MSE and Valur of B

plot(b,MSE,main = 'MSE vs B',xlab = 'B', ylab = 'MSE')


