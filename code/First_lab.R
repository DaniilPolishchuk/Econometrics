#Task 1 

# За початковими даними розрахувати парнi коефiцiєнти кореляцiї 
# мiж залежною (результативною) ознакою Y 
# i незалежними (факторними) ознаками X1–X5. 
# Перевiрити значущiсть отриманих коефiцiєнтiв. 
# Надати економiчного тлумачення результатам.

# y_3 
# x_2 x_3 x_7 x_12 x_14

#y - Рентабельнiсть (dependent)
#x_2 - Питома вага робiтникiв у складi промислово-виробничого персоналу
#X_3 - Питома вага покупних виробiв
#x_7 - Фондовiддача
#x_12 - Оборотнiсть нормованих оборотних коштiв
#x_14 - Невиробничi витрати
#X- independent 

#getting data
data = read.csv("D://Econometrics/data.csv")
data

Y_3 <- data$Y3
X_2 <- data$X2
X_3 <- data$X3
X_7 <- data$X7
X_12 <- data$X12
X_14 <- data$X14

#Task 1 - Solution 
n <- length(Y_3)

cor(x = X_2, y = Y_3)
cor(x = X_2, y = Y_3, method = "kendall")
cor(x = X_2, y = Y_3, method = "spearman")

cor.test(x = X_2, y = Y_3)

r_hat <- r/r_2

r <- n * sum(Y_3 * X_2) - sum(X_2) * sum(Y_3) 
r_2 <- sqrt( ( n*sum(X_2^2) - sum(X_2)^2 ) * (n * sum(Y_3^2) - sum(Y_3)^2))

t <- abs(r_hat) * sqrt( (n-2)/(1 - r_hat^2) )
qt(1 - 0.05/2, df = n -2)

p = (1 - 0.05)
pt(t, df = 51, lower.tail = T)
pt(t, df =  51)
# after estimated correlation between Y_3 and X_2 is 0.2891192.
# There is direct connection between study variables. If both x and Y is growing or drop 


# First of all lets specify hyp.
# H_0 : r = 0 
# H_1 : r != 0

#alpha = 0.05 
# df = n- 2 = 51


#computing t 
# t = 2.156836
# t table = 2.007584

# t > t table 
# so take H1 with p = 0.95??????
# Moderate evidence against H0
# confidence interval 0.0204213 0.5188652

cor(x = X_3,y = Y_3)

cor.test(x = X_3,y = Y_3)$p.value
pt(-0.1606165 , df = n -2)
#-0.1606165 
# after computing estimated correlation we get that r_hat  = -0.1606165 
# indirect connection betweeen studing vatiables. When one increse anther decrise.

# First of all lets specify hyp.
# H_0 : r = 0 
# H_1 : r != 0

#alpha = 0.05 
# df = n- 2 = 51

#computing t 
# t = -1.1621
# t table = 0.436515

# t < t table 
# no  evidence against H0
# so value ids not representive 
# confidence interval -0.4129813  0.1146549


cor.test(x = X_7, y = Y_3)
# after estimated correlation between Y_3 and X_7 is 0.3652772.
# There is direct connection between study variables. If both x and Y is growing or drop 

# First of all lets specify hyp.
# H_0 : r = 0 
# H_1 : r != 0

#alpha = 0.05 
# df = n- 2 = 51

pt(0.3652772 , df = n-2)
#computing t 
# t = 2.8022
# t table = 0.6417925

# t > t table 
#  strong evidence against H0
# p-value 0.007156
# confidence interval 0.1053887 0.5784586



cor.test(x = X_12, y = Y_3)
#-0.2026375 
# after computing estimated correlation we get that r_hat 
# indirect connection between studying variables. When one increase other decrease.

# First of all lets specify hyp.
# H_0 : r = 0 
# H_1 : r != 0

#alpha = 0.05 
# df = n- 2 = 51

pt(-0.2026375  , df = n-2)
#computing t 
# t = -1.4778
# t table = 0.4201123

# t < t table 
#  no evidence against H0
# p-value 0.1456
# confidence interval -0.4483732  0.0715767

cor.test(x = X_13, y = Y_3)
#-0.1452273 
# after computing estimated correlation we get that r_hat 
# indirect connection between studying variables. When one increase other decrease.

# First of all lets specify hyp.
# H_0 : r = 0 
# H_1 : r != 0

#alpha = 0.05 
# df = n- 2 = 51

pt(-0.1452273   , df = n-2)
#computing t 
# t = -1.0482
# t table = 0.4425519

# t < t table 
#  no evidence against H0
# p-value 0.2995
# confidence interval -0.3998263  0.1301764




#### TASK 2 ####


#create a data matrix 


x <- data[c("Y3","X2", "X3", "X7", "X12", "X13")]

x <-data[1:25, c(1, 3+1, 3+10, 3+11, 3+14)]
x <- data[, c(1,2+3,3+3,3+4,3+ 6, 3+14)]

R <- cor(x)

library(matlib)
Z <- solve(R)
Z <- inv(R)
solve(R) %*% R
inv(R) %*% R
Inverse(R) %*%R
S <- cov(x)
R_hat <- R * 25/24
Z <- round(solve(R), digits = 3)

# test 

data

r_hat <- matrix(rep(0,25), ncol = 5)
q <- c()

for(i in 1:5){
  for( j in 1:5){
    r_hat[i,j] = - Z[i,j]/sqrt(Z[i,i] * Z[j,j])
  }
}

for(j in 2:6){
  print(-Z[1,j]/sqrt(Z[1,1] * Z[j,j]))
}


-Z[1,2] / sqrt(Z[1,1] * Z[2,2])

i = 1
j = 1

R[2,1]

#  l = n
l = 5
M = R[c(1:i-1, i+1 : l), c(1:j-1, j+1 : l)]
A <- (-1)^(i + j) * M

A1 

A2 

-A

-A / (A %*% A)^0.5

# making tests 
# let's find t-statistic 
# later t values of out paramites
k = 4
alpha = 0.05
t_test <- qt(0.05, nrow(data) - k - 2 , lower.tail = F)

for(j in 2:6){
  r_hat <- -Z[1,j]/sqrt(Z[1,1] * Z[j,j])
  t_obs <- abs(r_hat) * sqrt((nrow(data) - k - 2)/(1 -r_hat^2))
  (t_obs)
}
 

#### NEW VERSION ####


#lets test the corr coef for theexaple given in the book

data[1:25,5:8]
cor(x = data[1:25,4], y = data[1:25,1])

# do the same tets for the significance level 

cor.test(x = data[1:25,4], y = data[1:25,1], conf.level = 0.8,method = "pearson")
cor.test(x = data[1:25,13], y = data[1:25,1])
cor.test(x = data[1:25,17], y = data[1:25,1])
#everething works so lets do the first task 

cor.test(x = X_3, y = Y_3)$p.value


