data = read.csv("D://Econometrics/data.csv")
test_data_wacor = read.csv("test_data_without_autocor")

x <-data[1:25, c(3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

x_1 <-data[1:9, c(3+1, 3+10, 3+11, 3+14)]
y_1 <- data[1:9, 1]

lm(y_1 ~ as.matrix(x_1))

data_warcor = read.csv("data_without_autocor")


# TEsT to understand ####

test_model <- lm(y ~ data[1:25,4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)

# library(orcutt)
# test <- cochrane.orcutt(test_model)
# test$model
# 
# library(lmtest)
# 
# bptest(model, ~ poly(fitted(model) , 2))
# 
data_new <- test_data_wacor[order(test_data_wacor$x4, decreasing = F), ]
# model <- lm(data_new[1:9,1] ~ data_new[1:9, 3] + 
#                              data_new[1:9, 4] + 
#                              data_new[1:9, 5] +
#                              data_new[1:9, 6])

# build the manual model 
add_a_col <- function(X){
  return(as.matrix(cbind(matrix( rep(1, nrow(X))),X)))
}
manual_model <- function(Y,X, ...){
  additional_args <- list(...)
  t <- additional_args['T']
  # if (is.null(t)){
  #   return (0)
  # }
  # if( is.na(t)){
  #   return(1)
  # }
  #   
  
  #T = nrow(X)
  N = ncol(X)
  
  X <- as.matrix(cbind(matrix( rep(1, nrow(X))),X))

  beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
  beta_hat
  
  #return(beta_hat)
  return(beta_hat)
}

manual_model(test_data_wacor[,1], 
             test_data_wacor[,3:6], 
             Q = 13)

# do for x4
T = nrow(data_new)
N = ncol(data_new) - 1
m = round(4/15 * T, 0)
ndraw <- (T - m)/2
ndraw2 <- T - ndraw + 1

model_1 <- lm(data_new[1:ndraw,1] ~ 
                data_new[1:ndraw,3]+
                data_new[1:ndraw,4]+
                data_new[1:ndraw,5]+
                data_new[1:ndraw,6], data = data_new)
model_2 <- lm(data_new[ndraw2:T,1] ~ 
                data_new[ndraw2:T,3]+
                data_new[ndraw2:T,4]+
                data_new[ndraw2:T,5]+
                data_new[ndraw2:T,6], data = data_new)

u_1 <- summary(model_1)$residuals
u_2 <- summary(model_2)$residuals


GK_test <- function(data_new){
  T = nrow(data_new)
  N = ncol(data_new) - 1
  m = round(4/15 * T, 0)
  ndraw <- (T - m)/2
  ndraw2 <- T - ndraw + 1
  
  # find u_1 
  
  X_1<- as.matrix(data_new[1:ndraw,2:ncol(data_new)])
  Y_1 <- as.matrix(data_new[1:ndraw,1])
  
  beta_hat_1 = solve(t(X_1) %*% X_1) %*% t(X_1) %*% Y_1
  beta_hat_1
  
  Yhat_1 = X_1 %*% beta_hat_1
  u_1 <- Y_1 - Yhat_1
  
  # find  u_2 
  
  X_2<- as.matrix(data_new[ndraw2:T, 2:ncol(data_new)])
  Y_2 <- as.matrix(data_new[ndraw2:T, 1])
  
  beta_hat_2 = solve(t(X_2) %*% X_2) %*% t(X_2) %*% Y_2
  beta_hat_2
  
  Yhat_2 = X_2 %*% beta_hat_2
  u_2 <- Y_2 - Yhat_2
  
  # result 
  
  F.st <- (sum(u_2^2)/(ndraw - N))/(sum(u_1^2)/(ndraw - N))
  F.test <- qf(1- 0.1, ndraw - N, ndraw - N)
  
  return(cbind(F.st, F.test, "T/F" = F.st > F.test))
}

GK_test(data_new)

for (i in 1:(ncol(test_data_wacor) - 2)){
  data_sort <- test_data_wacor[order(test_data_wacor[,i+2], decreasing = F), ]
  print(GK_test(data_sort))
}

for (i in 1:(ncol(data_warcor) - 2)){
  data_sort <- data_warcor[order(data_warcor[,i+2], decreasing = F), ]
  print(GK_test(data_sort))
}





# Task 2 


x<- as.matrix(test_data_wacor[,2:6])
X <- as.matrix(test_data_wacor[,3:6])
Y <- as.matrix(test_data_wacor[,1])

beta_hat = solve(t(x) %*% x) %*% t(x) %*% Y
beta_hat

Yhat = x %*% beta_hat
U <- Y - Yhat

beta_hat_u = solve(t(x) %*% x) %*% t(x) %*% U^2
beta_hat_u
# ----
# everything ok


Uhat = x %*% beta_hat_u
epsilon <- U^2 - Uhat



(t(Uhat) %*% Uhat * (25 - 5))/ 
  ( t(epsilon) %*% epsilon * (5 -1))

qf(1 - 0.05, 5 - 1, 25 - 5)

T_H <- matrix(rep(0, 25 * 25), ncol = 25)
diag(T_H) <- 1/sqrt(abs(Uhat))
#------
#Everething ok

Y_new <- T_H %*% Y
X_new <- T_H %*% x

beta_hat_y = solve(t(X_new) %*% X_new) %*% t(X_new) %*% Y_new
beta_hat_y


for(i in 2:5){
  data <- cbind(Y_new, X_new)
  data_sort <- data[order(data[,i+1], decreasing = F), ]
  print(GK_test(data_sort))
}
# X <- add_a_col(x)
# b_hat <- manual_model(Y,x)
# Yhat = X %*% b_hat
# u <- Y - Yhat
# 
# 
# 
# manual_model(u^2,x)
# 
# u^2

# lm(u^2 ~ data_new[,3]+
#      data_new[,4]+
#      data_new[,5]+
#      data_new[,6])

# beta_hat_u= solve(t(X_1) %*% X_1) %*% t(X_1) %*% as.matrix(u^2)
# beta_hat_u
# 
# uhat_1 = X_1 %*% beta_hat_u
# u <- u - uhat_1


# T_H = matrix(rep(0, T*T), ncol  = T)
# diag(T_H) <- 0
  


#_____________________________________________________
#### yaroboy method ####
#step 1 
beta_hat
#step 2 

sigma <- sum(u^2)/(25 - 4)

# step 3

N = X %*% solve(t(X) %*% X) %*% t(X)
T_H = matrix(rep(0, 25 * 25), ncol = 25)

diag(T_H) = 1/ sqrt(u^2 * (1 - diag(N)))

X_1 = T_H %*% X
Y_1 = T_H %*% Y  



for(i in 1:4){
  data <- cbind(Y_1,X_1)
  data_sort <- data[order(data[,i+1], decreasing = F), ]
  print(GK_test(data_sort))
}  


#####

library(lmtest)

model <- lm(Y_fix ~ x2+x3+x4+x7+X13,data = data_warcor)
bptest(model)
library (olsrr)
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)
ols_test_score(model, rhs = TRUE)
ols_test_f(model, rhs = TRUE)

ols_test_f(model, vars = c('x2'))
ols_test_f(model, vars =   'x3')
ols_test_f(model, vars =   'x4')
ols_test_f(model,  vars =  'x7')
ols_test_f(model, vars = 'X13')






