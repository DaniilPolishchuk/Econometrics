data = read.csv("D://Econometrics/data.csv")
data
nrow(data)

Y_3 <- data$Y3
X_2 <- data$X2
X_3 <- data$X3
X_7 <- data$X7
X_12 <- data$X12
X_13 <- data$X13

n <- length(Y_3)


#Test data 
#YaroBoy data
x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

ybar <- mean(y)


test_model <- lm(y ~ data[1:25,4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)

test_model <- lm(y ~ data$X14, data = data)

coef_model <- matrix(test_model$coefficients)
X = as.matrix(cbind(matrix( rep(1, nrow(x))),x))

yhat <- X %*% coef_model
ybar <- mean(y)
T = nrow(X)
N = ncol(X)

#R^2
R_2 <- sum((yhat - ybar)^2)/sum((y - ybar)^2)

#R_T

R_t <- 1 - (1 - R_2) * (T - 1)/(T - N)

#R_A^2
R_A <- 1 - (1 - R_2)*(T + N)/(T - N)

#F_st
F_stat <- (R_2 *(T - N))/((1 - R_2) * (N-1))

#F_kr 
alpha <- 0.05
F_cr <- qf(alpha, N-1, T-N, lower.tail = F)

#T/F
F_stat > F_cr



#TASK 2 

sigma = 1.280
Sigma_hat <- sigma * solve(t(X) %*% X)
diag(Sigma_hat)

#partion dertemination coeficient 

R = 0.7628
R_pair <- (1 - R)/(25 - 5) * (test_model$coefficients/sqrt(diag(Sigma_hat)))^2

# #test for the other models ####
# 
# test_model <- lm(y ~ data[1:25,4], data = data)
# 
# coef_model <- matrix(test_model$coefficients)
# x <- as.matrix(data[1:25,3+1])
# X = as.matrix(cbind(matrix( rep(1, nrow(x))),x))
# yhat <- X %*% coef_model
# 
# #R^2
# R <- sum((yhat - ybar)^2)/sum((y - ybar)^2)
# 
# #R_T
# 
# R_t <- 1 - (1 - R) * (nrow(X) - 1)/(nrow(X) - N)
# 
# #R_A^2
# R_A <- 1 - (1 - R)*(nrow(X) + nrow(coef_model))/(nrow(X) - nrow(coef_model))
# 
# cbind(R,R_t,R_A)

#####

#My data 
names <- c("X2", "X3", "X7", "X12", "X13")
names <- c("X1", "X10", "X11", "X14")
test_model<- lm(Y ~ as.matrix(x), data = data)

alpha <- 0.05

Y <- data$Y3
x <- data[1:25, names]

Y <- data[1:25, 1]

#sigma <- read.csv("models_characteristics.csv")[1,2]
result <- matrix(rep(0, 6*5), ncol = 6)

for(i in 0:length(names)){
  if (i == 0){
    x <- data[1:25, names]
    X <- as.matrix(x)
    #X = as.matrix(cbind(matrix( rep(1, nrow(x))),x))
    model = lm(Y ~ X)
    summary_model <- summary(model)
    #beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
    
    # extra data 
    # Y_hat = X %*% beta_hat
    # ybar = mean(Y)
    T = nrow(X)
    N = ncol(X) + 1 
    
    #R^2
    #R <- sum((Y_hat - ybar)^2)/sum((Y - ybar)^2)
    R <- summary_model$r.squared
    result[i+1,1] =  R
    #R_T
    R_t <- 1 - (1 - R) * (T - 1)/(T - N)
    result[i+1,2] = R_t
    
    #R_A^2
    R_A <- 1 - (1 - R)*(T + N)/(T - N)
    result[i+1,3] = R_A
    
    #F_st
    F_stat <- (R *(T - N))/((1 - R) * (N-1))
    result[i+1,4] = F_stat
    
    #F_kr 
    F_cr <- qf(alpha, N-1, T-N, lower.tail = F)
    result[i+1,5] = F_cr
    
    #T/F
    result[i+1,6] = F_stat > F_cr
    
    
    
  }
  else{
    x <- data[1:25, names[i]]
    X <- as.matrix(x)
    #X = as.matrix(cbind(matrix( rep(1, length(x))),x))
    model = lm(Y ~ X)
    summary_model <- summary(model)

    # beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

    # extra data
    # Y_hat = X %*% beta_hat
    # ybar = mean(Y)
    T = nrow(X)
    N = ncol(X) + 1

    #R^2
    # R <- sum((Y_hat - ybar)^2)/sum((Y - ybar)^2)
    R <- summary_model$r.squared
    result[i+1,1] =  R

    #R_T
    R_t <- 1 - (1 - R) * (T - 1)/(T - N)
    result[i+1,2] = R_t

    #R_A^2
    R_A <- 1 - (1 - R)*(T + N)/(T - N - 1)
    result[i+1,3] = R_A

    #F_st
    F_stat <- (R *(T - N))/((1 - R) * (N-1))
    result[i+1,4] = F_stat

    #F_kr
    F_cr <- qf(alpha, N-1, T-N, lower.tail = F)
    result[i+1,5] = F_cr

    #T/F
    result[i+1,6] = F_stat > F_cr


  }
  
}
result 

df4 <- data.frame(result)
rownames(df4) <- c("A", "B - X2", "c - X3", "D - X7", "E - X12", "F - X13")
colnames(df4) <- c("R^2", "R^2_T", "R^2_A", "f.stat", "f.test", "T/F")

read.csv("correlation.csv")

model_A <- lm(Y_3 ~ X_2 + X_3 + X_7 + X_12 + X_13, data = data )
model_B <- lm(Y_3 ~ X_2, data = data )
model_C <- lm(Y_3 ~ X_3, data = data )
model_D <- lm(Y_3 ~ X_7, data = data )
model_E <- lm(Y_3 ~ X_12, data = data )
model_F <- lm(Y_3 ~ X_13, data = data )


summary(model_A)
summary(model_B)
summary(model_C)
summary(model_D)
summary(model_E)
summary(model_F)


df4 <- data.frame(result)
colnames(df4) <- c("R^2", "R_T", "R_A", "f-stat", "f-test", "T/F")
rownames(df4) <- c("A","B","C","D","E","F")


read.csv("D:\\Econometrics/correlation.csv")


# back to teh test daat 

test_model <- lm(y ~ data[1:25, 4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)

test_model <- lm(y ~ data[1:25, 4], data = data)

sum_test_model <- summary(test_model)
sum_test_model$fstatistic[1]
sum_test_model$r.squared

cor(X,Y)



