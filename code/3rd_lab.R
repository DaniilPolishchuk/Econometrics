
# getting data
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

x2 <- data[c("X2", "X3", "X7", "X12", "X13")]


#Specify data ####
#my data
x <- data[c("X2", "X3", "X7", "X12", "X13")]

#YaroBoy data
x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]
#Vova data
x <- data[, c(2+3,3+3,3+4,3+ 6, 3+14)]


X_list  <- cbind(matrix( rep(1, nrow(x))),x)
X  <- as.matrix(cbind(matrix( rep(1, nrow(x))),x))
Y<- y

#task 1 ####
test_model <- lm(y ~ data[1:25, 4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)
model_A <- lm(Y_3 ~ X_2 + X_3 + X_7 + X_12 + X_13, data = data )
# beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

beta_hat = as.matrix(test_model$coefficients)

sd_x <- sapply(X_list, sd)
sd_y <- sd(y)

beta_hat_standardized <- beta_hat * sd_x/sd_y



#task 2###

xbar <- sapply(x, mean)
ybar <- mean(y)

epsilon_hat <- beta_hat[2:length(beta_hat)] * xbar/ybar
epsilon_hat

#task 3 ####

#get sigma_u from the pref result

#now lets count it ####
Y_hat = X %*% beta_hat
U_t_hat <- as.matrix(Y - Y_hat)
T = nrow(X)

#sigma^2
sigma = sum(U_t_hat^2)/(T-2)
sigma = 1.280
#####

var(beta_hat)


Sigma_hat <- sigma * solve(t(X) %*% X)
Sigma_hat
R <- matrix(rep(0, ncol(Sigma_hat) * nrow(Sigma_hat)), ncol = ncol(Sigma_hat))
for( i in 1:ncol(Sigma_hat)){
  for( k in 1:ncol(Sigma_hat)){
    R[i,k] <- Sigma_hat[i,k] / sqrt(Sigma_hat[i,i] * Sigma_hat[k,k])
  }
}
R

mean(beta_hat)

3.617 / sqrt(2.873 * 10.235)
# x1 <- matrix( rep(1, n))
#X  <- cbind(x1,x2)
#X <- as.matrix(X)




# standardized the data 
Z <- scale(X)

# cheaking the data or mean and variance 0, 1 respectiely 
# colMeans(scaled.dat)  
# apply(scaled.dat, 2, sd)

# as Iget problems with the model A lets take 
#the cooeficients for the task to test my functions. 

# later I should fix model A

# get data as a vector 
X <- data[c("X2", "X3", "X7", "X12", "X13")]
#take vector data from the example 
test <- c(10.439,-14.755, 0.00003, 0.198, -0.00009)
Y_1 = data$Y1
X_1 = data$X1
X_10 = data$X10
X_11 = data$X11
X_14 = data$X14

mean(X_1)
mean(X_10)
mean(X_11)
mean(X_14)
X_mean <- sapply(X, FUN = mean)

sd(Y_1)
sd(X_1)
sd(X_10)
sd(X_11)
sd(X_14)
X_sd <- sapply(X, FUN = sd)
X_sd_test <- c(0, .086,20719.923, 2.163, 4.340)

# now lets compute beta hat standardised 
# so first of all get our beta_hat vector 

x2 <- data[c("X2", "X3", "X7", "X12", "X13")]
x1 <- matrix( rep(1, n))
X  <- cbind(x1,x2)
X <- as.matrix(X)


#beta_hat = (t(X) %*% X)^(-1) %*% t(X) %*% Y_3
beta_hat = test
# than use th eformula from the lecture and get out data 
# beta_hat_standardised <- beta_hat * X_sd / sd(Y_3)

beta_hat_standardised_test <- beta_hat * X_sd_test / sd(Y_1)


#TASK 2 ####

# take mean value 

X_mean_test <- c(0.308, 29587.28, 6.301, 19.550, 8.068)
Y_mean_test <- 8.068
#Y_mean = mean(Y_3)

eps_hat <- beta_hat * X_mean_test / Y_mean_test
# we should get rig of the first one as it is useless 


#TASK 3 ####
x2 <- X_1
x1 <- matrix( rep(1, n))
X  <- cbind(x1,x2)
X <- as.matrix(X)

beta_hat_t
Y_hat = X %*% b_hat
sum(Y_hat)


u_t <- Y_1 - Y_hat
u_t <- as.matrix(u_t)

# variation of remainders 
var_rem <- sum(u_t^2)/(n-1)
var_rem_test = 1.280
#Sigma 
#Sigma <- var_rem * (T(X) * X)%^%(-1)
library(Matrix)
library(expm)
X <- as.matrix(X)
var_rem_test * 1
q <-  (t(X) %*% X)
for (i in 1:5){
  for (j in 1:5){
    q[i][j] <- q[i][j] ^ (-1)
  }
}
var_rem_test * q
2.873/1.280
1/28

# get problem with matrix sigma 

x <- data[, c(2+3,3+3,3+4,3+ 6, 3+14)]
y <- data$Y1



model_A <- lm(data$Y1 ~ 
                data$X2 + 
                data$X3 + 
                data$X4 + 
                data$X7 +
                data$X14)

beta_hat = as.matrix(model_A$coefficients)

X_list  <- cbind(matrix( rep(1, nrow(x))),x)
X_list <- matrix(x)


xbar <- sapply(x, mean)
ybar <- mean(y)

typeof(x)
typeof(y)

(y - ybar)/sd_y

plot((y - ybar)/sd_y)

sd_x <- sapply(X_list, sd)
sd_y <- sd(data$Y1)

summary(x)

x_scale <- sapply(X_list, scale)

lm(y_stand ~ x_stand)$coefficients * 10

beta_hat_standardized <- beta_hat * sd_x/sd_y
cbind.data.frame("beta_hat"  = beta_hat)
cbind.data.frame("standardized beta hat" = beta_hat_standardized)







