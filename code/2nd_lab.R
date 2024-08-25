#Getting data ####
data = read.csv("D://Econometrics/data.csv")
data

Y_3 <- data$Y3
X_2 <- data$X2
X_3 <- data$X3
X_7 <- data$X7
X_12 <- data$X12
X_13 <- data$X13

n <- length(Y_3)

#Specify data ####
#my data
x <- data[c("Y3","X2", "X3", "X7", "X12", "X13")]

#YaroBoy data
x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

#Vova data
x <- data[, c(1,2+3,3+3,3+4,3+ 6, 3+14)]


x1 <- matrix( rep(1, nrow(x)))
X  <- cbind(matrix( rep(1, nrow(x))),x)
X <- as.matrix(cbind(matrix( rep(1, nrow(x))),x))
Y<- y


beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y


#2nd part 
#sigma^2 ####
#u_t <- y - y_hat

#test b_hat
b_hat = matrix(c(14.156, -19.767), ncol = 1)



x <- data[1:25, 4]
x1 <- matrix( rep(1, 25))
X  <- cbind(x1,x)
X <- as.matrix(X)
  


Y_hat = X %*% b_hat

#y_hat = sum(b_hat[1,1] * X_1) + sum(b_hat[2,1] * X_1)

u_t <- Y - Y_hat
u_t <- as.matrix(u_t)
T = nrow(X)
#sigma u ^2 
sum(u_t^2)/(T-2)

#### MCE ####

mce = sum(u_t^2)/T
mce

#### MAPE ####

# 100% / T * sum(abs(u_t) / y_t)

mape = sum(abs(u_t) / Y) / T * 100
mape

#### M(^u) #### ??????
sum(U_t_hat)/T

#test model 
test_model <- lm(y ~ data[1:25, 4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)
beta_hat <- as.matrix(test_model$coefficients)
summary(test_model)


##### FINAL ####
####Let's build the final table ####

#pick YaroBoy data to test the table 
#change the data later 
Model1 <- data[c("X2", "X3", "X7", "X12", "X13")]
Model2 <- data[c("X2")]
Model3 <- data[c("X3")]
Model4 <- data[c("X7")]
Model5 <- data[c("X12")]
Model6 <- data[c("X13")]


#YaroBoy data
x <- Model1
y <- Y_3

#for each model we need to change paramites
X = as.matrix(cbind(matrix( rep(1, nrow(x))),x))
Y = y

#Add to the data column of ones 

# the main model 
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
model <- lm(Y_3 ~ X_2 + X_3 + X_7 + X_12 + X_13, data = data )
beta_hat <- as.matrix(model$coefficients)
cbind(t(beta_hat))
#later work with the output

# extra data 
Y_hat = X %*% beta_hat
cor(Y_hat,Y)
U_t_hat <- as.matrix(Y - Y_hat)
T = nrow(X)
N = ncol(X)
#sigma^2
sigma = sum(U_t_hat^2)/(T-N)
#MCE 
mce = sum(U_t_hat^2)/T
#MAPE
mape = sum(abs(U_t_hat) / Y) / T * 100

cbind(sigma,mce,mape)


model_A <- lm(Y_3 ~ X_2 + X_3 + X_7 + X_12 + X_13, data = data )
model_B <- lm(Y_3 ~ X_2, data = data )
model_C <- lm(Y_3 ~ X_3, data = data )
model_D <- lm(Y_3 ~ X_7, data = data )
model_E <- lm(Y_3 ~ X_12, data = data )
model_F <- lm(Y_3 ~ X_13, data = data )

names_model <- c("model_A")

names <- c(1,2,3,7,12,13)
string_coef <- function(model, names = c(1,2,3,7,12,13), coef_cord = 2){
  coef <- round(model$coefficients,5)
  t <- ""
  if (length(coef) > 3){
    for (i in 1:length(coef)){
      q <- paste(coef[i],"x",names[i]," ", sep = "")
      t <- paste(t,q)
    }
  }
  else{
    temp_names <- c(1,names[coef_cord])
    for(i in 1:length(coef)){
      q <- paste(coef[i],"x",temp_names[i]," ", sep="")
      t <- paste(t,q)
    }
  }
  return(t)
}

string_coef(model_A, names = names)
string_coef(model_B, coef_cord = 2)

temp <- c(t, sigma, mce, mape)
temp_mat <- rbind(temp_mat, temp)
nrow(temp_mat)
#temp_mat <- matrix(data = c(t, sigma, mce, mape), ncol = 4)



df_result <- data.frame(data = temp_mat)
colnames(df_result) <- c("Model", "sigam^2", "MSE", "MAPE")
rownames(df_result) <- c("A","B","C","D","E","F")
