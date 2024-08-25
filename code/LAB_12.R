data = read.csv("D://Econometrics/data.csv")

# let's get equation 

#b_11 = 0
# b_23 = 0
# b_32 = 0

# gamma 
# 13 = 0
# 15 = 0 
# 21 = 0 
# 32 = 0 

# 0 y1   + b12 y2  + b13y3 + g11x1  + g12x2 + 0x3 + g14x4 +   0x5   + a1 + u1 = 0 
# b21y1  + b22 y2  + 0y3   + 0x1    + g22x2 + g23x3 + g24x4 + g25x5 + a2 + u2 = 0
# b31y1  + 0y2 y2  + b33y3 + g31x1  + 0x2   + g33x3 + g34x4 + g35x5 + a3 + u3 = 0

N = 5
M = 3 


# N-n >= m - 1

data = read.csv("data.csv")
head(data)


model_y2 = lm(Y2 ~ X1 + X2 + X3 + X4 + X5, data = data)
model_y1 = lm(Y1 ~ X1 + X2 + X3 + X4 + X5, data = data)
model_y3 = lm(Y3 ~ X1 + X2 + X3 + X4 + X5, data = data)

y2 <- model_y2$residuals
y1 <- model_y1$residuals
y3 <- model_y3$residuals


modely2res = lm(Y2 ~ y3 + X1 + X2 + X4, data = data)
modely1res = lm(Y1 ~ y2 + X2 + X3 + X4 + X5, data = data)
modely3res = lm(Y3 ~ y1 + X1 +X2 + X4 + X5, data = data)



# Yaroboy data test 

model_y3 = lm(Y3 ~ X1 + X2 + X3, data = data[1:10,])
model_y1 = lm(Y1 ~ X1 + X2 + X3, data = data[1:10,])

model_y1
model_y3

y3 <- model_y3$residuals
y1 <- model_y1$residuals



lm(Y1 ~ y3 + X1, data = data[1:10,])
lm(Y3 ~ y1 + X2 + X3, data = data[1:10,])


