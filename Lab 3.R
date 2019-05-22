fat <- read.csv("Large.Bodyfat.csv", header = TRUE)
names(fat)
fat.cor <- fat
cor(fat.cor, use = "pairwise.complete.obs")
cor(fat$age, fat$thigh.circum..cm.)
#####1#####
cor(fat$neck.circum..cm., fat$abdomen.circum..cm.)
cor(fat$percent.fat, fat$chest.circum..cm.)
#####2####
summary(fat)
sd(fat$percent.fat)
sd(fat$chest.circum..cm.)
hist(fat$percent.fat)
hist(fat$chest.circum..cm.)
#The percent body fat has one possible outlier (45.1) which lies above 42.3. There is a slight skew to the left with a mean of 18.94 and a median of 19.00. The spread of fat percent is relatively normal. There is a bit of a fall off in the frequencies of fat percent below 5 and above 35 percent.#
#The chest circum has two high outliers (above the 1.5IQR + 3rd Quartile) including 128.3 and 136.2. The graph is slightly skewed to the right with the mean of 100.82 falling to the right of the median of 99.65. The spread is fairly close to a normal distribution, with the data centered around 100.#
par(mfrow = c(2, 1))
#The correlation coefficient for the fat percent and chest circumference is 0.7028852. This is a somewhat strong positive correlation since R can be between -1 and 1. Since R being positive indicates that as fat percent increases, so does chest circumference. As oppose to a negative R value, which would indicate that as one of these variables increases, the other would decrease.#
######3#######
model1 <- lm(percent.fat ~ chest.circum..cm., data= fat)
model1
summary(model1)
#Fatpercent= 0.6462*ChestCirumference-46.2164. As chest circumference increases by 1 cm, the percent of body fat increases by 0.6462 percent.####
#####4######
pairs(fat)
plot(fat$percent.fat, fat$chest.circum..cm.)
abline(lm(chest.circum..cm. ~ percent.fat, data = fat), col = "red")
#####5#####
#R squared is used to the variation in y as explained by the regresssion equation (or in other words, as x increases, how much does it explain how y moves). The R squared value of chest circumference and percent fat is 0.494. This means that the model created explains 49.4 percent of the variation in percent fat is explained by chest circumference.#
######6######
#The linear regression model is 0.6462*Chestcircumference-46.2164. So inputing 128.5 cm yields the following results: 0.6462*128.5-46.2164= 36.82 percent body fat.#
#####7######
x <- data.frame(chest.circum..cm. = 128.5)
predict(model1,newdata = x)
confint(model1)
predict(model1, newdata = x, interval = "confidence", se.fit = TRUE)
#The null hypothesis is that if the slope of the regression model is zero. The alternative hypothesis is that the regression model has a slope not equal to zero. The test statistic is found by the following calculation: 0.6462/.04136=15.62. The degrees of freedom is 250. This means that p-value < 0.001. Using are summary(model1) R states that the p value < 2e-16. The conclusion is that the null hypothesis can be rejected. There is sufficient evidence that chest circumference is valuable when explaining percent body fat.#
#t*SE=1.984*0.04136=0.0821. With 95 percent confidence it can be concluded that the slope of the regression line model lies between 0.564 and 0.728. This was found by adding or subtracting the margin of error from the slope of the calculated regression line.#

######This was me testing things out, mainly with the residuals###### 
names(model1)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))
resid <- model1$residuals
fitted <- model1$fitted.values
plot(fitted, resid, main = "Residual", xlab = "Fitted values")
abline(h = 0, col = "red", lty = 2)
plot(fat$percent.fat, resid, main = "Residual", xlab = "Percent Body Fat")
abline(h = 0, col = "red", lty = 2)
qqnorm(model1$residuals)
qqline(model1$residuals, col = "red")
x <- data.frame(percent.fat = 252)
predict(model1,newdata = fat)
model1 <- lm(percent.fat ~ chest.circum..cm., data = fat)
model1
confint(model1)
x <- data.frame(chest.circum..cm. = 128.5)
predict(model1,newdata = x)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))
resid <- model1$residuals
fitted <- model1$fitted.values
plot(fitted, resid, main = "Residual", xlab = "Fitted values")
abline(h = 0, col = "red", lty = 2)
plot(health$chest.circum..cm., resid, main = "Residual", xlab = "Chest Circumference")
abline(h = 0, col = "red", lty = 2)