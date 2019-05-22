data <- read.csv("Large.FEV.csv")
str(data)
age.group <- (data$age)
age.group <- sample(0:3, 654, replace = TRUE)
is.factor(age.group)
is.numeric(age.group)
f.age.group <- factor(age.group, labels = c("pre", "ele", "middle", "high"))
is.factor(f.age.group)
t <- table(data$age.group)
age.group <- factor (data$age,levels = c(3:5, 6:10, 11:13, 14:40), labels = c("preschool", "elementary", "middle school", "high school"))
age.table <- table(data$age)
age.table
as.factor <- (data$age)
age.group <- factor(data$age, levels(3:5, 6:10, 11:13, 14:19), labels ("preschool", "elementary", "middle school", "high school"))

age.group <- rep(NA, length(data$age))
age.group <- ifelse(data$age >= 3 && data$age <= 5, "pre", age.group)
?bin
data$age
sort(data$age)
sum(data$age >=3 & data$age <= 5)
rep("pre", 39)
as.factor(data$age)
elementary <- grouping(data$age, levels(3, 4, 5))
mean.pre <- mean(data$age[data$age.group == ""])

#####I think the actual progress started here######
dat <- read.csv("Large.FEV.csv")
dat <- dat[order(dat$age),]
sort(dat$age)
age.group <- dat[order(dat$age),]
t <- table(data$age)
t
model <- aov(fev ~ age, data = dat)
model
model <- aov(log.fev ~ age, data = dat)
model
summary(model)
age.group <- c(rep("pre", times = 39), rep("elementary", times = 351), rep("middle", times = 190), rep("high", times = 74))
model <- aov(fev ~ age, data = dat)
summary(model)
table(data$age)
?order
boxplot(fev ~ age.group, data = dat, main = "FEV Distribution in Different Age Groups", names = c("pre", "elementary", "middle", "high"))
mean.pre <- mean(dat$logfev[age.group == "pre"])
mean.pre
mean.ele <- mean(dat$logfev[age.group == "elementary"])
mean.ele
mean.middle <- mean(dat$logfev[age.group == "middle"])
mean.middle
mean.high <- mean(dat$logfev[age.group == "high"]) 
mean.high
s.pre <- sd(dat$logfev[age.group == "pre"])
s.ele <- sd(dat$logfev[age.group == "elementary"])
s.middle <- sd(dat$logfev[age.group == "middle"])
s.high <- sd(dat$logfev[age.group == "high"])
mean.pre
s.pre
mean.ele
s.ele
mean.middle
s.middle
mean.high
s.high
s.vec <- c(s.pre, s.ele, s.middle, s.high)
s.ratio <- max(s.vec)/min(s.vec)
s.ratio
dat$logfev <- log(dat$fev)
s.log.pre <- sd(dat$logfev[age.group == "pre"])
s.log.ele <- sd(dat$logfev[age.group == "elementary"])
s.log.middle <- sd(dat$logfev[age.group == "middle"])
s.log.high <- sd(dat$logfev[age.group == "high"])
s.log.vec <- c(s.log.pre, s.log.ele, s.log.middle, s.log.high)
s.log.ratio <- max(s.log.vec)/min(s.log.vec)
s.log.ratio
model <- aov(logfev ~ age.group, data = dat)
summary(model)
table(age.group)
MSE <- 0.2324
s.p <- sqrt(MSE)
N <- length(dat$logfev)
k <- 4
tstar <- abs(qt(0.25, N-k))
pre.lower <- mean.pre - (tstar * sqrt(MSE/39))
pre.upper <- mean.pre + (tstar * sqrt(MSE/39))
elementary.lower <- mean.ele - (tstar * sqrt(MSE/351))
elementary.upper <- mean.ele + (tstar * sqrt(MSE/351))
middle.lower <- mean.middle - (tstar * sqrt(MSE/190))
middle.upper <- mean.middle + (tstar * sqrt(MSE/190))
high.lower <- mean.high - (tstar * sqrt(MSE/74))
high.upper <- mean.high + (tstar * sqrt(MSE/74))
pre.lower
pre.upper
elementary.lower
elementary.upper
middle.lower
middle.upper
high.lower
high.upper

pre.var <- sqrt(MSE/39)
elementary.var <- sqrt(MSE/351)
middle.var <- sqrt(MSE/190)
high.var <- sqrt(MSE/74)
pre.var
elementary.var
middle.var
high.var
mes2()
s.p
