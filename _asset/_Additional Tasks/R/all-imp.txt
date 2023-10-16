# чертаене на точки

min<-1
max<-7

# cumulative function distribution (CDF)
plot((min-1):(max+1), c(0,cumsum(rep(1/max,max)),1), type="s", lwd=3, col="darkblue",
     main="Сumulative dustribution function",
     xlab="", ylab="Cumulative distribution")

points(min:max, cumsum(rep(1/max,max)), type="p", lwd=3, col="darkblue")
points(min:max, c(0, cumsum(rep(1/max,max-1))), type="p", lwd=1, col="darkblue")

# probability mass function (PMF)
plot(min:max, cumsum(rep(1/max,max)), type="h", lwd=3, col="darkblue",
     main="Probability mass function",
     xlab="", ylab="Probability")
points(min:max, cumsum(rep(1/max,max)), type="p", lwd=3, col="darkblue")

# SAMPLE
sample(x=c("H", "T"), size=10, replace=TRUE, prob=c(0.25, 0.75))
sample(1:6, size=10, replace=TRUE)

cards<-paste(rep(c(2:10, "J", "Q", "K", "A"),each=4),
             c("H", "D", "S", "C"),sep=":")
cards

dice<-as.vector(outer(1:6, 1:6, paste))
dice

sample(x=dice, size=5)



# БИНОМНО

n <- 10
p <- 0.5
set.seed(210)
x <- rbinom(n=500, size=n, p)
x

# barplot(prop.table(table(x)))

# теоретично срещу емпирично разпределение
hist(x, probability = TRUE, xlim = c(0, n), ylim = c(0, 0.3)) # може без xlim и ylim 
points(0:n, dbinom(x=0:n, size=n, prob=p), type = "h", lwd = 2, col = "blue")
points(0:n, dbinom(x=0:n, size=n, prob=p), type = "p", lwd = 3, col = "blue")
points(0:n, dbinom(x=0:n + 1, size=n, prob=p), type = "s", lwd = 3, col="green")

#!
n<-5
p<-0.25
x <- rbinom(n=500, size=n, p)
hist(x, probability = TRUE)
points(0:n, dbinom(x=0:n, size=n, prob=p), type = "h", lwd = 2, col = "blue")
points(0:n, dbinom(x=0:n, size=n, prob=p), type = "p", lwd = 3, col = "blue")
points(0:n, dbinom(x=0:n + 1, size=n, prob=p), type = "s", lwd = 3, col="green")
#!

rbinom(n=30, size=5, prob=0.25)

# да намерим Х такова че с 0,4 вероятност искаме да имаме
# число ПО-ГОЛЯМО от Х
qbinom(p = 0.4, size = 15, prob = 0.25, lower.tail = FALSE)

# да намерим Х такова че с 0,4 вероятност искаме да имаме
# число РАВНО на Х ИЛИ ПО-МАЛКО от Х
qbinom(p = 0.4, size = 15, prob = 0.25)

# вероятност да имаме повече от 0 успеха (НЕ 0 или повече)
pbinom(q=0, size=5, p=0.25, lower.tail = FALSE)
# вероятност да имаме 4 или по-малко успеха
pbinom(q=4, size=5, p=0.25)


# ГЕОМЕТРИЧНО РАЗПРЕДЕЛЕНИЕ

p <- 1/6
x <- rgeom(500, p)
hist(x, breaks = 30, probability = TRUE, ylim = c(0, 0.3))
points(0:100, dgeom(0:100,p), type = "h", lwd = 3, col = "darkblue")
points(0:100, dgeom(0:100,p), type = "p", lwd = 3, col = "darkblue")

rgeom(10, 0.2) # брой неуспехи до успех

# да свърлим 6за на 3/4/5 хвърляне
pgeom(q = 5, p) - pgeom(q = 2, p)

# квантилът Х, спрямо който имаме 0,5 вер. за успех на Х-тото хвърляне или по-рано
qgeom(p = 0.5, p) 


# ОТРИЦАТЕЛНО БИНОМНО РАЗПРЕДЕЛЕНИЕ

?rnbinom
# брой неуспехи до 5-ти успех
x<-rnbinom(n=100, size=5, prob=0.2)
hist(x, probability = TRUE)
points(0:100, dnbinom(0:100, n, p), type = "h", lwd = 3, col = "darkblue")
points(0:100, dnbinom(0:100, n, p), type = "p", lwd = 3, col = "darkblue")

p<-0.2
r<-5
# вероятност да имаме между 10 и 5 неуспеха преди r-ти успех
# q са неуспехите до 5-ти успех
pnbinom(q=10, size=r, p) - pnbinom(q=4, size=r, p)

#вероятност да имаме между 12 и 7 ОПИТА преди r-ти успех
# т.е. вер. да имаме между 12-r и 7-r неуспеха преди r-ти успех
pnbinom(q=12-r, size=r, p) - pnbinom(q=6-r, size=r, p)

a<-pnbinom(q=4, size=r, p, lower.tail = FALSE)
b<-pnbinom(q=4, size=r, p)
a+b # always 1

# ХИПЕРГЕОМЕТРИЧНО РАЗПРЕДЕЛЕНИЕ
?rhyper
rhyper(10, m = 10, n = 12, k = 15)
# m => маркиране
# n => немаркирани
# k => изтеглени

m <- 20; n <- 12; k <- 15
x <- rhyper(100, m, n, k)
hist(x, probability = TRUE)
points(0:100, dhyper(0:100, m, n, k), type = "h", lwd = 3, col = "darkblue")
points(0:100, dhyper(0:100, m, n, k), type = "p", lwd = 3, col = "darkblue")

# ПОАСАНОВО РАЗПРЕДЕЛЕНИЕ
?rpois
lmbd <- 20 
x <- rpois(n=100, lambda=lmbd) # lambda=EX=DX=n*p
x

hist(x, probability = TRUE)
points(0:100, dpois(0:100, lambda), type = "h", lwd = 3, col = "darkblue")
points(0:100, dpois(0:100, lambda), type = "p", lwd = 3, col = "darkblue")


# НЕПРЕКЪСНАТИ РАЗПРЕДЕЛЕНИЯ

# РАВНОМЕРНО разпределение

x <- runif(100, min = 0, max = 2)
x
hist(x, probability = TRUE, 
      main = "Continuous uniform on [0,2]",
      col = gray(.9))
curve(dunif(x, min = 0, max = 2),
      add = TRUE,
      col = 2)

dunif(x, min = 0, max = 2) # дава ни density-то
punif(q=0.2, min = 0, max = 2)# дава ни distribution function
?dunif

# install.packages("EnvStats")

library(EnvStats)
skewness(x) # close to 0 => симетрично
kurtosis(x) # negative => platykurtic

# ЕКСПОНЕНЦИАЛНО РАЗПРЕДЕЛЕНИЕ

?rexp

x <- rexp(n=100, rate=1/2) # rate=1/EX
x
summary(x)

hist(x, probability = TRUE, col = gray(.9), main = "Exponential distributed with mean = 2")
curve(dexp(x, rate=1/2), add = TRUE, col = "red")

skewness(x) # positive => дясна асиметрия
kurtosis(x) # positive => leptokurtic

pexp(1.5, rate = 2)
pexp(1.5, rate = 1/2)
pexp(1.5, rate = 1/2, lower.tail = FALSE)

pexp(2, rate = 1/2)

x <- rexp(n=100, rate=2) # rate=1/EX
hist(x, probability = TRUE, col = gray(.9), main = "Exponential distributed with mean = 1/2")
curve(dexp(x, rate=2), add = TRUE, col = "red")

qexp(0.6, rate = 2)
qexp(0.4, rate = 2, lower.tail = FALSE)


# ГАМА РАЗПРЕДЕЛЕНИЕ
?rgamma

x <- rgamma(n=100, shape = 5, rate = 3)
x
summary(x)

hist(x, probability = TRUE, col = gray(.9), main = "Gamma distributed with shape = 5 and rate = 3")
curve(dgamma(x, shape = 5, rate = 3), add = TRUE, col = "red")

skewness(x) # positive => дясна асиметрия
kurtosis(x) # positive => leptokurtic

# НОРМАЛНО РАЗПРЕДЕЛЕНИЕ

x <- rnorm(1000, mean = 100, sd = 16)
hist(x, probability = TRUE, col = gray(.9), main = "Normal distribution with mu = 100, sigma = 16")
curve(dnorm(x, mean = 100, sd = 16), add = TRUE, col = "red")

skewness(x) # 0 => симетрично
kurtosis(x) # 0 => mesokurtic

z.score <- (x - 100) / 16
z.score1 <- as.vector(scale(x))

summary(z.score)
summary(z.score1)
# almost the same -> gets better for bigger n

?scale

# ХИ КВАДРАТ РАЗПРЕДЕЛЕНИЕ
# Chi-SQuared РАЗПРЕДЕЛЕНИЕ

# rchisq(n=10000, df=n) = rgamma(n=10000, shape = n/2, rate = 1/2)

?rchisq

x <- rgamma(n=10000, shape = 5, rate = 1/2)
hist(x, probability = TRUE, col = gray(.9), main = "Gamma distributed with shape = 5 and rate = 3")
curve(dgamma(x, shape = 5, rate = 1/2), add = TRUE, col = "red")

x <- rchisq(10000, 10)
hist(x, probability = TRUE, col = gray(.9), main = "Chi-square with 10 degrees of freedom")
curve(dchisq(x, 10), add = TRUE, col = "red")

skewness(x) # positive => дясна асиметрия
kurtosis(x) # positive => leptokurtic

pchisq(0.7, df = 5, lower.tail = FALSE)


# t РАЗПРЕДЕЛЕНИЕ
# нормално разпределение със sample size=sz < 30
# fd = sz-1
?rt

skewness(x) # почти 0 => симетрично
kurtosis(x) # positive => leptokurtic

x <- rt(n=1000, fd=10)
hist(x, probability = TRUE, col = gray(.9), main = "Student t with 10 degrees of freedom")
curve(dt(x, df=10), add = TRUE, col = "red")
