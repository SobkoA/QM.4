# QM.4

#1a
x= 0:10
demand1A = function (x) 15-1.5*x
expenditure1A = function(x) demand1A(x)*x

plot(x, demand1A(x), type = "l", col=28, ylim = c(0,40), lwd=c(2),ylab = "liters, L", xlab = "price, €" ,main= "Beer consumption & expenditure (1A)")
lines(x, expenditure1A(x) , type = 'l' ,col=26, lwd=c(2))

names = c("Demand", "Expenditure")
legend( "topleft",legend = names, lwd = c(2), col = c(28,26), cex = .9, bty = "n")

#1b
x= 0:10
demand1B = function (x) 15*(.8)^x
expenditure1B = function(x) demand1B(x)*x
  
plot(x, demand1B(x), type = "l", col=34, ylim = c(0,30), lwd=c(2),ylab = "liters, L", xlab = "price, €" ,main= "Beer consumption & expenditure (1B)")
lines(x, expenditure1B(x) , type = 'l' ,col=36, lwd=c(2))

names = c("Demand", "Expenditure")
legend( "topleft",legend = names, lwd = c(2), col = c(34,36), cex = .9, bty = "n")

#2
pt = function(x){
  print(choose(x, k = 0:x))
}
pt(4)

#3
count_chars_in_table = function(x, t){
  m= strsplit(x, NULL)[[1]]
  v = t[match(m,t,nomatch = 0)]
  length(v)
}
count_chars_in_table("Wien", letters[1:13])

#4
no_of_odds = function(x) sum(x%%2 == 1)
no_of_odds(1:5)

#5a
x = seq(-2,21,.01)
f = function(x) 4-3*(x^3)

plot(x, f(x), type = "l", col=19, ylim = c(-3,9), xlim = c(-3,9),ylab = "", xlab = "" ,main= "The funciton and its inverse. f(x)")
mtext("X", side=1, line=2.5, col="red", cex = 1.3)
mtext("Y", side=2, line=2.5, col="blue", cex = 1.3)

nthroot <- function(a,b) ifelse(b%%2==1 | a>=0, sign(a)*abs(a)^(1/b), NaN)
f_In = function(x) nthroot((4-x)/3,3)
f_In(4)

lines(x, f_In(x), col = 6)
abline(0,1)
abline(v = 0)
abline(h = 0)

#5b
g = function(x) {
  ifelse((x>=1),1+x^2, NaN)
}

g_In =function(x){
  ifelse(x>=1, nthroot((x-1),2),NaN)
} 

plot(x, g(x), type = "l", col=19, ylim = c(-2,8), xlim = c(-2,8),ylab = "", xlab = "" ,main= "The funciton and its inverse. g(x)")
mtext("X", side=1, line=2.5, col="red", cex = 1.3)
mtext("Y", side=2, line=2.5, col="blue", cex = 1.3)

lines(x, g_In(x), col = 6)
abline(0,1)
abline(v = 0)
abline(h = 0)

#5c
h = function(x) {
  ifelse(x>=1, log(x^2), exp(x)-exp(1)) 
}
h_In =function(x){
  ifelse(x>=0, (exp(x))^.5,log(x + exp(1)))
} 

plot(x, h(x), type = "l", col=19, ylim = c(-2,8), xlim = c(-2,8),ylab = "", xlab = "" ,main= "The funciton and its inverse. h(x)")
mtext("X", side=1, line=2.5, col="red", cex = 1.3)
mtext("Y", side=2, line=2.5, col="blue", cex = 1.3)

lines(x, h_In(x), col = 6)


abline(0,1)
abline(v = 0)
abline(h = 0)

#6
"domaine = R
range = [5, -inf["

"f = f2 o f1
f1 = exp(x), R -> R+
f2 = −(x − 2)^2, R -> [0,-inf["
