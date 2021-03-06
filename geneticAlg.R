# TSP with 10 cities around a circular pattern
require(gaoptim)

op <- par(mfrow = c(2, 1))
n = 30
R = 10
angs = seq(0, 2*pi, length = n)
xp = R * cos(angs) + .1*rnorm(n)
yp = R * 2* sin(2*angs) # + rnorm(n)
xp = c(xp, xp[1])
yp = c(yp, yp[1])

base.M = matrix(c(xp, yp), ncol = 2)
dist.FUN = function(p) {
  p = c(p, p[1])
  M.diff = diff(base.M[p, ])
  dists = apply(M.diff, 1, function(x)sqrt(x[1]^2 + x[2]^2))
  1/sum(dists)
}

ga1 = GAPerm(dist.FUN, n, popSize = 300, mutRate = 0.3)
ga1$evolve(200)
plot(ga1)
plot(xp, yp, type = 'n', xlab = '', ylab = '', main = 'Best Tour')
res = ga1$bestIndividual()
res = c(res, res[1])

i = 1:n
xi = base.M[res[i], 1]
yi = base.M[res[i], 2]
xf = base.M[res[i + 1], 1]
yf = base.M[res[i + 1], 2]

arrows(xi, yi, xf, yf, col = 'red', angle = 10)
text(base.M[res, 1], base.M[res, 2], 1:n, cex = 0.9, col = 'gray20')
par(op)



fitness.FUN = function(x) sum(x)
lb = c(0, 0, 0, 0, 0)
ub = c(10, 10, 10, 10, 10)

ga1 = GAReal(fitness.FUN, lb, ub)
ga1$evolve(200)
plot(ga1)