# Numarul de puncte pe care il vom genera
N <- 1000

# Generam variabilele aleatoare R și θ
set.seed(123) 
theta <- runif(N, 0, 2 * pi) # θ uniform în [0, 2π]
r <- sqrt(runif(N, 0, 1)) # R transformare pentru uniformitate

# Schimbarea la coordonate polare
x <- r * cos(theta)
y <- r * sin(theta)

# Reprezentam grafic punctele
plot(
  x, y, col = "blue", pch = 16, cex = 0.6,
  xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
  xlab = "X", ylab = "Y",
  main = "Simulare a punctelor pe discul unitat (coordonate polare)"
)

# Adaugam conturul cercului
theta_circle <- seq(0, 2 * pi, length.out = 500)
lines(cos(theta_circle), sin(theta_circle), col = "black", lwd = 2)
