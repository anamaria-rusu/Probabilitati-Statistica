# Exercitiul 3 subpunctul c2

# Numarul de simulari
N <- 1e6

# Parametrii
d <- 2      # diametrul cercului (si distanta maxima pana la linie)
L <- 1      # lungimea acului (L < d)

# 1. Generam unghiul alpha uniform in [0, 2*pi)
alpha <- runif(N, 0, 2*pi)

# 2. Generam distanta rho uniforma in [0, d/2]
rho <- runif(N, 0, d/2)

# 3. Conditia de intersectie: rho <= (L/2)*abs(sin(alpha))
intersecteaza <- (rho <= (L/2) * abs(sin(alpha)))

# 4. Probabilitatea estimata (frecventa empirica):
prob_est <- mean(intersecteaza)

# 5. Valoarea teoretica:
prob_theo <- (2 * L) / (pi * d)

cat("Probabilitatea estimata  :", prob_est, "\n")
cat("Probabilitatea teoretica :", prob_theo, "\n")
cat("Eroarea absoluta         :", abs(prob_est - prob_theo), "\n")
