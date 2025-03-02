# Exercitiul 3 subpunctul d

# Parametri
d1 <- 2      # distanta intre linii orizontale
d2 <- 3      # distanta intre linii verticale
L  <- 1      # lungimea acului (trebuie L < min(d1,d2))

# Numarul de simulari
N <- 1e6

# 1. Generam X, Y, Theta
X <- runif(N, min = 0, max = d2/2)
Y <- runif(N, min = 0, max = d1/2)
Theta <- runif(N, min = 0, max = pi)

# 2. Conditie: intersecteaza verticala?
intersect_vertical <- (X <= (L/2)*abs(cos(Theta)))

# 3. Conditie: intersecteaza orizontala?
intersect_horizontal <- (Y <= (L/2)*abs(sin(Theta)))

# 4. Acul intersecteaza grilajul (vertical OR horizontal)
intersect_grid <- (intersect_vertical | intersect_horizontal)

# 5. Estimam probabilitatea (frecventa empirica)
prob_est <- mean(intersect_grid)

# 6. Probabilitatea teoretica
prob_theo <- (L * (2*(d1 + d2) - L)) / (pi * d1 * d2)

# Afisam rezultatele
cat("Probabilitate estimata  :", prob_est, "\n")
cat("Probabilitate teoretica :", prob_theo, "\n")
cat("Eroare absoluta         :", abs(prob_est - prob_theo), "\n")
