#Exercitiul 3 subpunctul c1

# Setam numarul de simulari
N <- 1e6

# Parametrii:
d <- 2    # distanta dintre linii
L <- 1    # lungimea acului, L < d

# 1. Generam X (distanta mijlocului acului fata de linia cea mai apropiata):
X <- runif(N, min = 0, max = d/2)

# 2. Generam unghiul Theta in [0, pi]:
Theta <- runif(N, min = 0, max = pi)

# 3. Verificam intersectia: X <= (L/2) * sin(Theta)
intersecteaza <- (X <= (L/2)*sin(Theta))

# 4. Probabilitatea estimata (frecventa empirica de intersectie):
prob_estimata <- mean(intersecteaza)

# 5. Probabilitatea teoretica:
prob_teoretica <- (2 * L) / (pi * d)

cat("Rezultate Buffon generalizat:\n")
cat(" - Probabilitatea estimata :", prob_estimata, "\n")
cat(" - Probabilitatea teoretica:", prob_teoretica, "\n")
