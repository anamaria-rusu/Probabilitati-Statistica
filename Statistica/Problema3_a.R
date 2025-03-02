# Exercitiul 3 subpunctul a

# Setam numarul de simulari
N <- 1e6

# 1. Generam distanta X la cea mai apropiata linie, uniform in [0, 0.5]
X <- runif(N, min = 0, max = 0.5)

# 2. Generam unghiul theta, uniform in [0, π]
theta <- runif(N, min = 0, max = pi)

# 3. Verificam conditia de intersectie: X <= 0.5 * sin(theta)
intersecteaza <- (X <= 0.5 * sin(theta))

# 4. Probabilitatea estimata (raportul dintre numarul de intersectii si numarul total de aruncari)
prob_estimata <- mean(intersecteaza)

# 5. Comparam cu valoarea teoretica 2 / π
prob_teoretica <- 2 / pi

cat("Probabilitatea estimata   =", prob_estimata, "\n")
cat("Probabilitatea teoretica =", prob_teoretica, "\n")
cat("Eroarea (absoluta)       =", abs(prob_estimata - prob_teoretica), "\n")
