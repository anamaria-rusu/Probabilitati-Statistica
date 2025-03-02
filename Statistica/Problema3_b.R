#Exercitiul 3 subpunctul b

# Numarul de experimente:
N <- 1e6

# 1. Generam distanta X in [0, 0.5]
X <- runif(N, min = 0, max = 0.5)

# 2. Generam unghiul Theta in [0, 2*pi)
Theta <- runif(N, min = 0, max = 2*pi)

# 3. Indicatori de intersectie pentru fiecare ac:
#    Ac 1 intersecteaza daca X <= 0.5 * |sin(Theta)|
X1 <- (X <= 0.5 * abs(sin(Theta)))

#    Ac 2 intersecteaza daca X <= 0.5 * |cos(Theta)|
X2 <- (X <= 0.5 * abs(cos(Theta)))

# 4. Numarul total de intersectii Z = X1 + X2.
Z <- X1 + X2  # poate fi 0, 1, sau 2 pentru o "jumatate" de cruce
# (in total crucea poate intersecta pana la 4 linii,
#  dar aici liniile sunt doar orizontale, deci max 2).

# 5. Media lui Z/2
mean_Z_over_2 <- mean(Z/2)

# 6. Estimam pi cu formula pi_est = 2 / mean(Z/2).
pi_est <- 2 / mean_Z_over_2

# 7. Eventual, vedem varianta empirica a lui Z/2
var_Z_over_2 <- var(Z/2)

cat("Rezultate pentru crucea lui Buffon:\n")
cat("Valoarea medie (Z/2):", mean_Z_over_2, "\n")
cat("Estimare pentru pi   :", pi_est, "\n")
cat("Var(Z/2) empiric     :", var_Z_over_2, "\n")
