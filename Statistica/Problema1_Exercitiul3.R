set.seed(123)
N <- 100000  # Numarul de puncte
suma <- 0  # Variabila pentru suma distantelor ce ne va ajuta la calculul mediei aritmetice
count <- 0  # Contor pentru punctele care sunt in interiorul discului unitate

# Generarea punctelor si calcularea distantei de la fiecare punct la origine
for (i in 1:N) 
{
  # Generam un punct (X, Y) în patratul [-1, 1] x [-1, 1]
  # Cu functia runif generam numere aleatoare uniforme : in acest caz, un singur numar intre -1 si 1
  X <- runif(1, -1, 1)
  Y <- runif(1, -1, 1)
  
  # Verificam dacă punctul se afla în interiorul discului
  if (X^2 + Y^2 <= 1) 
  {
    # Calculam distanța față de origine
    distanta <- sqrt(X^2 + Y^2)
    suma <- suma + distanta  # Adunam distanta la suma
    count <- count + 1  # Incrementăm contorul
  }
}

# Calculul mediei aritmetice a distantelor
media <- suma / count

# Afisarea rezultatului
cat("Media aritmetica a distantelor:", media, "\n")
