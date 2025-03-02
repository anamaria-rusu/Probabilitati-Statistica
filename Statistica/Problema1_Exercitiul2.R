# Numarul de puncte pe care il vom genera
N <- 1000

# Initializarea listelor pentru punctele din interiorul si exteriorul cercurlui
inside_points <- data.frame(x = numeric(), y = numeric())
outside_points <- data.frame(x = numeric(), y = numeric())

# Metoda acceptarii si respingerii
set.seed(123) 

for (i in 1:N) # Se vor genera N puncte 
{
  # Generam un punct (x, y) în patratul [-1, 1] x [-1, 1]
  # Cu functia runif generam numere aleatoare uniforme : in acest caz, un singur numar intre -1 si 1
  x <- runif(1, -1, 1)
  y <- runif(1, -1, 1)
  
  # Verificam daca punctul se afla în discul unitate
  if (x^2 + y^2 <= 1) 
  {
    inside_points <- rbind(inside_points, data.frame(x = x, y = y))
  } 
  else 
  {
    outside_points <- rbind(outside_points, data.frame(x = x, y = y))
  }
}

# Reprezentam grafic punctele folosind un plot
plot(
  inside_points$x, inside_points$y, 
  col = "blue", pch = 16, cex = 0.6, 
  xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
  xlab = "X", ylab = "Y", 
  main = "Simulare a punctelor pe discul unitate"
)
points(outside_points$x, outside_points$y, col = "red", pch = 16, cex = 0.6)
