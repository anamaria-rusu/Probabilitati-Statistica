# Exercitiul 3 subpunctul e

randomizedQuickSort <- function(A) {
  # Daca lungimea vectorului e 0 sau 1, e deja sortat
  if (length(A) < 2) {
    return(A)
  }
  
  # Alegem pivotul in mod aleator
  pivot_idx <- sample(1:length(A), 1)
  pivot <- A[pivot_idx]
  
  # Scoatem pivotul din vector si partitionam
  rest <- A[-pivot_idx]
  left <- rest[rest <= pivot]
  right <- rest[rest > pivot]
  
  # Apel recursiv
  left_sorted <- randomizedQuickSort(left)
  right_sorted <- randomizedQuickSort(right)
  
  # Recompunem rezultatul final
  return( c(left_sorted, pivot, right_sorted) )
}

# Exemplu 1: vector de mici dimensiuni
A <- c(3, 1, 4, 1, 5, 9, 2, 6, 5)
sorted_A <- randomizedQuickSort(A)
print(sorted_A)
print(sort(A))
# Verificam daca cele doua rezultate sunt egale, functia facuta de mine si sortarea generica:
identical(sorted_A, sort(A))

# Vector gol
print(randomizedQuickSort(c()))

# Vector cu un singur element
print(randomizedQuickSort(c(42)))

# Generam un vector de 20 de numere intregi aleatoare intre 1 si 100
set.seed(123)  # Pentru consistenta alegerii numerelor
random_vector <- sample(1:100, 20, replace = TRUE)
sorted_vector <- randomizedQuickSort(random_vector)
print(random_vector)
print(sorted_vector)
print(sort(random_vector))
