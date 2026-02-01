library(ggplot2)

set.seed(123)
N <- 1000

#a 

# distanta centrului acului fata de cea mai apropiata linie (x in [0, 0.5])
x <- runif(N, 0, 0.5)
# unghiul format de ac cu liniile (theta in [0, pi/2])
theta <- runif(N, 0, pi/2)

# Conditia de intersectie: (1/2)*sin(theta) >= x
intersecteaza_a <- (0.5 * sin(theta)) >= x
prob_a <- sum(intersecteaza_a) / N
pi_estimat_a <- 2 / prob_a

cat("Probabilitate estimata (Empiric):", round(prob_a, 4), "\n")
cat("Probabilitate teoretica (2/pi):", round(2/pi, 4), "\n")


#b 
N <- 10001 

# distanta centrului acului fata de cea mai apropiata linie (x in [0, 0.5])
x <- runif(N, 0, 0.5)
# unghiul format de ac cu liniile (theta in [0, pi/2])
theta <- runif(N, 0, pi/2)
intersecteaza_ac1 <- (0.5 * sin(theta)) >= x
intersecteaza_ac2 <- (0.5 * cos(theta)) >= x 

Z <- as.numeric(intersecteaza_ac1) + as.numeric(intersecteaza_ac2)
media_Z_pe_2 <- mean(Z/2)
var_Z_pe_2 <- var(Z/2)

cat("Media E[Z/2] (Empiric):", round(media_Z_pe_2, 4), "\n")
cat("Media E[Z/2] (Teoretic):", round(2/pi, 4), "\n")
cat("Varianta Var(Z/2) (Empiric):", round(var_Z_pe_2, 4), "\n")

#c
L <- 0.7
d <- 1.2
x_centru_c <- runif(N, 0, d/2)
intersecteaza_c <- (L/2 * sin(theta)) >= x_centru_c
prob_c <- sum(intersecteaza_c) / N

cat("Probabilitate estimata (Empiric):", round(prob_c, 4), "\n")
cat("Probabilitate teoretica (2L/pid):", round((2*L)/(pi*d), 4), "\n")

#d

# param
set.seed(123) 

buffon_needle_simulation <- function(L, d, n_simulations = 10000) {
  # L = lung ac
  # d = diam cerc
  # n_simulations = nr sim
  
  if (L > d) {
    warning("Acul este mai lung decât diametrul. Formula se aplică pentru L <= d.")
  }
  
 # unghi (uniform pe [0, d/2])
  lambda <- runif(n_simulations, 0, d/2)
  
  # unghi (uniform pe [0, π])
  theta <- runif(n_simulations, 0, pi)
  
  # 
  # acul se intersecteaza cu linia  lambda <= (L/2) * sin(theta)
  intersections <- lambda <= (L/2) * abs(sin(theta))
  
  # prob empirica
  prob_empirical <- sum(intersections) / n_simulations
  
  # prob teoretica
  prob_theoretical <- (2 * L) / (pi * d)
  
  # rez
  list(
    prob_empirical = prob_empirical,
    prob_theoretical = prob_theoretical,
    error = abs(prob_empirical - prob_theoretical),
    n_simulations = n_simulations,
    n_intersections = sum(intersections)
  )
}

# ex usage
cat("\n=== EXERCIȚIUL 3d: Probabilitatea intersecției (λ uniform) ===\n")
result_d <- buffon_needle_simulation(L = 1, d = 2, n_simulations = 100000)
cat(sprintf("Probabilitate empirică: %.6f\n", result_d$prob_empirical))
cat(sprintf("Probabilitate teoretică: 2L/(πd) = %.6f\n", result_d$prob_theoretical))
cat(sprintf("Eroare: %.6f\n", result_d$error))


#e 
buffon_grid_simulation <- function(L, d1, d2, n_simulations = 10000) {
  # L = lung ac
  # d1 = dist  linii paralele din primul set
  # d2 = dist dintre linii paralele din al doilea set
  # n_simulations = nr sim
  
  if (L >= min(d1, d2)) {
    warning("Acul ar trebui să fie mai scurt decât min{d1, d2} pentru formula standard.")
  }
  
  # poz centru ac
  x <- runif(n_simulations, 0, d1)
  y <- runif(n_simulations, 0, d2)
  
  # unhghi ac (uniform pe [0, 2π])
  theta <- runif(n_simulations, 0, 2*pi)
  
  # coord capete ac
  x1 <- x - (L/2) * cos(theta)
  x2 <- x + (L/2) * cos(theta)
  y1 <- y - (L/2) * sin(theta)
  y2 <- y + (L/2) * sin(theta)
  
  # intersectie linii verticale (la multipli de d1)
  intersect_vertical <- (floor(x1/d1) != floor(x2/d1))
  
  # inyteresctia linii orizontale (la multipli de d2)
  intersect_horizontal <- (floor(y1/d2) != floor(y2/d2))
  
  # intersectie grid
  intersections <- intersect_vertical | intersect_horizontal
  
  # prob empirica
  prob_empirical <- sum(intersections) / n_simulations
  
  # prob teoretica
  prob_theoretical <- (2*L*(d1 + d2) - L^2) / (pi * d1 * d2)
  
  # rez
  list(
    prob_empirical = prob_empirical,
    prob_theoretical = prob_theoretical,
    error = abs(prob_empirical - prob_theoretical),
    n_simulations = n_simulations,
    n_intersections = sum(intersections)
  )
}

# usage
cat("\n=== EXERCIȚIUL 3e: Probabilitatea intersecției cu grid ===\n")
result_e <- buffon_grid_simulation(L = 1, d1 = 3, d2 = 4, n_simulations = 100000)
cat(sprintf("Probabilitate empirică: %.6f\n", result_e$prob_empirical))
cat(sprintf("Probabilitate teoretică: (2L(d₁+d₂)-L²)/(πd₁d₂) = %.6f\n", result_e$prob_theoretical))
cat(sprintf("Eroare: %.6f\n", result_e$error))


#f
cat("\n=== EXERCIȚIUL 3f: Exemple de algoritmi aleatori ===\n")
cat("\n--- Exemplu Las Vegas: QuickSort Randomizat ---\n")
cat("Descriere: QuickSort cu pivot aleator\n")
cat("- Timpul de execuție variază în funcție de alegerea pivotului\n")
cat("- Rezultatul este întotdeauna corect (lista sortată)\n")
cat("- Complexitate medie: O(n log n), worst case: O(n²)\n\n")

randomized_quicksort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # pivot aleator
  pivot_index <- sample(1:length(arr), 1)
  pivot <- arr[pivot_index]
  
  # part
  left <- arr[arr < pivot]
  middle <- arr[arr == pivot]
  right <- arr[arr > pivot]
  
  # Recursiv
  c(randomized_quicksort(left), middle, randomized_quicksort(right))
}

# dem
test_arr <- sample(1:20, 20, replace = FALSE)
cat("Vector nesortate:", test_arr, "\n")
sorted_arr <- randomized_quicksort(test_arr)
cat("Vector sortate:", sorted_arr, "\n")

# monte carlo
cat("\n--- Exemplu Monte Carlo: Estimarea lui π ---\n")
cat("Descriere: Estimarea valorii lui π prin metoda Monte Carlo\n")
cat("- Timpul de execuție este fix (n simulări)\n")
cat("- Rezultatul este aproximativ (crește precizia cu n)\n\n")

estimate_pi_monte_carlo <- function(n_simulations) {
  # puncte random in partatul [0,1] x [0,1]
  x <- runif(n_simulations)
  y <- runif(n_simulations)
  
  # veric cate puncte in centru
  inside_circle <- (x^2 + y^2) <= 1
  
  # π estimate
  pi_estimate <- 4 * sum(inside_circle) / n_simulations
  
  list(
    estimate = pi_estimate,
    error = abs(pi_estimate - pi),
    n_simulations = n_simulations
  )
}
# dem cu dif dimensiuni de esantion
cat("Estimări π cu diferite numere de simulări:\n")
for (n in c(100, 1000, 10000, 100000)) {
  result <- estimate_pi_monte_carlo(n)
  cat(sprintf("n = %6d: π ≈ %.6f (eroare: %.6f)\n", 
              n, result$estimate, result$error))
}

cat("\n=== Justificare categorii ===\n")
cat("Las Vegas (QuickSort):\n")
cat("  ✓ Rezultat exact garantat\n")
cat("  ✗ Timp de execuție variabil\n")
cat("  Exemplu: Sortare, căutare cu hash randomizat\n\n")

cat("Monte Carlo (Estimare π):\n")
cat("  ✗ Rezultat aproximativ\n")
cat("  ✓ Timp de execuție previzibil\n")
cat("  Exemplu: Integrare numerică, simulări probabilistice\n")


visualize_buffon_needle <- function(L = 1, d = 2, n_needles = 50) {
  library(ggplot2)
  
  set.seed(42)
  
  # gen poz + angles
  lambda <- runif(n_needles, 0, d/2)
  theta <- runif(n_needles, 0, pi)
  
  # calc intersect
  intersects <- lambda <= (L/2) * abs(sin(theta))
  
 
  needle_data <- data.frame(
    x_center = runif(n_needles, 0, 10),
    y_center = lambda,
    theta = theta,
    intersects = intersects
  )
  
  needle_data$x1 <- needle_data$x_center - (L/2) * cos(needle_data$theta)
  needle_data$x2 <- needle_data$x_center + (L/2) * cos(needle_data$theta)
  needle_data$y1 <- needle_data$y_center - (L/2) * sin(needle_data$theta)
  needle_data$y2 <- needle_data$y_center + (L/2) * sin(needle_data$theta)
  
  # Plot
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    geom_segment(data = needle_data,
                 aes(x = x1, y = y1, xend = x2, yend = y2, 
                     color = intersects),
                 size = 0.8) +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                       labels = c("Nu intersectează", "Intersectează")) +
    labs(title = "Problema Acului lui Buffon",
         subtitle = sprintf("L = %.1f, d = %.1f, Intersecții: %d/%d",
                            L, d, sum(intersects), n_needles),
         x = "", y = "Distanță de la linie",
         color = "Status") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

# grid 3e
visualize_buffon_grid <- function(L = 1, d1 = 3, d2 = 3, n_needles = 30) {
  library(ggplot2)
  
  set.seed(42)
  
  x <- runif(n_needles, 0, 3*d1)
  y <- runif(n_needles, 0, 3*d2)
  theta <- runif(n_needles, 0, 2*pi)
  
  x1 <- x - (L/2) * cos(theta)
  x2 <- x + (L/2) * cos(theta)
  y1 <- y - (L/2) * sin(theta)
  y2 <- y + (L/2) * sin(theta)
  
  intersect_v <- (floor(x1/d1) != floor(x2/d1))
  intersect_h <- (floor(y1/d2) != floor(y2/d2))
  intersects <- intersect_v | intersect_h
  
  needle_data <- data.frame(x1, y1, x2, y2, intersects)
  
  # grid lines
  v_lines <- data.frame(x = seq(0, 3*d1, by = d1))
  h_lines <- data.frame(y = seq(0, 3*d2, by = d2))
  
  # Plot
  p <- ggplot() +
    geom_vline(data = v_lines, aes(xintercept = x), 
               color = "gray70", size = 0.5) +
    geom_hline(data = h_lines, aes(yintercept = y), 
               color = "gray70", size = 0.5) +
    geom_segment(data = needle_data,
                 aes(x = x1, y = y1, xend = x2, yend = y2,
                     color = intersects),
                 size = 0.8) +
    scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                       labels = c("Nu intersectează", "Intersectează")) +
    labs(title = "Problema Grid-ului Buffon",
         subtitle = sprintf("L = %.1f, d₁ = %.1f, d₂ = %.1f, Intersecții: %d/%d",
                            L, d1, d2, sum(intersects), n_needles),
         x = "x", y = "y",
         color = "Status") +
    coord_equal() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

cat("\n\n=== Generare vizualizări ===\n")
visualize_buffon_needle(L = 1.5, d = 2, n_needles = 50)
visualize_buffon_grid(L = 1, d1 = 3, d2 = 3, n_needles = 30)

cat("\n\n=== Script finalizat cu succes! ===\n")
