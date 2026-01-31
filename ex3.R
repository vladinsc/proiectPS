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

