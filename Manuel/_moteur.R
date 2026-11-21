# _moteur.R — moteur de l'application « Bayes et la pièce de monnaie », pour le manuel.
# Mêmes a priori, même mise à jour bêta-binomiale et même suite aléatoire que app.R :
# avec la même graine et les mêmes clics, on obtient exactement les lancers de l'application.

priors <- list(
  "Uniforme" = c(1, 1),
  "Plutôt équilibrée" = c(8, 8),
  "Face favorisée" = c(8, 3),
  "Pile favorisée" = c(3, 8)
)
fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits, decimal.mark = ",")

# A posteriori bêta(α + Face, β + Pile)
posterieure <- function(prior, faces, n) c(prior[1] + faces, prior[2] + n - faces)
moyenne_beta <- function(ab) ab[1] / sum(ab)
ic_beta <- function(ab, niveau = 0.95) qbeta(c((1 - niveau) / 2, (1 + niveau) / 2), ab[1], ab[2])
sd_beta <- function(ab) { a <- ab[1]; b <- ab[2]; sqrt(a * b / ((a + b)^2 * (a + b + 1))) }

# Une expérience de l'application : graine appliquée au premier lancer,
# puis chaque clic sur « Lancer la pièce » poursuit la même suite aléatoire.
experience <- function(graine, mode = c("connue", "mystere"), theta = 0.7) {
  mode <- match.arg(mode)
  set.seed(graine)
  th <- if (mode == "mystere") runif(1, 0.05, 0.95) else theta
  etat <- .Random.seed
  n <- 0L; faces <- 0L; hist <- data.frame(n = integer(), faces = integer())
  list(
    theta = function() th,
    lancer = function(lot) {
      assign(".Random.seed", etat, envir = globalenv())
      f <- rbinom(1, size = lot, prob = th)
      etat <<- get(".Random.seed", envir = globalenv())
      n <<- n + as.integer(lot); faces <<- faces + as.integer(f)
      hist <<- rbind(hist, data.frame(n = n, faces = faces))
      invisible(c(n = n, faces = faces))
    },
    etat = function() list(n = n, faces = faces, hist = hist)
  )
}
