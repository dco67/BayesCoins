# BayesCoins — Bayes et la pièce de monnaie

**Comprendre l'inférence bayésienne en lançant une pièce : de l'a priori à l'a posteriori, lancer après lancer.**

BayesCoins est une application pédagogique **R/Shiny**, en français. Une pièce a une probabilité de Face θ inconnue, choisie au hasard entre 0,05 et 0,95. On lance la pièce, et l'application met à jour la distribution de θ selon le modèle bêta-binomial. On peut ensuite comparer plusieurs a priori sur les mêmes données, puis révéler la vraie valeur de θ.

- [Utiliser l'application en ligne](https://shiny.dcstats.org/BayesCoins/)
- [Consulter le manuel pédagogique](https://dcstats.org/BayesCoins/)

## Objectifs pédagogiques

- comprendre le rôle de la distribution a priori, de la vraisemblance et de la distribution a posteriori;
- voir comment l'a posteriori se resserre à mesure que les lancers s'accumulent;
- mesurer l'influence d'un a priori plus ou moins confiant, et son effacement devant les données;
- calculer et interpréter une probabilité a posteriori (par exemple P(θ > seuil));
- distinguer intervalle de crédibilité et intervalle de confiance fréquentiste.

## Fonctionnalités

| Onglet | Contenu |
| --- | --- |
| Introduction | La pièce, le paramètre θ, la logique de la mise à jour bayésienne. |
| Explorer | Choix de l'a priori bêta (α, β), lancers par séries, a priori et a posteriori superposés, zone de probabilité mise en évidence. |
| Comparer les a priori | Quatre a priori mis à jour avec les mêmes lancers; évolution des moyennes a posteriori. |
| Comprendre le calcul | La mise à jour bêta-binomiale pas à pas, avec vos propres résultats et une valeur candidate de θ. |
| Expérimenter | Six missions guidées : influence de l'a priori, données surprenantes, pièce presque équilibrée, a priori très confiant… |

Graphiques interactifs (plotly) : survol, zoom, enregistrement.

## Exécuter l'application localement

```r
install.packages(c("shiny", "plotly"))
shiny::runApp("BayesCoins")
```

## Licence

GPL-3.0. Voir le fichier `LICENSE`.

## Auteur

Daniel Coulombe, Ph.D. — <https://dcstats.org>
