# Salaire-Minimum-Chomage-Europe

**Effet dynamique du salaire minimum sur le chomage en Europe — Modele Distributed Lag avec traitement continu**

Cette etude applique la methodologie de Schmidheiny & Siegloch (2023) a un panel de 25 pays europeens (2009-2022) pour estimer l'effet dynamique du salaire minimum sur le chomage. L'accent est mis sur la validation rigoureuse des hypotheses d'identification, qui revele une causalite inverse invalidant l'interpretation causale des resultats.

---

## Question de recherche

Le salaire minimum fait-il augmenter le chomage ? Cette question, debattue depuis Card & Krueger (1994), est abordee ici via un modele a retards distribues (Distributed Lag) avec effets fixes bidimensionnels et traitement continu (log du salaire minimum).

---

## Methodologie

| Composant | Description |
|-----------|-------------|
| **Modele** | Distributed Lag avec 2 leads et 5 lags, effets fixes pays et annee |
| **Traitement** | Continu : log(salaire minimum), et non indicatrice binaire |
| **Donnees** | Eurostat — 25 pays, 2009-2022, 332 observations pays-annees |
| **Erreurs-types** | Clusterisees par pays |
| **Validation** | Test des tendances paralleles, comparaison FE/FD, stabilite des fenetres, analyse de sensibilite Rambachan & Roth (2023) |

---

## Resultats principaux

L'estimation produit un effet cumule de +12,1 pp de chomage apres 5 ans pour une hausse de 1 unite log du salaire minimum. Cependant, **les quatre tests de validation echouent** :

| Test | Resultat | Statut |
|------|----------|--------|
| Tendances paralleles | p < 0.05 | Rejetees |
| Convergence FE / FD | Ecart max 4.14 pp | Divergence |
| Stabilite selon la fenetre | CV = 51% | Instable |
| Breakdown point (Rambachan & Roth) | M = 0.25 | Fragile |

**Conclusion :** les resultats doivent etre interpretes comme des correlations conditionnelles, pas comme des effets causaux. La pattern des leads suggere une causalite inverse : les gouvernements augmentent le salaire minimum lorsque la conjoncture de l'emploi s'ameliore.

---

## Structure du projet

```
Salaire-Minimum-Chomage-Europe/
├── Projet_donnee_de_panel.R                # Script principal (estimation, tests, graphiques)
├── rapport_salaire_minimum.pdf             # Rapport complet avec resultats et discussion
├── data/                                   # Donnees Eurostat (chomage, salaire minimum)
└── README.md
```

---

## References methodologiques

- **Schmidheiny, K. & Siegloch, S. (2023).** On Event Studies and Distributed-Lags in Two-Way Fixed Effects Models. *Journal of Applied Econometrics*, 38(5), 695-713.
- **Rambachan, A. & Roth, J. (2023).** A More Credible Approach to Parallel Trends. *Review of Economic Studies*, 90(5), 2555-2591.
- **Goodman-Bacon, A. (2021).** Difference-in-Differences with Variation in Treatment Timing. *Journal of Econometrics*, 225(2), 254-277.
- **Card, D. & Krueger, A.B. (1994).** Minimum Wages and Employment. *American Economic Review*, 84(4), 772-793.

---

## Installation et execution

```bash
git clone https://github.com/Midou94f/Salaire-Minimum-Chomage-Europe
cd Salaire-Minimum-Chomage-Europe
```

**Prerequis :** R + RStudio. Packages necessaires : `plm`, `lmtest`, `sandwich`, `ggplot2`, `dplyr`, `tidyr`.

```r
source("Projet_donnee_de_panel.R")
```

---

## Auteurs

Projet realise par **Mehdi Fehri** et **Emile Zeller** — Master 2 Econometrie, Statistiques et Data Science, Universite de Strasbourg (janvier 2026).

---

## Licence

Voir le fichier [LICENSE](LICENSE) pour les details.
