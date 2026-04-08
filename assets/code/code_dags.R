library(tidyverse)
library(stargazer)
set.seed(123) 
n <- 5000
## KEY POINT: Cada línea corresponde a una flecha del DAG.

## Los coeficientes son los "verdaderos" efectos causales.
motivacion <- rnorm(n)

## Motivación → Programa (las personas más motivadas participan más)
programa <- rbinom(n, 1, prob = plogis(0.8 * motivacion))

## Programa → Experiencia (el programa da ~2 años de experiencia en promedio)
experiencia <- programa * runif(n, 1, 3) + rnorm(n, 0, 0.5)

## Programa → Ingresos: efecto directo = 2
## Experiencia → Ingresos: cada año de experiencia = 0.5 ## Motivación → Ingresos: efecto = 1.5
ingresos <- 10 + 2 * programa + 1 * experiencia +  2 * motivacion + rnorm(n)

datos <- tibble(motivacion, programa, experiencia, ingresos)
## KEY POINT: El efecto TOTAL del programa sobre ingresos incluye ## el directo (2) + indirecto vía experiencia (0.5 * E[exp|prog=1] ## Efecto total 3

m1 <- lm(ingresos ~ programa, data = datos)
m2 <- lm(ingresos ~ programa + motivacion, data = datos)
m3 <- lm(ingresos ~ programa + motivacion + experiencia, data = datos)
m4 <- lm(ingresos ~ programa + experiencia, data = datos)

stargazer(m1, m2, m3, m4, type = "text",
          column.labels = c("Sin controles", "Backdoor cerrado",
                            "Efecto directo", "Sesgo + mediador bloqueado"),
          dep.var.labels = "Ingresos",
          keep.stat = c("n", "rsq"),
          notes = "Efecto total verdadero 3, efecto directo verdadero 2")




ascenso <- rpois(n, lambda = 0.5 * ingresos + 0.7 * programa)


datos2 <- tibble(motivacion, programa, experiencia, ingresos, ascenso)
m2 <- lm(ingresos ~ programa + motivacion, data = datos2)
m5 <- lm(ingresos ~ programa + motivacion + ascenso, data = datos2) 
m6 <- lm(ingresos ~ programa + ascenso, data = datos2)

stargazer(m2, m5, m6, type = "text",
          column.labels = c("Efecto total (ref.)",
                            "+ Ascenso (colisionador)",
                            "Solo ascenso"),
          dep.var.labels = "Ingresos",
          keep.stat = c("n", "rsq"))
