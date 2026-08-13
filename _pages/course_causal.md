---
layout: page
permalink: /causal/
title: Inferencia Causal
description: Primer Semestre 2026 &middot; Facultad de Administración y Economía &middot; USACH
nav: false
nav_order: 6
---

- [Programa del curso][syllabus]

---

# Módulo 1 — Fundamentos y Causalidad

## Clase 1 — Repaso e Introducción a la Causalidad
Historia intelectual y motivación. Repaso de probabilidad y regresión.

- [Introducción][intro]
- [Repaso de Probabilidad y Estadística][repaso-prob]
- [Repaso de Regresión][repaso-reg]

## Clase 2 — DAGs e Inferencia Causal
Nodos, aristas y caminos causales. Confusores, mediadores y colisionadores.

- [Clase][dags]

## Clase 3 — Resultados Potenciales y RCTs
El modelo de resultados potenciales. Ensayos controlados aleatorios.

- [Resultados Potenciales][potenciales]
- [Experimentos Aleatorizados][experimentos]

# Módulo 2 — Estrategias de Identificación I

## Clase 4 — Selección en Observables
Control con regresión. Emparejamiento y Propensity Score.

- [Condicionando en Observables][seleccion]

## Clase 5 — Cumplimiento Imperfecto
El problema de cumplimiento imperfecto en RCTs. Cumplidores, tomadores y rechazadores. El ITT y el LATE.

- [Parte 1 — Tipos de cumplimiento e ITT][ci1]
- [Parte 2 — Del ITT al LATE][ci2]

# Módulo 3 — Inferencia

## Clase 6 — Tópicos de Inferencia
Pruebas de hipótesis y potencia estadística. Clustering. Inferencia por aleatorización y Bootstrap.

- [Clase][inferencia]

# Módulo 4 — Estrategias de Identificación II

## Clase 7 — Variables Instrumentales I
Marco de IV: relevancia y exogeneidad. El LATE como estimador de IV.

- [Clase][iv]

## Clase 8 — Variables Instrumentales II
2SLS: implementación e interpretación. Aplicaciones empíricas clásicas.

- [Clase][iv]

## Clase 9 — Discontinuidad en la Regresión (RDD)
Identificación en el punto de corte. Estimación. Validación y aplicaciones. RDD aguda vs. difusa.

- [Clase][rdd]

## Clase 10 — Diferencias en Diferencias I
El supuesto de tendencias paralelas. Estimación con dos períodos y dos grupos. Event studies.

- [Clase][did]

## Clase 11 — Diferencias en Diferencias II
DID con múltiples períodos y tratamiento escalonado. Efectos heterogéneos.

- [Clase][did]

## Clase 12 — Control Sintético
Motivación y relación con DID. Construcción del contrafactual sintético. Inferencia por permutación.

- [Clase][sintetico]

# Módulo 5 — Integración

## Trabajo Final
Presentación y discusión de propuestas de proyecto.

- [Trabajo Final][trabajo-final]

# Bibliografía

- Huntington-Klein, N. (2021). *The Effect: An Introduction to Research Design and Causality.* Chapman and Hall/CRC.
- Cunningham, S. (2021). *Causal Inference: The Mixtape.* Yale University Press.
- Angrist, J.D., & Pischke, J.-S. (2009). *Mostly Harmless Econometrics: An Empiricist's Companion.* Princeton University Press.

[syllabus]:{{ site.url }}/assets/pdf/syllabus.pdf
[intro]:{{ site.url }}/assets/pdf/Introduccion.pdf
[repaso-prob]:{{ site.url }}/assets/pdf/RepasoProb.pdf
[repaso-reg]:{{ site.url }}/assets/pdf/RepasoRegresion.pdf
[dags]:{{ site.url }}/assets/pdf/DAGs.pdf
[potenciales]:{{ site.url }}/assets/pdf/ResultadosPotenciales.pdf
[experimentos]:{{ site.url }}/assets/pdf/Experimentos.pdf
[seleccion]:{{ site.url }}/assets/pdf/Selección.pdf
[ci1]:{{ site.url }}/assets/pdf/CI1.pdf
[ci2]:{{ site.url }}/assets/pdf/CI2.pdf
[inferencia]:{{ site.url }}/assets/pdf/Inferencia.pdf
[iv]:{{ site.url }}/assets/pdf/VariablesInstrumentales.pdf
[rdd]:{{ site.url }}/assets/pdf/RDD.pdf
[did]:{{ site.url }}/assets/pdf/DID.pdf
[sintetico]:{{ site.url }}/assets/pdf/ControlSintetico.pdf
[trabajo-final]:{{ site.url }}/assets/pdf/Trabajo_Final_2026_S1.pdf
