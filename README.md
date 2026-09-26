<p align="center">
  <img src="assets/portada.png" alt="Maestría en Ciencia de Datos" width="300">
</p>

<h1 align="center">Maestría en Ciencia de Datos</h1>

<p align="center">
  Repositorio de prácticas, proyectos e informes desarrollados durante la maestría.
</p>

<p align="center">
  <img src="https://img.shields.io/badge/LaTeX-informes-008080?logo=latex&logoColor=white" alt="LaTeX">
  <img src="https://img.shields.io/badge/Power%20BI-ETL-F2C811?logo=powerbi&logoColor=black" alt="Power BI">
  <img src="https://img.shields.io/badge/Weka-Machine%20Learning-c0392b" alt="Weka">
  <img src="https://img.shields.io/badge/Python-visualizaci%C3%B3n-3776AB?logo=python&logoColor=white" alt="Python">
  <img src="https://img.shields.io/badge/uso-acad%C3%A9mico-informational" alt="Uso académico">
</p>

---

## Autor

**Miguel Deza**
Bachiller en Ciencias de la Computación (UNSA, 2025) · cursando la Maestría en Ciencia de Datos.

## Contenido

| # | Práctica | Descripción | Herramientas |
|:-:|---|---|---|
| 01 | [3 V's del Big Data](practica-01-Vs-Big-Data) | Análisis de Volumen, Velocidad y Variedad aplicado a un caso real — estudio de caso Netflix (trabajo grupal) | — |
| 02 | [ETL con Power BI](practica-02-etl-powerbi/practica-02-etl-powerbi.pdf) | Integración de un libro de Excel y un feed OData (Northwind), modelado de relaciones y visualización interactiva | Power BI, Power Query |
| 03 | [Pipeline de clasificación con Weka](practica-03-weka/practica-03-weka.pdf) | Pipeline visual en KnowledgeFlow (Random Forest, J48, Multilayer Perceptron) sobre el dataset Iris, con análisis estadístico y comparación de modelos | Weka, Python |
| 05 | [Uso de la IA en el trabajo](practica-05-uso-ia-trabajo/practica-05-uso-ia-trabajo.pdf) | Diapositivas (5 caras) sobre cómo aplico IA generativa en mi trabajo en PROMEDIMSA S.R.L. — Docente: Yessenia Deysi Yari Ramos | LaTeX/Beamer |
| 05 | [Reglas de Asociación con Apriori](practica-05-reglas-asociacion/practica-05-reglas-asociacion.pdf) | Itemsets frecuentes y reglas de asociación (minsup ≥ 0.5, minconf ≥ 0.6) calculados a mano y verificados con Apriori en Weka | Weka |
| 06 | [Reglas de Asociación en R](practica-06-reglas-asociacion-r/practica-06-reglas-asociacion.pdf) | Mismo dataset transaccional, minado con el paquete `arules` (Apriori) en R: EDA, generación y visualización de reglas (scatter, grafo, matriz agrupada) | R, arules, arulesViz |
| 07 | [Algoritmos de Clasificación en R](practica-07-clasificacion-r/practica-07-clasificacion-r.pdf) · [diapositivas](practica-07-clasificacion-r/diapositivas-practica-07-clasificacion-r.pdf) | LDA, CART, kNN, SVM lineal y Random Forest con `caret` sobre el dataset [Obesity Levels](https://www.kaggle.com/datasets/fatemehmehrparvar/obesity-levels) de Kaggle (descargar como `obesidad.csv` para ejecutar el script); Random Forest obtuvo 96.9 % de accuracy en prueba | R, caret, randomForest |

Todas corresponden al curso **Introducción a Ciencia de Datos**, salvo donde se indique otro docente.

## Plantilla de diapositivas

[`plantilla-diapositivas/`](plantilla-diapositivas) contiene un tema de Beamer (`tema-maestria.sty`)
calcado del diseño de la Práctica 1 (barra de navegación superior, píldoras de título, pie de
página en dos tonos, listas con check verde) para reutilizar en futuras presentaciones de la
maestría. Incluye el PDF de referencia original.

## Estructura

Cada práctica vive en su propia carpeta, con su código/fuente y su entregable final:

```
maestria-ciencia-datos/
├── practica-01-Vs-Big-Data/
│   └── ...               # enunciado + presentación
├── practica-02-etl-powerbi/
│   ├── img/               # capturas y gráficos usados en el informe
│   ├── practica-02-etl-powerbi.tex
│   └── practica-02-etl-powerbi.pdf
├── practica-03-weka/
│   ├── img/
│   ├── practica-03-weka.tex
│   └── practica-03-weka.pdf
└── ...
```

<p align="center">
  <img src="assets/banner-trabajo.png" alt="" width="520">
</p>

## Licencia

Este repositorio se comparte con fines académicos y de portafolio.
