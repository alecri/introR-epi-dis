# A modern demonstration of R for epidemiologists
By Alessio Crippa


## Description

This repository contains all the material for the workshop "A modern demonstration of R for epidemiologists",
within the course [**Epidemiology: Swedish Case Studies**](https://disabroad.org/stockholm/courses/epidemiology-swedish-case-studies/) at [at DIS study abroad in Scandinavia](https://disabroad.org/).


## Aim

The aim of this workshop is to give a short yet comprehensive demonstration of the use of R for epidemiology.  
The repository mimics the project folder for an epidemiological analysis for identifying potential risk factors of hyponatremia in a cohort of marathon runners. The analyzed data and research questions come from a original scientific article published in *The The New England Journal of Medicine*.

**Reference**  
[*Hyponatremia among Runners in the Boston Marathon*, New England Journal of Medicine, 2005, Volume 352:1550-1556](https://pubmed.ncbi.nlm.nih.gov/15829535/)


## Project structure

The project structure for this repository resembles the structure of the folder for an epidemiological studies, following the processes that usually occur: getting raw data, reading and transforming data, visualizing and modelling, iterating the previous steps until "final" communication (report or scientific article). 

```
project
│   README.md
│   introR-epi-dis.Rproj   
│
└───articles
│   │   hyponatremia.pdf
│   │   file012.txt
│   │
└───data
│   └───raw
│   |    marathon_raw.csv
│   └───derived
│   |    marathon.RData
└───scripts
│   │   01_data_management.R
│   │   02_descriptive_statistics.R
│   │   03_analyses.R
│   │   ...
│   │
└───output
│   └───figures
│   |   ...
│   └───tables
│   │   ...
│   │
└───report
│   │   marathon_report1.qmd
│   |   ...
│   │
└───slides
│   |   ...

```
