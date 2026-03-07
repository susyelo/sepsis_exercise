# Sepsis regional analysis exercise

## Overview

This repository contains a reproducible workflow to analyse regional patterns of sepsis hospitalisations in France and explore their relationships with territorial indicators from INSEE.

The analysis is organised into four parts:
  
1. Description of sepsis outcomes across regions
2. Download and preparation of explanatory variables from INSEE
3. Exploratory analysis of associations between sepsis and territorial indicators
4. Cluster analysis to identify homogeneous regional profiles

## Repository structure

- `data-raw/`: original source files (sepsis, INSEE, geographic data)
- `data/derived/`: cleaned and merged datasets used for analysis
- `R/`: helper functions for import, cleaning, plotting, mapping, and clustering
- `scripts/`: numbered scripts defining the analytical pipeline
- `outputs/`: exported figures, tables, and model objects
- `report/`: final Quarto report


## Main data sources

- Regional sepsis response data
- INSEE territorial indicators
- Geographic boundaries for maps