# Highway Accidents Analysis and Clustering

This project analyzes highway accident data, focusing on various factors like event types, weather conditions, and accident timing. The analysis includes data cleaning, visualization, clustering, and association rule mining.

## Table of Contents

- [Installation](#installation)
- [Overview](#overview)
- [Files](#files)
- [Usage](#usage)
- [License](#license)

## Installation

Ensure you have R and the following libraries installed: `ggplot2`, `RColorBrewer`, `dplyr`, `Rtsne`, `cluster`, `arules`, `arulesViz`, `htmlwidgets`.

## Overview

1. **Data Cleaning**: The dataset is cleaned to handle missing values and remove unimportant variables.
2. **Data Visualization**: Various visualizations are created to analyze the distribution of accidents across different categories such as year, event type, day of the week, time intervals, and weather conditions.
3. **Clustering**: Clustering techniques are applied to identify patterns and group similar accident events together.
4. **Association Rules**: Association rule mining is used to find interesting relationships between different variables in the dataset.

## Files

- `Highway accidents.csv`: The dataset containing highway accident data.
- `analysis.R`: R script containing the code for data cleaning, visualization, clustering, and association rule mining.
- `README.md`: This file.

## Usage

1. **Load Data**: Load the highway accident data from the provided CSV file.
2. **Data Cleaning**: Clean the dataset by handling missing values and removing unimportant variables.
3. **Data Visualization**: Generate visualizations to explore and understand the data.
4. **Clustering**: Apply clustering techniques to find patterns in the data.
5. **Association Rules**: Use association rule mining to discover interesting relationships between variables.

## License
