# My Interactive Shiny Apps Collection

![R](https://img.shields.io/badge/R-4.3.1-blue) ![Shiny](https://img.shields.io/badge/Shiny-App-success) ![Last Commit](https://img.shields.io/github/last-commit/OliverMount/ShinyApps)

Welcome! This repository hosts **interactive Shiny apps** for demonstrations in mathematics, biology, and differential equations. More apps will be added regularly.

---

## List of Apps (Expanding Regularly…)

###  1. Visualizing Slope Fields

- **Description:** Explore and visualize the slope field for any first-order differential equation: dy/dx = f(x, y).  
- **Link:** [Interactive Slope Field App](https://olioli.shinyapps.io/slopefield/)  
- **Usage:** Enter any `f(x, y)` expression, adjust the X/Y range and grid density, and view the slope field.  

--- 

###  2.  

- **Description:** Simulate and visualize sample paths of the Ornstein–Uhlenbeck (OU) stochastic process, a mean-reverting Gaussian process widely used in neuroscience, physics, and finance. The app displays trajectories, stationary distributions, and allows exploration of how drift, volatility, and mean-reversion rate shape the dynamics.   
- **Link:**   [Ornstein-Uhlenbeck Process Simulator](https://olioli.shinyapps.io/OUprocess/)  
- **Usage:** 

--- 

###  3.  

- **Description:**    
- **Link:**  [Green's Theorem Visualization](https://olioli.shinyapps.io/greens_theorem/)  
- **Usage:** 

--- 



## How to Run Locally

1. Install R and RStudio.  
2. Install required packages:

```r
install.packages(c("shiny", "ggplot2", "dplyr"))
```

3. Clone the repository

```
git clone https://github.com/OliverMount/ShinyApps.git

```

4. Run the app

```
library(shiny)
runApp("path/to/app_folder")
```
