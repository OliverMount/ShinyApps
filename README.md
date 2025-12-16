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

###  2. Ornstein–Uhlenbeck (OU) stochastic process

- **Description:** Simulate and visualize sample paths of the Ornstein–Uhlenbeck (OU) stochastic process, a mean-reverting Gaussian process widely used in neuroscience, physics, and finance. The app displays trajectories, stationary distributions, and allows exploration of how drift, volatility, and mean-reversion rate shape the dynamics.   
- **Link:**   [Ornstein-Uhlenbeck Process Simulator](https://olioli.shinyapps.io/OUprocess/)  
- **Usage:** Choose parameters $\theta$ (mean-reversion rate), $\mu$ (long-term mean), $\sigma$ (volatility), and simulation length. Adjust the time step and number of sample paths, then click Simulate to generate trajectories. The plot updates interactively, allowing you to study how the OU process behaves under different parameter regimes.

---  

### **3. Green’s Theorem**  

#### **a. Curl form (relating boundary circulation to curl)**

- **Description:**  
  Explore Green’s Theorem in its **circulation form** by visualizing a 2D vector field, selecting a region (circle, rectangle, or polygon), and comparing the **line integral (circulation)** around the boundary with the **double integral of the curl** over the region. This app helps build intuition for how local rotation inside a region gives rise to global circulation along its boundary.

- **Link:**  
  [Circulation (Curl) Visualization](https://olioli.shinyapps.io/GreensTheorem_curl_form/)

- **Usage:**  
  Enter the vector field $\boldsymbol{F}(x,y)=(P(x,y),Q(x,y))$. Choose a contour shape and its parameters (radius, width/height, etc.). The app plots the vector field, draws the region, and numerically computes both the boundary circulation integral and the area integral of the curl. View the results side-by-side and experiment with different vector fields to see when Green’s Theorem holds.


####  **b. Flux form: relating boundary flux to divergence)**

- **Description:**  
  Explore Green’s Theorem in its **flux form** by visualizing a 2D vector field, selecting a region (circle, rectangle, or polygon), and comparing the **outward flux** across the boundary with the **double integral of the divergence** over the region. This app builds intuition for how sources and sinks within a region determine the net flow across its boundary.

- **Link:**  
  [Divergence (Flux) Visualization](https://olioli.shinyapps.io/GreensTheorem_divergence_form/)

- **Usage:**  
  Enter the vector field $\boldsymbol{F}(x,y)=(P(x,y),Q(x,y))$. Choose a contour shape and its parameters (radius, width/height, etc.). The app plots the vector field, draws the region, and numerically computes both the boundary flux integral and the area integral of the divergence. View the results side-by-side and experiment with different vector fields to see when Green’s Theorem holds in flux form.

 
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
