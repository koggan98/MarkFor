# Market Analysis and Forecasting

## Overview

This repository contains the complete analysis for the **Market Analysis and Forecasting** project.

The analysis was conducted using **R** inside a **Jupyter Notebook** (R kernel). This setup allows a structured and transparent workflow by combining code, explanations, and results within a single document.

---

## Environment and Tools

- Language: **R**
- Notebook environment: **Jupyter Notebook (R kernel)**
- Compatible IDEs:
  - VS Code with Jupyter support
  - JupyterLab
  - RStudio (for running the standalone script)

---

## Repository Structure

### `markfor_analysis_notebook.ipynb`

The original Jupyter Notebook containing:

- full analysis code  
- explanations and comments  
- generated outputs and visualizations  

Most modern IDEs can open and execute this file directly.

---

### `markfor_analysis_notebook.html`

HTML export of the notebook.

Use this if:

- you cannot open the `.ipynb` file
- you only want to inspect results without executing code

This version preserves layout and readability and is generally preferred over the PDF.

---

### `markfor_analysis_notebook.pdf`

PDF export of the notebook.

Useful for quick offline viewing.  
Note that some lines may be too long to display properly, which is why the HTML version is recommended.

---

### `markfor_analysis_script.R`

Standalone R script containing the entire analysis.

- Can be executed in any standard R environment
- Independent from Jupyter
- Suitable for reproducible execution or modifications

---

## Running the Analysis

### Option 1: Jupyter Notebook

Requirements:

- Jupyter environment (JupyterLab or VS Code)
- R kernel installed

Open:
**`markfor_analysis_notebook.ipynb`**

---

### Option 2: R Script (Recommended for simplicity)

Run:
**`markfor_analysis_script.R`**

All required packages are listed at the top of the script.

If packages are missing, install them by uncommenting:

```r
install.packages(...)
 ```
