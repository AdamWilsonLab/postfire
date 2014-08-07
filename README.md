Fire and Climate Change in Fynbos
=====

A workshop in modeling post-fire recovery and ecosystem resilience in _fynbos_ systems.

The workshop will consist primarily of stepping through example code that explores the use of LANDSAT NDVI data in modeling post-fire recovery of _fynbos_.  The materials for the course are all available in a Git Repository at [https://github.com/adammwilson/postfire](https://github.com/adammwilson/postfire).


# Repository Structure

The repository is structured as follows:

* **workflows:**       
    * [1_EarthEngine](workflow/1_EarthEngine)
    * [2_SolarRadiation](workflow/2_SolarRadiation/SolarRadiation.md)
    * [3_DataPrep](workflow/3_Data/DataPrep.md)
    * [4_SimulatedData](workflow/4_SimulatedData.md)
    * [5_PostFireTrajectories](workflow/4_PostFireTrajectory.md)
*  **data:**  Datasets used in analyses
* **output:** Temporary output from analysis


# Pre-Workshop Preparation
## Software
Please attempt to have the following software installed and functioning prior to the workshop:

1. [RStudio](http://www.rstudio.com/)
  This includes an installation of R.  Use of RStudio is not vital for the course, participants could use the  GUI included with R or another interface (e.g. [EMACS-ESS](http://ess.r-project.org/) if desired.  However, in class we'll be demonstrating the use of RStudio, so it will be most straightforward if you use that.
2. R packages.  These are installed either with a package manager or via the command line (e.g. `install.packages(raster)`).  See for [here for a list of packages](workflow/1_setup.R) that we'll be using.
3. [GRASS](http://grass.osgeo.org/download/).  We'll be giving a brief introduction to this full featured open-source GIS in one section of the workshop.  It isn't vital that you have it installed , but if you do, you'll be able to follow along.  If you do, please also install the [spGRASS6](http://cran.r-project.org/web/packages/spgrass6/index.html) R library.  

# Workshop objectives

1. Introduce participants to methods for quantifying post-fire recovery using satellite data
2. Provide a worked example of using LANDSAT data and covariates to quantify variability in post-fire recovery over the Cape Peninsula of South Africa.

## Day 1

### Session 1 (9am -- 12pm)

* Workshop overview
* Project Organization
	*  RStudio
	*  Git version control
* Introduction to Google Earth Engine
  * [Example Processing of LANDSAT data](workflow/1_EarthEngine/CapePointLANDSAT_cloud.js)
  * [Pre-processing LANDSAT data](workflow/1_EarthEngine/CapePointLANDSAT_daily.js)
*   Data Preparation
	*  Preparing the data for analysis, calculation of covariates (solar radiation, etc.)
	*  [Solar Radiation: and introduction to GRASS GIS](workflow/2_SolarRadiation/SolarRadiation.Rmd)
	*  Introduction to foreach parallel processing 
	
### Session 2 (1pm -- 4pm)
* [Data processing](workflow/3_Data/DataPrep.md)
  * Assembling covariates
  * Generating the model data frame
* [Exploration of simulated data](workflow/4_SimulatedData/SimulatedData.md)

## Day 2

### Session 1 (9am -- 12pm)
* [Exploration of post-fire curves](workflow/5_PostFireTrajectories/PostFireTrajectories.md)


### Session 2 (1pm -- 4pm)
* Introduction to bayesian model fitting
	* Hierarchical models
	* BUGS language
* Modeling Post-fire trajectories
* Linking NDVI to biomass/carbon

