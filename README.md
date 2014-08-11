Fire and Climate Change in Fynbos
=====

A workshop in modeling post-fire recovery and ecosystem resilience in _fynbos_ systems.

The workshop will consist primarily of stepping through example code that explores the use of LANDSAT NDVI data in modeling post-fire recovery of _fynbos_.  The materials for the course are all available in a Git Repository at [https://github.com/adammwilson/postfire](https://github.com/adammwilson/postfire).


# Repository Structure

The repository is structured as follows:

* **workflows:**       
    * [1_EarthEngine](workflow/1_EarthEngine)
    * [2_SolarRadiation](workflow/2_SolarRadiation/SolarRadiation.Rmd)
    * [3_DataPrepAnnual](workflow/3_DataAnnual/DataPrep.md) or   [3_DataPrep](workflow/3_Data/DataPrep.md)
    * [4_SimulatedData](workflow/4_SimulatedData/SimulatedData.md)
    * [5_PostFireTrajectoriesAnnual](workflow/5_PostFireTrajectoriesAnnual/PostFireTrajectories.md) or [5_PostFireTrajectories](workflow/5_PostFireTrajectories/PostFireTrajectories.md)
*  **data:**  Datasets used in analyses
* **output:** Temporary output from analysis


## Obtaining the code (via the repository)

1. Open RStudio
2. `File` -> `New Project` -> `Version Control` -> `GIT`
3. Repository URL [git@github.com:adammwilson/postfire.git](git@github.com:adammwilson/postfire.git)
4. Choose a location on your harddisk to save the repository


### Editing a Git repository
Git is designed to archive (multi-author) changes in text-based content (especially code).  It is completely decentralized, so when you `clone` a copy of a repository, you have the complete content and history of the files.  If you want to edit the code and later still be able to download updates, it's good practice to create a personal `branch` in your local copy.  You can do this as follows:

1. In RStudio, click on the `Git` tab in the upper right corner
2. Select the little gear icon -> `Shell`. This will open a terminal window where you have access to all git commands that are not available in the GUI.
2. in the Git command shell, type:

      `git checkout -b yourname`
      
      where `yourname` is a name for your new branch (e.g. `adam` or `testing`)
3. Now look for the branch selector menu on the right side in the Git tab.  You should see your new branch selected.  If you click it, you will also see the `master` branch.

After switching to your new branch, you can edit and save the scripts.  When you want to update to a newer version of the code, you need to:

1.   `commit` your changes
2.  `pull` updates from the master (look for the green down arrow or run `git pull` in the shell)
3.  If you've edited a script that has also been updated on the master, you many need to `merge` the documents




# Pre-Workshop Preparation
## Software
Please attempt to have the following software installed and functioning prior to the workshop:

1. [RStudio](http://www.rstudio.com/)
  This includes an installation of R.  Use of RStudio is not vital for the course, participants could use the  GUI included with R or another interface (e.g. [EMACS-ESS](http://ess.r-project.org/) if desired.  However, in class we'll be demonstrating the use of RStudio, so it will be most straightforward if you use that.
2. R packages.  These are installed either with a package manager or via the command line (e.g. `install.packages(raster)`).  See for [here for a list of packages](workflow/setup.R) that we'll be using.
3. [GRASS](http://grass.osgeo.org/download/).  We'll be giving a brief introduction to this full featured open-source GIS in one section of the workshop.  It isn't vital that you have it installed , but if you do, you'll be able to follow along.  If you do, please also install the [spGRASS6](http://cran.r-project.org/web/packages/spgrass6/index.html) R library.  

# Workshop objectives

1. Introduce participants to methods for quantifying post-fire recovery using satellite data
2. Provide an example of using LANDSAT data and covariates to quantify variability in post-fire recovery over the Cape Peninsula of South Africa.

## Day 1

### Session 1 (9am - 12pm)

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
	
### Session 2 (1pm - 4pm)
* [Data Prep](workflow/3_DataAnnual/DataPrep.md)
  * Assembling covariates
  * Generating the model data frame
* [Exploration of simulated data](workflow/4_SimulatedData/SimulatedData.md)

## Day 2

### Session 1 (9am - 12pm)
* [Exploration of post-fire curves](workflow/5_PostFireTrajectories/PostFireTrajectories.html)


### Session 2 (1pm - 4pm)
* Introduction to bayesian model fitting
	* Hierarchical models
	* BUGS language
* [Modeling Post-fire trajectories](workflow/6_Modeling/Modeling.md)

