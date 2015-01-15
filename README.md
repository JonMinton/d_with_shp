# Dissimilarity Inference Toolkit
*Jon Minton*

*15 Jan 2015*

# Introduction
The purpose of this document is to help guide people wishing to use the app to download and set up the app on their own computer. 
I will assume that users will not use GitHub, but will be using RStudio. 

# Process
1. Install [R](http://cran.r-project.org/);
2. Install [RStudio](http://www.rstudio.com/products/RStudio/);
3. Select to download this repository as a zipped file (icon on right);
4. Unzip the repository;
5. Find the file `d_with_shp.Rproj` inside the unzipped repository and select it. This should open up the project within Rstudio;
6. In the file navigation pane (bottom right pane), go into the directory `select_num_denom` and click on either the file `ui.r` or `server.r`;
7. The R script file should now open in the top left pane. On the top right corner of this pane there should be an option `Run App` - click on this. 
8. The app should now be run!

# Troubleshooting
- Any errrors and warnings should appear on the bottom left pane of Rstudio. These will tell you if any packages are missing.
- To install a missing package, type `install.packages("PACKAGENAME")`, where `PACKAGENAME` is the name of the package (Note that R is case sensitive). 
- 

