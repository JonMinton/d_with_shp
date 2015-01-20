# Dissimilarity Inference Toolkit
*Jon Minton*

*20 Jan 2015*

# Introduction
The purpose of this document is to help guide people wishing to use the app to download and set up the app on their own computer. 
I will assume that users will not use GitHub, but will be using RStudio. 

# Process
1. Install [R](http://cran.r-project.org/);
2. Install [RStudio](http://www.rstudio.com/products/RStudio/);
3. Select to download this repository as a zipped file by clicking on the `Download Zip` icon on the right side of this website;
4. Unzip the repository;
5. Find the file `d_with_shp.Rproj` inside the unzipped repository and select it. This should open up the project within Rstudio;
6. With RStudio open, look at the bottom right pane. This should contain a file `script.r`. Double click to open this; the contents of the file should appear in the top left pane. 
7. At the top right of the pane on the top left pane there should be a clickable icon labelled 'source'. Click on this and the `script.r` file should run. This script file loads the app. 
8. The app should now run!

# Troubleshooting and notes
- The bottom left pane is the IO window: from this you can enter commands to R directly (Input); and you can also see error messages, warnings and other outputs from R. When R is ready for an input, a little chevron arrow `>` will appear at the bottom of this pane. 
- The first time the project is loaded it is likely that a large number of additional files will be fetched using a package management program called `packrat`. Please wait for this to finish before proceeding.
- `Packrat` may not work effectively with university managed computers as it may not have appropriate permissions to install. Please try from a home or other unmanaged computer as well. 
- The first time the app is loaded a lot of data will need to be loaded. While this data is loading the app will not display all of the user options. In particular, you will not see two white windows around one third of the way up from the pane on the left. Please wait until these windows have appeared before proceeding.
- The white windows can be used to select groups of categories as numerators and denominators. The white window will show the selection so far. Please ensure that the numerator is smaller than the denominator. 

