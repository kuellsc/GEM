# GEM
Geohydrological Model for Namibia and applicable in semi-arid to arid regions. The model can be run on a shinyapps server or using RStudio with R on a desktop system. Climate and Rainfall and Runoff data (for calibration) are needed. There is an import function for csv files and two example files ‘Precipitation.csv’ and ‘Climate.csv’ are provided. 

If you want to run it on your computer and modify it, you need to install

R: https://cran.r-project.org/bin/windows/base/R-4.1.0-win.exe
RStudio: https://www.rstudio.com/products/rstudio/download/

When you have installed both, you can open the program. At this stage the program does the following:
-	Read a GIS map of the basin and plot it
-	Read a csv map containing the rainfall files with the colums ‘Date’ and ‘P’ for precipitation in mm. More stations can be included.
-	Read a climate file csv file with the columns ‘Date’, ‘Temperature’, ‘relative humidity’. The upload can be configured (header, comma, format etc.)
-	Plot the rainfall and do some statistics
-	Calculate Runoff in the basin using a modified SCS method. Initial losses and Storage, Infiltration can be adjusted
-	Calculate discharge at a certain gauging station for basin characteristics (area, channel length, width, slope), including the channel and compartment characteristics (width, compartment area, transmission loss rate, channel slope). This is used for the design
-	Do flood routing
-	Calculate transmission loss and recharge for the sand storage dam.  
-	Calibrate the discharge series.

The next version will include:
-	Sub-daily routing of hydrographs
-	Storage water level in the sand storage dam
-	Wet sand and vegetation transpiration losses
-	Pumping characteristics
-	Surface storage dams in the basin


