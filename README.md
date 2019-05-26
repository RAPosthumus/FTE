FTE is a repository with data containg FTE counts for several years, several functions.
Also inclused is the salary scale.
The years are divided into quarters.

######THIS (data added on 2019_05_26) IS A FIRST PROTOTYPE, BETTER DATA will follow.

The orginal data is in /input/rawdata/fte.csv

Also included is an R project that will clean the data and makes some graphics.
In the directory /scripts, the file 01_loaddata.R does the loading and cleaning.
It saves the cleaned data to /input/cleaneddata/fte_cleaned.csv

Noted exceptions: 
(1) the functiegebouw Rijk (FGR) is not completely introduced in all the ministeries.
The orginal data contains the function "geen FGR". In this case the salaryscale is put to 0.

Therefore the sum of the fte's in this data doesnot correspond to the size of the FTE's per quarter (functions not included in FGR are not in the data)
