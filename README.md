# massasaugaSDM

Species Distribution Modeling for Western Massasauga
by: Michelle Lawing and Danielle Walkup
Updated: 30 July 2020

The main script document for species distribution modeling of Massasaugas is in the top folder and is called massasaugaSDM.R. This is a wrapper script and sources all secondary scripts located in the scripts folder. Before attempting to run this script, make sure to open and read the other scripts in numerical order. The data included here are publicly sourced data and do not include the potentially sensive data included in the full analysis. Those data will be made available on request and in consulation with data constributers, and when relevant, state and federal authorities.

Caution, everything is set up to run from scratch, not load!!!
This means that the run time will be very long for some of the scripts.

Scripts 01_input through 07_evaluate build on each other and need to be run in order to setup and run the sdm model.

Scripts 08_currentProjections through 10_futureProjections rely on scripts 01 and 02, but may need to be run separately because of issues with memory in rJava. See the warnings at the top of each individual script for more information.

Scripts 11_variableResponse and 12_manuscriptFigures are stand alone scripts that load the needed data and packages.

Generally packages are loaded in the scripts as they are needed. Make sure to go through all the scripts and install the required libraries before loading them.

We provide example data to run these scripts. These include inaturalist, gbif, and vertnet occurrences, administrative boundaries for the U.S. and Mexico, Bioclimatic data from WorldClim database, and derived Envirem data.
