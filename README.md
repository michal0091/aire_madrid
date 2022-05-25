[![DOI](https://zenodo.org/badge/276616688.svg)](https://zenodo.org/badge/latestdoi/276616688)

# aire_madrid.R

## About
Descarga de datos de calidad de aire de las estaciones de medición de Madrid.

Download of air quality data from Madrid measurement stations.

[Raw data interpretation (ES)](https://datos.madrid.es/FWProjects/egob/Catalogo/MedioAmbiente/Aire/Ficheros/Interprete_ficheros_%20calidad_%20del_%20aire_global.pdf)

## Libraries needed
Libraries used in this script are: `data.table`, `stringr`, `XML`, `xml2`, `config`, `ggplot2`, `ggthemes` and `zoo`.

## Input data
[data files from Calidad del aire. https://datos.madrid.es/](https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=41e01e007c9db410VgnVCM2000000c205a0aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default)

## Outputs
* `data/processed/dt_aire_madrid.RDS` (raw format)
* `data/processed/dt_aire_madrid_sub.RDS` (friendly format) 

## Workflow

1. Load packages
2. Functions
    * `xts2ts`: xts_data xts time series data in daily, monthly, quarterly and yearly frequency,return ts object
3. Create and set the directory
4. Stations Info data, txt data structure.
5. Data download. Download the las year always for update, check if there are previous years.
6. Get the list of all files downloaded and keep only the txt files.
7. Adjust oct 2017 change in data structure.
8. Save the raw data as `dt_aire_madrid.RDS`.
9. Reshaping data and set friendly format. 🤗
10. Save the friendly format as `dt_aire_madrid_sub.RDS`
 
## Built With

* [R](https://www.r-project.org/) - R project ❤️

## Authors

* **Michal Kinel** - [michal0091](https://github.com/michal0091) 🤓
