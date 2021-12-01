# Code for editing and organizing the datasets that will be used in the analysis 

# This script will read in raw data from the input directory, and write clean
# datasets to the output directory.
# The final datasets will be: 
# -"trees_sno" has data for each tree measured in the field (diameter, height, species name...) 
# -"sites_sno" has geographic information of each sampling site (latitude, longitude)
# Both have the same site ids. 

# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)

#####################
### TREES DATASETS ###
#####################

# I made a trees and sites datasets for municipalities in Veracruz and Puebla that are 
# within SNO. Files are saved in the input directory and are called "infys0914_arbolado_ver.csv",
# infys0914_arbolado_pue.csv", "infys0914_conglomerados_ver.csv", and "infys0914_conglomerados_pue.csv"

#### 1. Read file ####
trees_oax <- read_csv("input/infys0914_arbolado_oax.csv")

#### 2. Edit file ####

# 2.1. Edit column names and make a tibble
trees_oax <- transmute(trees_oax, site = as.character(Conglomerado), # location id (each site contains 4 plots)
                   plot = as.character(Sitio), # plot number, there are four options per site (1,2,3 and 4),
                   tree_id = as.character(cgl_sit_arb), # location_plot_number of tree
                   year = as.numeric(Anio), # year of sampling
                   state = as.character(Estado), # state
                   municipality = as.character(Municipio), # municipality (like County)
                   prim_veg_infys = as.character(Veg_prim_levantada), # primary vegetation type
                   sec_veg_infys = as.character(Veg_sec_levantada), # type of secondary vegetation, options are: trees VSA, shrubs VSh, herbaceous VSaa (there are two more that I'm not sure what they stand for VSna VSn)
                   veg_inegi = as.character(Tipo_veg_SV), # primary vegetation type according to INEGI
                   veg_inegi_id = as.character(Cve_veg_SV), # ID of primary vegetation type according to INEGI, includes type of secondary vegetation 
                   tree_number = as.numeric(Arbol), # number assigned to each measured tree in each plot
                   family = as.character(Familia_APG), # taxonomic family
                   species = as.character(NombreCientifico_APG), # species
                   common_name = as.character(NomComun), # common name
                   status = as.character(Condicion), # alive, stump
                   longitude = as.numeric(X), # geographic coordinate-longitude
                   latitude = as.numeric(Y), # geographic coordinate-latitude
                   altitude = as.numeric(Altitud), # altitude
                   canopy_diam = as.numeric(DiametroCopa), # diameter of canopy in m
                   canopy_percentage = as.character(porc_CopaViva),# canopy proportion in relation to tree height
                   canopy_density = as.character(DensidadCopa), # canopy density
                   canopy_transparency = as.character(TransparenciaCopa), # canopy transparency
                   diam_base = as.numeric(DiametroBasal), # diameter of tree base in cm
                   life_form = as.character(Forma_Biologica_1), # life form: tree, shrub or herb
                   height = as.numeric(Altura_total), # height of tree in m
                   diam_bh = as.numeric(Diametro_normal), # diameter of tree at breast height in cm
                   basal_area = as.numeric(Area_basal), # basal area of tree in m 
                   canopy_area= as.numeric(Area_copa)) %>% # canopy area in m  
  as_tibble() # make it a tibble
str(trees_oax)

#### 3. Quality control ####

# 3.1. Explore each column and detect needed changes

# a) site, tree_id and year
trees_oax %>%
  filter(is.na(site) | is.na(plot)| is.na(tree_id) | is.na(year)) #no NAs in these columns

# b) state and municipality (Check for mispellings, accent marks, ñ, in municipio column)
trees_oax %>%
  filter(is.na(state) | is.na(municipality)) #no NAs in these two columns

trees_oax %>%
  group_by(state) %>%
  summarize(state_list = unique(state)) #Oaxaca must be the only state 

municipios <- trees_oax %>%
  group_by(municipality) %>%
  summarize(munlist = unique(municipality))

# **Changes needed**
# Calihualá -- Calihuala
# Chiquihuitlán de Benito Juar -- Chiquihuitlan de Benito Juarez
# Juchitán de Zaragoza -- Juchitan de Zaragoza
# Mártires de Tacubaya -- Martires de Tacubaya
# Mixistlán de la Reforma -- Mixistlan de la Reforma
# San Andres Huayápam -- San Andres Huayapam
# San Antonio Nanahuatípam -- San Antonio Nanahuatipam
# San Baltazar Chichicápam -- San Baltazar Chichicapam
# San Bartolome Yucuañe -- San Bartolome Yucuane
# San Francisco Cahuacuá -- San Francisco Cahuacua
# San Juan Ðumi -- San Juan Numi (San Juan Ñumí) 
# San Juan Tabaá -- San Juan Tabaa
# San Miguel Aloápam -- San Miguel Aloapam
# San Miguel Tilquiápam -- San Miguel Tilquiapam
# San Pedro Tidaá -- San Pedro Tidaa
# San Vicente Nuñu -- San Vicente Nunu
# Santa Maria Huazolotitlán -- Santa Maria Huazolotitlan
# Santa Maria Lachixío -- Santa Maria Lachixio
# Santa Maria Pápalo -- Santa Maria Papalo
# Santa Maria Peñoles -- Santa Maria Penoles
# Santiago Xiacuí -- Santiago Xiacui
# Santo Domingo Nuxaá -- Santo Domingo Nuxaa
# Santo Domingo Tonalá -- Santo Domingo Tonala
# Santo Domingo Xagacía -- Santo Domingo Xagacia
# Santos Reyes Pápalo -- Santos Reyes Papalo
# Silacayoápam -- Silacayoapam
# Villa de Tamazulápam del Pro -- Villa de Tamazulapam del Progreso
# San Juan Bautista Valle Naci -- San Juan Bautista Valle Nacional

# c) prim_veg_infys
prim_veg_infys <- trees_oax %>%
  group_by(prim_veg_infys) %>%
  summarize(prim_veg_infys_list = unique(prim_veg_infys), prim_veg_infys_count = sum(n())) 
# There are 31 veg types, one of them is NA

# d) prim_veg_infys
sec_veg_infys <- trees_oax %>%
  group_by(sec_veg_infys) %>%
  summarize(sec_veg_infys_list = unique(sec_veg_infys), sec_veg_infys_count = sum(n())) 
#There are 6 secondary veg types, one of them is NULL **must change to NA**

# e) veg_inegi
veg_inegi <- trees_oax %>%
  group_by(veg_inegi) %>%
  summarize(veg_inegi_list = unique(veg_inegi), veg_inegi_count = sum(n()))
#There are 22 vegetation types 

# f) veg_inegi_id
veg_inegi_id <- trees_oax %>%
  group_by(veg_inegi_id) %>%
  summarize(veg_inegi_id_list = unique(veg_inegi_id), veg_inegi_id_count = sum(n()))
#There are 50 id because secondary vegetation is included in different combinations with the 22 veg types

# g) tree_number
trees_oax %>%
  filter(is.na(tree_number)) #no NAs in tree_number column

# h) family
family <- trees_oax %>%
  group_by(family) %>%
  summarize(families = unique(family))
# There are 125 columns, two of them are NULL and ZZ Familia Desconocida.
# **Replace "NULL" and other unkown observations with NAs**

# i) species
trees_oax %>%
  filter(is.na(species)) #No NAs

sp <- trees_oax %>%
  group_by(species) %>%
  summarize(species_list = unique(species))
# There are 1139 observations, one of them is ZZ_Desconocido, **Replace it with NA**

# j) status
trees_oax %>%
  filter(is.na(status)) #No NAs
trees_oax %>%
  group_by(status) %>%
  summarize(status_list = unique(status), status_number = sum(n())) 
# There are four categories: 
# - dead (Muerto en pie: 7601),
# - stump with mark (Tocon con marca: 123),
# - stump without mark (Tocon sin marca: 4675),
# - alive (Vivo: 127692) 

# k) lat, lon and altitude
trees_oax %>%
  filter(is.na(longitude), is.na(latitude))
# There are 1453 observations without lat and lon
# **is it possible to get that information from the sites dataset?**

trees_oax %>%
  filter(is.na(altitude)) # There are not NAs in altitude column

# l) canopy information (diameter, percentage, density, transparency)
trees_oax %>%
  filter(is.na(canopy_diam)) #12,901 NAs
trees_oax %>%
  filter(is.na(canopy_percentage)) #1,026 NAs
trees_oax %>%
  filter(is.na(canopy_density)) #1,026 NAs
trees_oax %>%
  filter(is.na(canopy_transparency)) #1,026 NAs
# **Replace  n/a, NULL and -9999 by NAs**

# m) diam_base
trees_oax %>%
  filter(is.na(diam_base)) #124,132 observations lack diam_base. No n/a, -9999 nor NULL.

# n) life_form
trees_oax %>%
  filter(is.na(life_form)) #No NAs

trees_oax %>%
  group_by(life_form) %>%
  summarize(life_form_list = unique(life_form), life_form_number = sum(n()))
# There are ten categories, one of them is NULL
# **Replace NULL with NAs**

# o) height, diam_bh
trees_oax %>%
  filter(is.na(height) | is.na(diam_bh)) # No NAs in these two columns

trees_oax %>%
  arrange(height) # No 0s in height, smaller value is 0.3

trees_oax %>%
  arrange(diam_bh)# No 0s in diam_bh, smaller value is 7.5

# p) basal_area
trees_oax %>%
  filter(is.na(basal_area)) # 4,799 observations lack basal_area

# q) canopy_area
trees_oax %>%
  filter(is.na(canopy_area)) # 5,598 observations lack canopy_area

# 3.2. Edit columns in trees_oax dataset

trees_oax <- trees_oax %>% 
  # Remove ñ, accent marks, and wrong names in the municipality column
  mutate(municipality = ifelse(municipality == "Calihualá", "Calihuala", municipality),
         municipality = ifelse(municipality == "Chiquihuitlán de Benito Juar", "Chiquihuitlan de Benito Juarez", municipality),
         municipality = ifelse(municipality == "Juchitán de Zaragoza", "Juchitan de Zaragoza", municipality),
         municipality = ifelse(municipality == "Mártires de Tacubaya", "Martires de Tacubaya", municipality),
         municipality = ifelse(municipality == "Mixistlán de la Reforma", "Mixistlan de la Reforma", municipality),
         municipality = ifelse(municipality == "San Andres Huayápam", "San Andres Huayapam", municipality),
         municipality = ifelse(municipality == "San Antonio Nanahuatípam", "San Antonio Nanahuatipam", municipality),
         municipality = ifelse(municipality == "San Baltazar Chichicápam", "San Baltazar Chichicapam", municipality),
         municipality = ifelse(municipality == "San Bartolome Yucuañe", "San Bartolome Yucuane", municipality),
         municipality = ifelse(municipality == "San Francisco Cahuacuá", "San Francisco Cahuacua", municipality),
         municipality = ifelse(municipality == "San Juan Ðumi", "San Juan Numi", municipality),
         municipality = ifelse(municipality == "San Juan Tabaá", "San Juan Tabaa", municipality),
         municipality = ifelse(municipality == "San Miguel Aloápam", "San Miguel Aloapam", municipality),
         municipality = ifelse(municipality == "San Miguel Tilquiápam", "San Miguel Tilquiapam", municipality),
         municipality = ifelse(municipality == "San Pedro Tidaá", "San Pedro Tidaa", municipality),
         municipality = ifelse(municipality == "San Vicente Nuñu", "San Vicente Nunu", municipality),
         municipality = ifelse(municipality == "Santa Maria Huazolotitlán", "Santa Maria Huazolotitlan", municipality),
         municipality = ifelse(municipality == "Santa Maria Lachixío", "Santa Maria Lachixio", municipality),
         municipality = ifelse(municipality == "Santa Maria Pápalo", "Santa Maria Papalo", municipality),
         municipality = ifelse(municipality == "Santa Maria Peñoles", "Santa Maria Penoles", municipality),
         municipality = ifelse(municipality == "Santiago Xiacuí", "Santiago Xiacui", municipality),
         municipality = ifelse(municipality == "Santo Domingo Nuxaá", "Santo Domingo Nuxaa", municipality),
         municipality = ifelse(municipality == "Santo Domingo Tonalá", "Santo Domingo Tonala", municipality),
         municipality = ifelse(municipality == "Santo Domingo Xagacía", "Santo Domingo Xagacia", municipality),
         municipality = ifelse(municipality == "Santos Reyes Pápalo", "Santos Reyes Papalo", municipality),
         municipality = ifelse(municipality == "Silacayoápam", "Silacayoapam", municipality),
         municipality = ifelse(municipality == "Villa de Tamazulápam del Pro", "Villa de Tamazulapam del Progreso", municipality),
         municipality = ifelse(municipality == "San Juan Bautista Valle Naci", "San Juan Bautista Valle Nacional", municipality)) %>%
  # Replace NULLs, -9999s, ZZ and other unkowns by NAs
  mutate(sec_veg_infys = ifelse(sec_veg_infys == "NULL", NA, sec_veg_infys)) %>% 
  mutate(family = ifelse(family == "NULL" | family == "ZZ Familia Desconocida", NA, family)) %>%
  mutate(species = ifelse(species == "ZZ_Desconocido", NA, species)) %>%
  # Make a column for genus and another one for species
  separate(species, c("genus", "sp"), sep = " ") %>%
  mutate(common_name = ifelse(common_name == "NULL", NA, common_name)) %>%
  mutate(canopy_diam = ifelse(canopy_diam == "NULL" |
                                canopy_diam == "-9999" |
                                canopy_diam == "n/a", NA, canopy_diam),
         canopy_percentage = ifelse(canopy_percentage == "NULL" |
                                      canopy_percentage == "-9999" |
                                      canopy_percentage == "n/a", NA, canopy_percentage),
         canopy_density = ifelse(canopy_density == "NULL" |
                                   canopy_density == "-9999" |
                                   canopy_density == "n/a", NA, canopy_density),
         canopy_transparency = ifelse(canopy_transparency == "NULL" |
                                        canopy_transparency == "-9999" |
                                        canopy_transparency == "n/a", NA, canopy_transparency)) %>%
  mutate(life_form = ifelse(life_form == "NULL", NA, life_form))


# 2.3. Check for outliers
plot(trees_oax$height)
plot(trees_oax$diam_bh)
plot(trees_oax$diam_bh, trees_oax$height)
plot(trees_oax$longitude, trees_oax$latitude) # There is one point at -90 lon that is probably wrong **filter out this datapoint**
plot(trees_oax$altitude)

##Read files

trees_ver <- read_csv("input/infys0914_arbolado_ver.csv")
trees_pue <- read_csv("input/infys0914_arbolado_pue.csv")

##Edit file columns and make it a tibble

trees_ver <- transmute(trees_ver, site = as.character(Conglomerado), #location id (each site contains 4 plots)
                       plot = as.character(Sitio), #plot number, there are four options per site (1,2,3 and 4),
                       tree_id = as.character(cgl_sit_arb), #location_plot_number of tree
                       year = as.numeric(Anio), #year of sampling
                       state = as.character(Estado), #State
                       municipality = as.character(Municipio), #municipality (like County)
                       prim_veg_infys = as.character(Veg_prim_levantada), #Primary vegetation type
                       sec_veg_infys = as.character(Veg_sec_levantada), #type of secondary vegetation, options are: trees VSA, shrubs VSh, herbaceous VSaa (there are two more that I'm not sure what they stand for VSna VSn)
                       veg_inegi = as.character(Tipo_veg_SV), #Primary vegetation type according to INEGI
                       veg_inegi_id = as.character(Cve_veg_SV), #ID of primary vegetation type according to INEGI, includes type of secondary vegetation 
                       tree_number = as.numeric(Arbol), #number assigned to each measured tree in each plot
                       family = as.character(Familia_APG), #taxonomic family
                       species = as.character(NombreCientifico_APG), #species
                       common_name = as.character(NomComun), #Common name
                       status = as.character(Condicion), #alive, stump
                       longitude = as.numeric(X), #geographic coordinate-longitude
                       latitude = as.numeric(Y), #geographic coordinate-latitude
                       altitude = as.numeric(Altitud), #altitude
                       canopy_diam = as.numeric(DiametroCopa), #diameter of canopy in m
                       canopy_percentage = as.character(porc_CopaViva),#canopy proportion in relation to tree height
                       canopy_density = as.character(DensidadCopa), #canopy density
                       canopy_transparency = as.character(TransparenciaCopa), #canopy transparency
                       diam_base = as.numeric(DiametroBasal), #diameter of tree base in cm
                       life_form = as.character(Forma_Biologica_1), #life form: tree, shrub or herb
                       height = as.numeric(Altura_total), #height of tree in m
                       diam_bh = as.numeric(Diametro_normal), #diameter of tree at breast height in cm
                       basal_area = as.numeric(Area_basal), #basal area of tree in m (?)
                       canopy_area= as.numeric(Area_copa)) %>% #canopy area in m  
  as_tibble() #make it a tibble
str(trees_ver)

trees_pue <- transmute(trees_pue, site = as.character(Conglomerado), #location id (each site contains 4 plots)
                       plot = as.character(Sitio), #plot number, there are four options per site (1,2,3 and 4),
                       tree_id = as.character(cgl_sit_arb), #location_plot_number of tree
                       year = as.numeric(Anio), #year of sampling
                       state = as.character(Estado), #State
                       municipality = as.character(Municipio), #municipality (like County)
                       prim_veg_infys = as.character(Veg_prim_levantada), #Primary vegetation type
                       sec_veg_infys = as.character(Veg_sec_levantada), #type of secondary vegetation, options are: trees VSA, shrubs VSh, herbaceous VSaa (there are two more that I'm not sure what they stand for VSna VSn)
                       veg_inegi = as.character(Tipo_veg_SV), #Primary vegetation type according to INEGI
                       veg_inegi_id = as.character(Cve_veg_SV), #ID of primary vegetation type according to INEGI, includes type of secondary vegetation 
                       tree_number = as.numeric(Arbol), #number assigned to each measured tree in each plot
                       family = as.character(Familia_APG), #taxonomic family
                       species = as.character(NombreCientifico_APG), #species
                       common_name = as.character(NomComun), #Common name
                       status = as.character(Condicion), #alive, stump
                       longitude = as.numeric(X), #geographic coordinate-longitude
                       latitude = as.numeric(Y), #geographic coordinate-latitude
                       altitude = as.numeric(Altitud), #altitude
                       canopy_diam = as.numeric(DiametroCopa), #diameter of canopy in m
                       canopy_percentage = as.character(porc_CopaViva),#canopy proportion in relation to tree height
                       canopy_density = as.character(DensidadCopa), #canopy density
                       canopy_transparency = as.character(TransparenciaCopa), #canopy transparency
                       diam_base = as.numeric(DiametroBasal), #diameter of tree base in cm
                       life_form = as.character(Forma_Biologica_1), #life form: tree, shrub or herb
                       height = as.numeric(Altura_total), #height of tree in m
                       diam_bh = as.numeric(Diametro_normal), #diameter of tree at breast height in cm
                       basal_area = as.numeric(Area_basal), #basal area of tree in m (?)
                       canopy_area= as.numeric(Area_copa)) %>% #canopy area in m  
  as_tibble() #make it a tibble
str(trees_pue)

#Quality control

#Replace NULLs, -9999s, ZZ and other unkowns by NAs
trees_ver <- trees_ver %>%
  mutate(sec_veg_infys = ifelse(sec_veg_infys == "NULL", NA, sec_veg_infys)) %>% 
  mutate(family = ifelse(family == "NULL" | family == "ZZ Familia Desconocida", NA, family)) %>%
  mutate(species = ifelse(species == "ZZ_Desconocido", NA, species)) %>%
  #make a column for genus and another one for species
  separate(species, c("genus", "sp"), sep = " ") %>%
  mutate(common_name = ifelse(common_name == "NULL", NA, common_name)) %>%
  mutate(canopy_diam = ifelse(canopy_diam == "NULL" |
                                canopy_diam == "-9999" |
                                canopy_diam == "n/a", NA, canopy_diam),
         canopy_percentage = ifelse(canopy_percentage == "NULL" |
                                      canopy_percentage == "-9999" |
                                      canopy_percentage == "n/a", NA, canopy_percentage),
         canopy_density = ifelse(canopy_density == "NULL" |
                                   canopy_density == "-9999" |
                                   canopy_density == "n/a", NA, canopy_density),
         canopy_transparency = ifelse(canopy_transparency == "NULL" |
                                        canopy_transparency == "-9999" |
                                        canopy_transparency == "n/a", NA, canopy_transparency)) %>%
  mutate(life_form = ifelse(life_form == "NULL", NA, life_form))

#Replace NULLs, -9999s, ZZ and other unkowns by NAs
trees_pue <- trees_pue %>%
  mutate(sec_veg_infys = ifelse(sec_veg_infys == "NULL", NA, sec_veg_infys)) %>% 
  mutate(family = ifelse(family == "NULL" | family == "ZZ Familia Desconocida", NA, family)) %>%
  mutate(species = ifelse(species == "ZZ_Desconocido", NA, species)) %>%
  #make a column for genus and another one for species
  separate(species, c("genus", "sp"), sep = " ") %>%
  mutate(common_name = ifelse(common_name == "NULL", NA, common_name)) %>%
  mutate(canopy_diam = ifelse(canopy_diam == "NULL" |
                                canopy_diam == "-9999" |
                                canopy_diam == "n/a", NA, canopy_diam),
         canopy_percentage = ifelse(canopy_percentage == "NULL" |
                                      canopy_percentage == "-9999" |
                                      canopy_percentage == "n/a", NA, canopy_percentage),
         canopy_density = ifelse(canopy_density == "NULL" |
                                   canopy_density == "-9999" |
                                   canopy_density == "n/a", NA, canopy_density),
         canopy_transparency = ifelse(canopy_transparency == "NULL" |
                                        canopy_transparency == "-9999" |
                                        canopy_transparency == "n/a", NA, canopy_transparency)) %>%
  mutate(life_form = ifelse(life_form == "NULL", NA, life_form))

# Replace ñ with n 
trees_pue <- trees_pue %>%
  mutate(municipality = ifelse(municipality == "Cañada Morelos", "Canada Morelos", municipality),
         municipality = ifelse(municipality == "San Antonio Cañada", "San Antonio Canada", municipality))

# Merge all trees datasets

trees_pue_ver <- bind_rows(trees_pue, trees_ver)
trees <- bind_rows(trees_oax, trees_pue_ver)

rm(trees_pue_ver)

######################
### SITES DATASETS ###
######################

#### 1. Read file ####
sites_oax <- read_csv("input/infys0914_conglomerados_oax.csv")

#### 2. Edit file ####

# 2.1. Edit column names and make a tibble

sites_oax <- transmute(sites_oax, site = as.character(Conglomerado), # location id (contains 4 plots)
            year = as.numeric(Anio), # year of sampling
            date = as.Date(Fecha_cgl, format = "%d/%m/%Y"), # date of data collection
            dec_date = decimal_date(date), # decimal date
            state = as.character(Estado), # state
            municipality = as.character(Municipio), # municipality (like County)
            veg_type = as.character(Formato), # wheter forest, rainforest or other type of vegetation
            site_type = as.character(Tipo_cgl_estandar), # inicial (if sampled where planned) o reemplazo (if a new site was selected)
            monitoring = as.character(Monitoreo), # whether the site is being long-term monitored
            sampled = as.character(Muestreado), # whether sampling was conducted in site or not 
            vegetation_cover = as.character(Cubierta_Vegetal),
            land_tenure = as.character(Tenencia),
            altitude = as.numeric(Altitud),
            average_slope = as.numeric(Pendiete_promedio),
            landform = as.character(Fisiografia),
            aspect = as.character(Exposicion),
            primary_veg = as.character(Veg_prim_levantada),
            sec_veg = as.character(Veg_sec_levanatda), # type of secondary vegetation, options are: trees, shrubs, herbaceous (there are two more that I'm not sure what they stand for)
            plots_by_site = as.numeric(Sitios_x_cgl), # number of plot per site
            plot1 = as.numeric(Sitio_1), # if plot was sampled= 1, if not=0
            plot2 = as.numeric(Sitio_2), # if plot was sampled= 1, if not=0
            plot3 = as.numeric(Sitio_3), # if plot was sampled= 1, if not=0
            plot4 = as.numeric(Sitio_4), # if plot was sampled= 1, if not=0
            longitude = as.numeric(X), # geographic coordinate-longitude
            latitude = as.numeric(Y)) %>% # geographic coordinate-latitude
  as_tibble() # make it a tibble
str(sites_oax)

#### 3. Quality control ####

# 3.1. Explore each column and detect needed changes

nosampledsites <- sites_oax %>%
  filter(sampled == 0)
# There are 318 sites that were meant to be sampled but were not due to different reasons (social, topography)
# It's unclear which observations are real 0s, and which ones are just NAs 
# (there are many options: NULL, No aplica, NA)
# **Remove non sampled sites**

sites_oax %>%
  group_by(veg_type) %>%
  summarize(veg_type_list = unique(veg_type)) 
# **Remove veg_type column (it's non-informative)**

# primary and secondary vegetation columns have NULL values **Replace NULL by NA**
# **Replace No aplica in landform and aspect columns with NA**
# **Edit municipality column **

# 3.2. Edit columns in sites_oax dataset

# Remove non sampled sites_oax
sites_oax <- sites_oax %>%
  #filter(!sampled == 0) %>%
  select(-veg_type) %>%
  # Replace NULLs by NAs in secondary and primary vegetation
  mutate(primary_veg = ifelse(primary_veg == "NULL", NA, primary_veg)) %>%
  mutate(sec_veg = ifelse(sec_veg == "NULL", NA, sec_veg)) %>%
  mutate(landform = ifelse(landform == "No aplica", NA, landform)) %>%
  mutate(aspect = ifelse(aspect == "No aplica", NA, aspect)) %>%
  # Remove ñ, accent marks, and wrong names in the municipality column
  mutate(municipality = ifelse(municipality == "Calihualá", "Calihuala", municipality),
         municipality = ifelse(municipality == "Chiquihuitlán de Benito Juar", "Chiquihuitlan de Benito Juarez", municipality),
         municipality = ifelse(municipality == "Juchitán de Zaragoza", "Juchitan de Zaragoza", municipality),
         municipality = ifelse(municipality == "Mártires de Tacubaya", "Martires de Tacubaya", municipality),
         municipality = ifelse(municipality == "Mixistlán de la Reforma", "Mixistlan de la Reforma", municipality),
         municipality = ifelse(municipality == "San Andres Huayápam", "San Andres Huayapam", municipality),
         municipality = ifelse(municipality == "San Antonio Nanahuatípam", "San Antonio Nanahuatipam", municipality),
         municipality = ifelse(municipality == "San Baltazar Chichicápam", "San Baltazar Chichicapam", municipality),
         municipality = ifelse(municipality == "San Bartolome Yucuañe", "San Bartolome Yucuane", municipality),
         municipality = ifelse(municipality == "San Francisco Cahuacuá", "San Francisco Cahuacua", municipality),
         municipality = ifelse(municipality == "San Juan Ðumi", "San Juan Numi", municipality),
         municipality = ifelse(municipality == "San Juan Tabaá", "San Juan Tabaa", municipality),
         municipality = ifelse(municipality == "San Miguel Aloápam", "San Miguel Aloapam", municipality),
         municipality = ifelse(municipality == "San Miguel Tilquiápam", "San Miguel Tilquiapam", municipality),
         municipality = ifelse(municipality == "San Pedro Tidaá", "San Pedro Tidaa", municipality),
         municipality = ifelse(municipality == "San Vicente Nuñu", "San Vicente Nunu", municipality),
         municipality = ifelse(municipality == "Santa Maria Huazolotitlán", "Santa Maria Huazolotitlan", municipality),
         municipality = ifelse(municipality == "Santa Maria Lachixío", "Santa Maria Lachixio", municipality),
         municipality = ifelse(municipality == "Santa Maria Pápalo", "Santa Maria Papalo", municipality),
         municipality = ifelse(municipality == "Santa Maria Peñoles", "Santa Maria Penoles", municipality),
         municipality = ifelse(municipality == "Santiago Xiacuí", "Santiago Xiacui", municipality),
         municipality = ifelse(municipality == "Santo Domingo Nuxaá", "Santo Domingo Nuxaa", municipality),
         municipality = ifelse(municipality == "Santo Domingo Tonalá", "Santo Domingo Tonala", municipality),
         municipality = ifelse(municipality == "Santo Domingo Xagacía", "Santo Domingo Xagacia", municipality),
         municipality = ifelse(municipality == "Santos Reyes Pápalo", "Santos Reyes Papalo", municipality),
         municipality = ifelse(municipality == "Silacayoápam", "Silacayoapam", municipality),
         municipality = ifelse(municipality == "Villa de Tamazulápam del Pro", "Villa de Tamazulapam del Progreso", municipality),
         municipality = ifelse(municipality == "San Juan Bautista Valle Naci", "San Juan Bautista Valle Nacional", municipality))
 
# 2.3. Check for outliers

plot(sites_oax$altitude)
plot(sites_oax$longitude, sites_oax$latitude)  
plot(sites_oax$dec_date)
plot(sites_oax$average_slope)
sites_oax %>%
  group_by(sec_veg) %>%
  summarize(sec_veg_list = unique(sec_veg))

# Read files

sites_ver <- read_csv("input/infys0914_conglomerados_ver.csv")
sites_pue <- read_csv("input/infys0914_conglomerados_pue.csv")

##Edit file
#Re-name columns

sites_ver <- transmute(sites_ver, site = as.character(Conglomerado), #location id (contains 4 plots)
                       year = as.numeric(Anio), #year of sampling
                       date = as.Date(Fecha_cgl, format = "%d/%m/%Y"), #date of data collection
                       dec_date = decimal_date(date), #decimal date
                       state = as.character(Estado), #State
                       municipality = as.character(Municipio), #municipality (like County)
                       veg_type = as.character(Formato), #wheter forest, rainforest or other type of vegetation
                       site_type = as.character(Tipo_cgl_estandar), #Inicial o reemplazo --look for definition
                       monitoring = as.character(Monitoreo), #whether the site is being long-term monitored
                       sampled = as.character(Muestreado), #whether samplingwas conducted in site 
                       vegetation_cover = as.character(Cubierta_Vegetal),
                       land_tenure = as.character(Tenencia),
                       altitude = as.numeric(Altitud),
                       average_slope = as.numeric(Pendiete_promedio),
                       landform = as.character(Fisiografia),
                       aspect = as.character(Exposicion),
                       primary_veg = as.character(Veg_prim_levantada),
                       sec_veg = as.character(Veg_sec_levanatda), #type of secondary vegetation, options are: trees, shrubs, herbaceous (there are two more that I'm not sure what they stand for)
                       plots_by_site = as.numeric(Sitios_x_cgl), #number of plot per site
                       plot1 = as.numeric(Sitio_1), #If plot was sampled= 1, if not=0
                       plot2 = as.numeric(Sitio_2), #If plot was sampled= 1, if not=0
                       plot3 = as.numeric(Sitio_3), #If plot was sampled= 1, if not=0
                       plot4 = as.numeric(Sitio_4), #If plot was sampled= 1, if not=0
                       longitude = as.numeric(X), #geographic coordinate-longitude
                       latitude = as.numeric(Y)) %>% #geographic coordinate-latitude
  as_tibble() #make it a tibble

str(sites_ver)

sites_pue <- transmute(sites_pue, site = as.character(Conglomerado), #location id (contains 4 plots)
                       year = as.numeric(Anio), #year of sampling
                       date = as.Date(Fecha_cgl, format = "%d/%m/%Y"), #date of data collection
                       dec_date = decimal_date(date), #decimal date
                       state = as.character(Estado), #State
                       municipality = as.character(Municipio), #municipality (like County)
                       veg_type = as.character(Formato), #wheter forest, rainforest or other type of vegetation
                       site_type = as.character(Tipo_cgl_estandar), #Inicial o reemplazo --look for definition
                       monitoring = as.character(Monitoreo), #whether the site is being long-term monitored
                       sampled = as.character(Muestreado), #whether samplingwas conducted in site 
                       vegetation_cover = as.character(Cubierta_Vegetal),
                       land_tenure = as.character(Tenencia),
                       altitude = as.numeric(Altitud),
                       average_slope = as.numeric(Pendiete_promedio),
                       landform = as.character(Fisiografia),
                       aspect = as.character(Exposicion),
                       primary_veg = as.character(Veg_prim_levantada),
                       sec_veg = as.character(Veg_sec_levanatda), #type of secondary vegetation, options are: trees, shrubs, herbaceous (there are two more that I'm not sure what they stand for)
                       plots_by_site = as.numeric(Sitios_x_cgl), #number of plot per site
                       plot1 = as.numeric(Sitio_1), #If plot was sampled= 1, if not=0
                       plot2 = as.numeric(Sitio_2), #If plot was sampled= 1, if not=0
                       plot3 = as.numeric(Sitio_3), #If plot was sampled= 1, if not=0
                       plot4 = as.numeric(Sitio_4), #If plot was sampled= 1, if not=0
                       longitude = as.numeric(X), #geographic coordinate-longitude
                       latitude = as.numeric(Y)) %>% #geographic coordinate-latitude
  as_tibble() #make it a tibble
str(sites_pue)

#Quality control

#Remove non sampled sites

#Remove non sampled sites
sites_ver <- sites_ver %>%
  #filter(!sampled == 0) %>%
  select(-veg_type) %>%
  #Replace NULLs by NAs in secondary and primary vegetation
  mutate(primary_veg = ifelse(primary_veg == "NULL", NA, primary_veg)) %>%
  mutate(sec_veg = ifelse(sec_veg == "NULL", NA, sec_veg)) %>%
  mutate(landform = ifelse(landform == "No aplica", NA, landform)) %>%
  mutate(aspect = ifelse(aspect == "No aplica", NA, aspect))

sites_pue <- sites_pue %>%
  #filter(!sampled == 0) %>%
  select(-veg_type) %>%
  #Replace NULLs by NAs in secondary and primary vegetation
  mutate(primary_veg = ifelse(primary_veg == "NULL", NA, primary_veg)) %>%
  mutate(sec_veg = ifelse(sec_veg == "NULL", NA, sec_veg)) %>%
  mutate(landform = ifelse(landform == "No aplica", NA, landform)) %>%
  mutate(aspect = ifelse(aspect == "No aplica", NA, aspect))

#Replace ñ with n 
sites_pue <- sites_pue %>%
  mutate(municipality = ifelse(municipality == "Cañada Morelos", "Canada Morelos", municipality),
         municipality = ifelse(municipality == "San Antonio Cañada", "San Antonio Canada", municipality))

#### 5. Merge all trees and all sites datasets ####

sites_pue_ver <- bind_rows(sites_pue, sites_ver)
sites <- bind_rows(sites_oax, sites_pue_ver)

rm(sites_pue_ver)

####################################
### SIERRA NORTE de OAXACA (SNO) ###
####################################

# Script to select sites within SNO

#### 1. Read file ####

sno_mun <- read_csv("input/bmmix_municipios.csv") 
# This file is a list of municipios in the Sierra
# I got this list making a select by location search in ArcPro

#### 2. Edit names an make it a tibble ####

sno_mun <- sno_mun %>% 
  transmute(municipality = as.character(NOM_MUN),
            mun_id = as.numeric(CVE_MUN),
            state_id = as.numeric(CVE_ENT)) %>%
  as_tibble()
str(sno_mun)

#### 3. Quality control ####

sno_mun <- sno_mun %>% 
  # Remove ñ, accent marks, and wrong names in the municipality column 
  # **Change:**
  # Cañada Morelos -- Canada Morelos 
  # San Antonio Cañada -- San Antonio Canada
  mutate(municipality = ifelse(municipality == "Cañada Morelos", "Canada Morelos", municipality),
         municipality = ifelse(municipality == "San Antonio Cañada", "San Antonio Canada", municipality))

#### 4. Subset trees and sites datasets ####

# Using sno_mun, subset trees and sites datasets to get sites within SNO

sites_sno <- left_join(sno_mun, sites, by = "municipality")

sites_sno %>%
  group_by(municipality) %>%
  summarize(municipality_list = unique(municipality))
# These two datasets share only 108 municipalities
# which means that, either not all municipalities in SNO were sampled,
# or municipality names are somehow different and impossible to join (worse case scenario)
# or a combination of both

plot(sites_sno$longitude, sites_sno$latitude)

nonsampledmun <- sites_sno %>% 
  filter(is.na(sampled))
# Municipalities that are not in infys list:

# San Pedro Cajonos
# San Francisco Huehuetlan
# Villa Hidalgo
# Ateixtlahuaca
# San Pedro Ocopetatillo
# San Mateo Yoloxochitlan
# San Martin Toxpalan
# San Lorenzo Cuaunecuiltitla
# Santa Cruz Acatepec
# Tanetze de Zaragoza
# Santiago Zoochila
# Santiago Lalopa
# Santa Maria Tlalixtac
# Santa Maria Temaxcalapa
# San Pedro Yaneri
# San Pablo Yaganiza
# San Miguel Santa Flor
# San Melchor Betaza
# San Mateo Cajonos
# San Juan Juquila Vijanos is in the original list but wasn't sampled
# San Juan Evangelista Analco
# San Bartolome Zoogocho
# Natividad
# Eloxochitlan de Flores Magon
# Santiago Texcalcingo
# Guelatao de Juarez
# Santa Ana del Valle
# San Pablo Etla
# San Miguel del Rio
# There are other mun that are not in Oaxaca, but in Puebla and Veracruz 

# Fix municipalities names from Oaxaca:
# **San Juan Bautista Valle Naci -- San Juan Bautista Valle Nacional**
# **Tamazulapam del Espiritu San -- Tamazulapam del Espiritu Santo**
# **San Juan Yae -- San Juan Yaee**
# **San Juan Bautista Tlacoatzin -- San Juan Bautista Tlacoatzintepec**
# **San Baltazar Yatzachi el Baj -- San Baltazar Yatzachi el Bajo**
# **San Andres Yaá -- San Andres Yaa**
# **Cuyamecalco Villa de Zaragoz -- Cuyamecalco Villa de Zaragoza**

trees <- trees %>%
  mutate(municipality = ifelse(municipality == "Tamazulapam del Espiritu San", "Tamazulapam del Espiritu Santo", municipality),
         municipality = ifelse(municipality == "San Juan Yae", "San Juan Yaee", municipality),
         municipality = ifelse(municipality == "San Juan Bautista Tlacoatzin", "San Juan Bautista Tlacoatzintepec", municipality),
         municipality = ifelse(municipality == "San Baltazar Yatzachi el Baj", "San Baltazar Yatzachi el Bajo", municipality),
         municipality = ifelse(municipality == "San Andres Yaá", "San Andres Yaa", municipality),
         municipality = ifelse(municipality == "Cuyamecalco Villa de Zaragoz", "Cuyamecalco Villa de Zaragoza", municipality))

sites <- sites %>%
  mutate(municipality = ifelse(municipality == "Tamazulapam del Espiritu San", "Tamazulapam del Espiritu Santo", municipality),
         municipality = ifelse(municipality == "San Juan Yae", "San Juan Yaee", municipality),
         municipality = ifelse(municipality == "San Juan Bautista Tlacoatzin", "San Juan Bautista Tlacoatzintepec", municipality),
         municipality = ifelse(municipality == "San Baltazar Yatzachi el Baj", "San Baltazar Yatzachi el Bajo", municipality),
         municipality = ifelse(municipality == "San Andres Yaá", "San Andres Yaa", municipality),
         municipality = ifelse(municipality == "Cuyamecalco Villa de Zaragoz", "Cuyamecalco Villa de Zaragoza", municipality))

# Now the left_join should result in a shorter list of non sampled municipalities:

sites_sno2 <- left_join(sno_mun, sites, by = "municipality")
sites_sno2 %>%
  group_by(municipality) %>%
  summarize(municipality_list = unique(municipality))

nonsampledmun2 <- sites_sno2 %>% 
  filter(is.na(sampled))

# It worked! Now I will work with the samples from Veracruz and Puebla
# I will make a list with the municipios from Veracruz and Puebla that are whithin SNO 
# (available in the nonsampledmun2 file)

mun_pue <- nonsampledmun2 %>%
  filter(state_id == 21) %>%
  select(municipality)
mun_pue <- as.list(mun_pue)

mun_ver <- nonsampledmun2 %>%
  filter(state_id == 30) %>%
  select(municipality)
mun_ver <- as.list(mun_ver)

#Subset sites and trees dataset to SNO

sites_sno3 <- left_join(sno_mun, sites, by = "municipality")
sites_sno3 %>%
  group_by(municipality) %>%
  summarize(municipality_list = unique(municipality))

#These are the municipalities that are not included in infys 
#(53 municipalities = 44 from Oaxaca, 1 from Puebla and 8 from Veracruz) 
#(they do not exist in the dataset --other sites were not sampled ("sampled" = 0)):
nonsampledmun3 <- sites_sno3 %>% 
  filter(is.na(sampled))

###FINAL SITES DATASET:
sites_sno <- left_join(sno_mun, sites, by = "municipality") %>%
  filter(!is.na(sampled)) #Filter out non-sampled sites

###FINAL TREES DATASET:
trees_sno <- left_join(sno_mun, trees, by = "municipality")
trees_sno <- trees_sno %>%
  filter(!is.na(site)) #Filter out non-sampled sites


#####################
### PLOTS DATASET ###
#####################

#### 6. Make plots_sno dataset ####

plots_sno <- trees_sno %>%
  group_by(site, plot) %>%
  summarize(state = unique(state),
            state_id = unique(state_id),
            municipality = unique(municipality),
            mun_id = unique(mun_id),
            prim_veg_infys = unique(prim_veg_infys),
            sec_veg_infys = unique(sec_veg_infys),
            veg_inegi = unique(veg_inegi),
            veg_inegi_id = unique(veg_inegi_id),
            longitude = unique(longitude),
            latitude = unique(latitude),
            altitude = unique(altitude))
plots_sno <- as_tibble(plots_sno)

#########################
### EPIPHYTES DATASET ###
#########################

#### 7. Epiphytes dataset ####

epiphytes <- read_csv("input/epiphytes.csv") 
head(epiphytes)
epiphytes <- epiphytes %>%
  select(-'...1') %>%
  transmute(site = as.character(Conglomerado),
            year = as.numeric(Anio),
            state = as.character(Estado),
            municipality = as.character(Municipio),
            epiphyte_type = as.character(Tipo_Epifita),
            on_trunk = as.character(Presencia_Troncos),
            on_branches = as.character(Presencia_Ramas))

# Remove ñ, accent marks, and wrong names in the municipality column

epiphytes <- epiphytes %>% 
  mutate(municipality = ifelse(municipality == "Calihualá", "Calihuala", municipality),
       municipality = ifelse(municipality == "Chiquihuitlán de Benito Juar", "Chiquihuitlan de Benito Juarez", municipality),
       municipality = ifelse(municipality == "Juchitán de Zaragoza", "Juchitan de Zaragoza", municipality),
       municipality = ifelse(municipality == "Mártires de Tacubaya", "Martires de Tacubaya", municipality),
       municipality = ifelse(municipality == "Mixistlán de la Reforma", "Mixistlan de la Reforma", municipality),
       municipality = ifelse(municipality == "San Andres Huayápam", "San Andres Huayapam", municipality),
       municipality = ifelse(municipality == "San Antonio Nanahuatípam", "San Antonio Nanahuatipam", municipality),
       municipality = ifelse(municipality == "San Baltazar Chichicápam", "San Baltazar Chichicapam", municipality),
       municipality = ifelse(municipality == "San Bartolome Yucuañe", "San Bartolome Yucuane", municipality),
       municipality = ifelse(municipality == "San Francisco Cahuacuá", "San Francisco Cahuacua", municipality),
       municipality = ifelse(municipality == "San Juan Ðumi", "San Juan Numi", municipality),
       municipality = ifelse(municipality == "San Juan Tabaá", "San Juan Tabaa", municipality),
       municipality = ifelse(municipality == "San Miguel Aloápam", "San Miguel Aloapam", municipality),
       municipality = ifelse(municipality == "San Miguel Tilquiápam", "San Miguel Tilquiapam", municipality),
       municipality = ifelse(municipality == "San Pedro Tidaá", "San Pedro Tidaa", municipality),
       municipality = ifelse(municipality == "San Vicente Nuñu", "San Vicente Nunu", municipality),
       municipality = ifelse(municipality == "Santa Maria Huazolotitlán", "Santa Maria Huazolotitlan", municipality),
       municipality = ifelse(municipality == "Santa Maria Lachixío", "Santa Maria Lachixio", municipality),
       municipality = ifelse(municipality == "Santa Maria Pápalo", "Santa Maria Papalo", municipality),
       municipality = ifelse(municipality == "Santa Maria Peñoles", "Santa Maria Penoles", municipality),
       municipality = ifelse(municipality == "Santiago Xiacuí", "Santiago Xiacui", municipality),
       municipality = ifelse(municipality == "Santo Domingo Nuxaá", "Santo Domingo Nuxaa", municipality),
       municipality = ifelse(municipality == "Santo Domingo Tonalá", "Santo Domingo Tonala", municipality),
       municipality = ifelse(municipality == "Santo Domingo Xagacía", "Santo Domingo Xagacia", municipality),
       municipality = ifelse(municipality == "Santos Reyes Pápalo", "Santos Reyes Papalo", municipality),
       municipality = ifelse(municipality == "Silacayoápam", "Silacayoapam", municipality),
       municipality = ifelse(municipality == "Villa de Tamazulápam del Pro", "Villa de Tamazulapam del Progreso", municipality),
       municipality = ifelse(municipality == "San Juan Bautista Valle Naci", "San Juan Bautista Valle Nacional", municipality),
       municipality = ifelse(municipality == "Cañada Morelos", "Canada Morelos", municipality),
       municipality = ifelse(municipality == "San Antonio Cañada", "San Antonio Canada", municipality))

# Join sno_mun and epiphytes to select municipalities in epiphytes within SNO

epiphytes_sno <- left_join(sno_mun, epiphytes, by = "municipality")

epiphytes_sno %>%
  group_by(municipality) %>%
  summarize(mun_list = unique(municipality))

# Edit on_trunk and on_branches columns 

epiphytes_sno %>%
  group_by(on_trunk) %>%
  summarize(on_trunk_list = unique(on_trunk))

#on_trunk_list       
#<chr>                <chr>               
#  1 Abundante 15 - 40 %   
#  2 Escasa < 15%                 
#  3 Muy Abundante > 40 % 
#  4 Nula presencia             
#5 NA                     

epiphytes_sno <- epiphytes_sno %>%
  mutate(on_trunk = ifelse(on_trunk == "Abundante 15 - 40 %", "Abundant", on_trunk),
         on_trunk = ifelse(on_trunk == "Escasa < 15%", "Scarce", on_trunk),
         on_trunk = ifelse(on_trunk == "Muy Abundante > 40 %", "Very abundant", on_trunk),
         on_trunk = ifelse(on_trunk == "Nula presencia", "Absent", on_trunk))

epiphytes_sno %>%
  group_by(on_branches) %>%
  summarize(on_branches_list = unique(on_branches))

epiphytes_sno <- epiphytes_sno %>%
  mutate(on_branches = ifelse(on_branches == "Abundante 15 - 40 %", "Abundant", on_branches),
         on_branches = ifelse(on_branches == "Escasa < 15%", "Scarce", on_branches),
         on_branches = ifelse(on_branches == "Muy Abundante > 40 %", "Very abundant", on_branches),
         on_branches = ifelse(on_branches == "Nula presencia", "Absent", on_branches))

epiphytes_sno <- epiphytes_sno %>%
  mutate(epiphyte_type2 = as.character(epiphyte_type)) %>%
  filter(!is.na(site))

epiphytes_sno <- epiphytes_sno %>%
  mutate(epiphyte_type = ifelse(epiphyte_type == "Bromeliaceas", "bromelids_trunk", epiphyte_type),
         epiphyte_type = ifelse(epiphyte_type == "Cactaceas", "cacti_trunk", epiphyte_type),
         epiphyte_type = ifelse(epiphyte_type == "Helechos", "ferns_trunk", epiphyte_type),
         epiphyte_type = ifelse(epiphyte_type == "Liquenes", "lichens_trunk", epiphyte_type),
         epiphyte_type = ifelse(epiphyte_type == "Musgos", "moss_trunk", epiphyte_type),
         epiphyte_type = ifelse(epiphyte_type == "Orquideas", "orchids_trunk", epiphyte_type),
         epiphyte_type = ifelse(epiphyte_type == "Otras", "others_trunk", epiphyte_type)) %>%
  mutate(epiphyte_type2 = ifelse(epiphyte_type2 == "Bromeliaceas", "bromelids_branches", epiphyte_type2),
         epiphyte_type2 = ifelse(epiphyte_type2 == "Cactaceas", "cacti_branches", epiphyte_type2),
         epiphyte_type2 = ifelse(epiphyte_type2 == "Helechos", "ferns_branches", epiphyte_type2),
         epiphyte_type2 = ifelse(epiphyte_type2 == "Liquenes", "lichens_branches", epiphyte_type2),
         epiphyte_type2 = ifelse(epiphyte_type2 == "Musgos", "moss_branches", epiphyte_type2),
         epiphyte_type2 = ifelse(epiphyte_type2 == "Orquideas", "orchids_branches", epiphyte_type2),
         epiphyte_type2 = ifelse(epiphyte_type2 == "Otras", "others_branches", epiphyte_type2))

epiphytes_sno <- epiphytes_sno %>%
 pivot_longer(c("on_trunk", "on_branches"), names_to = "epiphyte", values_to = "percent", values_drop_na = T)

epiphytes_sno <-epiphytes_sno %>%
  mutate(epiphyte = ifelse(epiphyte == "on_trunk", epiphyte_type, epiphyte),
         epiphyte = ifelse(epiphyte == "on_branches", epiphyte_type2, epiphyte)) %>%
  select(-epiphyte_type, -epiphyte_type2) %>%
  mutate(presence = as.character(percent)) %>%
  select (-percent) %>%
  spread(key = epiphyte, value = presence)

############################
### DISTURBANCES DATASET ###
############################

#### 1. Read file

disturbance <- read_csv("input/disturbance.csv")

#### 2. Edit file

disturbance <- transmute(disturbance,
                         site = as.character(Conglomerado),
                         state = as.character(Estado),
                         municipality = as.character(Municipio),
                         cause = as.character(Causa),
                         on_veg = as.character(ImpactoVegetacion),
                         on_soil = as.character(ImpactoSuelo),
                         on_water = as.character(ImpactoAgua),
                         notes = as.character(Observaciones)) %>%
  as_tibble()

disturbance_sno <- sites_sno %>%
  select(site, state, municipality) %>%
  left_join(disturbance, by = c("site", "state", "municipality"))

#### 3. Quality control

###########################
### SAVE FINAL DATASETS ###
###########################

#### 9. Save final datasets ####

# Save trees_sno dataset in output directory
save(trees_sno, file = "output/trees_sno.RData")

# Save sites_sno dataset in output directory
save(sites_sno, file = "output/sites_sno.RData")

# Save plots_sno dataset in output directory
save(plots_sno, file = "output/plots_sno.RData")

# Save epiphytes_sno dataset in output directory

save(epiphytes_sno, file = "output/epiphytes_sno.RData")

#Save disturbance dataset

save(disturbance_sno, file = "output/disturbance_sno.RData")
