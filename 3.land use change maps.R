
library(terra)
library(sf)
library(tmap)



#set working directory 
setwd("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/landuse maps")

# load rasters
lu2020 <- rast("2020_21/NLUM_v7_250_ALUMV8_2020_21_alb.tif")
lu2015 <- rast("2015_16/NLUM_v7_250_ALUMV8_2015_16_alb.tif")
lu2010 <- rast("2010_11/NLUM_v7_250_ALUMV8_2010_11_alb.tif")


list.files("2005", recursive = TRUE)
lu2005 <- rast("2005/luav4g9abll07811a01egialb132/lu05v4aa")


list.files("1993_02/LanduseofAustralia_93-02v3/grids")


lu1992 <- rast("1993_02/LanduseofAustralia_93-02v3/grids/lu92v3")
lu1996 <- rast("1993_02/LanduseofAustralia_93-02v3/grids/lu96v3")
lu2000 <- rast("1993_02/LanduseofAustralia_93-02v3/grids/lu00v3")


################################################################################
### 2020 #####
getwd()
lookup2020 <- read.csv("2020_21/NLUM_v7_250_ALUMV8_2020_21_alb.csv")
names(lookup2020)
head(lookup2020)

table(lookup2020$SIMP)
table(lookup2020$AGIND)


lookup2020$final_class <- match(
  lookup2020$AGIND,
  c("Cropping",
    "Grazing modified pastures",
    "Grazing native vegetation",
    "Horticulture",
    "Intensive plant and animal industries",
    "Not agricultural industry")
)


rcl_2020 <- as.matrix(lookup2020[, c("Value", "final_class")])
lu2020_simple <- classify(lu2020, rcl_2020)

names(lu2020_simple)


## Plot helpers




cols <- c(
  "#7a9a01",  # 1 Cropping (olive)
  "#d9ef8b",  # 2 Grazing modified (pale yellow-green)
  "#e3ded6",  # 3 Grazing native (lighter warm grey ✅)
  "#e31a1c",  # 4 Horticulture (red)
  "#6ecae3",  # 5 Intensive (soft blue)
  "#ffffff"   # 6 Other (pure white ✅)
)









par(mar = c(3, 6, 3, 2))  
# bottom, left, top, right



plot(lu2020_simple,
     col = cols,
     legend = FALSE,
     axes = FALSE,
     box = FALSE,
     main = "Land Use Australia 2020–21")

legend("left",
       legend = c("Cropping",
                  "Grazing modified pastures",
                  "Grazing native vegetation",
                  "Horticulture",
                  "Intensive industries",
                  "Other uses"),
       fill = cols,
       bty = "n",
       cex = 0.9,
       xpd = NA)   # allows drawing outside plot area




#ok thats got it I think.
## This may be a messy process 
#I will need to use the lookup table in each downlaod folder and assign the classes
# there is s folder called maps which have png for the maps I want to make to compare to.