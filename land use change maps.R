
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


#Check
plot(lu2020)



# based on ALUM = Australian Land Use and Management classification
reclass_fun <- function(x) {
  ifelse(
    x >= 200 & x < 300,
    1,
    # Cropping
    ifelse(
      x >= 300 & x < 400,
      2,
      # Grazing modified
      ifelse(
        x >= 100 & x < 200,
        3,
        # Grazing natural
        ifelse(
          x >= 500 & x < 600,
          4,
          # Horticulture
          ifelse(
            x >= 600 & x < 700,
            5,
            # Intensive
            6
          )
        )
      )
    )
  )
}

lu_all_simple <- list(
  lu1992 = app(lu1992, reclass_fun),
  lu1996 = app(lu1996, reclass_fun),
  lu2000 = app(lu2000, reclass_fun),
  lu2005 = app(lu2005, reclass_fun),
  lu2010 = app(lu2010, reclass_fun),
  lu2015 = app(lu2015, reclass_fun),
  lu2020 = app(lu2020, reclass_fun)
)


lu2020_simple <- app(lu2020, reclass_fun)

## Plot helpers

cols <- c(
  "#8c8c8c",  # Grazing natural (grey)
  "#c7e75f",  # Grazing modified (light green)
  "#7a9a01",  # Cropping (olive green)
  "#e31a1c",  # Horticulture (red)
  "#4fc3dc",  # Intensive (blue)
  "#d9d9d9"   # Other (light grey)
)






plot(lu2020_simple,
     col = cols,
     legend = FALSE,
     axes = FALSE,
     box = FALSE,
     main = "Land Use Australia 2020–21")

legend("bottomleft",
       legend = c("Grazing native vegetation",
                  "Grazing modified pastures",
                  "Cropping",
                  "Horticulture",
                  "Intensive industries",
                  "Other uses"),
       fill = cols,
       bty = "n",
       cex = 0.8)




#Something looks wrong there should be no cropping in the center
