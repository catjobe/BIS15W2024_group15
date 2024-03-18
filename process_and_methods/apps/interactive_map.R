## Interactive Map

library(mapview)
library(sf)

dog_gwas <- read_csv("../../data/dog_gwas.csv")

dog_geographic_distribution <- dog_gwas %>% 
        filter(longitude != "NA" | latitude != "NA") %>%
        filter(longitude != "NA" | latitude != "NA") %>%
        filter(longitude != "#N/A" | latitude != "#N/A") %>%
        mutate(height_category = case_when(height_cm <= 20.0 ~ "small",
                                           height_cm > 20.0 & height_cm < 73.0 ~ "medium",
                                           height_cm >= 70.0 ~ "large")) %>%
        filter(height_category != "NA") %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mapview(dog_geographic_distribution, zcol = "height_category", legend = TRUE, map.types = "CartoDB.Positron") 

