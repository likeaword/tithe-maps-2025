library(tidyverse)
library(sf)

# Read in the shape files
polygon <- st_read("grid_ceredigion.shp")
points <- st_read("tithe_points_ceredigion.shp")

# For reasons they have different CRS so we reproject points to the CRS of polygon
points <- st_transform(points, st_crs(polygon))

# Count the number of points in each polygon
inter <- st_intersects(polygon, points)

# then add this count of points as a new field
polygon$count <- lengths(inter)

# This is a little process to identify all the different types of landuses and then to simplify them
# (done by hand in Excel)
poss_uses <- data.frame(unique(points$land_use))
write_csv(poss_uses, "uses_ref.csv")
poss_uses <- read_csv("uses_ref_simpl.csv")

# Now we use a left join to add a field with the simplified land uses
points <- points |>
  left_join(poss_uses)

# Now we go through the 6 different land uses and add each as a new field
## First Arable
Arable <- points |>
  filter(simpl_land_use == "Arable")

inter_Arable <- st_intersects(polygon, Arable)
polygon$Arable <- lengths(inter_Arable)  

## Now Building
Building <- points |>
  filter(simpl_land_use == "Building")

inter_Building <- st_intersects(polygon, Building)
polygon$Building <- lengths(inter_Building)

## Now Pasture
Pasture <- points |>
  filter(simpl_land_use == "Pasture")

inter_Pasture <- st_intersects(polygon, Pasture)
polygon$Pasture <- lengths(inter_Pasture)

## Now Waste
Waste <- points |>
  filter(simpl_land_use == "Waste")

inter_Waste <- st_intersects(polygon, Waste)
polygon$Waste <- lengths(inter_Waste)

## Now Water
Water <- points |>
  filter(simpl_land_use == "Water")

inter_Water <- st_intersects(polygon, Water)
polygon$Water <- lengths(inter_Water)

## Finally Woodland
Woodland <- points |>
  filter(simpl_land_use == "Woodland")

inter_Woodland <- st_intersects(polygon, Woodland)
polygon$Woodland <- lengths(inter_Woodland)

polygon_calc <- data.frame(polygon) |>
  select(c(id,count,Arable,Building,Pasture,Water,Waste,Woodland))

polygon_calc$max <- apply(test_polygon[,3:8], 1, max)

polygon_calc$predom_land_use <- ifelse(polygon_calc$Arable == polygon_calc$max, "Arable",
                                       ifelse(polygon_calc$Building == polygon_calc$max,"Building",
                                              ifelse(polygon_calc$Pasture == polygon_calc$max,"Pasture",
                                                     ifelse(polygon_calc$Water == polygon_calc$max,"Water",
                                                            ifelse(polygon_calc$Waste == polygon_calc$max,"Waste",
                                                                   ifelse(polygon_calc$Woodland == polygon_calc$max,"Woodland","Not Known"))))))


polygon_calc$Arable_pc <- polygon_calc$Arable / polygon_calc$count
polygon_calc$Building_pc <- polygon_calc$Building / polygon_calc$count
polygon_calc$Pasture_pc <- polygon_calc$Pasture / polygon_calc$count
polygon_calc$Waste_pc <- polygon_calc$Waste / polygon_calc$count
polygon_calc$Water_pc <- polygon_calc$Water / polygon_calc$count
polygon_calc$Woodland_pc <- polygon_calc$Woodland / polygon_calc$count

polygon <- polygon |>
  left_join(polygon_calc,by = "id")

polygon <- polygon |>
  rename(`Predominant land use` = predom_land_use)



  
ggplot(polygon) + geom_sf(aes(fill = `Predominant land use`)) +
  labs(
    title = "Predominant land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  theme_minimal()
ggsave("Predom_land_use.png")

ggplot(polygon) + geom_sf(aes(fill = Arable_pc)) +
  labs(
    title = "Arable share of land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  scale_fill_viridis_c()+
  theme_minimal()
ggsave("Arable.png")

ggplot(polygon) + geom_sf(aes(fill = Building_pc)) +
  labs(
    title = "Building share of land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  scale_fill_viridis_c()+
  theme_minimal()
ggsave("Building.png")

ggplot(polygon) + geom_sf(aes(fill = Pasture_pc)) +
  labs(
    title = "Pasture share of land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  scale_fill_viridis_c()+
  theme_minimal()
ggsave("Pasture.png")

ggplot(polygon) + geom_sf(aes(fill = Waste_pc)) +
  labs(
    title = "Waste share of land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  scale_fill_viridis_c()+
  theme_minimal()
ggsave("Waste.png")

ggplot(polygon) + geom_sf(aes(fill = Water_pc)) +
  labs(
    title = "Water share of land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  scale_fill_viridis_c()+
  theme_minimal()
ggsave("Water.png")

ggplot(polygon) + geom_sf(aes(fill = Woodland_pc)) +
  labs(
    title = "Woodland share of land-use in Ceredigion 1830-1840s",
    subtitle = "Based on tithe maps",
    caption = "Data: National Library of Wales | Creation: Ben Proctor"
  ) +
  scale_fill_viridis_c()+
  theme_minimal()
ggsave("Woodland.png")

ggplot() + geom_sf(data = polygon) + geom_sf(data = points, size = 0.01)
ggsave("dots.png")
