# At what resolution are communites best assembled?

separated_coord <- mosquito_points %>%
  mutate(lat = unlist(map(mosquito_points$geometry,1)),
         long = unlist(map(mosquito_points$geometry,2)))

presence <- separated_coord %>% filter(DistributionStatus == "Present") %>% as.data.frame()

get_xy(presence, "long", "lat")

get_species(presence, SpeciesName)



foo <- points2presab(presence, "long", "lat", SpeciesName, CRS = CRS(mosquito_points))


plot(foo)
