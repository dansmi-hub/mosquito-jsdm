# Trap or collection method vs species composition

names <- mosquito_km %>% separate(SpeciesName, c("Genus", "Species"), sep = " ") %>% as.data.frame()


names %>% 
  #group_by(Genus) %>% 
  filter(Genus == "Aedes" | Genus == "Anopheles" | Genus == "Culex") %>% 
  filter(VectorSpeciesLifeStageCode != "") %>% 
  filter(VectorCollectionMethodCode != "") %>% 
  ggplot(aes(x = Genus, fill = VectorCollectionMethodCode)) + 
  geom_bar() +
  labs(fill = "Trap Type") +
  ylab(NULL) + xlab(NULL)

names %>% 
  #group_by(Genus) %>% 
  filter(Genus == "Aedes" | Genus == "Anopheles" | Genus == "Culex") %>% 
  ggplot(aes(x = VectorSpeciesLifeStageCode)) + 
  geom_bar() +
  facet_grid(~ Genus)


names %>% 
  #group_by(Genus) %>% 
  filter(Genus == "Aedes" | Genus == "Anopheles" | Genus == "Culex") %>% 
  ggplot(aes(x = Genus, fill = VectorSpeciesLifeStageCode)) + 
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Life Stage") +
  ylab(NULL) +xlab(NULL)

ggsave("temp.png", dpi = 300, width = 9, height = 6)  


"VI",
"CO2OTH",
"MMT",
"NSP",
"GT",
"HB",
"OTH",
"BGLCO2",

"BGL",
"CDCCO2",
"AAB",
"EVSCO2",
"NET",
"LSD",
"RC",
"STOTH",
"SD",
"IMT",
"SSS",
"EVS",
"CDC",

names$VectorCollectionMethodCode %>% as.factor() %>% table() %>% enframe() %>% 
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  coord_flip() +
  ylab("Count") + xlab("Trap Type")
