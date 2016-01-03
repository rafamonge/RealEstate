parsePage <-
function(page){
  searchPage <- html(page)
  blocks <- searchPage %>%  html_nodes(".advice.linear")
  listOfDataFrames <- lapply(blocks, parseBlock)
  
  df<- ldply(listOfDataFrames, data.frame) %>% filter(Property != "Visitas")  %>%
  mutate(Property = camel(as.character(Property)))  %>%
  spread(Property, Value)  %>%
  separate(EstatePrice, c("Currency", "Price"), sep=" ", extra="drop")  %>%
  mutate(Price = as.numeric(gsub(",", "", Price)))
  
  if(!"MtsTerreno" %in% names(df)){
    df$MtsTerreno <- c(0)
  }
  if(!"MtsConstruccion" %in% names(df)){
    df$MtsConstruccion <- c(0)
  }
  df  %>%
  mutate( Currency = enc2native(Currency)
         ,Provincia = enc2native(Provincia)
         ,Canton = enc2native(Canton)
         ,Direccion = enc2native(Direccion)
         ,EstateName = enc2native(EstateName)
         ,EstateSeller = enc2native(EstateSeller)
         ,MtsConstruccion = as.numeric(MtsConstruccion)
         ,MtsTerreno	 = as.numeric(MtsTerreno)
         ) %>%
  select(Source, EstateId, Provincia, Canton, Direccion, EstateName, Currency, Price, EstateSeller, MtsConstruccion, MtsTerreno, Publicado, URL)
}
