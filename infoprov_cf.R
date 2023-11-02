
#select the data needed for pf and pg
pf <- entities_complete %>% filter(type.subject == "individual")

pg <- entities_complete %>% filter(type.subject == "corporate" | type.subject == "confidi")


pf <- pf %>% select(cf.piva,name,city,province,region)

infoprov1 <- infoproviding_cf %>% select(id_anagrafica,cf_piva,
                                         `STATO OCCUPAZIONE`,
                                          NOTE...17,
                                         `EMOLUMENTI MENSILI LORDI`,P.IVA)

infoprov1[]<- lapply(infoprov1,tolower)

colnames(infoprov1) <- tolower(colnames(infoprov1))

infoprov1 <- infoprov1 %>% rename(cf.piva = cf_piva)


infoprov2 <- left_join(pf,infoprov1,by = "cf.piva")

infoprov2 <- infoprov2 %>% mutate(data.infoprov = "2023-07-12")

infoprov2 <- infoprov2 %>%
  mutate(
    solvency.base = case_when(
      `stato occupazione` == "deceduto"  ~ "deceased",
      `stato occupazione` == "disoccupato"& is.na(p.iva) ~ "insolvent",
      `stato occupazione` == "disoccupato" & !is.na(p.iva) ~ "self employed",
      `stato occupazione` == "dipendente" & grepl("indeterminato", note...17) ~ "employee - permanent",
      `stato occupazione` == "dipendente" & grepl("determinato", note...17) ~ "employee - temporary",
      `stato occupazione` == "dipendente" & is.na(note...17) ~ "employee - N/A",
      `stato occupazione` == "pensionato"  ~ "pensioner"
      
    )
  )

#eliminate duplicates
infoprov2 <- infoprov2[!duplicated(infoprov2), ]

infoprov2$`emolumenti mensili lordi` <- sub(" / .*|/.*", "", infoprov2$`emolumenti mensili lordi`)
 
infoprov2$`emolumenti mensili lordi` <-  gsub("\\ €", "",infoprov2$`emolumenti mensili lordi`)

infoprov2$`emolumenti mensili lordi` <- sub(",0+","",infoprov2$`emolumenti mensili lordi`)

infoprov2$`emolumenti mensili lordi` <- as.numeric(infoprov2$`emolumenti mensili lordi`)

infoprov2$income.net <- round(fct.emp(infoprov2$`emolumenti mensili lordi`),2)

infoprov3 <- infoprov2 %>%
  mutate(
    solvency.adj = case_when(
      is.na(solvency.base) ~ NA_character_,
      solvency.base == "pensioner" & income.net > 1200 ~ "pensioner",
      grepl("employee",solvency.base) & income.net > 500 ~ "employee",
      TRUE ~ "insolvent"
    ))

GeoData[] <- lapply(GeoData,tolower)

GeoData_selected <- GeoData %>% select(label.prov,province,region,area)

GeoData_selected <- GeoData_selected %>% rename(province = label.prov , prov = province)


infoprov4 <- left_join(infoprov3,GeoData_selected,by = "province")

infoprov4 <- infoprov4 %>% distinct()

infoprov_pf_final <- infoprov4 %>% select(cf.piva,data.infoprov,name,solvency.base,
                                          solvency.adj,`emolumenti mensili lordi`,
                                          income.net,city,prov,region.y)

infoprov_pf_final <- infoprov_pf_final %>% rename(date.infoprov = data.infoprov,income.gross = `emolumenti mensili lordi`,
                                                  province = prov , region = region.y)


#change columns format

columns_to_transform <- c("solvency.base", "solvency.adj", "province", "region")

infoprov_pf_final[columns_to_transform] <- lapply(infoprov_pf_final[columns_to_transform], as.factor)

columns_to_transform <- c("income.gross", "income.net")

infoprov_pf_final[columns_to_transform] <- lapply(infoprov_pf_final[columns_to_transform], as.numeric)

infoprov_pf_final$date.infoprov <- infoprov_pf_final$date.infoprov %>% as.Date()
