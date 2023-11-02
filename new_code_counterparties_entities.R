source("Functions.R")
source("Functions_Orange.R")
#tolower and space cleaning for column names and content
anagrafiche_cleaned <- Anagrafiche_Raw
garanzie_cleaned <- Garanzie_Raw

colnames(anagrafiche_cleaned) <- tolower(colnames(anagrafiche_cleaned)) %>% gsub(" |_", ".",.)
colnames(garanzie_cleaned) <- tolower(colnames(garanzie_cleaned)) %>% gsub(" |_", ".",.)

anagrafiche_cleaned[] <- lapply(anagrafiche_cleaned,tolower)
garanzie_cleaned[] <- lapply(garanzie_cleaned,tolower)

anagrafiche_cleaned <- anagrafiche_cleaned %>% rename(type = `tipo.anagrafica.(d=debitore,.g=garante,.dg=debitore.e.garante)`)


garanzie_selected <- garanzie_cleaned %>% select(ndg, ndg.garante, intestazione.garante) %>% rename(ndg_loan = ndg, ndg = ndg.garante, "intestaz..unica./.rag..soc." = intestazione.garante)

garanzie_selected <- garanzie_selected[!duplicated(garanzie_selected[c("ndg_loan", "ndg", "intestaz..unica./.rag..soc.")]), ]
anagrafiche_dg <- anagrafiche_cleaned %>% filter(type== "dg")
guarantors_only <- garanzie_selected %>% merge(anagrafiche_dg, by= "ndg")
guarantors_only <- guarantors_only %>% select(-c(ndg, `intestaz..unica./.rag..soc..y`)) %>% 
  rename(ndg = ndg_loan, "intestaz..unica./.rag..soc." =`intestaz..unica./.rag..soc..x`)
guarantors_only$type <- "g"

anagrafiche_cleaned <- rbind(anagrafiche_cleaned, guarantors_only)


#garanzie_cleaned <- garanzie_cleaned %>% rename(id.loan = numero.rapporto.garantito, name =intestazione.garante)


#count entities inside each counterparty
selected_columns <- c("nome.1", "nome.2", "nome.3", "nome.4", "nome.5")

# Count non-empty values in each row of the selected columns and return 1 if none found
anagrafiche_cleaned$n.entities <- rowSums(!is.na(anagrafiche_cleaned[selected_columns]))

# Replace 0 counts with 1
anagrafiche_cleaned$n.entities <- ifelse(anagrafiche_cleaned$n.entities == 0, 1, anagrafiche_cleaned$n.entities)

anagrafiche_cleaned <- anagrafiche_cleaned[!duplicated(anagrafiche_cleaned[c("ndg", "intestaz..unica./.rag..soc.")]), ]






#create id.counterparty
anagrafiche_cleaned <- anagrafiche_cleaned %>% mutate(id.counterparty = paste0("c_",row_number()))
#anagrafiche_cleaned <- anagrafiche_cleaned %>% group_by(`intestaz..unica./.rag..soc.`, type) %>% mutate(id.entity = paste0("c_", cf.piva)) %>% ungroup()



anagrafiche_cleaned$nome.1 <- ifelse(is.na(anagrafiche_cleaned$nome.1), anagrafiche_cleaned$`intestaz..unica./.rag..soc.`, anagrafiche_cleaned$nome.1)
anagrafiche_cleaned$codice.fiscale.1 <-  ifelse(is.na(anagrafiche_cleaned$codice.fiscale.1), anagrafiche_cleaned$`codice.fiscale.(persona.fisica)`, anagrafiche_cleaned$codice.fiscale.1)
anagrafiche_cleaned$ndg.1 <- ifelse(anagrafiche_cleaned$ndg.1==0, anagrafiche_cleaned$ndg, anagrafiche_cleaned$ndg.1)

#obtain entities
anagrafiche_entities1 <- anagrafiche_cleaned %>% select(id.counterparty, ndg.1, nome.1,codice.fiscale.1, forma.giuridica, comune, provincia)
#anagrafiche_entities1<-anagrafiche_entities1[complete.cases(anagrafiche_entities1),]
anagrafiche_entities1 <- anagrafiche_entities1 %>% rename(ndg= ndg.1, name = nome.1, cf.piva=codice.fiscale.1)

anagrafiche_entities_piva <- anagrafiche_cleaned %>% select(id.counterparty, ndg.1, nome.1, `partita.iva.(aziende)`, forma.giuridica, comune, provincia)
anagrafiche_entities_piva<-anagrafiche_entities_piva[complete.cases(anagrafiche_entities_piva),]
anagrafiche_entities_piva <- anagrafiche_entities_piva %>% rename(ndg= ndg.1, name = nome.1, cf.piva=`partita.iva.(aziende)`)

anagrafiche_entities2 <- anagrafiche_cleaned %>% select(id.counterparty,ndg.2, nome.2, codice.fiscale.2, forma.giuridica, comune, provincia)
anagrafiche_entities2<-anagrafiche_entities2[complete.cases(anagrafiche_entities2),]
anagrafiche_entities2 <- anagrafiche_entities2 %>% rename(ndg= ndg.2, name = nome.2, cf.piva=codice.fiscale.2)

anagrafiche_entities3 <- anagrafiche_cleaned %>% select(id.counterparty,ndg.3, nome.3, codice.fiscale.3, forma.giuridica, comune, provincia)
anagrafiche_entities3<-anagrafiche_entities3[complete.cases(anagrafiche_entities3),]
anagrafiche_entities3 <- anagrafiche_entities3 %>% rename(ndg= ndg.3, name = nome.3, cf.piva=codice.fiscale.3)

anagrafiche_entities4 <- anagrafiche_cleaned %>% select(id.counterparty,ndg.4, nome.4, codice.fiscale.4, forma.giuridica, comune, provincia)
anagrafiche_entities4<-anagrafiche_entities4[complete.cases(anagrafiche_entities4),]
anagrafiche_entities4 <- anagrafiche_entities4 %>% rename(ndg= ndg.4, name = nome.4, cf.piva=codice.fiscale.4)

anagrafiche_entities5 <- anagrafiche_cleaned %>% select(id.counterparty, ndg.5, nome.5, codice.fiscale.5, forma.giuridica, comune, provincia)
anagrafiche_entities5<-anagrafiche_entities5[complete.cases(anagrafiche_entities5),]
anagrafiche_entities5 <- anagrafiche_entities5 %>% rename(ndg= ndg.5, name = nome.5, cf.piva=codice.fiscale.5)



combined_entities <- bind_rows(anagrafiche_entities1, anagrafiche_entities_piva, anagrafiche_entities2, anagrafiche_entities3,
                               anagrafiche_entities4, anagrafiche_entities5)



unique_entities <- combined_entities[!duplicated(combined_entities[c("id.counterparty", "cf.piva")]), ]
#unique_entities <- unique_entities %>% distinct(cf.piva, .keep_all = TRUE)

#create id.entities
unique_entities <- unique_entities %>% group_by(cf.piva) %>% mutate(id.entity = paste0("e_", cf.piva)) %>% ungroup()

#create the link_counterparty_entity table
link_entity_counterparty <- unique_entities %>% select(id.counterparty, id.entity)

checkcolumns(anagrafiche_cleaned$id.counterparty, link_entity_counterparty$id.counterparty)

#clean counterparty table
counterparty_finished <- anagrafiche_cleaned %>% select(id.counterparty, ndg, `intestaz..unica./.rag..soc.`,type, 
                                                        n.entities)

counterparty_finished <-counterparty_finished %>% rename(id.bor = ndg, name = `intestaz..unica./.rag..soc.`,
                                                         role= type)

counterparty_finished <- counterparty_finished %>%
    mutate(role = case_when(
        str_detect(role, "dg")  ~ "borrower",
        str_detect(role, "d")  ~ "borrower",
        str_detect(role, "g")  ~ "guarantor",
        TRUE ~ "Other"  # Catch-all condition for "corporate"
      ))

counterparty_finished <- counterparty_finished %>% mutate(id.group = NA, flag.imputed = NA)

columns_to_transform <- c("id.counterparty", "id.bor", "id.group", "name")

counterparty_finished[columns_to_transform] <- lapply(counterparty_finished[columns_to_transform], as.character)

counterparty_finished$role <- counterparty_finished$role %>% as.factor()

columns_to_transform <- c("n.entities", "flag.imputed")

counterparty_finished[columns_to_transform] <- lapply(counterparty_finished[columns_to_transform], as.integer)


#refine entities
piva_counterparty_entity <- anagrafiche_cleaned %>% select(id.counterparty, `partita.iva.(aziende)`)
entities_complete <- unique_entities %>% left_join(piva_counterparty_entity, by = "id.counterparty")

entities_complete$forma.giuridica <- entities_complete$forma.giuridica %>% as.factor()

entities_complete <- entities_complete %>%
  mutate(forma.giuridica = case_when(
    str_detect(forma.giuridica, "coi")  ~ "individual",
    str_detect(forma.giuridica, "ind")  ~ "individual",
    str_detect(forma.giuridica, "prv")  ~ "individual",
    str_detect(forma.giuridica, "coo")  ~ "corporate",
    str_detect(forma.giuridica, "snc")  ~ "corporate",
    str_detect(forma.giuridica, "sas")  ~ "corporate",
    str_detect(forma.giuridica, "spa")  ~ "corporate",
    str_detect(forma.giuridica, "srl")  ~ "corporate",
    str_detect(forma.giuridica, "ass")  ~ "corporate",
    str_detect(forma.giuridica, "ent")  ~ "confidi",
    TRUE ~ "Other"  # Catch-all condition for "corporate"
  ))
entities_complete <- entities_complete %>% rename(type.subject = forma.giuridica, city = comune, province = provincia)

#add type.pg column
entities_complete <- add_type.pg_column(entities_complete)
#add age column
entities_complete <- add_age_column(entities_complete)
#add age range column
entities_complete <- add_age_range_column(entities_complete)
#add sex column
entities_complete <- add_sex_column(entities_complete)

#replace cf with piva when we have both
entities_complete$`partita.iva.(aziende)` <- ifelse(is.na(entities_complete$`partita.iva.(aziende)`), entities_complete$cf.piva, entities_complete$`partita.iva.(aziende)`)

entities_complete <- entities_complete %>% select(-c(id.counterparty, ndg, cf.piva))
entities_complete <- entities_complete %>% rename(cf.piva = `partita.iva.(aziende)`)

#insert region and area columns
entities_complete <- create_region_city(entities_complete, entities_complete$city)
entities_complete <- create_area_city(entities_complete, entities_complete$city)

#create NA columns
entities_complete <- entities_complete %>% mutate(dummy.info= NA, solvency.pf = NA, income.pf = NA, 
                                                  status.pg = NA, date.cessation= NA, flag.imputed = NA)

#change columns format

columns_to_transform <- c("type.subject", "sex", "range.age", "solvency.pf", "type.pg", "status.pg", 
                          "province", "region", "area")

entities_complete[columns_to_transform] <- lapply(entities_complete[columns_to_transform], as.factor)

columns_to_transform <- c("dummy.info", "age", "flag.imputed")

entities_complete[columns_to_transform] <- lapply(entities_complete[columns_to_transform], as.integer)

entities_complete$income.pf <- entities_complete$income.pf %>% as.numeric()
entities_complete$date.cessation <- entities_complete$date.cessation %>% as.Date()

entities_complete <- entities_complete[!duplicated(entities_complete$id.entity), ]

entities_complete[] <- lapply(entities_complete,tolower)













