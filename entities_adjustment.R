#########################
##### entities and counterparties adjustment
#########################

piva_table <- Anagrafiche_Raw %>% filter(forma.giuridica =="ind") %>% select(ndg, forma.giuridica,
                                                                             `partita.iva.(aziende)`,
                                                                             `codice.fiscale.(persona.fisica)`)
complete_cases <- piva_table[complete.cases(piva_table), ]

complete_cases <- complete_cases %>% rename(cf.piva = `codice.fiscale.(persona.fisica)`)


complete_cases1 <- left_join(entities_final,complete_cases, by = "cf.piva")


complete_cases2 <- complete_cases1 %>% filter(!is.na(`partita.iva.(aziende)`))


complete_cases3 <- complete_cases2 %>% select(-ndg, -cf.piva,-forma.giuridica) 

complete_cases3 <- complete_cases3 %>% rename(cf.piva = `partita.iva.(aziende)`)


entities_complete <- bind_rows(entities_final,complete_cases3)


entities_complete <- entities_complete %>% mutate(id.entities = paste0("e",row_number()))

entities_complete <- entities_complete %>% select(-id.bor)


################################
### CHANGE DATA TYPE
###############################

columns_to_convert <- c("province", "region","area","type.pg","solvency.pf","range.age","sex","type.subject","status.pg")
entities_complete[columns_to_convert] <- lapply(entities_complete[columns_to_convert], as.factor)


columns_to_convert <- c("flag.imputed", "age","dummy.info")
entities_complete[columns_to_convert] <- lapply(entities_complete[columns_to_convert], as.integer)


entities_complete$income.pf <- as.numeric(entities_complete$income.pf)

entities_complete$date.cessation <- as.Date(entities_complete$date.cessation)

