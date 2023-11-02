infoprov_piva <- pg %>% select(cf.piva,name,type.pg,status.pg,date.cessation,city,province,region)

infoprov_piva <- left_join(infoprov_piva,GeoData_selected,by = "province")

infoprov_piva <- infoprov_piva %>% distinct()


infoprov_piva <- infoprov_piva %>% select(-province,-region.x,-area)
                                          
infoprov_piva <- infoprov_piva %>% rename(province = prov , region = region.y,status = status.pg)

infoprov_piva <- infoprov_piva %>% mutate(date.infoprov = "2023-07-13")

piva_report <- infoproviding_piva %>% select(out_dati_attivita_stato_attivita,cod_fiscale)

piva_report <- piva_report %>% rename(cf.piva = cod_fiscale, status = out_dati_attivita_stato_attivita)

infoprov_piva1 <- left_join(infoprov_piva,piva_report,by = "cf.piva")

infoprov_piva1[] <- lapply(infoprov_piva1,tolower)

infoprov_piva1 <- infoprov_piva1 %>%
  mutate(
    status = case_when(
      status.y == "attiva"  ~ "active",
      status.y == "cancellata" ~ "canceled",
      grepl("liquidazione",status.y) | grepl("scioglimento",status.y) ~ "liquidation",
      status.y == "fallita" ~ "bankruptcy",
      status.y == "in procedura concorsuale" ~ "bankruptcy",
      status.y == "inattiva"  ~ "inactive"

    )
  )


infoprov_piva_final <- infoprov_piva1 %>% select(-status.y,-status.x)

infoprov_piva_final <- infoprov_piva_final %>% rename(type = type.pg)


#change columns format

columns_to_transform <- c("type", "status", "province", "region")

infoprov_piva_final[columns_to_transform] <- lapply(infoprov_piva_final[columns_to_transform], as.factor)

columns_to_transform <- c("date.infoprov", "date.cessation")

infoprov_piva_final[columns_to_transform] <- lapply(infoprov_piva_final[columns_to_transform], as.Date)
