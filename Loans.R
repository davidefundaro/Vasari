
#--------------------------------------#
#------            Loans        ------
#--------------------------------------#


Posizioni_selected_raw <- Posizioni_Raw %>% select(`Numero rapporto`,NDG,
                                           `Forma tecnica rapporto`,`Gross book value Totale`,
                                           `Gross book value Capitale`,`Gross book value Spese`,
                                           `Gross book value Interessi`,`Data di passaggio a sofferenza`)


### Clean columns and make lower case

colnames(Posizioni_selected_raw) <- tolower(colnames(Posizioni_selected_raw))
Posizioni_selected_raw[] <- lapply(Posizioni_selected_raw,tolower)
#Posizioni_selected_raw <- Posizioni_selected_raw %>% distinct()

names(Posizioni_selected_raw) <- colname_function(names(Posizioni_selected_raw))

#Posizioni_selected_raw$intestazione <- gsub("\\s+"," ",Posizioni_selected_raw$intestazione)

Posizioni_selected_raw <- Posizioni_selected_raw %>% rename(id.loans = numero.rapporto,id.bor = ndg,type = forma.tecnica.rapporto
                                                   ,gbv.original = gross.book.value.totale,principal = gross.book.value.capitale,
                                                   penalties = gross.book.value.spese,interest = gross.book.value.interessi,
                                                   date.status = data.di.passaggio.a.sofferenza)
columns_to_convert <- c("gbv.original","principal","interest","penalties")
Posizioni_selected_raw <- Posizioni_selected_raw %>% mutate(across(all_of(columns_to_convert),as.numeric))

Posizioni_selected_raw$gbv.residual <- Posizioni_selected_raw$gbv.original

Posizioni_selected_raw$date.status <- as.Date(Posizioni_selected_raw$date.status)

Posizioni_selected_raw$type <- as.factor(Posizioni_selected_raw$type)

#check_primary_key(Posizioni_selected_raw,"id.loans") TRUE id.loans is unique

Posizioni_selected_raw <- Posizioni_selected_raw %>% mutate(status = "bad",id.group=NA_real_,originator=NA_real_,ptf=NA_real_,cluster=NA_real_ ,
                                                            ptf=NA_real_,date.origination=NA_real_,date.last.act=NA_real_,
                                                            flag.imputed=NA_real_,expenses = penalties,penalties=NA_real_)


Posizioni_selected_raw <- Posizioni_selected_raw %>%
  mutate(type = case_when(
    type == "finanziamento" ~ "personal loans",
    type == "scopertocc" ~ "bank accounts",
    type == "mutuo" ~ "mortgages",
    TRUE ~ as.factor(type)
  ))