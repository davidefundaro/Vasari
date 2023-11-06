###################################################
###-             Agreement_Summary            -###
###################################################

#clean column names and set to lower column names and content
colnames(pdr) <- tolower(colnames(pdr)) %>% gsub(" |_", ".",.)
pdr[] <- lapply(pdr,tolower)


#change type to numeric for all column with values
columns_to_transform <- c("importo.dovuto", "importo.singole.rate", "pagamento", "importosaldo", "sumattivo")

pdr[columns_to_transform] <- lapply(pdr[columns_to_transform], as.numeric)

pdr_raw <-pdr
#set date as date
pdr_raw$dtpagamento <- as.Date(pdr_raw$dtpagamento, origin= "1899-12-30")

#pdr_raw <- pdr_raw %>% mutate(date.last.payment = if_else(!is.na(dtfatturazione), dtpagamento, NA))

#create a new table where you put the total paid amount and set the max date of payment
paid <- pdr_raw %>% group_by(idccpdr) %>% summarise(amount.agreement = sum(importo.singole.rate),
                                                    date.end = max(dtpagamento),
                                                    date.last.payment = max(dtfatturazione, na.rm = TRUE),
                                                    n.instalment = n(),
                                                    length = n())
#paid <- paid %>% mutate(date.last.payment = if_else(date.last.payment == -Inf, NA, date.last.payment))


#create a distinct table
pdr_distinct <- pdr_raw %>% distinct(idccpdr, .keep_all = TRUE)
pdr_distinct <- pdr_distinct %>% left_join(paid, by= "idccpdr")

#select from counterparties only the borowers
cp <- counterparty_finished %>% filter(role== "borrower") %>% select(id.counterparty,id.bor)

#left join to complete the table
pdr_distinct <- pdr_distinct %>% rename(id.bor = ndg)

pdr_distinct <- pdr_distinct %>% left_join(cp, by= "id.bor")

#set date as date and take the year
pdr_distinct$datapdr <- as.Date(pdr_distinct$datapdr, origin= "1899-12-30")
pdr_distinct$year.agreement <- format(pdr_distinct$datapdr, "%Y")

#set residual and case when for creating the status
pdr_distinct <- pdr_distinct %>% mutate(residual= amount.agreement - sumattivo)

#months_passed <- difftime(Sys.Date(), start_date, units = "weeks") / 4.34812
#months_passed <- round(months_passed)

pdr_distinct <- pdr_distinct %>% mutate(status = case_when(
  stato == "chiuso" & is.na(date.last.payment)  ~ "failed",
  stato == "aperto" & residual != 0 & sumattivo !=0 ~ "active",
  stato == "chiuso" & residual ==0  ~ "closed",
  stato == "piano di rientro" & residual != 0 ~ "active",
  stato == "aperto" & sumattivo == 0 ~ "proposal"
))

#set the id.group and select the column you need
pdr_distinct$id.group <- NA
pdr_distinct <- pdr_distinct %>% select(idccpdr, id.counterparty, id.bor, id.group, year.agreement, datapdr, importo.dovuto,
                                        amount.agreement, data.inizio.pagamento, date.end, n.instalment,
                                        length, status, sumattivo, residual, date.last.payment)
#rename
pdr_distinct <- pdr_distinct %>% rename(id.agreement= idccpdr,
                                        data.agreement=datapdr, gbv.agreement=importo.dovuto,
                                        date.start=data.inizio.pagamento, paid=sumattivo)
#set the last date remaining as date
pdr_distinct$data.agreement <- as.Date(pdr_distinct$data.agreement, origin= "1899-12-30")

#set some columns as integer
columns_to_transform <- c("year.agreement", "n.instalment", "length")

pdr_distinct[columns_to_transform] <- lapply(pdr_distinct[columns_to_transform], as.integer)

Agreement_Summary <- pdr_distinct



###################################################
###-             Agreement_Proj            -###
###################################################

Agreement_Proj <- pdr_raw %>% select(idccpdr, dtpagamento, importo.singole.rate, dtfatturazione, pagamento) %>%
  rename(id.agreement = idccpdr, date.due = dtpagamento, amount.due = importo.singole.rate,
         date.paid= dtfatturazione, amount.paid=pagamento)








