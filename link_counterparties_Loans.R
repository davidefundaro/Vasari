link_fake1 <- Loans %>% select(id.loans,id.bor)

link_garanzie <- Garanzie_Raw %>% select(NDG,id.bor)

link_disperati <- left_join(counterparties_finished,link_garanzie,by = "id.bor")

link_disperati$NDG <- ifelse(is.na(link_disperati$NDG), link_disperati$id.bor, link_disperati$NDG)

link_disperati <- link_disperati %>% select(-id.bor)

link_disperati <- link_disperati %>% rename(id.bor = NDG)

link_disperati <- link_disperati %>% distinct()

link_fake2 <- left_join(link_disperati,link_fake1, by = "id.bor")


link_fake3 <- link_fake2 %>% select(id.counterparty,id.loans)

link_counterparties_loans <- distinct(link_fake3)
