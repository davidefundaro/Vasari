
link_counterparties_loans_first_half <- Loans %>% select(id.loan, id.bor)
link_counterparties_loans_second_half <- counterparty_finished %>% select(id.counterparty, id.bor)

link_counterparties_loans <- link_counterparties_loans_second_half %>% merge(link_counterparties_loans_first_half,
                                                                                by= "id.bor")
link_counterparties_loans <- link_counterparties_loans %>% select(-id.bor)
link_counterparties_loans <- link_counterparties_loans[!duplicated(link_counterparties_loans), ]

checkcolumns(link_counterparties_loans$id.loan, Loans$id.loan)
