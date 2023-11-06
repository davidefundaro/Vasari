source("Functions_Orange.R")










###------------------------------------------###
#---         Check primary keys       -----
###------------------------------------------###

possible_keys_LOANS <- possiblePKs(Loans)
possible_keys_COUNTERPARTIES <- possiblePKs(counterparty_finished)
print(possible_keys_LOANS)
print(possible_keys_COUNTERPARTIES)

possible_keys_LOANS <- possible_keys_LOANS %>% rename("NA Presence" = "Has_NA")
possible_keys_COUNTERPARTIES <- possible_keys_COUNTERPARTIES %>% rename("NA Presence" = "Has_NA")

#########################################
##---      Profiling     ---##
#########################################
#abbellire i profiles
#non numeric profile LOANS
Profile_LOANS <- ExpData(data=Loans,type=2) %>% as.data.frame()
Profile_LOANS <- Profile_LOANS %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_LOANS <- Profile_LOANS %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                         "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_LOANS$`% NAs` <- paste0(Profile_LOANS$`% NAs` *100, "%")
Profile_LOANS <- Profile_LOANS %>% select(-NAs)

#numeric profile LOANS
Profile_Numeric <- ExpData(data=Loans,type=2, fun = c("mean", "median", "var")) %>% as.data.frame()
Profile_Numeric<-Profile_Numeric[complete.cases(Profile_Numeric),]
Profile_Numeric <- Profile_Numeric %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                          "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values",
                                          "Mean (k)" = "mean", "Median (k)" = "median", "Var (M)" = "var")
Profile_Numeric$`% NAs` <- paste0(Profile_Numeric$`% NAs` *100, "%")
Profile_Numeric$`Mean (k)` <- round(Profile_Numeric$`Mean (k)`/1000, 2)
Profile_Numeric$`Median (k)` <- round(Profile_Numeric$`Median (k)`/1000, 2)
Profile_Numeric$`Var (M)` <- round(Profile_Numeric$`Var (M)`/1000000, 2)
Profile_Numeric <- Profile_Numeric %>% select(-NAs)

#profile for COUNTERPARTIES
Profile_COUNTERPARTIES <- ExpData(data=counterparty_finished,type=2) %>% as.data.frame()
Profile_COUNTERPARTIES <- Profile_COUNTERPARTIES %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                              "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                              "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_COUNTERPARTIES$`% NAs` <- paste0(Profile_COUNTERPARTIES$`% NAs` *100, "%")


#########################################
##---      charts and tables    ---##
#########################################

###-----------------------------------------------------------------------###
#-----             Loans                     -----         
###-----------------------------------------------------------------------###
#summary table for totals
r.introductionP6 <- Loans %>% 
  summarise(
    'N Borrowers' = n_distinct(id.bor),
    'N LOANS' = n_distinct(id.loan),
    'GBV (M)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loan) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )

#counts and % of type loans - min mean, max gbv for each
total.n <- nrow(Loans)
total.gbv <- sum(Loans$gbv.residual)

r.type.gbv.totals <- Loans %>% summarise("type" = "total", 'Type Cases' = n_distinct(id.loan),
                                                     '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                                     'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                                     'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                     '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

r.type.gbv <- Loans %>% group_by(type) %>% summarise('Type Cases' = n_distinct(id.loan),
                                                     '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                                     'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                                     'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                     '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

r.type.gbv <- rbind(r.type.gbv.totals, r.type.gbv)
r.type.gbv <- r.type.gbv %>% rename(Type = type)

#counts and % of status loans - min mean, max gbv for each
total.n <- nrow(Loans)
total.gbv <- sum(Loans$gbv.residual)

r.status.gbv.totals <- Loans %>% summarise(status = "total", 'Status Cases' = n_distinct(id.loan),
                                                         '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                                         'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                                         'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                         '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))


r.status.gbv <- Loans %>% group_by(status) %>% summarise('Status Cases' = n_distinct(id.loan),
                                                     '% Cases' = paste0(round(n_distinct(id.loan)/total.n*100, 1), "%"),
                                                     'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.loan)/ 1e3, 1) ,
                                                     'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                     '% GBV' =  paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

#in no UTP also use this other half
df <- data.frame('status' = "utp",
                 'StatusCases' = 0,
                 '%Cases' = paste0(0, "%"),
                 'MeanGBVResidual(k)' = 0,
                 'SumGBVResidual(k)' = 0,
                 '%GBVResidual' = paste0(0, "%")
                 )
df <- df %>% rename('status' = "status",
                    'Status Cases' = 'StatusCases',
                 '% Cases' = 'X.Cases',
                 'Mean GBV Residual (k)' = 'MeanGBVResidual.k.',
                 'Sum GBV Residual (k)' = 'SumGBVResidual.k.',
                 '% GBV' = 'X.GBVResidual'
)
r.status.gbv <- rbind(r.status.gbv.totals,r.status.gbv, df)
r.status.gbv <- r.status.gbv %>% rename(Status = status)


# % gbv in each type class
total.n <- nrow(Loans)
total.gbv <- sum(Loans$gbv.residual)
r.gbv.perc <- Loans %>% group_by(type) %>% summarise('GBV %' = round(sum(gbv.residual)/total.gbv*100, 1))
chart.gbv.perc <- ggplot(r.gbv.perc, aes(x = type, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Type", y = "GBV %", title= "GBV Residual % by Loan Type") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
chart.gbv.perc



#cases for date.status
Loans$year <- format(Loans$date.status, "%Y")
r.date.status.cases <- Loans %>% group_by(year) %>% summarise("N Cases" = n_distinct(id.loan)) %>% ungroup()
chart.date.status.cases <- ggplot(r.date.status.cases, aes(x = year, y = `N Cases`, group= 1)) +
geom_bar(stat = 'identity', color= "blue") + labs(x= "Year of State Passage", y= "N Loans",
                                                  title= "Last Status Date")
chart.date.status.cases

###-----------------------------------------------------------------------###
#-----                     loans ranges                              -----         
###-----------------------------------------------------------------------###

quartiles <- quantile(Loans$gbv.residual, probs = c(0.25, 0.5, 0.75))
plot(Loans$gbv.residual)
hist(Loans$gbv.residual)

#gbv ranges chart
gbv_ranges <- c(0, 150000, 300000, 450000, Inf)  # Define the age ranges
gbv_labels <- c("0-100k", "150k-300k", "300k-450k", "+450k")  # Define labels for the ranges

#create range.gbv column, calculate the total gbv, create table with range.gbv and % of gbv per range
df <- Loans
df$range.gbv <- cut(df$gbv.residual, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
total.gbv <- sum(df$gbv.residual)
r.p28.gbvByLoanSize <- df %>% select(gbv.residual, range.gbv) %>% group_by(range.gbv) %>% 
  summarise("GBV %" = round(sum(gbv.residual) / total.gbv *100, 1))

r.p28.g.gbvByLoanSize <- ggplot(r.p28.gbvByLoanSize, aes(x = range.gbv, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "slategray", color = "blue") +    #71EAF7
  labs(x = "GBV Ranges", y = "GBV %", title= "GBV Residual % by Loan Size") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5, color= "white")
r.p28.g.gbvByLoanSize


gg <- ggplot(Loans, aes(x = gbv.residual)) +
  geom_histogram(binwidth = 150000, color = "blue") +
  xlab("GBV Amount") +
  ylab("N Loans") +
  ggtitle("Number of Loans by Amount")
gg


###-----------------------------------------------------------------------###
#-----                     borrowers                               -----         
###-----------------------------------------------------------------------###
Borrowers <- counterparty_finished %>% filter(role=='borrower') %>% distinct()
Borrowers <- left_join(Borrowers,Loans, by = "id.bor",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,link_entity_counterparty,by = "id.counterparty",relationship = "many-to-many")
Borrowers <- Borrowers %>% distinct(id.bor, id.loan, .keep_all = TRUE)
Borrowers <- left_join(Borrowers,entities_complete, by = "id.entity")

#% Borrowers by areas 
total.gbv <- sum(Loans$gbv.residual)
total.borrowers <- n_distinct(Borrowers$id.entity)
r.p27.geographicalDistribution <- Borrowers %>%
  mutate(
    area = case_when(
      area == "center"  ~ "center",
      area == "islands" ~ "south and islands",
      area == "south" ~ "south and islands",
      area == "north-east" ~ "north-east",
      area == "north-west" ~ "north-west"
    )) %>%
  select(id.entity, area, gbv.residual) %>%
  group_by(area) %>%
  summarise("Borrower %" = round(n_distinct(id.entity) / total.borrowers * 100, 1),
            "GBV %" = round(sum(gbv.residual) / total.gbv * 100, 1)
            )
r.p27.borrowerByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `Borrower %`)) +
  geom_bar(stat = 'identity', fill = "#79CDCD") +
  labs(x = "Area", y = "Borrowers %", title= "Borrowers % per Area") + 
  geom_text(aes(label = `Borrower %`), vjust = 0.5, size = 5)
r.p27.borrowerByArea

r.p27.gbvByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#66CDAA") +
  labs(x = "Area", y = "GBV %", title= "GBV % per Area") + 
  geom_text(aes(label = `GBV %`), vjust = 0.5, size = 5)
r.p27.gbvByArea


#table n.entities per borrower
total.n <- n_distinct(Borrowers$id.counterparty)
total.gbv <- sum(Borrowers$gbv.residual)
r.borrower.n.entities.totals <- Borrowers %>% summarise(n.entities = "total",'Borrowers' = n_distinct(id.counterparty),
                                                                          '% Cases' = paste0(round(n_distinct(id.counterparty)/total.n*100, 1), "%"),
                                                                          'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.counterparty)/ 1e3, 1) ,
                                                                          'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                                          '% GBV' = paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))


r.borrower.n.entities <- Borrowers %>% group_by(n.entities) %>% summarise('Borrowers' = n_distinct(id.counterparty),
                                                     '% Cases' = paste0(round(n_distinct(id.counterparty)/total.n*100, 1), "%"),
                                                     'Mean GBV Residual (k)' =   round(total.gbv/n_distinct(id.counterparty)/ 1e3, 1) ,
                                                     'Sum GBV Residual (k)' = round(sum(gbv.residual)/ 1e3, 1),
                                                     '% GBV' = paste0(round(sum(gbv.residual)/total.gbv*100, 1), "%"))

r.borrower.n.entities <- rbind(r.borrower.n.entities.totals, r.borrower.n.entities)
r.borrower.n.entities <- r.borrower.n.entities %>% rename("N Entities" = n.entities)


#table N borrowers with N loans

df <- Borrowers %>% group_by(id.counterparty) %>% summarise("N Loans per Borrower"= n_distinct(id.loan),
                                                            "GBV Sum" = sum(gbv.residual))
total.gbv <- sum(df$`GBV Sum`)
r.n.borrowers.n.loans.totals <- df %>% summarise("N Loans per Borrower" = "total", "Borrowers"= n_distinct(id.counterparty),
                                                                               "GBV Residual Sum (k)" = round(sum(`GBV Sum`)/1e3, 1),
                                                                               '% GBV' = paste0(round(sum(`GBV Sum`)/total.gbv*100, 1), "%"))


r.n.borrowers.n.loans <- df %>% group_by(`N Loans per Borrower`) %>% summarise("Borrowers"= n_distinct(id.counterparty),
                                                            "GBV Residual Sum (k)" = round(sum(`GBV Sum`)/1e3, 1),
                                                            '% GBV' = paste0(round(sum(`GBV Sum`)/total.gbv*100, 1), "%"))


r.n.borrowers.n.loans <- rbind(r.n.borrowers.n.loans.totals, r.n.borrowers.n.loans)

entities_merged <- entities_complete %>% merge(link_entity_counterparty, by= "id.entity")
entities_merged2 <- entities_merged %>% merge(counterparty_finished, by="id.counterparty")
entities_merged2 <- entities_merged2 %>% filter(role== "borrower")

# n. borrowers by type
total.borrowers <- n_distinct(entities_merged2$id.counterparty)
r.type.subject.n.cases.totals <- entities_merged2 %>% summarise(type.subject = "total","N Borrowers" = n_distinct(id.counterparty),
                                                                                    "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.borrowers * 100, 1), "%"))


r.type.subject.n.cases <- entities_merged2 %>% group_by(type.subject) %>% summarise("N Borrowers" = n_distinct(id.counterparty),
                                                                  "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.borrowers * 100, 1), "%"))

r.type.subject.n.cases <- rbind(r.type.subject.n.cases.totals, r.type.subject.n.cases)
r.type.subject.n.cases <- r.type.subject.n.cases %>% rename("Borrower Type" = type.subject)


#age range borrowers
individuals <- entities_merged2 %>% filter(type.subject == "individual")
individuals_distinct <- individuals[!duplicated(individuals[c("id.counterparty")]), ]
total.individuals <- nrow(individuals_distinct)

r.individuals.range.age.totals <- individuals_distinct %>% 
  summarise(range.age = "total","N Borrowers"= n_distinct(id.counterparty),
            "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.individuals * 100, 1), "%"))


r.individuals.range.age <- individuals_distinct %>% group_by(range.age) %>% 
  summarise("N Borrowers"= n_distinct(id.counterparty),
            "Borrower %" = paste0(round(n_distinct(id.counterparty) / total.individuals * 100, 1), "%"))

r.individuals.range.age <- rbind(r.individuals.range.age.totals, r.individuals.range.age)
r.individuals.range.age <- r.individuals.range.age %>% rename("Age Range" = range.age)







#GBV by borrower province
r.p27.borrowersByProvince <- Borrowers %>% select(id.counterparty,gbv.residual,province) %>% distinct() %>% 
  group_by(province) %>% 
  summarise(sum_gbv = round(sum(gbv.residual)/ 1e6, 1), 
            N_borr = n_distinct(id.counterparty), 
            avg_size = round( sum(gbv.residual)/N_borr / 1e3, 1)) %>% 
  arrange(desc(sum_gbv))
# the top 5 are Roma (rm), Teramo(te), Pescara(pe) ,Milano (mi), Genova (ge)
r.p27.borrowersByProvince.head <- head(r.p27.borrowersByProvince, 5)
r.p27.borrowersByProvince.head <- r.p27.borrowersByProvince.head %>% as.data.frame()

r.p27.borrowersByProvince.head <- r.p27.borrowersByProvince.head %>% mutate(
  province = case_when(
    province == "fi"  ~ "firenze",
    province == "bo" ~ "bologna",
    province == "ra" ~ "ravenna",
    province == "pi" ~ "pisa",
    is.na(province) ~ "NA"
  ))

r.p27.borrowersByProvince.head <- r.p27.borrowersByProvince.head %>%
  arrange(desc(sum_gbv))

r.p27.borrowersByProvince.head.support.table <- r.p27.borrowersByProvince.head %>% select(-sum_gbv)
r.p27.borrowersByProvince.head.support.table <- r.p27.borrowersByProvince.head.support.table %>% 
  rename("N Borrowers" = "N_borr", "Mean Size Borrowers (k)" = "avg_size")

r.p27.borrowersByProvince.top.5 <- ggplot(r.p27.borrowersByProvince.head, aes(x = sum_gbv, y = reorder(province, sum_gbv))) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "GBV Sum (M)", y = "Province", title= "GBV Sum per Province") + 
  geom_text(aes(label = sum_gbv), vjust = 1, size = 5)
r.p27.borrowersByProvince.top.5



###-----------------------------------------------------------------------###
#-----                profiling  info providing pf                       -----         
###-----------------------------------------------------------------------###


Profile_infoprov_pf <- ExpData(data=infoprov_pf_final,type=2) %>% as.data.frame()
Profile_infoprov_pf <- Profile_infoprov_pf %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_infoprov_pf <- Profile_infoprov_pf %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                          "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_infoprov_pf$`% NAs` <- paste0(Profile_infoprov_pf$`% NAs` *100, "%")
Profile_infoprov_pf <- Profile_infoprov_pf %>% select(-NAs)




Profile_Numeric_infoprov <- ExpData(data=infoprov_pf_final,type=2, fun = c("mean", "median", "var")) %>% as.data.frame()
Profile_Numeric_infoprov<-Profile_Numeric_infoprov[complete.cases(Profile_Numeric_infoprov),]
Profile_Numeric_infoprov <- Profile_Numeric_infoprov %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                              "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                              "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values",
                                              "Mean (k)" = "mean", "Median (k)" = "median", "Var (M)" = "var")
Profile_Numeric_infoprov$`% NAs` <- paste0(Profile_Numeric_infoprov$`% NAs` *100, "%")
Profile_Numeric_infoprov$`Mean (k)` <- round(Profile_Numeric_infoprov$`Mean (k)`/1000, 2)
Profile_Numeric_infoprov$`Median (k)` <- round(Profile_Numeric_infoprov$`Median (k)`/1000, 2)
Profile_Numeric_infoprov$`Var (M)` <- round(Profile_Numeric_infoprov$`Var (M)`/1000000, 2)
Profile_Numeric_infoprov <- Profile_Numeric_infoprov %>% select(-NAs)


###-----------------------------------------------------------------------###
#-----                profiling  info providing pg                       -----         
###-----------------------------------------------------------------------###

Profile_infoprov_pg <- ExpData(data=infoprov_piva_final,type=2) %>% as.data.frame()
Profile_infoprov_pg <- Profile_infoprov_pg %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_infoprov_pg <- Profile_infoprov_pg %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                                      "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                                      "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_infoprov_pg$`% NAs` <- paste0(Profile_infoprov_pg$`% NAs` *100, "%")
Profile_infoprov_pg <- Profile_infoprov_pg %>% select(-NAs)

#infoprov_cf_final  n.cases per solvency type
chart.info.pf.solvency.region <- infoprov_pf_final %>% 
  group_by(solvency.adj) %>% 
  summarise(cases = n())

my.palette <- c("#97FFFF", "#79CDCD", "#528B8B")

chart.solvency.class.cases <- ggplot(chart.info.pf.solvency.region, aes(x = solvency.adj, y= cases)) +  
  geom_col(aes(fill = solvency.adj)) +  
  labs(title = "Solvency Adjusted N Cases", x = "Solvency Type", y= "N Cases") +  
  scale_fill_manual(values = my.palette) + geom_text(aes(label = cases), vjust = 1.5, size = 5)
chart.solvency.class.cases

#mean income.net per solvency type
table.info.income.solvency.totals <- infoprov_pf_final %>% filter(!is.na(income.net)) %>%
  summarise(solvency.adj = "total", mean_income = round(mean(income.net), 1))
table.info.income.solvency <- infoprov_pf_final %>% filter(!is.na(income.net)) %>%
  group_by(solvency.adj) %>% 
  summarise(mean_income = round(mean(income.net), 1))
table.info.income.solvency <- rbind(table.info.income.solvency.totals, table.info.income.solvency)
table.info.income.solvency <- table.info.income.solvency %>% rename("Solvency Type" = solvency.adj,
                                                                    "Mean Net Income" = mean_income)



table.info.income.region.totals <- infoprov_pf_final %>% filter(!is.na(income.net)) %>% 
  summarise(region = "total", mean_income = round(mean(income.net), 1))

table.info.income.region <- infoprov_pf_final %>% filter(!is.na(income.net)) %>%
  group_by(region) %>% summarise(mean_income = round(mean(income.net), 1)) %>% arrange(desc(mean_income))
table.info.income.region <- rbind(table.info.income.region.totals, table.info.income.region)
table.info.income.region <- table.info.income.region %>% rename("Region" = region,
                                                                    "Mean Net Income" = mean_income)

#infoprov_piva_final 
#% of status per type of corporate

table.info.piva.type <- infoprov_piva_final %>% filter(!is.na(type)) %>% group_by(type, status) %>%
  summarise(cases= n())

table.info.piva.type <- table.info.piva.type %>% pivot_wider(names_from= status, values_from = cases,
                                                             id_expand = FALSE)
table.info.piva.type[is.na(table.info.piva.type)] <- 0



rowtotals <- rowSums(table.info.piva.type[,-1])
table.info.piva.type2 <- round(table.info.piva.type[,-1]/rowtotals*100,1)
table.info.piva.type2 <- table.info.piva.type2 %>% mutate(type = table.info.piva.type$type)
table.info.piva.type2 <- table.info.piva.type2[, c(7,1:6)]
table.info.piva.type <- table.info.piva.type2

#n.cases status per region


table.info.piva.status.region <- infoprov_piva_final %>% filter(!is.na(status)) %>%
  group_by(region, status) %>% summarise(cases = n()) %>% arrange(desc(cases))
table.info.piva.status.region <- table.info.piva.status.region %>% pivot_wider(names_from= status, values_from = cases,
                                                             id_expand = FALSE)

table.info.piva.status.region[is.na(table.info.piva.status.region)] <- 0



###-----------------------------------------------------------------------###
#-----               Agreement Summary                     -----         
###-----------------------------------------------------------------------###

agreement.summary.status <- Agreement_Summary 

agreement.summary.status.totals <- agreement.summary.status %>% 
  summarise(status = "total", "GBV Mean" = round(mean(gbv.agreement),1), "GBV sd" = round(sd(gbv.agreement),1),
            "GBV Sum" = round(sum(gbv.agreement),1),
            "Amount Mean" = mean(amount.agreement), "Amount sd" = round(sd(amount.agreement),1),
            "Amount Sum" = sum(amount.agreement),
            Months = mean(length), "Sum Amount Paid" = sum(paid), 
            "% of Payback" = paste0(round(sum(paid)/sum(amount.agreement)*100,1), "%"))

agreement.summary.status <- agreement.summary.status %>% group_by(status) %>% 
  summarise("GBV Mean" = round(sum(gbv.agreement)/n(),1), "GBV sd" = round(sd(gbv.agreement),1),
            "GBV Sum" = round(sum(gbv.agreement),1),
            "Amount Mean" = sum(amount.agreement)/n(), "Amount sd" = round(sd(amount.agreement),1),
            "Amount Sum" = sum(amount.agreement),
            Months = mean(length), "Sum Amount Paid" = sum(paid), 
            "% of Payback" = paste0(round(sum(paid)/sum(amount.agreement)*100,1), "%"))

agreement.summary.status <- rbind(agreement.summary.status.totals, agreement.summary.status)
agreement.summary.status <- agreement.summary.status %>% rename("Status" = status)

agreement.proj.period <- Agreement_Proj
agreement.proj.period$year_month <- format(as.Date(agreement.proj.period$date.paid), "%Y-%m")
agreement.proj.period<- agreement.proj.period[complete.cases(agreement.proj.period$year_month), ]
agreement.proj.period <- agreement.proj.period %>% group_by(year_month) %>% 
  summarise("Amount Paid" = sum(amount.paid))

chart.date.amount.paid <- ggplot(agreement.proj.period, aes(x = year_month, y = `Amount Paid`, group= 1)) +
  geom_line(stat = 'identity', color= "blue") + labs(x= "Year and Month", y="Amount", title= "Payback") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chart.date.amount.paid



agreement.summary.n.cases.status <- Agreement_Summary 

agreement.summary.n.cases.status.totals <- agreement.summary.n.cases.status %>% 
  summarise(status = "total", "N Agreements" = n(), "Mean Instalments" = mean(n.instalment))

agreement.summary.n.cases.status <- agreement.summary.n.cases.status %>% group_by(status) %>% summarise(
  "N Agreements" = n(), "Mean Instalments" = mean(n.instalment)
)

agreement.summary.n.cases.status <- rbind(agreement.summary.n.cases.status.totals, agreement.summary.n.cases.status)
agreement.summary.n.cases.status <- agreement.summary.n.cases.status %>% rename(Status = status)



###-----------------------------------------------------------------------###
#-----                     Page 29 report                                -----         
###-----------------------------------------------------------------------###
#gbv by default vintage
bv_ranges <- c(0, 8, 11, 14, 17, 20, Inf)  # Define the age ranges
gbv_labels <- c("0-8 years", "9-11 years", "12-14 years", "15-17 years", "18-20 years", "+20 years")  # Define labels for the ranges

df <- LOANS
df <- df %>%
  mutate(vintage = floor(as.numeric(interval(date.status, as.Date("2021-11-30")) / dyears(1))))
df$range.date <- cut(df$vintage, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
r.p29.gbvByDefaultVintage <- df %>% select(gbv.residual, range.date) %>% group_by(range.date) %>% 
  summarise("GBV Total (m)" = round(sum(gbv.residual)/1e6,1),
            "GBV %" = round(sum(gbv.residual) / total.gbv *100, 1),
            "Loans %" = round(n() / nrow(df) * 100, 1),
            "Avg. Loan size (€k)" = round(sum(gbv.residual)/n()/1e3,1))

r.p29.g.gbvByDefaultVintage <- ggplot(r.p29.gbvByDefaultVintage, aes(x = range.date, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  geom_text(aes(label = paste0(`GBV %`, "%"), vjust = -0.1), size = 5) +
  labs(x = "Year Ranges", y = "GBV %", title = "GBV By Default Vintage") +
  ylim(0, 32) +
  annotate("text", x = 1:6, y = rep(0, 6), label = paste0(r.p29.gbvByDefaultVintage$`GBV Total (m)`, "M"), size = 5, hjust = 0.5, vjust = -25, color = "#054A53")
r.p29.g.gbvByDefaultVintage

r.p29.g.loansByDefaultVintage <- ggplot(r.p29.gbvByDefaultVintage, aes(x = range.date, y = `Loans %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  geom_text(aes(label = paste0(`GBV %`, "%"), vjust = -0.1), size = 5) +
  labs(x = "Year Ranges", y = "Loans %", title = "Loans By Default Vintage", subtitle = "and avg. loan size") +
  ylim(0, 32) +
  annotate("text", x = 1:6, y = rep(0, 6), label = paste0(r.p29.gbvByDefaultVintage$`Avg. Loan size (€k)`, "€k"), size = 5, hjust = 0.5, vjust = -24.5, color = "#054A53")
r.p29.g.loansByDefaultVintage


###-----------------------------------------------------------------------###
#-----                     Page 31 report                                -----         
###-----------------------------------------------------------------------###
garanzia <- garanzie_cleaned %>% rename(id.loan = numero.rapporto.garantito)
df <- merge(Loans, garanzia[,c("id.loan", "descrizione.garanzia")], by = "id.loan", all.x= TRUE)
df <- df %>%
  distinct(id.loan, .keep_all = TRUE)
df$guarantee.presence <- ifelse(!is.na(df$descrizione.garanzia), "guaranteed", "not guaranteed")
total.gbv <- sum(df$gbv.residual)

r.p31.gbvByGuaranteePresence.totals <- df %>% summarise(guarantee.presence = "total", "N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.gbv * 100, 1), "%"))

r.p31.gbvByGuaranteePresence <- df %>%
  #select(gbv.residual, guarantee.presence) %>%
  group_by(guarantee.presence) %>%
  summarise("N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.gbv * 100, 1), "%"))

r.p31.gbvByGuaranteePresence <- rbind(r.p31.gbvByGuaranteePresence.totals, r.p31.gbvByGuaranteePresence)
r.p31.gbvByGuaranteePresence <- r.p31.gbvByGuaranteePresence %>% rename("Guarantee Status"=guarantee.presence)

#gbv % per guaranteed and no guaranteed
custom_colors <- c("#054A53", "#ACDBE1")
r.p31.g.gbvByGuaranteePresence <- ggplot(r.p31.gbvByGuaranteePresence, aes(x = "", y = `GBV %`, fill = guarantee.presence)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Guarantee Presence") +
  theme_void() +
  geom_text(aes(label = paste0(`GBV %`, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = custom_colors)
r.p31.g.gbvByGuaranteePresence


#gbv % per guarantee type chart
df1 <- df %>%
  filter(guarantee.presence == "guaranteed")
total.guaranteed.gbv <- sum(df1$gbv.residual)

df1 <- df1 %>%
  mutate(descrizione.garanzia = case_when(
    str_detect(descrizione.garanzia, "fideiussione")  ~ "surety",
    descrizione.garanzia == "effetto in bianco"  ~ "surety",
    TRUE ~ "pledge"  # Catch-all condition for "corporate"
  ))
r.p31.gbvByGuaranteedType.totals <- df1 %>% 
  summarise(descrizione.garanzia = "total", "N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.guaranteed.gbv *100, 1), "%"))

r.p31.gbvByGuaranteedType <- df1 %>% group_by(descrizione.garanzia) %>% 
  summarise("N Loans" = n(), "GBV %" = paste0(round(sum(gbv.residual) / total.guaranteed.gbv *100, 1), "%"))

r.p31.gbvByGuaranteedType <- rbind(r.p31.gbvByGuaranteedType.totals, r.p31.gbvByGuaranteedType)
r.p31.gbvByGuaranteedType <- r.p31.gbvByGuaranteedType %>% rename("Guarantee Type" = descrizione.garanzia)




r.p31.g.gbvByGuaranteedType <- ggplot(r.p31.gbvByGuaranteedType, aes(x = guarantee.type, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Guarantee Type", y = "GBV %", title= "GBV Residual % per Guarantee Type") + 
  geom_text(aes(label = `GBV %`), vjust = 1, size = 5)
r.p31.g.gbvByGuaranteedType
#custom_colors <- c("#054A53", "#ACDBE1", "#008197", "#28464B", "#003A44", "#595F60")

