source("Functions_Orange.R")
###------------------------------------------###
#---         Check primary keys       -----
###------------------------------------------###

possible_keys_LOANS <- possiblePKs(Loans)
possible_keys_COUNTERPARTIES <- possiblePKs(counterparties_finished)
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
Profile_COUNTERPARTIES <- ExpData(data=counterparties_finished,type=2) %>% as.data.frame()
Profile_COUNTERPARTIES <- Profile_COUNTERPARTIES %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                              "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                              "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_COUNTERPARTIES$`% NAs` <- paste0(Profile_COUNTERPARTIES$`% NAs` *100, "%")


#########################################
##---      charts and tables    ---##
#########################################

###-----------------------------------------------------------------------###
#-----                     Page 6 report                                 -----         
###-----------------------------------------------------------------------###
#summary table for totals
r.introductionP6 <- Loans %>% 
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '# LOANS' = n_distinct(id.loans),
    'GBV (m)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loans) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )

###-----------------------------------------------------------------------###
#-----                     Page 27 reports                                -----         
###-----------------------------------------------------------------------###
Borrowers <- counterparties_finished %>% filter(role=='borrower') %>% distinct()
Borrowers <- left_join(Borrowers,Loans, by = "id.bor",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,link_table_entities_counterparties,by = "id.counterparty",relationship = "many-to-many")
Borrowers <- Borrowers %>% distinct(id.bor, id.loans, .keep_all = TRUE)
Borrowers <- left_join(Borrowers,entities_complete, by = "id.entities")

#% Borrowers by areas 
total.gbv <- sum(Loans$gbv.residual)
total.borrowers <- n_distinct(Borrowers$id.entities)
r.p27.geographicalDistribution <- Borrowers %>%
  mutate(
    area = case_when(
      area == "center"  ~ "center",
      area == "islands" ~ "south and islands",
      area == "south" ~ "south and islands",
      area == "north-east" ~ "north-east",
      area == "north-west" ~ "north-west"
    )) %>%
  select(id.entities, area, gbv.residual) %>%
  group_by(area) %>%
  summarise("Borrower %" = round(n_distinct(id.entities) / total.borrowers * 100, 1),
            "GBV %" = round(sum(gbv.residual) / total.gbv * 100, 1)
            )
r.p27.borrowerByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `Borrower %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Area", y = "Borrowers %", title= "Borrowers % per Area") + 
  geom_text(aes(label = `Borrower %`), vjust = 0.5, size = 5)
r.p27.borrowerByArea

r.p27.gbvByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Area", y = "GBV %", title= "GBV % per Area") + 
  geom_text(aes(label = `GBV %`), vjust = 0.5, size = 5)
r.p27.gbvByArea


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
#-----                     Page 28 report                                -----         
###-----------------------------------------------------------------------###

quartiles <- quantile(Borrowers$gbv.residual, probs = c(0.25, 0.5, 0.75))
plot(Borrowers$gbv.residual)
hist(Borrowers$gbv.residual)

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
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "GBV Ranges", y = "GBV %", title= "GBV Residual % by Loan Size") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
r.p28.g.gbvByLoanSize


gg <- ggplot(Borrowers, aes(x = gbv.residual)) +
  geom_histogram(binwidth = 150000, fill = "lightblue", color = "black") +
  xlab("Values") +
  ylab("Frequency") +
  ggtitle("Histogram of Values")
gg

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

#infoprov_cf_final  n_distinct(solvency.asj) per region, sum or % of income.net per solvency.asj class

chart.info.pf.solvency.region <- infoprov_pf_final %>% 
  group_by(region, solvency.adj) %>% 
  summarise(cases = sum(!is.na(solvency.adj), na.rm = TRUE))




#infoprov_piva_final n(status) per type











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
df <- merge(LOANS, original.GuaranteesData[,c("id.loan", "guarantee.type")], by = "id.loan", all.x= TRUE)
df <- df %>%
  distinct(id.loan, .keep_all = TRUE)
df$guarantee.presence <- ifelse(!is.na(df$guarantee.type), "guaranteed", "not guaranteed")

r.p31.gbvByGuaranteePresence <- df %>%
  select(gbv.residual, guarantee.presence) %>%
  group_by(guarantee.presence) %>%
  summarise("GBV %" = round(sum(gbv.residual) / total.gbv * 100, 2))

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
r.p31.gbvByGuaranteedType <- df1 %>% select(gbv.residual, guarantee.type) %>% group_by(guarantee.type) %>% 
  summarise("GBV %" = round(sum(gbv.residual) / total.guaranteed.gbv *100, 2))

r.p31.g.gbvByGuaranteedType <- ggplot(r.p31.gbvByGuaranteedType, aes(x = guarantee.type, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Guarantee Type", y = "GBV %", title= "GBV Residual % per Guarantee Type") + 
  geom_text(aes(label = `GBV %`), vjust = 1, size = 5)
r.p31.g.gbvByGuaranteedType
#custom_colors <- c("#054A53", "#ACDBE1", "#008197", "#28464B", "#003A44", "#595F60")

