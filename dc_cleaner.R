#Scripts licensed under GPL v3 - check license for more info
#Remove hashtag from below to install all packages for dc_cleaner & app.R  and initiate renv
#source("renv_install.R")
gitlink <- "www.yourlinkher.ca"
library(tidyverse)
library(lubridate)
library(RColorBrewer)
`%notin%` <- Negate(`%in%`)

#####--------------------END FUNCTIONS------------------------------------#####
####---Dates and data loading----####
#Creates a data frame of date ranges to be used later for making sliding scale in network graph and line chart
source("Day_making.R")
#Link to a scraper that pulls from the BCCSU legacy table
#There are a lot of issues with this dataset that the new DrugSense app doesn't have, 
#but the legacy table is more accessible
#Link above provided for educational purposes only

dcbc <- read.csv(gitlink, na.strings=c("N/A","", "None"), stringsAsFactors=FALSE)



####----------------Pre-processing/Cleaning Data----------------------------####
#Subset of the Data that will be used
interest <- c("Fentanyl/Down", "Opioids Minus Fentanyl (Grouped)", "All Opioids (Grouped)", "Methamphetamine",
              "Ketamine", "Cocaine HCl", "Crack Cocaine", "MDMA")


#Creates data frame columns, and does basic cleaning
colnames(dcbc) <- c("Date",
                    "City.Town","Site",
                    "Expected.Substance",
                    "Benzo.Test.Strip",
                    "Fentanyl.Test.Strip","FTIR.Spectrometer")
####----General Cleaning from Scrape----####
dcbc$FTIR.Spectrometer <- gsub("\\Q['\\E|\\Q']\\E|\\Q'\\E", "", dcbc$FTIR.Spectrometer)
dcbc$FTIR.Spectrometer[dcbc$FTIR.Spectrometer == "1, 4-butanediol"] <- "BDO"
dcbc <- dcbc %>%
  mutate(across(ends_with("Strip"), ~ str_replace_all(.x, c("Pos" = "True",
                                                            "Neg" = "False",
                                                            "Inv" = NA))),
         FTIR.Spectrometer = str_replace_all(FTIR.Spectrometer,c("Neg"= "No Library Match",
                                                                 "hcl" = "HCl",
                                                                 "base" = "Base"))) %>%
  separate(FTIR.Spectrometer,
           into = c("FTIR.1", "FTIR.2", "FTIR.3", "FTIR.4", "FTIR.5", "FTIR.6"),
           sep = ", ", extra = "merge",
           fill = "right", remove = FALSE) %>%
  rowid_to_column("ID") #Create a row ID for the columns, necessary for later


#####----Subsets/formats date range convert & Convert Test Strip to T/F-----####

dcbc2 <- dcbc #backup, just in case
dcbc2$Date <- ymd(dcbc2$Date)
dcbc2 <- dcbc2[dcbc2$Date > as.Date("2019-12-28"),]
dcbc2$Date <- as.Date(dcbc2$Date, format = "%Y-%m-%d")
poss.w <- poss.w[poss.w$Days >= min(dcbc2$Date) & poss.w$Days <= date(head(dcbc2$Date, n = 1)),]
dcbc2$fent.p <- 0
dcbc2$fent.p[grepl("[Ff]ent", dcbc2$FTIR.Spectrometer)] <- 1
dcbc2$fent.p[which(dcbc2$Fentanyl.Test.Strip == "True")] <- 1

#Change test strips to binary
dcbc2$Benzo.Test.Strip[dcbc2$Benzo.Test.Strip == "True"] <- "1"
dcbc2$Benzo.Test.Strip[dcbc2$Benzo.Test.Strip == "False"] <- "0"
dcbc2$Benzo.Test.Strip[which(is.na(dcbc2$Benzo.Test.Strip))] <- NA
dcbc2$Benzo.Test.Strip <- as.numeric(dcbc2$Benzo.Test.Strip)


####----Finds all the benzos & fent to create a count----####
#First find the start & end variables
ftnum <- c(match("FTIR.1",names(dcbc2)), match("FTIR.6",names(dcbc2)))
#Finds all benzos
benzo.match <- unlist(dcbc2[,ftnum]) %>%
  .[grepl("epam|olam", .)]
benzo.match <- unique(benzo.match)
dcbc2$has.benzo <- 0

#Finds all benzo positive strips which do not have positive benzo responses
dcbc2$has.benzo[dcbc2$FTIR.1 %notin% benzo.match & dcbc2$Benzo.Test.Strip ==1 &
                  dcbc2$FTIR.2 %notin% benzo.match & dcbc2$FTIR.3 %notin% benzo.match &
                  dcbc2$FTIR.4 %notin% benzo.match & dcbc2$FTIR.5 %notin% benzo.match &
                  dcbc2$FTIR.6 %notin% benzo.match] <- 1
#loop finds the positive fent and benzo strips where they are not listed
#Answer from stackoverflow: https://stackoverflow.com/a/64994253/7263991
cols_have <- paste("FTIR", 1:6, sep=".")
colidx <- names(dcbc2) %in% cols_have
for(i in 1:2){
  ci <- seq_along(dcbc2[colidx])[max.col(cbind(is.na(dcbc2[colidx]), TRUE), ties.method = "first")]
  if(i == 1){
    ri <- rowSums(sapply(dcbc2[colidx], `%in%`, op2)) == 0 &  dcbc2$fent.p == 1 & !grepl("[Ff]ent", dcbc2$FTIR.1)
    dcbc2[colidx][na.omit(cbind(which(ri), ci[ri]))] <- "Fent <5%"
  }
  if(i==2){
    benzo.match2 <- c(benzo.match,"Fent <5%")
    ri <- rowSums(sapply(dcbc2[colidx], `%in%`, benzo.match2)) == 0 &  dcbc2$has.benzo == 1
    dcbc2[colidx][na.omit(cbind(which(ri), ci[ri]))] <- "Benzodiazepine <5%"
  }
}
#In order to compute the data all FTIR.2 cannot be empty - no cuts, represents when no adulterants were found
dcbc2$FTIR.2[which(is.na(dcbc2$FTIR.2))] <- paste("No Cuts\n", dcbc2$FTIR.1[which(is.na(dcbc2$FTIR.2))], sep = "")
dcbc2$FTIR.1[which(grepl("No Cuts", dcbc2$FTIR.2))] <- dcbc2$FTIR.2[which(grepl("No Cuts", dcbc2$FTIR.2))]
dcbc2 <- dcbc2[dcbc2$FTIR.1 != "",] %>%
  mutate(across(where(is.character), str_trim))

rownames(dcbc2) <- NULL
hb2 <- paste(benzo.match, collapse = "|")
dcbc2$has.benzo2 <- 0
dcbc2 <- dcbc2 %>%
  mutate(has.benzo2 = ifelse(str_detect(FTIR.Spectrometer, hb2) == TRUE |
                               Benzo.Test.Strip == 1, 1,0))
dcbc2$has.benzo2[which(is.na(dcbc2$has.benzo2))] <- 0
####----------------Changing names for readability in graph----------------####
dcbc2 <- dcbc2 %>%
  mutate(Expected.Substance = str_replace_all(Expected.Substance, 
                                              c("Down \\Q(Unknown Opioid)\\E|Fentanyl"="Fentanyl/Down",
                                                "^Cocaine$" = "Cocaine HCl"
                                                
                                              ))) %>%
  pivot_longer(cols = c(FTIR.1:FTIR.6),
               names_to = "name",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(value = str_replace_all(value,c("Fentanyl HCl" = "Fentanyl or Analog", 
                                         "Dextromethorphan" = "DXM",
                                         "6-Monoacetylmorphine" = "6-MAM", "Surcrose" = "Sucrose", 
                                         "Polyethylene Glycol" = "PEG", "Lysergic Acid Diethylamide" = "LSD", 
                                         "Cocaine Base" = "Crack Cocaine", 
                                         "etizolam" = "Etizolam"
  ))) %>%
  mutate(value = str_replace(value,"Sugar Possibly [A-Za-z]+", "Sugar Uncertain")) %>%
  mutate(value = str_replace(value, "Heroin \\Q(Trace)\\E", "Heroin")) %>%
  mutate(value = gsub("Uncertain Oil|Sugar Uncertain|Uncertain Carbohydrate",
                      "Uncertain Oil/Carb/Sugar", value)) %>%
  mutate(value = str_replace(value, " HCl", "")) %>%
  mutate(value = str_replace(value, "Fentanyl$", "Fentanyl or Analog"))


dcbc2$value <- str_to_title(dcbc2$value)
#Subset the n of Expected Substances to what will be used

interest <- c(unique(dcbc2$Expected.Substance[grepl("[Ff]ent|[Mm]orph",
                                                    dcbc2$Expected.Substance)]), 
              "Methamphetamine","Ketamine", "Cocaine HCl",
              "Crack Cocaine", "MDMA", "Alprazolam", 
              "Etizolam", 
              unique(dcbc2$Expected.Substance[grepl("zene",
                                                    dcbc2$Expected.Substance)]))
####-------------------Create Grouped Categories---------------------------####
#This will create a hand written dataset
#Honestly it's a bit of an unwieldy process to try to change at this point
#I suggest making a csv file and editing it manually in excel & reading it back in
#Conversely you can use, the package "editData" to edit it
source("Drug Classification.R")
#Resulting data.frame is type.of.drug - see source code for more info
no_fent <- type.of.drug %>%
  filter(Classification == "Opioid" & !str_detect(Drug.Name, "[Ff]ent") &
           !str_detect(Drug.Name, "No Cuts"))


int_expect <- plyr::count(dcbc2$Expected.Substance[dcbc$Date >as.Date("2020-01-01")]) %>%
  dplyr::filter(freq > 20, x != "Unknown") %>%
  arrange(-freq)
main_drugs <- unique(c(interest, int_expect[c(1:20),1]))

#Selects all opioids - fent
dc_nofent <- dcbc2[dcbc2$Expected.Substance %in% no_fent$Drug.Name,]
dc_nofent$Expected.Substance <- "All Opioids Minus Fentanyl/Fentanyl Analogues"

dcbc2 <- dcbc2 %>%
  filter(Expected.Substance %in% main_drugs) %>%
  bind_rows(dc_nofent)



###Creates df for classification and the colour palette & nodes------------####
df_sub <- which(names(dcbc)%in%c("Expected.Substance", "value"))

#Creates the edgelist - note that this takes really long & is not recommended
#To be run on your own computer, especially older ones
dcbc4 <- dcbc2 %>%
  select(ID,Date, Expected.Substance, City.Town) %>%
  distinct(ID, .keep_all = TRUE)
dcbc3 <- dcbc2%>%
  select(ID, value) %>%
  nest(data=(value)) %>%
  mutate(pairs=map(data, ~as_tibble(t(combn(.$value, 2))), .name_repair= "unique", .keep))
Edges <- dcbc3 %>%
  unnest(pairs) %>%
  select(-data)%>%
  inner_join(dcbc4, by = "ID") %>%
  select(ID, Date, Expected.Substance, City.Town, V1, V2)
rm(dcbc3, dcbc4)



node_col <- data.frame(Names = unique(dcbc2$value)) %>%
  rowid_to_column("ID")

node_col$Classification <- type.of.drug$Classification[match(node_col$Names,type.of.drug$Drug.Name)]
#Since the dictionary is handwritten  this next line just makes sure it won't throw errors
node_col$Classification[is.na(node_col$Classification)] <- "Other or NA" 
regrouped <- data.frame(ID = seq(2000, 1999+length(unique(node_col$Classification)),by=1),
                        Names = unique(unique(node_col$Classification)),
                        Classification = unique(unique(node_col$Classification)))
node_col <- rbind(node_col, regrouped)

new.op <- unique(dcbc2$Expected.Substance[grepl("[Ff]ent|zene", dcbc2$Expected.Substance)])
new.op <- new.op[!grepl("Minus", new.op)]



####------------------Creates Table for Benzos------------------------------####
#Create new df
b.match2 <- paste(benzo.match, collapse = "|")

benzo <- dcbc2 %>%
  filter(Expected.Substance %in% interest) %>%
  pivot_wider(names_from = name, values_from = value)
#Grouped categories
b_fent <- dcbc2 %>%
  filter(Expected.Substance %in% new.op) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(Expected.Substance = "All Fentanyl & Fentanyl Analogues (Grouped)")
benzo <- bind_rows(benzo, b_fent)
benzo.all <-benzo %>%
  mutate(City.Town = "All of BC")
benzo <- bind_rows(benzo,benzo.all) 
benzo$WeekID <- poss.w$ID[match(benzo$Date, poss.w$Days)]


#Creates df that includes whether a drug was *not* tested for benzos
benzo2 <- benzo %>%
  mutate(B.Not.Test = ifelse(is.na(Benzo.Test.Strip) & has.benzo2 == 0, 1,0)) %>%
  select(WeekID,  City.Town, B.Not.Test, Expected.Substance, fent.p, has.benzo2) %>%
  group_by(WeekID, Expected.Substance, City.Town) %>%
  summarize(WeekID = unique(WeekID), 
            City.Town = unique(City.Town),
            Expected.Substance = unique(Expected.Substance),
            fent.count = sum(fent.p),
            benzo.count = sum(has.benzo2),
            tot = n(),
            tot.not.test = n()-sum(B.Not.Test)) %>%
  ungroup()


#makes %
benzo2$fent.perc <- benzo2$fent.count/benzo2$tot*100
benzo2$benzo.perc <- benzo2$benzo.count/benzo2$tot*100
benzo2$b.perc.na <- benzo2$benzo.count/benzo2$tot.not.test*100

#
benzo2 <- benzo2 %>%
  pivot_longer(c(fent.count, benzo.count, fent.perc, benzo.perc, b.perc.na), 
               names_to = "name", values_to = "Percent") %>%
  mutate(name = str_replace_all(name, 
                                c("fent.count" = "Fentanyl Count",
                                  "fent.perc"= "% Fentanyl",
                                  "benzo.perc" = "% Benzo",
                                  "benzo.count" = "Benzo Count",
                                  "b.perc.na" = "% Benzo's excluding Samples not tested"
  )))

closure <-  c("Mar 16-\nMar 22\n2020", "Mar 23-\nMar 29\n2020", "Mar 30-\nApr 05\n2020")
#Creates a df for finding which Cities actually have a high enough % for testing
place.with.percent <- benzo2 %>%
  filter(Expected.Substance == "Fentanyl/Down") %>%
  group_by(City.Town) %>%
  summarise(med = median(tot), the_max = max(tot), the_q = quantile(tot, 0.75)) %>%
  ungroup() %>%
  filter(med < 20)

benzo2 <- benzo2 %>%
  filter((!str_detect(name, "%") & !City.Town %in% place.with.percent$City.Town) | str_detect(name, "Count") | (City.Town %notin% place.with.percent$City.Town & str_detect(name, "%")))

benzo2$Percent[grepl("\\Q%\\E", benzo2$name) & benzo2$tot < 20] <- NA
benzo2$Percent[which(benzo2$WeekID %in% closure)] <- NA
benzo2$Percent[which(is.na(benzo2$Percent))] <- NA
benzo2$Cheque <- poss.w$Cheque[match(benzo2$WeekID, poss.w$ID)]

benzo2$Size <- ifelse(benzo2$Cheque == "cheque", 6,4)
benzo2 <- benzo2 %>%
  drop_na()

#Selects only cities that have sufficiently high enough count for analysis
city.keep <- benzo2 %>%
  filter(name == "Fentanyl Count") %>%
  group_by(City.Town) %>%
  summarise(WeekID = unique(WeekID)) %>%
  count() %>%
  ungroup() %>%
  filter(n >30)
benzo2 <- benzo2[which(benzo2$City.Town %in% city.keep$City.Town),]


#Outputs data - you can either directly read this into the app or use gitlab
#I recommend the latter due to the slowness of calculating the edges
writexl::write_xlsx(list(dc_data = dcbc2, node_data = node_col, Edges = Edges, benzo = benzo2), "DC_BC.xlsx")
