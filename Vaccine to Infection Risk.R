library(tidyverse)

`%!in%` = Negate(`%in%`)


# load data from MDPH
load(file = "vaccine.sex.Rdata")
load(file = "vaccine.age.Rdata")
load(file = "vaccine.town.Rdata")
load(file = "vaccine.race.Rdata") #just to get the race/eth breakdown by town

#weekly MDPH printed summary reports, statewide
load(file = "VaxRace.Rdata")
load(file = "CasesRace.Rdata")

# laod color pallate
jama<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF")

#Last data available at time of analysis
analysisdt<-as.Date("2021-06-24")

# towns without town-specific vaccination data (share zip code with another community in MIIS)
no.townvax.data <-c(
  "Great Barrington", "Alford","Chilmark", "Aquinnah", 
"North Adams", "Clarksburg", 
"Lanesborough", "Hancock",
"Charlemont", "Hawley",
"Greenfield", "Leyden",
"Westfield", "Montgomery",
"Egremont", "Mount Washington",
"Lanesborough", "New Ashford",
"Amherst", "Pelham",
"Hinsdale", "Peru",
"Athol", "Phillipston",
"Granville", "Tolland",
"Becket", "Washington",
"Easthampton", "Westhampton",
"Gosnold", "Monroe", "Florida")



#get town prevalence of vaccine, using separate file from MDPH
town.vax1 <- (left_join( vaccine.town %>% filter(reportdate == analysisdt) %>%
                           filter(Age_group =="Total" & !is.na(fullvax) & Town != "Unspecified") %>%
                           rename(population = pop, fullvax.total = fullvax) %>%
                           mutate(vaccine_prev=ifelse(fullvax.total*100/population < 100, fullvax.total*100/population, 99),  # some communities with vaccinated numbers higher than last pop estimate
                                  Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")  ) %>%  #remove parenthetic phrase
                           filter(Town %!in% no.townvax.data), 
                         
                         town.age <- vaccine.town %>% filter(reportdate == analysisdt)  %>%
                           group_by(Town)%>%
                           mutate(Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")  ) %>% #remove parenthetic phrase
                           filter(Age_group=="65-74 Years" | Age_group=="75+ Years" ) %>%
                           filter(Town %!in% no.townvax.data) %>%
                           summarise(age65up=sum(pop)),
                         by="Town") %>%
                mutate(age65up.pct=age65up/population)) %>% dplyr::select(-age65up) %>%
  select(Town, population, fullvax.total, vaccine_prev, age65up.pct)


#get boston neighborhood prevalence of vaccine
boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
           "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

#read in .csv files of zipcodes and population
MAtownsbos <- read_csv("MAtowns.csv") %>% filter(Town %in% boston)


bos.vax1<-left_join(MAtownsbos, t1<-vaccine.sex %>%  ungroup()%>%
                      filter(reportdate == analysisdt), by="zipcode") %>%
  group_by(Town) %>%
  summarise(population=mean(town_population, na.rm= TRUE),
            fullvax.total=sum(fullvax.total, na.rm= TRUE)) %>%
  mutate(vaccine_prev=if_else(fullvax.total*100/population < 100, fullvax.total*100/population, 99),  # some communitity with vaccinated numbers higher than last pop estimate
         age65up.pct = 0.1166) # boston wide population, does not use zipcode-specific

#add boston neighbhoorhoods to MA town table
town.vax2<- rbind(town.vax1, bos.vax1) %>%
  filter (Town !="Boston", Town !="Unspecified")

#read in incidence covid cases and characteristics of communities
allcovidtowns <-left_join(read_csv("allcovidtowns.csv", guess_max=10000),
                          MAtownSES <-read_csv("MAtownSES.csv", guess_max=10000), by="Town") %>%
  rename(SVI_SES=RPL_THEME1.town, SVI_ages_disability=RPL_THEME2.town,
         SVI_minority=RPL_THEME3.town, SVI_house=RPL_THEME4.town,
         SVI_overall=RPL_THEMES.town)  %>%
  # Socioeconomic – RPL_THEME1
  # Household Composition & Disability – RPL_THEME2
  # Minority Status & Language – RPL_THEME3
  # Housing Type & Transportation – RPL_THEME4
  # Overall tract rankings:  RPL_THEMES.
  mutate(
    quartile.SVI_SES=cut(SVI_SES, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_ages_disability=cut(SVI_ages_disability, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_minority=cut(SVI_minority, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_house=cut(SVI_house, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE),
    quartile.SVI_overall=cut(SVI_overall, breaks=c(0, 0.25, 0.50, 0.75, 1), labels=FALSE)
  ) 

town.vax<-left_join(town.vax2, allcovidtowns %>% filter(date == analysisdt), by="Town") %>%
  mutate(cumulative_incidence=Count*100/population,
         VIR=vaccine_prev/cumulative_incidence, 
         blacklatinx.pct=(pop.black + pop.latino)/pop.total, 
         blacklatinx.ord=cut(blacklatinx.pct, breaks=c(-1, 0.20, Inf), labels=c(1,2)),
         small.town=if_else(population>7499, 0, 1),
         age65up.ord=as.numeric(cut(age65up.pct, breaks=c(-1, 0.15, 0.2, 0.25, Inf), labels=c(1,2,3,4)))) 

#number of towns < 7500 pop
town.vax %>%
  count(small.town)

#total evaluated population
totalpop <- town.vax %>%
  tally(population)
totalpop

#total covid cases and percent
totalcovid<- town.vax %>%
  tally(Count)
totalcovid
totalcovid/totalpop

#total vax and percent
totalvax<- town.vax %>%
  tally(fullvax.total)
totalvax
totalvax/totalpop

#statewide VIR (fig 1)
totalvax/totalcovid

#min/max of vax and incidence
min(town.vax$cumulative_incidence)
max(town.vax$cumulative_incidence)

min(town.vax$vaccine_prev)
max(town.vax$vaccine_prev)





## MV model
library(sandwich)
library(pscl)
library(MASS)

    #eval dispersion (variance larger than conditional means, overdispersed)
    ggplot(town.vax , aes(VIR, fill = quartile.SVI_SES)) + 
             geom_histogram(binwidth = 1) + 
             facet_grid(quartile.SVI_SES ~ ., margins = TRUE, scales = "free")
    with(town.vax, tapply(VIR, quartile.SVI_SES, function(x) {
      sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))

#primary neg binomial model
summary(VIR.nb<- glm.nb(VIR ~ blacklatinx.ord + small.town + quartile.SVI_SES  + age65up.ord, 
                    data = town.vax))
cov.VIR.nb<- vcovHC(VIR.nb, type="HC0")
std.err <- sqrt(diag(cov.VIR.nb))
VIR.nb.est <- cbind(Estimate= exp(coef(VIR.nb)), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(VIR.nb)/std.err), lower.tail=FALSE),
               LL = exp(coef(VIR.nb) - 1.96 * std.err),
               UL = exp(coef(VIR.nb) + 1.96 * std.err))
VIR.nb.est



# sensitivity analyses for other SVI domains, as suggested by reviewer
    #SVI_ages_disability (non-sig)
    summary(m1 <- glm.nb(VIR ~ blacklatinx.ord + small.town + quartile.SVI_SES  + age65up.ord + quartile.SVI_ages_disability , 
                         data = town.vax))
        #obtain robust errors
        cov.m1 <- vcovHC(m1, type="HC0")
        std.err <- sqrt(diag(cov.m1))
        m1.est <- cbind(Estimate= exp(coef(m1)), "Robust SE" = std.err,
                       "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
                       LL = exp(coef(m1) - 1.96 * std.err),
                       UL = exp(coef(m1) + 1.96 * std.err))
        m1.est

    #SVI_household (non-sig)
        summary(m2 <- glm.nb(VIR ~ blacklatinx.ord + small.town + quartile.SVI_SES  + age65up.ord + quartile.SVI_house, 
                             data = town.vax))
        #obtain robust errors
        cov.m2 <- vcovHC(m2, type="HC0")
        std.err <- sqrt(diag(cov.m2))
        m2.est <- cbind(Estimate= exp(coef(m2)), "Robust SE" = std.err,
                       "Pr(>|z|)" = 2 * pnorm(abs(coef(m2)/std.err), lower.tail=FALSE),
                       LL = exp(coef(m2) - 1.96 * std.err),
                       UL = exp(coef(m2) + 1.96 * std.err))
        m2.est
        
        
    #All SVI components, excluding blacklatinx given expected colinerarity with SVI_minority (only SES sig of SVI domains)
        summary(m3 <- glm.nb(VIR ~ age65up.ord + small.town + quartile.SVI_SES  +  
                               quartile.SVI_house + quartile.SVI_ages_disability + quartile.SVI_minority, 
                             data = town.vax))

        #obtain robust errors
        cov.m3 <- vcovHC(m3, type="HC0")
        std.err <- sqrt(diag(cov.m3))
        m3.est <- cbind(Estimate= exp(coef(m3)), "Robust SE" = std.err,
                       "Pr(>|z|)" = 2 * pnorm(abs(coef(m3)/std.err), lower.tail=FALSE),
                       LL = exp(coef(m3) - 1.96 * std.err),
                       UL = exp(coef(m3) + 1.96 * std.err))
        m3.est

detach(package:pscl,unload=TRUE)
detach(package:sandwich,unload=TRUE)
detach(package:MASS,unload=TRUE)

library(REAT)

CasesRace1<-CasesRace.equity %>%
  filter(date == analysisdt) %>%
  mutate(AllOther=AI.AN + Multi + NH.PI + Unknown) %>%
  dplyr::select(date, Asian, Black, Hispanic, White, AllOther) %>%
  pivot_longer(cols=c(Asian:AllOther), names_to = "RaceEth", values_to = "CaseCount")

VaxRace1<- VaxRace %>% filter(date == analysisdt) %>%
  mutate(AllOther=AI.AN + Multi + NH.PI + Unknown) %>%
  dplyr::select(date, Asian, Black, Hispanic, White, AllOther) %>%
  pivot_longer(cols=c(Asian:AllOther), names_to = "RaceEth", values_to = "VaccineCount") 

## developing Lorenz curves

##by Towns/Communities, sorted by SVI_SES

totalvaxMA<-(town.vax %>% tally(fullvax.total))$n
totalcaseMA<-(town.vax %>% tally(Count))$n

g3<-town.vax %>% filter(population>3000) %>%
  arrange(desc(SVI_SES)) %>%  #arrange by SES
  mutate(cum_Count=cumsum(Count), 
         cum_Count_prop=cum_Count/totalcaseMA,
         cum_fullvax=cumsum(fullvax.total),
         cum_fullvax_prop=cum_fullvax/totalvaxMA,
         identity_x=cum_Count_prop,
         identity_y=cum_Count_prop) %>%
  dplyr::select(Town, Count, cum_Count, cum_Count_prop, fullvax.total, cum_fullvax, cum_fullvax_prop, 
                quartile.SVI_SES, SVI_SES, identity_y, identity_x)

g3.hoover<-hoover(town.vax$fullvax.total, town.vax$Count)
g3.gini<-gini(town.vax$fullvax.total, town.vax$Count, coefnorm = FALSE, na.rm = TRUE)

#community hoover and gini
g3.hoover
g3.gini

# number of vaccine courses redistributed to achieve equity between communities
totalvaxMA*g3.hoover

# need to duplicate to allow colors to go from min to max cum cases/vax
g4<-rbind(g3, g3) %>%
  arrange(desc(SVI_SES)) %>%
  mutate(cum_fullvax_prop=lag(cum_fullvax_prop,1),
         cum_Count_prop=lag(cum_Count_prop,1),
         identity_x = if_else(is.na(cum_Count_prop),0,cum_Count_prop),
         identity_y = if_else(is.na(cum_Count_prop),0,cum_Count_prop))%>%
  replace(is.na(.), 0) %>%
  mutate(quartile.SVI_SES= fct_reorder(as.factor(quartile.SVI_SES), desc(desc(SVI_SES))))

l1<- g4 %>%
  ggplot() +
  geom_ribbon(aes(ymin=cum_fullvax_prop-0.015, ymax=cum_fullvax_prop+0.015,x=cum_Count_prop, fill=quartile.SVI_SES))+
  geom_line(aes(x=identity_x, y=identity_y),linetype="dashed") +
  theme_light() +
  labs(title= "Massachusetts Communities",
       x="Cumulative Proportion of Confirmed SARS-CoV-2 Infection",
       y="Cumulative Proportion of Fully-Vaccinated Individuals") +
  scale_fill_manual(values=jama, name="Socioeconomic Vulnerabilty\n(CDC SVI Percentile)",
                    labels=c("Low", "Low to Moderate","Moderate to High",  "High"))  +
  annotate("text", x=0.8, y=0.1, label=paste0("Gini Index ", round(g3.gini,2), "\nHoover Index ", round(g3.hoover,2)),size=7)+
  theme(legend.position = c(.3, .8), aspect.ratio = 1, panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5) ),
        legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5)) )


##by individuals
#vaccines and cases by race/ethnic categories and combined for all categories
total.byindiv<-left_join(CasesRace1, VaxRace1, by=c("date", "RaceEth")) 
totalvaxMA.byindiv<-(total.byindiv %>% tally(VaccineCount))$n
totalcaseMA.byindiv<-(total.byindiv %>% tally(CaseCount))$n


#cumulative number of cases/vaccines for Lorenz
g1<-total.byindiv %>%  
  mutate(VIR=VaccineCount/CaseCount) %>%
  arrange(VIR) %>%
  mutate(cum_Count=cumsum(CaseCount),
         cum_Count_prop=cum_Count/totalcaseMA.byindiv,
         cum_fullvax=cumsum(VaccineCount), 
         cum_fullvax_prop=cum_fullvax/totalvaxMA.byindiv,
         identity_x=cum_Count_prop,
         identity_y=cum_Count_prop) %>%
  dplyr::select(cum_fullvax_prop, cum_Count_prop, identity_x, identity_y, RaceEth, VIR)

#calculating Hoover and Gini indices using REAT package
g1.hoover<-hoover(total.byindiv$VaccineCount, total.byindiv$CaseCount)
g1.gini<-gini(total.byindiv$VaccineCount, total.byindiv$CaseCount, coefnorm = FALSE,  na.rm = TRUE)

#hoover and gini by individuals
g1.hoover
g1.gini
#need to duplicate to get colors to go from min to max
g2<-rbind(g1, g1) %>%
  arrange(VIR) %>%
  mutate(cum_fullvax_prop=lag(cum_fullvax_prop,1),
         cum_Count_prop=lag(cum_Count_prop,1),
         RaceEth=fct_reorder(fct_recode(as.factor(RaceEth), 
                                        "Latinx" = "Hispanic",
                                        "Multiple/Other" = "AllOther" ), VIR),
         identity_x = if_else(is.na(cum_Count_prop),0,cum_Count_prop),
         identity_y = if_else(is.na(cum_Count_prop),0,cum_Count_prop))%>%
  replace(is.na(.), 0)


jama5<-c(  "#374E55ff", "#80796BFF","#DF8F44FF", "#B24745FF", "#00A1D5FF")
l2<-g2 %>%
  ggplot() +
  geom_ribbon(aes(ymin=cum_fullvax_prop-0.015, ymax=cum_fullvax_prop+0.015, x=cum_Count_prop, fill=RaceEth))+
  geom_line(aes(x=identity_x, y=identity_y),linetype="dashed") +
  theme_light() +
  labs(title= "Massachusetts Residents",
       x="Cumulative Proportion of Confirmed SARS-CoV-2 Infection",
       y="Cumulative Proportion of Fully-Vaccinated Individuals") +
  scale_fill_manual(values=jama5, name="Race/Ethnicity")+
  annotate("text", x=0.8, y=0.1, label=paste0("Gini Index ", round(g1.gini,2), "\nHoover Index ", round(g1.hoover,2)), size=7)+
  theme(legend.position = c(.3, .8), aspect.ratio = 1, panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5) ),
        legend.text = element_text(size = rel(1.5)), legend.title = element_text(size = rel(1.5)) )


detach(package:REAT, unload=TRUE)



library(patchwork)
l1 +l2 + plot_layout(ncol=2) + plot_annotation(tag_levels = 'A')
ggsave("Lorenz plots MA vaccination equity.pdf", units = "in", width = 16, height=8)



#mean VIR for MA
MAmeanVIR <- ((town.vax %>% tally(fullvax.total)) / (town.vax %>% tally(Count)))$n

MAmeanVIR_SVI1 <- ((town.vax %>% filter(quartile.SVI_SES == 1) %>% tally(fullvax.total)) / (town.vax %>% filter(quartile.SVI_SES == 1) %>% tally(Count)))$n


town.vax %>%
  group_by(quartile.SVI_SES) %>%
  summarise(meanVIR = sum(fullvax.total)/sum(Count)) %>%
  mutate(meanVIR = signif(meanVIR, 3))


library(ggrepel)
ggplot(town.vax %>% filter(population >22284), aes(x=reorder(Town, SVI_SES), 
                                                   y=VIR, fill=as.factor(quartile.SVI_SES)))+
  geom_col()+ theme_classic() + 
  scale_fill_manual(values=jama, name="Socioeconomic Vulnerabilty\n(CDC SVI Percentile)",
                    labels=c("Low", "Low to Moderate","Moderate to High",  "High"))  +
  theme(legend.position = c(.95, .95), legend.justification = c(1, 1), 
        legend.direction = "vertical",axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  geom_hline(yintercept = MAmeanVIR, linetype="dashed") +
  annotate("text", x=98, y=MAmeanVIR+0.2, label=paste0("Massachusetts mean: ", round(MAmeanVIR, 2)), hjust=1)+
  labs(x="Communities (ordered by vulnerabilty)", y="Vaccination to Infection Risk Ratio")
ggsave("Town VIR ratio by SVI.pdf", units = "in", width = 15, height=12)
