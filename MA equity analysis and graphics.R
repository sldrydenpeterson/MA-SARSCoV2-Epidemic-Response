library(tidyverse)
library(ggrepel)
library(viridis)
library(cowplot)
library(ggmap)
library(patchwork)


#########################
## MA TOWNS 

#read in .csv files of zipcodes and population
MAtowns <- read_csv("MAtowns_no_bosneighborhoods.csv")  #reading in with consolidated Boston
MAtownpoptable <- read_csv("MAtownpoptable.csv")

allcovidtowns <- read_csv("allcovidtowns_no_bosneighborhoods.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26") & date < as.Date("2020-10-15")) %>% # MDPH town testing data available from 2020-05-27
  mutate(date=(lubridate::ymd(date)))

#Adding town_population to allcovidtowns
covidtowns1<-left_join(allcovidtowns, MAtownpoptable, by="Town")

#sorting by town and date, and incident cases in past week
covidtowns<-covidtowns1 %>% 
  group_by(Town) %>%
  dplyr::arrange(date, .by_group = TRUE) %>% 
  mutate(Rate=(Count/town_population)*100000,
         Count1wk=Count-(lag(Count,1)),
         Count1wk=if_else(Count1wk<0, 0, Count1wk),
         incidence7day=Rate-lag(Rate,1), 
         incidence7day=if_else(incidence7day<0, 0, incidence7day), #some instances where cases removed in subsequent weeks (?changed patient residence info, errors)
         incidence1day=incidence7day/7,
         Tests1wk=Tests-(lag(Tests,1)),
         positivity1wk=ifelse(Tests1wk>0, Count1wk/Tests1wk, NA), #preventing NaN when Tests1wk are zero
         testingrate=100000*Tests1wk/town_population) %>%
  filter(date > as.Date("2020-05-27"))  #first row is NA for all as calculations lagged 1 week
#allcovidtowns.long<-covidtowns #make a dataset with all observations


#getting overall testing statistics for MA
ALL.MA.testing<-covidtowns %>%
  filter(date >= as.Date("2020-06-03")) %>%
  group_by()%>%
  dplyr::summarise(
    Tests.AllMA=sum(Tests1wk),
    Count.AllMA=sum(Count1wk),
    Positivity.AllMA=Count.AllMA/Tests.AllMA
  )

ALL.MA.testing

#community testing summary
town.summary<-covidtowns %>%
  filter(date >= as.Date("2020-06-03")) %>%
  group_by(Town)%>%
  dplyr::summarise(
    Tests.towns=sum(Tests1wk),
    Count.towns=sum(Count1wk),
    Testing.towns=(sum(Tests1wk)/mean(town_population))*100000,
    Incidence.towns=(sum(Count1wk)/mean(town_population))*100000,
    Positivity.towns=Count.towns/Tests.towns) %>%
  group_by() %>%
  dplyr::summarise(
    median.testing=median(Testing.towns),
    lower.iqr.testing=quantile(Testing.towns, .25),
    upper.iqr.testing=quantile(Testing.towns, .75),
    min.testing=min(Testing.towns),
    max.testing=max(Testing.towns),
    median.positivity=median(Positivity.towns),
    lower.iqr.positivity=quantile(Positivity.towns, .25),
    upper.iqr.positivity=quantile(Positivity.towns, .75),
    min.positivity=min(Positivity.towns),
    max.positivity=max(Positivity.towns),
    min.incidence=min(Incidence.towns),
    max.incidence=max(Incidence.towns),
    median.incidence=median(Incidence.towns)
  )
town.summary

totalpop<-MAtownpoptable %>%
  dplyr::summarise(pop=(sum(town_population)))

testingmatch<-covidtowns %>%
  distinct(Town, date, .keep_all = TRUE) %>%
  group_by(date) %>%
  mutate(
    rankpositivity= dense_rank(desc(positivity1wk)),
    ranktesting= dense_rank(desc(testingrate)),
    testaccess.index=(rankpositivity-ranktesting)/n(),
    index.pct=cume_dist(testaccess.index),
    pop.w=town_population/totalpop$pop, #population from totalpop above
    n=n()
  ) %>%
  dplyr::select(Town, date, rankpositivity:n, positivity1wk, town_population, testingrate, Tests1wk, Count1wk)


#add testing target (testing rate for rankpositivity for that week)
testing.target<-inner_join(testingmatch %>%
                             dplyr::select(date, Town, rankpositivity, ranktesting, testingrate) %>%
                             filter(!is.na(rankpositivity) & !is.na(testingrate)), 
                           testingmatch %>% 
                             filter(!is.na(rankpositivity) & !is.na(testingrate)) %>%
                             rename(testingrate.target=testingrate) %>%
                             dplyr::select(date, testingrate.target, ranktesting),
                           by=c("date" ="date" , "rankpositivity"="ranktesting")
)

testingmatch<- left_join(testingmatch, testing.target  %>% 
                           dplyr::select(date, Town, testingrate.target), by= c("date", "Town")) %>%
  mutate(
    testingrate.target.cap=if_else(testingrate.target>10000, 10000, testingrate.target), #capping at 10% of population (10000/100000) to reduce distortion of collegetown testing
    testingrate.delta=testingrate.target.cap-testingrate,
    testingrate.gap=if_else(testingrate.delta<0, 0, testingrate.delta), #excess testing set to 0, no benefit from earlier/later testing
    testingrate.excess=if_else(testingrate.delta<0, abs(testingrate.delta), 0),
    testingrate.excess=if_else(testingrate.excess>10000, 10000, testingrate.excess), #capping at 10% of population  (10000/100000) to reduce distortion of collegetown testing
    testing.gap=testingrate.gap*(town_population/100000),
    testing.excess=testingrate.excess*(town_population/100000),
    week=lubridate::week(date))

#creating dataset of mean mismatch over data period (june to present)
testingmatch.means <- testingmatch %>%
  filter(date >= as.Date("2020-06-03")) %>%
  mutate(testingrate.gap=ifelse(testingrate.gap>0, testingrate.gap, 0)) %>%
  group_by(Town) %>%
  dplyr::summarize(testaccess.mean=mean(testaccess.index, na.rm = TRUE),
                   testingrate.gap.mean=mean(testingrate.gap, na.rm = TRUE), 
                   testingrate.excess.mean=mean(testingrate.excess, na.rm = TRUE),
                   town_population=mean(town_population, na.rm = TRUE),
                   cum.cases=sum(Count1wk),
                   cum.incid.cases=(cum.cases/town_population)*100000,
                   cum.tests=sum(Tests1wk),
                   cum.incid.test=(cum.tests/town_population)*100000,
                   cum.incid.tests.mean=(mean(Tests1wk)/town_population)*100000
  ) %>%
  mutate(quintile.testaccess.mean=ntile(testaccess.mean, 5))

#creating dataset of weekly mean mismatch over data period (june to present)
testingmatch.weekly<- testingmatch %>% filter(date >= as.Date("2020-06-03")) %>%
  filter(!is.na(testing.gap)) %>%
  group_by(date)%>%
  dplyr::summarise(sum=sum(pop.w), #check on weightings, should be ~1
                   number=n(), 
                   mean.testing.gap=mean(testing.gap),
                   mean.testingrate.gap=mean(testingrate.gap),
                   median.testingrate.gap=median(testingrate.gap),
                   mean.testing.excess=mean(testing.excess),
                   mean.testingrate.excess=mean(testingrate.excess),
                   median.testingrate=median(testingrate),
                   median.positivity=median(positivity1wk)
  )



#make SVI labels and create quartiles
MAtownSES<- read_csv("MAtownSES_no_bosneighborhoods.csv") %>%
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


#MA ACS SES-type data and testing summaries, over entire period
Equitytown<- left_join(MAtownSES, testingmatch.means, by="Town") 

# weekly measures to look change over time
Equitytown.weekly<- left_join(testingmatch, MAtownSES, by="Town")


############## and summarized by SVI quartiles

#SVI_SES
Equitytown.weekly.SVI_SES<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_SES) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testingrate.median=median(testingrate, na.rm = TRUE),
    positivity.median=median(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_SES, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Socioeconomic",
    SVI_quartile=quartile.SVI_SES) %>%
  dplyr::select(-testingrate.mean, -positivity.mean) %>%
  group_by(quartile.SVI_SES)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_SES<-Equitytown.weekly.SVI_SES %>%
  group_by(quartile.SVI_SES) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))


#SVI_ages_disability
Equitytown.weekly.SVI_ages_disability<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_ages_disability) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_ages_disability, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Household Composition and Disability",
    SVI_quartile=quartile.SVI_ages_disability ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_ages_disability)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_ages_disability<-Equitytown.weekly.SVI_ages_disability %>%
  group_by(quartile.SVI_ages_disability) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))


#SVI_minority
Equitytown.weekly.SVI_minority<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_minority) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_minority, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Minority Status and Language",
    SVI_quartile=quartile.SVI_minority) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_minority)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_minority<-Equitytown.weekly.SVI_minority %>%
  group_by(quartile.SVI_minority) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

#SVI_house
Equitytown.weekly.SVI_house<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_house) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_house, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Housing Type and Transportation",
    SVI_quartile=quartile.SVI_house ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_house)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_house<-Equitytown.weekly.SVI_house %>%
  group_by(quartile.SVI_house) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

#SVI_overall
Equitytown.weekly.SVI_overall<-Equitytown.weekly %>%
  group_by(date, quartile.SVI_overall) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_overall, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Overall",
    SVI_quartile=quartile.SVI_overall ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean )%>%
  group_by(quartile.SVI_overall)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytown.quartile.SVI_overall<-Equitytown.weekly.SVI_overall %>%
  group_by(quartile.SVI_overall) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

SVI.combined.weekly<- rbind(Equitytown.weekly.SVI_SES, Equitytown.weekly.SVI_ages_disability, 
                            Equitytown.weekly.SVI_house, Equitytown.weekly.SVI_minority, Equitytown.weekly.SVI_overall) %>%
  dplyr::select(-c(quartile.SVI_SES, quartile.SVI_ages_disability:quartile.SVI_overall))



#### Poisson modeling
library(sandwich)
library(pscl)
library(MASS)

#removing those with NA testing.gap and uncertain how handled in modeling
Equitytown.weekly.poisson <-Equitytown.weekly %>% 
  filter(!is.na(testing.gap))  %>% 
  filter(!is.na(quartile.SVI_overall)) %>% 
  mutate(collegetown=if_else(date>as.Date("2020-08-11"), if_else(college.pct>=0.11, 1, 0),0),
         highest_minority=if_else(quartile.SVI_minority >= 4, 1, 0),
         highest_SES=if_else(quartile.SVI_SES >= 4, 1, 0),
         highest_house=if_else(quartile.SVI_house >= 4, 1, 0),
         highest_disability=if_else(quartile.SVI_ages_disability >= 4, 1, 0),
         week2=week*week,
         week3=week*week*week
  )

summary(model.ma<- glm.nb(formula = testing.gap ~ quartile.SVI_SES + highest_minority + week +
                            collegetown + offset(log(town_population)), data = Equitytown.weekly.poisson, control=glm.control(maxit=100),
                          init.theta = 0.15, link = log))
cov.m1 <- vcovCL(model.ma, cluster = ~ Town)
std.err <- sqrt(diag(cov.m1))
rr.m1.est <- cbind(Estimate= exp(coef(model.ma)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(model.ma)/std.err), lower.tail=FALSE),
                   LL = exp(coef(model.ma) - 1.96 * std.err),
                   UL = exp(coef(model.ma) + 1.96 * std.err))
model.ma.estimates<-data.frame(rr.m1.est)
model.ma.estimates

## sensitivity analysis zero-inflated model, negative binomial
summary(zipb<- zeroinfl(round(testing.gap,0) ~ quartile.SVI_SES + highest_minority + collegetown + week + offset(log(town_population)),
                        dist="negbin", data = Equitytown.weekly.poisson))
cov.zipb <- vcovCL(zipb, cluster = ~ Town)
std.err <- sqrt(diag(cov.zipb))
rr.zipb.est <- cbind(Estimate= exp(coef(zipb)), "Robust SE" = std.err,
                     "Pr(>|z|)" = 2 * pnorm(abs(coef(zipb)/std.err), lower.tail=FALSE),
                     LL = exp(coef(zipb) - 1.96 * std.err),
                     UL = exp(coef(zipb) + 1.96 * std.err))
zipb.estimates<-data.frame(rr.zipb.est)
zipb.estimates


#########################
## BOSTON NEIGHBHOROODS

boston<- c("West Roxbury", "Roslindale", "Hyde Park", "Mattapan", "Jamaica Plain", "Dorchester Codman", "Dorchester Uphams", "Roxbury",
           "Fenway", "Allston Brighton", "Back Bay Downtown", "South End", "South Boston", "Charlestown", "East Boston")

#read in .csv files of zipcodes and population
MAtownsbos <- read_csv("MAtowns.csv") %>% filter(Town %in% boston)

MAtownbospoptable <- read_csv("MAtownpoptable.csv")  %>% filter(Town %in% boston)

#read in updating (twice weekly) .csv files of cases/testing
allcovidtownsbos <- read_csv("allcovidtowns.csv", guess_max=2000) %>% filter(date > as.Date("2020-05-26") & date < as.Date("2020-10-15") & Town %in% boston) %>% # MDPH town testing data available from 2020-05-27
  mutate(date=(lubridate::ymd(date)))



#Adding town_population to allcovidtownsbos
covidtownsbos1<-left_join(allcovidtownsbos, MAtownbospoptable, by="Town")

#sorting by town and date, and incident cases in past week
covidtownsbos<-covidtownsbos1 %>%
  group_by(Town) %>%
  dplyr::arrange(date, .by_group = TRUE) %>% 
  mutate(Rate=(Count/town_population)*100000,
         Count1wk=Count-(lag(Count,1)),
         Count1wk=if_else(Count1wk<0, 0, Count1wk),
         incidence7day=Rate-lag(Rate,1), 
         incidence7day=if_else(incidence7day<0, 0, incidence7day), #some instances where cases removed in subsequent weeks (?changed patient residence info, errors)
         incidence1day=incidence7day/7,
         Tests1wk=Tests-(lag(Tests,1)),
         Tests2wk=Tests-(lag(Tests,2)),
         positivity1wk=ifelse(Tests1wk>0, Count1wk/Tests1wk, NA), #preventing NaN when Tests1wk are zero
         testingrate=100000*Tests1wk/town_population) %>%
  filter(date > as.Date("2020-05-27"))  #first row is NA for all as calculations lagged 1 week
#allcovidtownsbos.long<-covidtownsbos #make a dataset with all observations


#community testing summary
town.summarybos<-covidtownsbos %>%
  filter(date >= as.Date("2020-06-03")) %>%
  group_by(Town)%>%
  dplyr::summarise(
    Tests.towns=sum(Tests1wk),
    Count.towns=sum(Count1wk),
    Testing.towns=(sum(Tests1wk)/mean(town_population))*100000,
    Positivity.towns=Count.towns/Tests.towns) %>%
  group_by() %>%
  dplyr::summarise(
    median.testing=median(Testing.towns),
    lower.iqr.testing=quantile(Testing.towns, .25),
    upper.iqr.testing=quantile(Testing.towns, .75),
    median.positivity=median(Positivity.towns),
    lower.iqr.positivity=quantile(Positivity.towns, .25),
    upper.iqr.positivity=quantile(Positivity.towns, .75)
  )


totalpop<-MAtownbospoptable %>%
  dplyr::summarise(pop=(sum(town_population)))

testingmatchbos<-covidtownsbos %>%
  distinct(Town, date, .keep_all = TRUE) %>%
  group_by(date) %>%
  mutate(
    rankpositivity= dense_rank(desc(positivity1wk)),
    ranktesting= dense_rank(desc(testingrate)),
    testaccess.index=(rankpositivity-ranktesting)/n(),
    index.pct=cume_dist(testaccess.index),
    pop.w=town_population/totalpop$pop, #population from totalpop above
    n=n(),
    index.pct=cume_dist(testaccess.index)
  ) %>%
  dplyr::select(Town, date, rankpositivity:n, positivity1wk, town_population, testingrate, Tests1wk, Count1wk)


#add testing target (testing rate for rankpositivity for that week)
testing.targetbos<-inner_join(testingmatchbos %>%
                                dplyr::select(date, Town, rankpositivity, ranktesting, testingrate) %>%
                                filter(!is.na(rankpositivity) & !is.na(testingrate)), 
                              testingmatchbos %>% 
                                filter(!is.na(rankpositivity) & !is.na(testingrate)) %>%
                                rename(testingrate.target=testingrate) %>%
                                dplyr::select(date, testingrate.target, ranktesting),
                              by=c("date" ="date" , "rankpositivity"="ranktesting")
)

testingmatchbos<- left_join(testingmatchbos, testing.targetbos  %>% 
                              dplyr::select(date, Town, testingrate.target), by= c("date", "Town")) %>%
  mutate(
    testingrate.target.cap=if_else(testingrate.target>10000, 10000, testingrate.target), #capping at 10% of population  (10000/100000) to reduce distortion of collegetown testing
    testingrate.delta=testingrate.target.cap-testingrate,
    testingrate.gap=if_else(testingrate.delta<0, 0, testingrate.delta), #excess testing set to 0, no benefit from earlier/later testing
    testingrate.excess=if_else(testingrate.delta<0, abs(testingrate.delta), 0),
    testingrate.excess=if_else(testingrate.excess>10000, 10000, testingrate.excess), #capping at 10% of population  (10000/100000) to reduce distortion of collegetown testing
    testing.gap=testingrate.gap*(town_population/100000),
    testing.excess=testingrate.excess*(town_population/100000),
    week=lubridate::week(date))

#creating dataset of mean mismatch over data period (june to present)
testingmatchbos.means <- testingmatchbos %>%
  filter(date >= as.Date("2020-06-03")) %>%
  group_by(Town) %>%
  dplyr::summarize(testaccess.mean=mean(testaccess.index,na.rm = TRUE),
                   testingrate.gap.mean=mean(testingrate.gap,na.rm = TRUE),
                   testingrate.excess.mean=mean(testingrate.excess,na.rm = TRUE),
                   town_population=mean(town_population, na.rm = TRUE),
                   cum.cases=sum(Count1wk),
                   cum.incid.cases=(cum.cases/town_population)*100000,
                   cum.tests=sum(Tests1wk),
                   cum.incid.test=(cum.tests/town_population)*100000,
                   cum.incid.tests.mean=(mean(Tests1wk)/town_population)*100000
  ) %>%
  mutate(quintile.testaccess.mean=ntile(testaccess.mean, 5))

#creating dataset of weekly mean mismatch over data period (june to present)
testingmatchbos.weekly<- testingmatchbos %>% filter(date >= as.Date("2020-06-03")) %>%
  filter(!is.na(testing.gap)) %>%
  group_by(date)%>%
  dplyr::summarise(sum=sum(pop.w), #check on weightings, should be ~1
                   number=n(), 
                   mean.testing.gap=mean(testing.gap),
                   mean.testingrate.gap=mean(testingrate.gap),
                   median.testingrate.gap=median(testingrate.gap),
                   mean.testing.excess=mean(testing.excess),
                   mean.testingrate.excess=mean(testingrate.excess),
                   median.testingrate=median(testingrate),
                   median.positivity=median(positivity1wk)
  )



#make SVI labels and create quartiles
MAtownSESbos<- read_csv("MAtownSES.csv") %>%
  filter(Town %in% boston) %>%
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


#MA ACS SES-type data and testing summaries, over entire period
Equitytownbos<- left_join(MAtownSESbos, testingmatchbos.means, by="Town") 

# weekly measures to look change over time
Equitytownbos.weekly<- left_join(testingmatchbos, MAtownSESbos, by="Town")


############## and summarized by SVI quartiles

#SVI_SES
Equitytownbos.weekly.SVI_SES<-Equitytownbos.weekly %>%
  group_by(date, quartile.SVI_SES) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testingrate.median=median(testingrate, na.rm = TRUE),
    positivity.median=median(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_SES, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Socioeconomic",
    SVI_quartile=quartile.SVI_SES) %>%
  dplyr::select(-testingrate.mean, -positivity.mean) %>%
  group_by(quartile.SVI_SES)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytownbos.quartile.SVI_SES<-Equitytownbos.weekly.SVI_SES %>%
  group_by(quartile.SVI_SES) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))


#SVI_ages_disability
Equitytownbos.weekly.SVI_ages_disability<-Equitytownbos.weekly %>%
  group_by(date, quartile.SVI_ages_disability) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_ages_disability, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Household Composition and Disability",
    SVI_quartile=quartile.SVI_ages_disability ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_ages_disability)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytownbos.quartile.SVI_ages_disability<-Equitytownbos.weekly.SVI_ages_disability %>%
  group_by(quartile.SVI_ages_disability) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))


#SVI_minority
Equitytownbos.weekly.SVI_minority<-Equitytownbos.weekly %>%
  group_by(date, quartile.SVI_minority) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_minority, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Minority Status and Language",
    SVI_quartile=quartile.SVI_minority) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_minority)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytownbos.quartile.SVI_minority<-Equitytownbos.weekly.SVI_minority %>%
  group_by(quartile.SVI_minority) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

#SVI_house
Equitytownbos.weekly.SVI_house<-Equitytownbos.weekly %>%
  group_by(date, quartile.SVI_house) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_house, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Housing Type and Transportation",
    SVI_quartile=quartile.SVI_house ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean)%>%
  group_by(quartile.SVI_house)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytownbos.quartile.SVI_house<-Equitytownbos.weekly.SVI_house %>%
  group_by(quartile.SVI_house) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

#SVI_overall
Equitytownbos.weekly.SVI_overall<-Equitytownbos.weekly %>%
  group_by(date, quartile.SVI_overall) %>%
  dplyr::summarise (
    testingrate.mean=mean(testingrate, na.rm = TRUE),
    positivity.mean=mean(positivity1wk, na.rm = TRUE),
    testing.gap.byquartile=sum(testing.gap, na.rm = TRUE),
    testing.excess.byquartile=sum(testing.excess, na.rm = TRUE),
    population.byquartile=sum(town_population, na.rm = TRUE),
    testingrate.gap.byquartile=(testing.gap.byquartile/population.byquartile)*100000,
    testingrate.excess.byquartile=(testing.excess.byquartile/population.byquartile)*100000
  ) %>%
  mutate(
    label=recode(quartile.SVI_overall, "Low\nVulnerability\nCommunities", "Low-Moderate\nVulnerabilty\nCommunities",
                 "Moderate-High\nVulnerability\nCommunities", "High\nVulnerability\nCommunities"), 
    SVI_index="Overall",
    SVI_quartile=quartile.SVI_overall ) %>%
  dplyr::select(-testingrate.mean, -positivity.mean )%>%
  group_by(quartile.SVI_overall)%>%
  mutate(cum.testingrate.gap.byquartile=cumsum(testingrate.gap.byquartile))

Equitytownbos.quartile.SVI_overall<-Equitytownbos.weekly.SVI_overall %>%
  group_by(quartile.SVI_overall) %>%
  dplyr::summarise (testing.gap = sum(testing.gap.byquartile))

SVI.combined.weekly.bos<- rbind(Equitytownbos.weekly.SVI_SES, Equitytownbos.weekly.SVI_ages_disability, 
                                Equitytownbos.weekly.SVI_house, Equitytownbos.weekly.SVI_minority, Equitytownbos.weekly.SVI_overall) %>%
  dplyr::select(-c(quartile.SVI_SES, quartile.SVI_ages_disability:quartile.SVI_overall))


#### Poisson modeling
library(sandwich)
library(pscl)
library(MASS)


#removing those with NA testing.gap and uncertain how handled in modeling
Equitytownbos.weekly.poisson <-Equitytownbos.weekly %>% 
  filter(!is.na(testing.gap))  %>% 
  filter(!is.na(quartile.SVI_overall)) %>% 
  mutate(highest_minority=if_else(quartile.SVI_minority >= 4, 1, 0),
         highest_SES=if_else(quartile.SVI_SES >= 4, 1, 0),
         highest_house=if_else(quartile.SVI_house >= 4, 1, 0),
         highest_disability=if_else(quartile.SVI_ages_disability >= 4, 1, 0)
  )

summary(model.bos<- glm.nb(formula = testing.gap ~ quartile.SVI_SES + week +  offset(log(town_population)), 
                         data = Equitytownbos.weekly.poisson, control=glm.control(maxit=100),
                         init.theta = 0.15, link = log))
cov.bos <- vcovCL(model.bos, cluster = ~ Town)
std.err <- sqrt(diag(cov.bos))
rr.bos.est <- cbind(Estimate= exp(coef(model.bos)), "Robust SE" = std.err,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(model.bos)/std.err), lower.tail=FALSE),
                   LL = exp(coef(model.bos) - 1.96 * std.err),
                   UL = exp(coef(model.bos) + 1.96 * std.err))
model.bos.estimates<-data.frame(rr.bos.est)
model.bos.estimates



#########################
##FIGURES


## MA Towns

#gap chart

#collapsing quartiles 1 and 2
low.mod<-SVI.combined.weekly %>%
  filter(SVI_quartile ==1 |SVI_quartile ==2) %>%
  group_by(SVI_index, date)%>%
  dplyr::summarise(cum.testingrate.gap.byquartile=mean(cum.testingrate.gap.byquartile)) %>%
  mutate(SVI_quartile=2)
mod.high<-SVI.combined.weekly %>% group_by()%>%
  filter(SVI_quartile ==3 |SVI_quartile ==4) %>%
  dplyr::select(date, SVI_quartile, cum.testingrate.gap.byquartile, SVI_index)

gap.plot<-full_join(low.mod, mod.high)

colors<-c(  "#80796BFF","#DF8F44FF", "#B24745FF")
colors.rev<-c( "#B24745FF","#DF8F44FF",  "#80796BFF")
gap.ma.plot<-ggplot(gap.plot %>%
                      mutate(quartile=factor(SVI_quartile, levels=c("4", "3", "2"),  labels=c("High\n(75-100 percentile)", 
                                                                                              "Moderate to High\n(50-75 percentile)", "Low/Low to Moderate\n(0-50 percentile)")))  %>%
                      filter(SVI_index=="Socioeconomic" | SVI_index=="Minority Status and Language"),
                    aes(x=date,y=cum.testingrate.gap.byquartile,  fill=quartile)) +
  theme_classic() + #theme( plot.background = element_rect(fill = "white", colour = "black"))+
  geom_area(alpha=.95, size=0.5, colour="black") +
  facet_wrap(vars(SVI_index), ncol = 1, as.table = FALSE) +
  scale_fill_manual(values=colors.rev,  
                    name="SVI Domain Percentile")  +
  scale_x_date(limits= c(as.Date("2020-06-01"), as.Date("2020-12-07"))) +
  theme(legend.position = c(.45, .95), legend.justification = c(1, 1),legend.key.size = unit(0.3, "in")) +
  labs(x="Date", y="Cumulative Testing Gap\n(tests per 100,000 residents)",
       title="Massachusetts Cities and Towns")


# map plots
townsgeo1<-read.csv("townsgeo1.csv")
townsgeo2<-read.csv("townsgeo2.csv")
townsgeo<- rbind(townsgeo1,townsgeo2)

EquitytownGeo <-  left_join(townsgeo, Equitytown, by = "Town")
colors<-c(  "#80796BFF","#DF8F44FF", "#B24745FF", "#B24745FF")
borders<-c( "#FFFFFF", "#26619c")

#getting centroids for location of town names
library(data.table)
library(geosphere)
cent<- EquitytownGeo %>% 
  mutate(collegetown= if_else(college.pct>=0.11, 1, 0)) %>% 
  dplyr::select(long, lat, Town, collegetown)
setDT(cent)

#make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
findCentroid <- function(long, lat, ...){
  centroid(cbind(long, lat), ...)
}

centroids<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
centroids<-centroids %>%
  distinct(Town, xname, yname, collegetown) %>%
  mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
         yname= ifelse(Town=="Nantucket", yname-0.02, yname),
         yname= ifelse(Town=="Chilmark", yname+0.02, yname),
         yname= ifelse(Town=="Boston", yname-0.02, yname),
         xname= ifelse(Town=="Boston", xname+0.04, xname),
         xname= ifelse(Town=="Salem", xname-0.02, xname),
         xname= ifelse(Town=="Westport", xname-0.02, xname),
         yname= ifelse(Town=="Provincetown", yname+0.01, yname))

#SES categorical
SES.ma<-ggplot()+
  geom_polygon(data = EquitytownGeo %>%
                 mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.50, 0.75, Inf),
                                    labels=c("Low/Low to Moderate\n(0-50 percentile)",
                                             "Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
                 filter(!is.na(SES.cat)),
               aes(x = long, y = lat, group = group, fill=SES.cat), 
               colour = "white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.3, .2), 
        legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=colors,  
                    name="Socioeconomic SVI\nDomain Percentile")  +
  geom_point(data=centroids %>% filter(collegetown==1), aes(x = xname, y = yname), color="#00A1D5FF", size=0.75)+
  annotate("Text", x = -73.6, y = 42.96, 
           label = c("A. Socioeconomic Vulnerability") , color="black", hjust=0, vjust=1, 
           size= 5)

#covid testing categorical
test.ma<-ggplot()+
  geom_polygon(data = EquitytownGeo %>% 
                 mutate(cum.test.cat=cut(cum.incid.test, breaks = c(-1, 40000, 50000, Inf),
                                         labels = c("5000 to 40,000","40,000 to 50,000",  "> 50,000"))) %>%
                 filter(!is.na(cum.test.cat)),
               aes(x = long, y = lat, group = group, fill=cum.test.cat), color="white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .2), legend.key.size = unit(0.3, "in"),
        legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=colors,  
                    name="Total Tests\nper 100,000") +
  geom_point(data=centroids %>% filter(collegetown==1), aes(x = xname, y = yname), color="#00A1D5FF", size=0.75)+
  # geom_point(data=centroids %>% filter(vacationtown==1), aes(x = (xname+0.02), y = yname), color="#79AF97FF", size=0.75)+
  annotate("Text", x = -73.6, y = 42.96, 
           label = c("C. Testing Intensity") , color="black", hjust=0, vjust=1, 
           size=5 )

#gap categorical
gap.ma<-ggplot()+
  geom_polygon(data = EquitytownGeo  %>%
                 mutate(testingrate.gap.cat=cut(testingrate.gap.mean, 
                                                breaks=c(-1, 500, 1000, Inf), 
                                                labels=c("0 to 500", "500 to 1000",  "> 1000"))),
               aes(x = long, y = lat, group = group, fill=as.factor(testingrate.gap.cat)), 
               colour = "white", alpha = 0.95, size = 0.05) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .2), legend.key.size = unit(0.3, "in"),
        legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=colors,  
                    name="Testing Gap\nper 100,000")  +
  geom_point(data=centroids %>% filter(collegetown==1), aes(x = xname, y = yname), color="#00A1D5FF", size=0.75)+
  annotate("Text", x = -73.6, y = 42.96, 
           label = c("E. Average Weekly Testing Gap") , color="black", hjust=0, vjust=1, 
           size= 5)

## BOSTON NEIGHBORHOODS


#gap chart

#collapse quartiles 1 and 2
low.mod.bos<-SVI.combined.weekly.bos %>%
  filter(SVI_quartile ==1 |SVI_quartile ==2) %>%
  group_by(SVI_index, date)%>%
  dplyr::summarise(cum.testingrate.gap.byquartile=mean(cum.testingrate.gap.byquartile)) %>%
  mutate(SVI_quartile=2)

mod.high.bos<-SVI.combined.weekly.bos %>% group_by()%>%
  filter(SVI_quartile ==3 |SVI_quartile ==4) %>%
  dplyr::select(date, SVI_quartile, cum.testingrate.gap.byquartile, SVI_index)

gap.plot.bos<-full_join(low.mod.bos, mod.high.bos)


colors.rev<-c( "#B24745FF","#DF8F44FF",  "#80796BFF")
gap.bos.plot<- ggplot(gap.plot.bos %>%
                        mutate(quartile=factor(SVI_quartile, levels=c("4", "3", "2"), labels=c("High\n(75-100 percentile)", 
                                                                                               "Moderate to High\n(50-75 percentile)", "Low/Low to Moderate\n(0-50 percentile)"))) %>%
                        filter(SVI_index=="Socioeconomic" | SVI_index=="Minority Status and Language"),
                      aes(x=date,y=cum.testingrate.gap.byquartile,  fill=quartile)) +
  theme_classic() + #theme( plot.background = element_rect(fill = "white", colour = "black"))+
  geom_area(alpha=.95, size=0.5, colour="black") +
  facet_wrap(vars(SVI_index), ncol = 1, as.table = FALSE) +
  scale_fill_manual(values=colors.rev,  
                    name="SVI Domain Percentile")  +
  scale_x_date(limits= c(as.Date("2020-06-01"), as.Date("2020-12-07"))) +
  theme(legend.position = c(.45, .95), legend.key.size = unit(0.3, "in"), legend.justification = c(1, 1)) +
  labs(x="Date", y="",
       title="Boston Neighborhoods")

gap.ma.plot + gap.bos.plot
ggsave("updatedgap.pdf", width=18, height=12)



###MAPS
townsgeobos<-read.csv("townsgeo_onlybostonneigh.csv")
EquitytownbosGeo <-  left_join(townsgeobos, Equitytownbos, by = "Town")

library(data.table)
library(geosphere)
cent<- EquitytownbosGeo %>% 
  mutate(collegetown= if_else(college.pct>=0.11, 1, 0)) %>% 
  dplyr::select(long, lat, Town, collegetown)
setDT(cent)

#make function make matrix (https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame)
findCentroid <- function(long, lat, ...){
  centroid(cbind(long, lat), ...)
}

centroidsbos<-cent[, c("xname", "yname") := as.list(findCentroid(long, lat)), by = Town]
centroidsbos<-centroidsbos %>%
  distinct(Town, xname, yname, collegetown) %>%
  mutate(xname= ifelse(Town=="Nantucket", xname+0.04, xname),
         yname= ifelse(Town=="Nantucket", yname-0.02, yname),
         yname= ifelse(Town=="Chilmark", yname+0.02, yname),
         yname= ifelse(Town=="Boston", yname-0.02, yname),
         xname= ifelse(Town=="Boston", xname+0.04, xname),
         xname= ifelse(Town=="Salem", xname-0.02, xname),
         xname= ifelse(Town=="Westport", xname-0.02, xname),
         yname= ifelse(Town=="Provincetown", yname+0.01, yname))

colors<-c(  "#80796BFF","#DF8F44FF", "#B24745FF", "#B24745FF")

## SES categorical
SES.bos<-ggplot()+
  geom_polygon(data = EquitytownbosGeo %>%
                 mutate(SES.cat=cut(SVI_SES, breaks=c(-1, 0.50, 0.75, Inf),
                                    labels=c("Low/Low to Moderate\n(0-50 percentile)","Moderate to High\n(50-75 percentile)", "High\n(75-100 percentile)"))) %>%
                 filter(!is.na(SES.cat)),alpha = 0.95,
               aes(x = long, y = lat, group = group, fill=SES.cat), 
               colour = "white",  size = 0.05) +
  coord_quickmap() + theme_void() +
  theme(legend.position = c(.8, .2), 
        legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=colors, name="Socioeconomic SVI\nDomain Percentile")  +
  geom_point(data=centroidsbos %>% filter(collegetown==1), aes(x = xname, y = yname), color="#00A1D5FF", size=1)+
  annotate("Text", x = -71.2, y = 42.415, 
           label = c("B. Socioeconomic Vulnerability") , color="black", hjust=0, vjust=1, 
           size= 5)

#covid testing categorical
test.bos<-ggplot()+
  geom_polygon(data = EquitytownbosGeo %>% 
                 mutate(cum.test.cat=cut(cum.incid.test, breaks = c(-1, 25000, 35000,  Inf),
                                         labels = c("5000 to 30,000","30,000 to 40,000", "> 40,000")))  %>%
                 filter(!is.na(cum.test.cat)),alpha = 0.95,
               aes(x = long, y = lat, group = group, fill=cum.test.cat), 
               colour = "white", size = 0.05) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.8, .2),  legend.key.size = unit(0.3, "in"),
        legend.direction = "vertical", panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=colors,  
                    name="Individuals Tested\nper 100,000") +
  geom_point(data=centroidsbos %>% filter(collegetown==1), aes(x = xname, y = yname), color="#00A1D5FF", size=1)+
  annotate("Text", x = -71.2, y = 42.415, 
           label = c("D. Testing Intensity") , color="black", hjust=0, vjust=1, 
           size=5 )

#gap categorical
gap.bos<-ggplot()+
  geom_polygon(data = EquitytownbosGeo %>%
                 mutate(testingrate.gap.cat=cut(testingrate.gap.mean, 
                                                breaks=c(-1, 500, 1000, Inf), 
                                                labels=c("0 to 500", "500 to 1000", "> 1000"))),
               aes(x = long, y = lat, group = group, fill=as.factor(testingrate.gap.cat)), 
               colour = "white", alpha = 0.95,size = 0.05) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.8, .2), 
        legend.direction = "vertical", legend.key.size = unit(0.3, "in"),
        panel.background = element_rect(fill = "White", size=1, color="Black")) +
  scale_fill_manual(values=colors,  
                    name="Testing Gap\nper 100,000")  +
  geom_point(data=centroidsbos %>% filter(collegetown==1), aes(x = xname, y = yname), color="#00A1D5FF", size=1)+
  annotate("Text", x = -71.2, y = 42.415, 
           label = c("F. Average Weekly Testing Gap") , color="black", hjust=0, vjust=1, 
           size= 5)

SES.ma + SES.bos + test.ma + test.bos + gap.ma + gap.bos + plot_layout(ncol = 2)  + 
  plot_annotation(title = 'Massachusetts                                                                Boston', theme = theme(plot.title = element_text(size = 18))) 











