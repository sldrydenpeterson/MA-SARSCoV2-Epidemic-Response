library(tidyverse)
library(ggsci)
`%!in%` = Negate(`%in%`)

load(file = "vaccine.town.Rdata")
load(file = "vaccine.anydose.age.Rdata")
#load(file = "vaccine.schoolage.Rdata")


## vaccination by children


# School age population by zip code
# library(tidycensus)
# v2019 <- load_variables(2019, "acs5", cache = TRUE)
# 
# vars <- c(
#   "B01001_004",  #male, 5-9
#   "B01001_005",  #male, 10-14
#   "B01001_006",  #male, 15-17
#   "B01001_007",  #male, 18-19
#   "B01001_028",  #female, 5-9
#   "B01001_029",  #female, 10-14
#   "B01001_030",  #female, 15-17
#   "B01001_031"  #female, 18-19
# )
# ALLzips.age <-get_acs(geography = "zcta",
#                       variables = vars,
#                       #state = "25", # cannot limit to state in ZCTA
#                       year = 2019)
# 
# save(ALLzips.age , file="ALLzips.age.Rdata")

## get vaccinations for school age for MA
load("ALLzips.age.Rdata")
boston.zips<-c("02163",
               "02135","02134","02129","02108","02114","02116","02199",
               "02109","02110","02103","02113","02128","02121","02125","02122","02124","02115","02215",
               "02136","02130","02126","02131","02119","02120","02127","02210","02111","02118","02132")


# non-Boston towns
vaccine.schoolage<- rbind(  vaccine.town %>%
                              filter(Town !=" Boston") %>%
                              filter(Age_group == "5-11 Years" | Age_group == "12-15 Years" | Age_group == "16-19 Years" | Age_group == "0-19 Years" ) %>%
                              select(-proportion_pop),
                            #Boston neighborhoods
                            
                            left_join(
                              left_join(
                                vaccine.anydose.age %>%
                                  filter(zipcode %in% boston.zips) %>%
                                  pivot_longer(cols = c(age5to11:age80plus.boost), names_to = "Age_group", values_to = "vaxnum") %>%
                                  mutate(onedose = if_else(str_detect(Age_group, ".full"), as.numeric(NA), vaxnum),
                                         fullvax = if_else(str_detect(Age_group, ".full"), vaxnum, as.numeric(NA)),
                                         boostvax = if_else(str_detect(Age_group, ".boost"), vaxnum, as.numeric(NA)),
                                         Age_group = case_when(
                                           Age_group == "age5to11" ~ "age5to11",
                                           Age_group == "age5to11.full" ~ "age5to11",
                                           Age_group == "age5to11.boost" ~ "age5to11",
                                           Age_group == "age12to15" ~ "age12to15",
                                           Age_group == "age12to15.full" ~ "age12to15",
                                           Age_group == "age12to15.boost" ~ "age12to15",
                                           Age_group == "age16to19" ~ "age16to19",
                                           Age_group == "age16to19.full" ~ "age16to19",
                                           Age_group == "age16to19.boost" ~ "age16to19")) %>% select(reportdate, zipcode,  Age_group, onedose, fullvax, boostvax) %>%
                                  filter(!is.na(Age_group)) %>%
                                  group_by(reportdate, zipcode, Age_group) %>% 
                                  summarize(onedose = sum(onedose, na.rm=TRUE), 
                                            fullvax = sum(fullvax, na.rm=TRUE),
                                            boostvax = sum(boostvax, na.rm=TRUE)) %>%
                                  ungroup(),
                                ALLzips.age %>%
                                  rename(zipcode=GEOID,
                                         pop = estimate)%>%
                                  filter(zipcode %in% boston.zips) %>%
                                  ungroup() %>%
                                  # mutate(pop = if_else(variable == "B01001_005" |variable == "B01001_029", pop*2/5, pop), # splitting age group 10-14 into two years 
                                  #        pop = if_else(variable == "B01001_006" |variable == "B01001_030", pop*2/3, pop)) %>%  # splitting age group 15-17 into two years 
                                  filter(variable != "B01001_007" ) %>%  # not using 18 to 19 age estimates
                                  filter(variable != "B01001_031" ) %>%  # not using 18 to 19 age estimates
                                  mutate(
                                    age5to11 = case_when(
                                      variable == "B01001_004" ~ pop,  #5to9, 
                                      variable == "B01001_005" ~ 0.4*pop,  #10to11, 2/5 of 10to14
                                      variable == "B01001_028" ~ pop, #same for girls
                                      variable == "B01001_029" ~ 0.4*pop), #same for girls
                                    age12to15 = case_when(
                                      variable == "B01001_005" ~ 0.8*pop, #4 years from 10to14 
                                      variable == "B01001_029" ~ 0.8*pop), #same for girls
                                    age16to19 = case_when(
                                      variable == "B01001_006" ~ (4/3)*pop, # 4 years of 15-17,(mitigate effect of excess 18/19 college students)
                                      variable == "B01001_030" ~ (4/3)*pop)  ) %>% #same for girls
                                  group_by(zipcode) %>%
                                  summarise(age5to11 = round(sum(age5to11, na.rm = TRUE),0),
                                            age12to15 = round(sum(age12to15, na.rm = TRUE),0),
                                            age16to19 = round(sum(age16to19, na.rm = TRUE),0)) %>%
                                  pivot_longer(cols = c(age5to11:age16to19), names_to = "Age_group",values_to = "pop"), by=c("zipcode", "Age_group")),
                              read_csv("MAtowns.csv") %>% 
                                select(Town, zipcode) %>%
                                mutate(zipcode = sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})),
                              by = "zipcode") %>%
                              group_by(Town, reportdate, Age_group) %>%
                              summarise(pop = sum(pop, na.rm = TRUE),
                                        onedose = sum(onedose, na.rm = TRUE),
                                        fullvax = sum(fullvax, na.rm = TRUE),
                                        boostvax = sum(boostvax, na.rm = TRUE)) %>%
                              ungroup()) %>%
  mutate(anydose.pct = onedose/pop, 
         fullvax.pct = fullvax/pop,
         boostvax.pct = boostvax/pop,
         Age_group = case_when(
           Age_group == "5-11 Years" ~ "5-11 Years",
           Age_group == "12-15 Years" ~ "12-15 Years",
           Age_group == "16-19 Years" ~ "16-19 Years",
           Age_group == "age5to11" ~ "5-11 Years",
           Age_group == "age12to15" ~ "12-15 Years",
           Age_group == "age16to19" ~ "16-19 Years"), 
         Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")) #remove parenthetic phrase) 



#vaccination data pooled for this towns, listed under Town A
sharedtowns<-  tribble(
  ~Town.A, ~Town.B,
  "Great Barrington", "Alford", 
  "Chilmark", "Aquinnah", 
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
)


# vaccine.schoolage1 <- left_join(
#  # read.csv("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/MAtowns_no_bosneighborhoods.csv") %>% distinct(Town),
#   vaccine.town %>%
#   #filter(reportdate == max(reportdate)) %>%
#   filter(Age_group == "5-11 Years" | Age_group == "12-15 Years" | Age_group == "16-19 Years" | Age_group == "0-19 Years" ) %>%
#   group_by(Town, reportdate) %>%
#   summarise(pop = sum(pop),
#             fullvax =sum(fullvax, na.rm=TRUE)) %>%
#   mutate(pct.vax = fullvax/pop,
#          pct.vax = if_else(pct.vax>0.99, 0.99, pct.vax), 
#         Town=str_replace(Town, " \\s*\\([^\\)]+\\)", "")  ) %>%   #remove parenthetic phrase
#     ungroup(),
#   read.csv("~/Dropbox (Partners HealthCare)/GitHub/Ma-Covid-Testing/MAtowns_no_bosneighborhoods.csv") %>% distinct(Town),
#   by = "Town") 


vaccine.schoolage <- 
  left_join(
      rbind(vaccine.schoolage %>% filter(Town %!in% sharedtowns$Town.B) %>%
              select(Town, reportdate, Age_group, pop, fullvax, onedose, boostvax,  anydose.pct, fullvax.pct, boostvax.pct),    # add in values for towns share zips and thus vaccine data
    left_join(vaccine.schoolage, sharedtowns, by = c("Town" = "Town.A")) %>%
        filter(Town %in% sharedtowns$Town.A) %>%
        select(Town.B, reportdate, Age_group, pop, fullvax, onedose, boostvax,  anydose.pct, fullvax.pct, boostvax.pct) %>%
        rename(Town = Town.B)),
  read.csv("MAtownSES.csv"), by= "Town") %>%
  mutate(blacklatinx.pct=(pop.black + pop.latino)/pop.total, 
      blacklatinx.ord=cut(blacklatinx.pct, breaks=c(-1, 0.20, Inf), labels=c("<20% Black and/or Latinx",">20% Black and/or Latinx")))
 
townsgeobos<-read.csv("townsgeo_onlybostonneigh.csv")
townsgeo<-read.csv("townsgeo.csv") %>% filter(Town !="Boston")
matownsgeo<-rbind(townsgeo, townsgeobos)

vaccine.schoolage.geo <- 
  left_join(
    left_join( 
      matownsgeo,
      vaccine.schoolage %>% 
      filter(reportdate == max(reportdate)), by = "Town"),
    read_csv("town.neighborhood.labels.csv"), by="Town")

allma.5to11vax<-ggplot()+
geom_polygon(data = vaccine.schoolage.geo %>% filter(Age_group == "5-11 Years") %>%
               mutate(fullvax.pct= case_when(fullvax.pct >0.81 ~ 0.81,
                                             fullvax.pct <0.21 ~ 0.21,
                                             fullvax.pct <=0.81 ~ fullvax.pct)), 
             aes(x = long, y = lat, group = group, fill=fullvax.pct*100), 
             colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "light grey",
                       limits=c(21,81),breaks = c(25, 50, 75),
                       name="Percent of 5 to 11\nyear-old residents") +
  labs(title="Complete vaccination among 5 to 11 year-olds, Massachusetts",
       subtitle = paste0("Data through ", max(vaccine.schoolage$reportdate, na.rm = TRUE)),
       caption="Source: MDPH. Towns with <30 vaccinations among 5-11 supressed for privacy reasons (colored grey)")
allma.5to11vax
ggsave("allma 5to11vax.pdf", width=16, height=9)


allma.12to15vax<-ggplot()+
  geom_polygon(data = vaccine.schoolage.geo %>% filter(Age_group == "12-15 Years") %>%
                 mutate(fullvax.pct= case_when(fullvax.pct >0.81 ~ 0.81,
                                               fullvax.pct <0.21 ~ 0.21,
                                               fullvax.pct <=0.81 ~ fullvax.pct)), 
               aes(x = long, y = lat, group = group, fill=fullvax.pct*100), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.3, .28), legend.direction = "horizontal") +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "light grey",
                       limits=c(21,81),breaks = c(25, 50, 75),
                       name="Percent of 12 to 15\nyear-old residents") +
  labs(title="Complete vaccination among 12 to 15 year-olds, Massachusetts",
       subtitle = paste0("Data through ", max(vaccine.schoolage$reportdate, na.rm = TRUE)),
       caption="Source: MDPH. Towns with <30 vaccinations among 12-15 supressed for privacy reasons (colored grey)")
allma.12to15vax
ggsave("allma 12to15vax.pdf", width=16, height=9)


boston.5to11vax<-ggplot()+
  geom_polygon(data = vaccine.schoolage.geo %>% filter(Age_group == "5-11 Years") %>% 
                 mutate(fullvax.pct= case_when(fullvax.pct >0.81 ~ 0.81,
                                               fullvax.pct <0.21 ~ 0.21,
                                               fullvax.pct <=0.81 ~ fullvax.pct)), 
               aes(x = long, y = lat, group = group, fill=fullvax.pct*100), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.75) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))+
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "light grey",
                       limits=c(21,81),breaks = c(25, 50, 75),
                       name="Percent of 5 to 11\nyear-old residents") +
  labs(title="Complete vaccination among 5 to 11 year-olds, Metro Boston",
       subtitle = paste0("Data through ", max(vaccine.schoolage$reportdate, na.rm = TRUE)),
       caption="Source: MDPH. Towns with <30 vaccinations among 5-11 supressed for privacy reasons (colored grey)") +
  geom_text( data = vaccine.schoolage.geo %>% filter(Age_group == "5-11 Years") %>%
               distinct(Town, .keep_all = TRUE),
             size=1.8,
             aes(x=xname, y=yname,  label=paste(Town,"\n(", round(fullvax.pct*100,0), "%)", sep="")))
boston.5to11vax
ggsave("boston 5to11vax.pdf", height = 8, width =12)


boston.12to15vax<-ggplot()+
  geom_polygon(data = vaccine.schoolage.geo %>% filter(Age_group == "12-15 Years") %>% 
                 mutate(fullvax.pct= case_when(fullvax.pct >0.81 ~ 0.81,
                                               fullvax.pct <0.21 ~ 0.21,
                                               fullvax.pct <=0.81 ~ fullvax.pct)), 
               aes(x = long, y = lat, group = group, fill=fullvax.pct*100), 
               colour = "white", alpha = 0.9, size = .25) +
  coord_quickmap() + theme_void() + 
  theme(legend.position = c(.95, .6), legend.justification = c(1, 1),
        legend.direction = "vertical", aspect.ratio = 0.75) +
  coord_cartesian(xlim=c(-71.4, -70.6), ylim = c(42.18, 42.62))+
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value = "light grey",
                       limits=c(21,81),breaks = c(25, 50, 75),
                       name="Percent of 12 to 15\nyear-old residents") +
  labs(title="Complete vaccination among 12 to 15 year-olds, Metro Boston",
       subtitle = paste0("Data through ", max(vaccine.schoolage$reportdate, na.rm = TRUE)),
       caption="Source: MDPH. Towns with <30 vaccinations among 12-15 supressed for privacy reasons (colored grey)") +
  geom_text( data = vaccine.schoolage.geo %>% filter(Age_group == "12-15 Years") %>%
               distinct(Town, .keep_all = TRUE),
             size=1.8,
             aes(x=xname, y=yname,  label=paste(Town,"\n(", if_else(fullvax.pct <.991, as.character(round(fullvax.pct*100,0)), ">99") , "%)", sep="")))
boston.12to15vax
ggsave("boston 12to15vax.pdf", height = 8, width =12)


library(ggsci)
library(ggrepel)
vaccine.schoolage.ses <-
vaccine.schoolage %>% filter(reportdate == max(reportdate) & Age_group != "16-19 Years") %>% group_by(Town) %>% 
                           summarize(fullvax = sum(fullvax, na.rm = TRUE),
                           pop = sum(pop, na.rm = TRUE),
                           RPL_THEME1.town = mean(RPL_THEME1.town, na.rm = TRUE),
                           fullvax.pct= if_else(fullvax/pop>0.99, 0.99, fullvax/pop),
                           blacklatinx.ord = blacklatinx.ord) %>%
  distinct(Town, .keep_all = TRUE) %>%
  filter(pop>=200 & !is.na(blacklatinx.ord)) %>%
  ggplot(aes(x=RPL_THEME1.town*100, y=fullvax.pct*100, fill=blacklatinx.ord, size=pop))+
  scale_size_continuous(range = c(1, 5))+
  geom_smooth(method=lm, color="dark grey", fill=NA)+
  geom_point(shape=21)+
  geom_hline(yintercept = 80, linetype= "dotted")+
  theme_light() +
  labs(title= "Vaccination among eligible students (5 to 15 years), Massachusetts",
       subtitle=paste0("Towns with at least 200 vaccine-eligible students included | Data current as of ", max(vaccine.schoolage$reportdate)),
       x="Socioeconomic Vulnerabilty (CDC SVI percentile)",
       y="Percent Fully-Vaccinated") +
  scale_fill_jama(palette = c("default"), alpha = 1, name="Black and Latinx Population")+
  scale_color_jama(palette = c("default"), alpha = 1, name="Black and Latinx Population")+
  guides(color="none", size="none")+
  geom_label_repel(data=
                     vaccine.schoolage %>% filter(reportdate == max(reportdate) & Age_group != "16-19 Years") %>% group_by(Town) %>% summarize(fullvax = sum(fullvax, na.rm = TRUE),
                                                                                                                                               pop = sum(pop, na.rm = TRUE),
                                                                                                                                               fullvax.pct= if_else(fullvax/pop>0.99,0.99, fullvax/pop),
                                                                                                                                               RPL_THEME1.town = mean(RPL_THEME1.town, na.rm = TRUE),
                                                                                                                                               blacklatinx.ord = blacklatinx.ord) %>%
                     distinct(Town, .keep_all = TRUE) %>%
                     filter(pop>=200) %>%
                     filter(fullvax.pct<0.3 & pop>=1000 | fullvax.pct>0.8 & pop>=1000 | pop>4000 | Town=="Wellesley" | Town=="Chelsea" | 
                              Town=="Swampscott"| fullvax.pct< 0.5 & RPL_THEME1.town*100 < 20),
                   aes(x=RPL_THEME1.town*100, y=fullvax.pct*100,  label=Town, color=blacklatinx.ord), fill="white",
                   min.segment.length = 0, max.overlaps = Inf)+
  coord_cartesian(xlim=c(0,100), ylim=c(0,100)) +
  scale_x_continuous(breaks=c(0, 20, 40, 60, 80, 100))+
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(.86, .92))
vaccine.schoolage.ses 
ggsave("vaccine.schoolage.ses.pdf", height = 12, width =12)



## Code needs updating to integrate expanded eligibility of vaccination, boosting; doesn't work now

# schoolage.ses.long<-rbind(vaccine.schoolage %>%
#   filter(pop>=200 & !is.na(fullvax)) %>%
#   mutate(SES.cat = cut(RPL_THEME1.town, breaks= c(-0.1, 0.25, 0.5, 0.75, 1.1),
#                        labels =c("Low Community\nSES Vulnerablity", "Low/Moderate Community\nSES Vulnerablity", 
#                                  "Moderate/High Community\nSES Vulnerability", "High Community\nSES Vulnerability")),
#          fullvax = if_else(fullvax > pop, pop-1, fullvax)) %>%
#   group_by(Town) %>%
#     mutate(pop= min(pop)) %>%
#   ungroup() %>%
#   filter(!is.na(SES.cat)) %>%
#   group_by(reportdate, SES.cat) %>%
#   summarise(pop = sum(pop),
#             fullvax =sum(fullvax)) %>%
#   ungroup() %>%
#   mutate(pct.vax = fullvax/pop,
#          pct.vax = if_else(pct.vax>0.99, 0.99, pct.vax)) %>%
#   select(reportdate, pct.vax, SES.cat),
#  vaccine.schoolage %>% filter(Town=="Chelsea" & !is.na(fullvax)) %>%
#   mutate(SES.cat = as.factor("Chelsea\nHigh SES Vulnerablity"),
#           pop= min(pop)) %>%
#    group_by(reportdate, SES.cat) %>%
#    summarise(pop = sum(pop),
#              fullvax =sum(fullvax)) %>%
#    ungroup() %>%
#    mutate(pct.vax = fullvax/pop,
#           pct.vax = if_else(pct.vax>0.99, 0.99, pct.vax)) %>%
#   select(reportdate, pct.vax, SES.cat)) %>% ungroup()
# 
# schoolage.ses.long %>%
#   ggplot()+
#   geom_line(aes(x=reportdate, y=pct.vax*100, group=SES.cat, color=SES.cat), size=2, alpha=0.8) +
#   scale_color_jama() +
#   theme_classic() + theme(aspect.ratio= 0.75, plot.title = element_text(size = rel(1.5)),
#                           axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
#   coord_cartesian(ylim=c(0,100), xlim=c(as.Date("2021-03-10"), Sys.Date()+50))+
#   scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
#   scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) +
#   geom_hline(yintercept = 80, linetype= "dotted")+
#   labs(title= "Vaccination among eligible students (12 to 19 years), Massachusetts",
#        subtitle=paste0("Towns with at least 200 vaccine-eligible students included | Data current as of ", max(schoolage.ses.long$reportdate)),
#        x="Date",
#        y="Percent Fully-Vaccinated",
#        caption="Source: MDPH for vaccination and CDC Social Vulnerability Index (aggregated by city/town)")+
#   guides(color="none") +
#   geom_label_repel(data=schoolage.ses.long %>% filter(reportdate == max(reportdate)),
#                  aes(x=reportdate, y=pct.vax*100,  label=paste0(SES.cat, "\n", round(pct.vax*100, 1), "%"), color=SES.cat), fill="white",
#                  min.segment.length = 0, max.overlaps = Inf, xlim=c(max(schoolage.ses.long$reportdate), as.Date(NA)))
# 
# ggsave("longitudinal schoolage vaccination.pdf", width=16, height=12)
# library(patchwork)
# 
# allma.vax.schoolage +  vaccine.schoolage.ses + plot_layout(ncol=2)
# ggsave("vaccineschoolage.pdf", width=20, height = 10)
# 
# 
# #focus communities only
# focus<- vaccine.schoolage %>% filter(!is.na(pct.vax)) %>%
#   filter(Town=="Chelsea" | Town=="Everett" | Town=="Revere" | Town=="Lawrence") %>%
#   group_by(reportdate, Town) %>%
#   summarise(pop = mean(pop),
#             fullvax =sum(fullvax),
#             SES.cat = as.factor(paste0(Town, "\nHigh SES Vulnerablity"))) %>%
#   group_by(Town) %>%
#     mutate(pop = min(pop)) %>%
#   ungroup()%>%
#   mutate(pct.vax = fullvax/pop) %>%
#   select(reportdate, pct.vax, SES.cat)
# 
# 
# focus %>%
#   ggplot()+
#   geom_line(aes(x=reportdate, y=pct.vax*100, group=SES.cat, color=SES.cat), size=2, alpha=0.8) +
#   scale_color_jama() +
#   theme_classic() + theme(aspect.ratio= 0.75, plot.title = element_text(size = rel(1.5)),
#                           axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
#   coord_cartesian(ylim=c(0,100), xlim=c(as.Date("2021-03-10"), Sys.Date()+50))+
#   scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
#   scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) +
#   geom_hline(yintercept = 80, linetype= "dotted")+
#   labs(title= "Vaccination among eligible students (12 to 19 years)",
#        subtitle=paste0("Data current as of ", max(schoolage.ses.long$reportdate)),
#        x="Date",
#        y="Percent Fully-Vaccinated",
#        caption="Source: MDPH for vaccination and CDC Social Vulnerability Index (aggregated by city/town)")+
#   guides(color="none") +
#   geom_label_repel(data=focus %>% filter(reportdate == max(reportdate)),
#                    aes(x=reportdate, y=pct.vax*100,  label=paste0(SES.cat, "\n", round(pct.vax*100, 1), "%"), color=SES.cat), fill="white",
#                    min.segment.length = 0, max.overlaps = Inf, xlim=c(max(schoolage.ses.long$reportdate), as.Date(NA)))
# 
# ggsave("longitudinal schoolage vaccination_focus towns.pdf", width=16, height=12)
# library(patchwork)
# 
# 
# 
# library(officer)
# 
# slides <- read_pptx("wide template.pptx")
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = allma.vax.schoolage, 
#                  location = ph_location_fullsize() )
# slides <- add_slide(slides)
# slides<- ph_with(x = slides, value = vaccine.schoolage.ses, 
#                  location = ph_location_fullsize() )
# 
# 
# 
# 
# print(slides, target = "20210916 Teenage vaccination.pptx")
# 
# 
# 
# ## School age by zip code
# library(tidycensus)
# v2019 <- load_variables(2019, "acs5", cache = TRUE)
# 
# vars <- c(
#   "B01001_004",  #male, 5-9 
#   "B01001_005",  #male, 10-14
#   "B01001_006",  #male, 15-17
#   "B01001_007",  #male, 18-19
#   "B01001_028",  #female, 5-9
#   "B01001_029",  #female, 10-14
#   "B01001_030",  #female, 15-17
#   "B01001_031"  #female, 18-19
# )
# ALLzips.age <-get_acs(geography = "zcta",
#                  variables = vars,
#                  #state = "25", # cannot limit to state in ZCTA
#                  year = 2019) 
# 
# library(tidyverse) 
# boston.zips<-c("02163",
#                "02135","02134","02129","02108","02114","02116","02199",
#                "02109","02110","02103","02113","02128","02121","02125","02122","02124","02115","02215",
#                "02136","02130","02126","02131","02119","02120","02127","02210","02111","02118","02132")
# 
# Bostonzips.age<- left_join(ALLzips.age %>%
#     rename(zipcode=GEOID,
#            pop = estimate)%>%
#   filter(zipcode %in% boston.zips) %>%
#       mutate(pop = if_else(variable == "B01001_005" |variable == "B01001_029", pop/2, pop)) %>%  # splitting age group 10-14 into equal parts: 10-11yrs and 12-14 
#       mutate( group= fct_collapse(variable,
#                                         "age5_11" = c("B01001_004", "B01001_005", "B01001_028", "B01001_029"),
#                                         "age12_14" = c("B01001_005", "B01001_029"),
#                                        "age15_17" = c("B01001_006", "B01001_030"),
#                                        "age18_19" = c("B01001_007", "B01001_031"))) %>%
#        group_by(zipcode, group) %>%
#       summarize(pop = round(sum(pop),0)) %>%
#         ungroup(),
#       read_csv("MAtowns.csv") %>% 
#             select(Town, zipcode) %>%
#             mutate(zipcode = sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})),
#     by = "zipcode") 
# 
# # included.neighborhoods<-
# # left_join(
# #       Bostonzips.age %>%
# #           filter(group == "age5_11") %>%
# #           group_by(Town) %>%
# #           summarise(pop.age5_11=sum(pop)),
# #       Bostonzips.age %>%
# #         filter(group == "age12_14") %>%
# #         group_by(Town) %>%
# #         summarise(pop.age12_14=sum(pop))),
# # Bostonzips.age %>%
# #   filter(group == "age18_19") %>%
# #   group_by(Town) %>%
# #   summarise(pop.age18_19=sum(pop))
# # ) %>% 
# #   mutate(ratio = pop.age18_19/pop.age12_14) %>%
# #   filter(ratio< 1.25 | Town == "Roxbury")
#   
# 
# load( file="vaccine.anydose.age.Rdata")
# 
# schoolage.vax.boston<- left_join(
#  left_join(
#    Bostonzips.age %>%  
#          group_by(zipcode, group) %>%
#          summarize(pop = sum(pop)) %>%  #add male+female
#            ungroup() %>%
#           mutate(agegroup = fct_collapse(group, 
#                                          "age12to15" = "age12_14",
#                                          "age16to19" = c("age15_17", "age18_19")),
#                  studentpop = if_else(agegroup=="age12to15", pop*1.33333, pop)),
#          read_csv("MAtowns.csv") %>% 
#            select(Town, zipcode) %>%
#            mutate(zipcode = sapply(zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})),
#          by = "zipcode") %>%
#     group_by(Town, zipcode, agegroup) %>%
#     summarize(pop = sum(studentpop))%>% ungroup(), 
#   vaccine.anydose.age %>%
#   select(reportdate, zipcode, age12to15.full, age16to19.full) %>% 
#    rename("age12to15" = "age12to15.full", "age16to19"="age16to19.full") %>%
#   pivot_longer(cols = c(age12to15, age16to19), names_to = "agegroup", values_to = "fullvax"), by=c("zipcode", "agegroup")) %>%
#   group_by(Town, agegroup, reportdate) %>%
#   summarise(fullvax =sum(fullvax, na.rm=TRUE),
#             pop = sum(pop, na.rm=TRUE)) %>%
#   mutate(pct.vax = fullvax/pop,
#          pct.vax = if_else(pct.vax>0.99, 0.99, pct.vax)) %>%   
#   ungroup() 
# 
# 
# schoolage.vax.boston %>% filter(agegroup=="age12to15") %>%
#   ggplot()+
#   geom_line(aes(x=reportdate, y=pct.vax*100, group=Town, color=Town), size=2, alpha=0.8) +
#   scale_color_simpsons() +
#   theme_classic() + theme(aspect.ratio= 0.75, plot.title = element_text(size = rel(1.5)),
#                           axis.title = element_text(size = rel(1.0)), axis.text = element_text(size = rel(1.0)))+
#   coord_cartesian(ylim=c(0,100), xlim=c(as.Date("2021-05-10"), Sys.Date()+50))+
#   scale_x_date(date_breaks = "month", labels = scales::label_date_short()) +
#   scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100)) +
#   geom_hline(yintercept = 80, linetype= "dotted")+
#   labs(title= "Full vaccination among students, 12 to 15 years",
#        subtitle=paste0("Data current as of ", max(schoolage.vax.boston$reportdate)),
#        x="Date",
#        y="Percent Fully-Vaccinated",
#        caption="Source: MDPH zipcode-level vaccinations aggregated by BPHC health district. ACS poplulation estimates.")+
#   geom_label_repel(data=schoolage.vax.boston %>% filter(reportdate == max(reportdate) & agegroup=="age12to15"),
#                    aes(x=reportdate, y=pct.vax*100,  label=paste0(Town, "\n", round(pct.vax*100, 1), "%"), color=Town), fill="white",
#                    min.segment.length = 0, max.overlaps = Inf, xlim=c(max(schoolage.vax.boston$reportdate), as.Date(NA))) +
#   guides(color="none") 
# 
# ggsave("Boston 12to15 vaccination.pdf", width=16, height=12)
