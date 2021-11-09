library(haven)
library(ggplot2)
library(margins)
library(survey)
library(sjPlot)
library(ggeffects)
library(questionr)
library(ggpubr)
library(MASS)
library(stats)
library(wCorr)
library(EdSurvey)
library(mlogit)
library(stargazer)
library(nnet)
library(svyVGAM)


setwd("D:/Makhare Files/Makhare/Blogs/2021_Most_Important_Isues")



ned<-read_dta("NED_NED-Tbilisi_Public_14.09.21.dta")
names(ned)


###_________________________________Independant variables___________________--
table(ned$RESPDISTRICT)
#Tbilisi District
ned$RESPDISTRICT_new<-ned$RESPDISTRICT
ned$RESPDISTRICT_new<-factor(ned$RESPDISTRICT_new, levels = c(1,2,3,4,5,6,7,8,9,10) , labels = c("Mtatsminda", "Vake", "Saburtalo", "Krtsanisi", "Isani", "Samgori", "Chughureti", "Didube", "Nadzaladevi", "Gldani"))
table(ned$RESPDISTRICT_new)



#sex

table(ned$RESPSEX)
ned$RESPSEX_new<-ned$RESPSEX
ned$RESPSEX_new<-factor(ned$RESPSEX_new, levels = c(1,2), labels = c("Male", "Female"))
table(ned$RESPSEX_new)

##agegroup

table(ned$AGEGROUP)
ned$AGEGROUP_new<-ned$AGEGROUP
ned$AGEGROUP_new<-factor(ned$AGEGROUP_new, levels = c(1,2,3), labels = c("18-34", "35-54", "55+"))
table(ned$AGEGROUP_new)

##Education
table(ned$RESPEDU)
ned$RESPEDU_new<-ned$RESPEDU
ned$RESPEDU_new[ned$RESPEDU_new<5]<-1
ned$RESPEDU_new[ned$RESPEDU_new==5]<-2
ned$RESPEDU_new[ned$RESPEDU_new>5]<-3
ned$RESPEDU_new<-factor(ned$RESPEDU_new, levels = c(1,2,3), labels = c("Secondary or lower", "Technical", "Tertiary"))
table(ned$RESPEDU_new)


#employment

table(ned$EMPLSIT)
ned$EMPLIST_new<-ned$EMPLSIT
ned$EMPLIST_new[ned$EMPLIST_new<0]<-NA
ned$EMPLIST_new[ned$EMPLIST_new<5]<-0
ned$EMPLIST_new[ned$EMPLIST_new==7]<-0
ned$EMPLIST_new[ned$EMPLIST_new==8]<-NA
ned$EMPLIST_new[ned$EMPLIST_new>4]<-1

ned$EMPLIST_new<-factor(ned$EMPLIST_new, levels = c(0,1), labels = c("Not employed", "Employed"))
table(ned$EMPLIST_new)



#ETHNICITY

ned$ETHNIC_new<-ned$ETHNIC
ned$ETHNIC_new[ned$ETHNIC_new<3]<-1
ned$ETHNIC_new[ned$ETHNIC_new>3]<-1
ned$ETHNIC_new[ned$ETHNIC_new==3]<-2
ned$ETHNIC_new<-factor(ned$ETHNIC_new, levels = c(1,2), labels = c("NonGeorgian", "Georgian"))
table(ned$ETHNIC_new)



#IDP status

ned$CONFLMV_new<-ned$CONFLMV
ned$CONFLMV_new[ned$CONFLMV_new<0]<-NA
ned$CONFLMV_new<-factor(ned$CONFLMV_new, levels = c(0,1), labels = c("Non-IDP", "IDP"))
table(ned$CONFLMV_new)



#Wealth (10 items);
table(ned$OWNAIRC)
table(ned$OWNCARS)
table(ned$OWNCHTG)
table(ned$OWNCOMP)
table(ned$OWNCOTV)
table(ned$OWNFRDG)
table(ned$OWNHWT)
table(ned$OWNSPHN)
table(ned$OWNTBLT)
table(ned$OWNWASH)

ned$OWNAIRC_n<-ned$OWNAIRC
ned$OWNAIRC_n[ned$OWNAIRC_n<=-1]<-NA
ned$OWNCARS_n<-ned$OWNCARS
ned$OWNCARS_n[ned$OWNCARS_n<=-1]<-NA
ned$OWNCHTG_n<-ned$OWNCHTG
ned$OWNCHTG_n[ned$OWNCHTG_n<=-1]<-NA
ned$OWNCOMP_n<-ned$OWNCOMP
ned$OWNCOMP_n[ned$OWNCOMP_n<=-1]<-NA
ned$OWNCOTV_n<-ned$OWNCOTV
ned$OWNCOTV_n[ned$OWNCOTV_n<=-1]<-NA
ned$OWNFRDG_n<-ned$OWNFRDG
ned$OWNFRDG_n[ned$OWNFRDG_n<=-1]<-NA
ned$OWNHWT_n<-ned$OWNHWT
ned$OWNHWT_n[ned$OWNHWT_n<=-1]<-NA
ned$OWNSPHN_n<-ned$OWNSPHN
ned$OWNSPHN_n[ned$OWNSPHN_n<=-1]<-NA
ned$OWNTBLT_n<-ned$OWNTBLT
ned$OWNTBLT_n[ned$OWNTBLT_n<=-1]<-NA
ned$OWNWASH_n<-ned$OWNWASH
ned$OWNWASH_n[ned$OWNWASH_n<=-1]<-NA

table(ned$OWNAIRC_n)
table(ned$OWNCARS_n)
table(ned$OWNCHTG_n)
table(ned$OWNCOMP_n)
table(ned$OWNCOTV_n)
table(ned$OWNFRDG_n)
table(ned$OWNHWT_n)
table(ned$OWNSPHN_n)
table(ned$OWNTBLT_n)
table(ned$OWNWASH_n)


ned$wealth<-(ned$OWNAIRC_n+
             ned$OWNCARS_n+
             ned$OWNCHTG_n+
             ned$OWNCOMP_n+
             ned$OWNCOTV_n+
             ned$OWNFRDG_n+
             ned$OWNHWT_n+
             ned$OWNSPHN_n+
             ned$OWNTBLT_n+
             ned$OWNWASH_n)
table(ned$wealth)


# Party support
table(ned$VOTPPELG)
ned$VOTPPELG_new<-ned$VOTPPELG
ned$VOTPPELG_new[ned$VOTPPELG_new<1]<-0
ned$VOTPPELG_new[ned$VOTPPELG_new>1]<-2
ned$VOTPPELG_new<-factor(ned$VOTPPELG_new, levels = c(0,1,2), labels = c("Didn't name party", "Georgian Dream", "Opposition"))
table(ned$VOTPPELG_new)




### _______________________ 1st dependent variable - party leaders vs party promises____________________
ned$TBVOTDCD_new<-ned$TBVOTDCD
table(ned$TBVOTDCD_new)
ned$TBVOTDCD_new<-factor(ned$TBVOTDCD_new, levels = c(-5, -2, -1, 1,2), labels = c("Agree with neither", "Refuse to answer", "Don't know", "Who are party leaders is more important", "What party promises is more important"))

## Weighted proportional table:
prop.table(xtabs(WTIND~TBVOTDCD_new, data=ned, na.action = "na.omit"))

## Not weighted proportional table:
ggplot(ned, aes(x = factor(TBVOTDCD_new))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#0073C2FF")+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  labs(title = "Speaking of the local elections, which of the following do you agree?",
       y = "Percent",
       x = "Agree or disagree")+ theme_pubclean()


#recoding as three category
ned$TBVOTDCD_new_2<-ned$TBVOTDCD
table(ned$TBVOTDCD_new_2)
ned$TBVOTDCD_new_2[ned$TBVOTDCD_new_2==1]<-0
ned$TBVOTDCD_new_2[ned$TBVOTDCD_new_2<=-1]<-1

ned$TBVOTDCD_new_2<-factor(ned$TBVOTDCD_new_2, levels = c(0, 1, 2), labels = c("Party leaders are more important", "Agree none of them", "Party promises are more important"))


##_______________________________________________Important issues to vote for_____________
table(ned$ISSINAC)
table(ned$ISSPRKNG)
table(ned$ISSHOAN)
table(ned$ISSCOKI)
table(ned$ISSACKI)
table(ned$ISSSCH)
table(ned$ISSUT)
table(ned$ISSSWG)
table(ned$ISSELSU)
table(ned$ISSGASU)
table(ned$ISSWASU)
table(ned$ISSSTLI)
table(ned$ISSSPREFC)
table(ned$ISSRDS)
table(ned$ISSPUTR)
table(ned$ISSRESPCS)
table(ned$ISSTRF)
table(ned$ISSARPLL)
table(ned$ISSPLYRDS)
table(ned$ISSCUFCLT)
table(ned$ISSCLSTR)
table(ned$ISSTRCLL)
table(ned$ISSOTH)


#environment issues:
ned$env<-0
table(ned$env)
ned$env[ned$ISSRESPCS==1 | ned$ISSARPLL==1 | ned$ISSCLSTR==1]<-1 #parks green spaces | pollution | clean streets

#road infrastructure issues:
ned$road<-0
table(ned$road)
ned$road[ned$ISSTRF ==1 | ned$ISSPUTR ==1 | ned$ISSRDS==1]<-1 #traffic | public transport | roads





##design survey
ned_svy<-svydesign(~1, weights = ned$WTIND, data = ned)

##_____________________________________________Model - environmental issues who______________________


model_env<-svyglm(env~RESPSEX_new+
                    AGEGROUP_new+
                    RESPDISTRICT_new+
                    EMPLIST_new+
                    RESPEDU_new+
                    CONFLMV_new+
                    ETHNIC_new+
                    wealth+
                    VOTPPELG_new, design = ned_svy)
summary(model_env)


p<-sjPlot::plot_model(model_env,
                      title = "Environmental issues as most important issues to determine who you'll vote for. By demographic variables",
                      colors = c("seagreen3", "deepskyblue2", "orchid"),
                      show.values = TRUE,
                      value.offset = .4,
                      value.size = 6,
                      dot.size = 4,
                      line.size = 2,
                      vline.color = "red",
                      width = 0.5,
                      axis.labels = c("Opposition supporter","Georgian Dream supporter", "Wealth index", "Ethnic Georgians","IDPs", "Tertiary education" ,"Technical education", "Employed","Gldani", "Nadzaladevi", "Didube", "Chughureti","Samgori","Isani", "Krtsanisi","Saburtalo","Vake", "55+ age group", "35-54 age group", "Female"))+
  font_size(title = 18, labels.y = 14)
p

##Print statistically significant variables predicted probabilities:
print(ggemmeans(model_env, terms = "RESPSEX_new"))
print(ggemmeans(model_env, terms = "AGEGROUP_new"))
print(ggemmeans(model_env, terms = "RESPDISTRICT_new"))
print(ggemmeans(model_env, terms = "RESPEDU_new"))

##_____________________________________________Model - transport infrastructure issues_________________



model_road<-svyglm(road~RESPSEX_new+
                    AGEGROUP_new+
                    RESPDISTRICT_new+
                    EMPLIST_new+
                    RESPEDU_new+
                    CONFLMV_new+
                    ETHNIC_new+
                    wealth+
                    VOTPPELG_new, design = ned_svy)
summary(model_road)


p2<-sjPlot::plot_model(model_road,
                      title = "Transport infrustructure issues as most important issues to determine who you'll vote for. By demographic variables",
                      colors = c("seagreen3", "deepskyblue2", "orchid"),
                      show.values = TRUE,
                      value.offset = .4,
                      value.size = 6,
                      dot.size = 4,
                      line.size = 2,
                      vline.color = "red",
                      width = 0.5,
                      axis.labels = c("Opposition supporter","Georgian Dream supporter", "Wealth index", "Ethnic Georgians","IDPs", "Tertiary education" ,"Technical education", "Employed","Gldani", "Nadzaladevi", "Didube", "Chughureti","Samgori","Isani", "Krtsanisi","Saburtalo","Vake", "55+ age group", "35-54 age group", "Female"))+
  font_size(title = 18, labels.y = 14)
p2

##Print statistically significant variables predicted probabilities:
print(ggemmeans(model_road, "RESPDISTRICT_new"))
table(ned_svy$variables$RESPDISTRICT_new)
g<-ggemmeans(model_road, "RESPDISTRICT_new")
View(g)










