Test<-'Ana Macanovic'
funiform_string(Test,numbers=T,punct=F,spaces=T,caps=T)

#dubbeltjes in de universiteit variabele:
#RU/RUG is jochem
#RUG/RU is marcel lubbers
#RUG/Tilburg is siegwart lindenberg
#UU/UvA is ineke maas
#UvA/EUR is agniestska kanas

unicheck<-cbind.data.frame(df_ego3$unaam,df_ego3$universiteit.22,df_ego3$universiteit.24,df_ego3$universiteit.25)
funccheck<-cbind.data.frame(df_ego3$unaam,df_ego3$functie.22,df_ego3$functie.24,df_ego3$functie.25)
gendcheck<-cbind.data.frame(df_ego3$unaam,df_ego3$gender,df_ego3$universiteit.22,df_ego3$universiteit.24,df_ego3$universiteit.25)
unicheck2<-cbind.data.frame(df_ego3$unaam,df_ego3$universiteit.22,df_ego3$universiteit.24,df_ego3$universiteit.25,df_ego3$university)
funccheck2<-cbind.data.frame(df_ego3$funcsub,df_ego3$func22)

#'staff'
#86 david de kort, phd in later years, change to phd
#89 dennis raven, RUG website says researcher, change to that
#143 gerben moerman, linkedin says 'senior lecturer', change to assistant professor
#190 jennifer schijf, RUG website mentions education but little research, change to lecturer
#284 marijn van klingeren, RU website mentions education, change to lecturer
#385 sanne berends, RUG website says 'data steward', change to researcher
#418 theo schuyt, emeritus professor, change to full professor
#419 theo van der zee, linkedin says research bureau director, change to researcher
#431 tim immerzeel, UU website says education coordinator, change to lecturer
#thats it, nice job

#missing genders, all of these people work at RUG
#f.goedkoop, RUG website has a photo of someone with decidedly womanly gender signifiers, change to woman
#l.zijlstra, RUG website has a photo of someone with decidedly manly gender signifiers, change to man
#s.liu, researchgate has a photo of someone with decidedly womanly gender signifiers, change to woman
#t.nowak, RUG website has a photo of someone with decidedly manly gender signifiers, change to man
#z.e.pardoel, RUG website has a photo of someone with decidedly womanly gender signifiers, change to woman
#sociologists gendered!

plot(netvis1,
     vertex.shape = ifelse(df_egoF$icsAny == 1, "square", "circle"), #ics is square, other round
     vertex.color = df_egoF$universityN, #makes ics people red, others blue 
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.2,
     edge.arrow.size =0.2)

plot(netvis2,
     vertex.shape = ifelse(df_egoF$icsAny == 1, "square", "circle"), #ics is square, other round
     vertex.color = df_egoF$universityN,
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.3,
     edge.arrow.size =0.2)

plot(netvis2,
     vertex.color = ifelse(dfNiso$icsAny == 1, "red", "blue"),
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.2,
     edge.arrow.size =0.2)

transitivity(netvis2)
transitivity(netvis2,type='average')
transitivity(netvis1)
transitivity(netvis1,type='average')

install.packages("sna")
require(sna)

sna::dyad.census(wave1)
sna::dyad.census(wave2)
sna::dyad.census(wave3)
sna::triad.census(wave1,mode="graph")
sna::triad.census(wave2,mode="graph")
sna::triad.census(wave3,mode="graph")

dyadicCov(obsData=collabnet1,icsanyD)

plot(model3gof1)
plot(model3gof2)

#Attempt 3, with separate ics variables and controls
model3gof1 <- sienaGOF(testEstimate3, IndegreeDistribution, verbose = FALSE, join = TRUE, varName = "net")
model3gof1
plot(model3gof1)

plot(model3gof1)
plot(model3gof2)
plot(model3gof3)

plot(model4gof1)
plot(model4gof2)
plot(model4gof3)

transGplot
transAplot

plot(model5gof1)
plot(model5gof2)
plot(model5gof3)                     

plot(model6gof1)
plot(model6gof2)
plot(model6gof3)  

plot(netvis0,
     vertex.color = ifelse(df_egoF$icsAny == 1, "red", "blue"),
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.2,
     edge.arrow.size =0.2)

plot(netvis1,
     vertex.color = ifelse(dfNiso1$icsAny == 1, "red", "blue"),
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.2,
     edge.arrow.size =0.2)

plot(netvis2,
     vertex.color = ifelse(dfNiso2$icsAny == 1, "red", "blue"),
     vertex.label = NA,
     vertex.size = 5,
     edge.width = 0.2,
     edge.arrow.size =0.2)


#rm(list = ls())

dim(wave1)
dim(netNiso1)
dim(netNiso2)

table(df_egoF$icsAny)
table(df_egoF$icsAffiliate)
table(df_egoF$icsGraduate)
table(df_egoF$functionS)
table(df_egoF$university)
table(df_egoF$gender)

#Frequency table ICS
ValueICS<-c('Non affiliate staff','Affiliate staff',
            'Non graduate','Graduate',
            'Total non affiliate','Total affiliate')
CountICS<-c(342,116,364,94,297,161)
freICSdf<-data.frame(Value=ValueICS,Count=CountICS)
fshowdf(freICSdf)

#Frequency table university
Valueuni<-c('UvA','VU','EUR','UU','RU','TiU','RUG')
Countuni<-c(76,87,50,57,52,32,104)
freUNIdf<-data.frame(University=Valueuni,Count=Countuni)
fshowdf(freICSdf)

#Frequency table function
Valuefun<-c('PhD student','Postdoc','Lecturer','Researcher',
            'Assistant professor','Associate professor','Full professor')
Countfun<-c(146,21,27,39,92,43,77)
freFUNdf<-data.frame(Function=Valuefun,Count=Countfun)
fshowdf(freICSdf)

#Frequency table gender
Valuegen<-c('Female','Male')
Countgen<-c(245,213)
freGENdf<-data.frame(Gender=Valuegen,Count=Countgen)
fshowdf(freICSdf)  


#Table of convergence and fit statistics
model <- c("Target", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
conv <- c('>0.25', 0.048, 0.377, 0.199, 0.323, 0.209, 1.119)
indegreegof <- c('>0.05', 0, 0.007, 0.006, 0.024, 0.010, 0.037)
outdegreegof <- c('>0.05', 0.003, 0.007, 0.003, 0.069, 0.062, 0.075)
triadgof <- c('>0.05', 0, 0.004, 0.003, 0.093, 0.014, 0.093)
convGOFdf <- data.frame(Model=model,Max.Conv=conv,
                        IndegreeGOF=indegreegof,OutdegreeGOF=outdegreegof,
                        TriadGOf=triadgof)

#Combined table of results for structural variables
model2<-c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
densityEst<-c(-3.82, -3.97, -4.62, -3.26, -3.11, -2.61)
densitySe<-c(0.08, 0.18, 0.29, 0.82, 1.44, 0.77)
densityT<-c(-46.54, -21.69, -15.41, -3.96, -2.16, -3.39)
recipEst<-c(3.25, 2.06, 1.88, 2.58, 2.55, 2.69)
recipSe<-c(0.19, 0.21, 0.21, 0.36, 0.31, 0.50)
recipT<-c(17.21, 10.07, 9.11, 7.17, 8.24, 5.39)
transtrEst<-c(' ', 1.11, 0.98, 1.51, 1.55, 1.71)
transtrSe<-c(' ', 0.14, 0.14, 0.27, 0.39, 0.43)
transtrT<-c(' ', 8.05, 7.12, 5.68, 3.99, 3.96)
inpopEst<-c(' ', 0.17, 0.17, 0.19, 0.18, 0.19)
inpopSe<-c(' ', 0.04, 0.06, 0.04, 0.11, 0.04)
inpopT<-c(' ', 4.43, 2.79, 4.64, 1.58, 4.55)
outactEst<-c(' ', ' ', ' ', -0.20, -0.22, -0.29)
outactSe<-c(' ', ' ', ' ', 0.11, 0.18, 0.13)
outactT<-c(' ', ' ', ' ', -1.90, -1.19, -2.35)
inactEst<-c(' ', ' ', ' ', -0.56, -0.59, -0.71)
inactSe<-c(' ', ' ', ' ', 0.26, 0.30, 0.45)
inactT<-c(' ', ' ', ' ', -2.17, -1.98, -1.59)
isoEst<-c(' ', 4.24, 4.09, 5.49, 5.74, 5.82)
isoSe<-c(' ', 0.78, 0.68, 1.11, 2.45, 0.85)
isoT<-c(' ', 5.43, 5.99, 4.97, 2.34, 6.85)
strucResultdf<-data.frame(Model=model2, DensityEst=densityEst,DensitySE=densitySe,DensityT=densityT,
                          RecipEst=recipEst,RecipSE=recipSe,RecipT=recipT,
                          TransEst=transtrEst,TransSE=transtrSe,TransT=transtrT,
                          InpopEst=inpopEst,InpopSE=inpopSe,InpopT=inpopT,
                          OutactEst=outactEst,OutactSE=outactSe,OutactT=outactT,
                          InactEst=inactEst,InactSE=inactSe,InactT=inactT,
                          IsolEst=isoEst,IsolSE=isoSe,IsolT=isoT)

model3<-c('Model 2', 'Model 3', 'Model 4', 'Model 5', 'Model 6')
anyICSest<-c(0.41, 0.21, 0.19, 0.19, ' ')
anyICSse<-c(0.10, 0.12, 0.13, 0.11, ' ')
anyICSt<-c(4.12, 1.79, 1.49, 1.70, ' ')
affICSest<-c(' ', ' ', ' ', ' ', -0.05)
affICSse<-c(' ', ' ', ' ', ' ', 0.14)
affICSt<-c(' ', ' ', ' ', ' ', -0.36)
gradICSest<-c(' ', ' ', ' ', ' ', -0.13)
gradICSse<-c(' ', ' ', ' ', ' ', 0.19)
gradICSt<-c(' ', ' ', ' ', ' ', -0.68)
uniest<-c(' ', 1.18, 1.21, 1.21, 1.28)
unise<-c(' ', 0.11, 0.13, 0.11, 0.11)
uniT<-c(' ', 10.35, 9.16, 11.29, 11.29)
funcest<-c(' ', 0.43, 0.41, 0.42, 0.39)
funcse<-c(' ', 0.23, 0.26, 0.23, 0.33)
funcT<-c(' ', 1.89, 1.61, 1.81, 1.19)
genest<-c(' ', 0.05, 0.06, 0.06, 0.07)
gense<-c(' ', 0.09, 0.12, 0.11, 0.12)
genT<-c(' ', 0.49, 0.53, 0.54, 0.59)
covarResultdf<-data.frame(Model=model3,ICSest=anyICSest,ICSse=anyICSse,ICSt=anyICSt,
                          UnivEst=uniest,UnivSE=unise,UnivT=uniT,
                          FunctEst=funcest,FunctSE=funcse,FunctT=funcT,
                          GendEst=genest,GendSE=gense,GendT=genT,
                          AffilICSest=affICSest,AffilICSse=affICSse,AffilICSt=affICSt,
                          GradICSest=gradICSest,GradICSse=gradICSse,GradICSt=gradICSt)
print(covarResultdf)
print(strucResultdf)
print(convGOFdf)

testEstimates2
testEstimates3
testEstimates4
testEstimates5
testEstimates6

fshowdf(covarResultdf)

table(df_egoF$functie.22,useNA='always')
table(df_egoF$functie.24,useNA='always')
table(df_egoF$functie.25,useNA='always')
