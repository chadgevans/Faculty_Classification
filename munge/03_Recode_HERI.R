# Salary and Income
df$SALARYALL=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # returns the max of the two

df$AGE<-2010-df$BIRTHYR # other age varaible is cut in intervals for some reason

df<- df %>% 
  mutate(CARNEGIE = recode(CARNEGIE, `RU/VH: Research Universities (very high research activity)`="R1",  
                           `RU/H: Research Universities (high research activity)`="R2",                                                              
                           `DRU: Doctoral/Research Universities`="R3/Doctoral",                                                                                 
                           `Assoc/Pub-R-L: Associates--Public Rural-serving Large` = "Associates", 
                           `Assoc/Pub-R-M: Associates--Public Rural-serving Medium`="Associates", 
                           `Assoc/Pub-S-MC: ASSOCIATE--Public Suburban-serving Multicampus`="Associates",
                           `Assoc/Pub-S-SC: ASSOCIATE--Public Suburban-serving Single Campus`="Associates",                                                    
                           `Assoc/Pub-U-MC: ASSOCIATE--Public Urban-serving Multicampus`="Associates",                                                         
                           `Assoc/Pub-U-SC: ASSOCIATE--Public Urban-serving Single Campus`="Associates",                                                       
                           `Assoc/Pub2in4: ASSOCIATE--Public 2-year colleges under 4-year universities`="Associates",                                          
                           `Assoc/Pub4: ASSOCIATE--Public 4-year Primarily ASSOCIATE`="Associates",
                           `Bac/A&S: Baccalaureate Colleges--Arts & Sciences`="Bachelors/Masters",                                                                  
                           `Bac/Assoc: Baccalaureate/ASSOCIATE Colleges`="Bachelors/Masters",                                               
                           `Bac/Diverse: Baccalaureate Colleges--Diverse Fields`="Bachelors/Masters",                                                                 
                           `Masters L: Masters Colleges and Universities (larger programs)`="Bachelors/Masters",                                                      
                           `Masters M: Masters Colleges and Universities (medium programs)`="Bachelors/Masters",                                                      
                           `Masters S: Masters Colleges and Universities (smaller programs)`="Bachelors/Masters",
                           `Spec/Arts: Special Focus Institutions--Schools of art, music, and design`="Other",                                           
                           `Spec/Bus: Special Focus Institutions--Schools of business and management`="Other",                                            
                           `Spec/Faith: Special Focus Institutions--Theological seminaries, Bible colleges, and other faith-related institutions`="Other",
                           `Spec/Health: Special Focus Institutions--Other health professions schools`="Other",                                           
                           `Spec/Other: Special Focus Institutions--Other special-focus institutions`="Other", `-3` = NA_character_))
df$CARNEGIE<-factor(df$CARNEGIE,levels = c("R1","R2","R3/Doctoral","Bachelors/Masters","Associates","Other"))

df<- df %>% 
  mutate(BIGLAN = recode(DEPT, `Agriculture/natural resources/related`="Hard/Applied",
                         `Architecture and related services`="Hard/Applied",
                         `Area/ethnic/cultural/gender studies`="Soft/Pure",
                         `Arts (visual and performing)`="Soft/Pure",
                         `Biological and biomedical sciences`="Hard/Applied",
                         `Business/management/marketing/related`="Hard/Applied",
                         `Communication/journalism/ comm. tech`="Soft/Applied",
                         `Computer/info sciences/support tech`="Hard/Applied",
                         `Construction trades`="Hard/Applied",
                         `Education`="Soft/Applied",
                         `Engineering technologies/technicians`="Hard/Applied",
                         `English language and literature/letters`="Soft/Pure",
                         `Family/consumer sciences, human sciences`="Soft/Applied",
                         `Foreign languages/literature/linguistics`="Soft/Pure",
                         `Health professions/clinical sciences`="Hard/Applied",
                         `Legal professions and studies`="Soft/Applied",
                         `Library science`="Soft/Applied",
                         `Mathematics and statistics`="Hard/Applied",
                         `Mechanical/repair technologies/techs`="Hard/Applied",
                         `Multi/interdisciplinary studies`="Soft/Pure",
                         `Other`="Other",
                         `Parks/recreation/leisure/fitness studies`="Soft/Applied",
                         `Personal and culinary services`="Soft/Applied",
                         `Philosophy, religion & theology`="Soft/Pure",
                         `Physical sciences`="Hard/Pure",
                         `Psychology`="Soft/Pure",
                         `Public administration/social services`="Soft/Applied",
                         `Science technologies/technicians`="Hard/Applied",
                         `Security & protective services`="Soft/Applied",
                         `Social sciences (except psych) and history`="Soft/Pure",
                         `Transportation & materials moving`="Other"))
df$BIGLAN<-factor(df$BIGLAN, levels = c("Hard/Applied","Hard/Pure","Soft/Applied","Soft/Pure","Other"))
df$BIGLAN2<-df$DEPT; levels(df$BIGLAN2)<-c("Hard.Applied.Life","Soft.Applied.NonLife","Soft.Pure.Life","Soft.Pure.NonLife","Hard.Applied.Life","Soft.Applied.NonLife","Soft.Applied.NonLife","Hard.Applied.NonLife","Hard.Applied.NonLife","Soft.Applied.Life","Hard.Applied.NonLife","Soft.Pure.NonLife","Soft.Applied.Life","Soft.Pure.NonLife","Hard.Applied.Life","Soft.Applied.NonLife","Soft.Applied.NonLife","Hard.Pure.NonLife","Hard.Applied.NonLife","Soft.Pure.Life","Soft.Applied.Life","Hard.Applied.NonLife","Soft.Pure.NonLife","Soft.Pure.NonLife","Hard.Pure.NonLife","Soft.Pure.Life","Soft.Applied.NonLife","Hard.Applied.NonLife","Soft.Applied.NonLife","Soft.Pure.Life","Soft.Applied.NonLife","Other")
df$BIGLAN3<-df$BIGLAN2; levels(df$BIGLAN3)<-c("Hard.Applied","Soft.Applied","Soft.Pure.Life","Soft.Pure.NonLife","Hard.Applied","Soft.Applied","Hard.Pure","Other")

df<- df %>%
  mutate(OBEREG=recode(OBEREG, `Far West - AK CA HI NV OR WA`="West",
                       `Great Lakes - IL IN MI OH WI`="Midwest",
                       `Mid East - DE DC MD NJ NY PA`="East",
                       `New England - CT ME MA NH RI VT`="East",
                       `Other`="Other",
                       `Plains - IA KS MN MO NE ND SD`="Midwest",
                       `Rocky Mountains - CO ID MT UT WY`="West",
                       `Southeast - AL AR FL GA KY LA MS NC SC TN VA WV`="South",
                       `Southwest - AZ NM OK TX`="West"))
df$OBEREG<-relevel(df$OBEREG,"East")

df$MARITAL2<-df$MARITAL; levels(df$MARITAL2)<-c("Not","Married","Not","Not","Married","Not") # cohab=married
df$RACEGROUP2<-df$RACEGROUP; levels(df$RACEGROUP2)<-c(rep("Minority",6),"White") # cohab=married
df$NCHILD3<-as.factor(as.numeric(df$NCHILD1)-1+as.numeric(df$NCHILD2)-1)
levels(df$NCHILD3)<-c("No Children","One Child",rep("Multiple Children",7)) # cohab=married

# Professional Development factor
profdf<-df %>% select(starts_with("PROFDEV")) %>% as.data.frame
PROFDEVvars<-df %>% select(starts_with("PROFDEV")) %>% names() 
for(i in PROFDEVvars){
  levels(profdf[,i])=c(rep("0",3),"1")
  profdf[,i]<-as.numeric(profdf[,i])-1
}
pca<-princomp(~ ., data = profdf, na.action=na.exclude)
df$PROFDEVFAC<--(pca$scores[,1]) # I think this is the scores (for comp 1). sign is negative so inverse it.

levels(df$GENACT01) <- c("Non-Union", "Union") #Act: Are you a member of a faculty union?
levels(df$GENACT02) <- c("Non-Citizen", "Citizen") #Act: Are you a member of a faculty union?

df<- df %>%
  mutate(DEGWORK2=recode(DEGWORK, `Bachelors (B.A., B.S., etc.)`="Yes",                                   
                        `Ed.D.`="Yes",
                        `LL.B., J.D.`="Yes",
                        `M.D., D.D.S. (or equivalent)`="Yes",
                        `Masters (M.A., M.S., M.F.A., M.B.A., etc.)`="Yes",
                        `None`="No",
                        `Other degree`="Yes",
                        `Other first professional degree beyond B.A. (e.g., D.D., D.V.M.)`="Yes",
                        `Ph.D.`="Yes"))
df$DEGWORK2<-relevel(df$DEGWORK2,"No")


df<- df %>%
  mutate(DEGEARN2=recode(DEGEARN, `Bachelors (B.A., B.S., etc.)`="BA or Less",                                   
                         `Ed.D.`="Prof Degree",
                         `LL.B., J.D.`="Prof Degree",
                         `M.D., D.D.S. (or equivalent)`="Prof Degree",
                         `Masters (M.A., M.S., M.F.A., M.B.A., etc.)`="Prof Degree",
                         `None`="BA or Less", 
                         `Other degree`="BA or Less", # Assuming this means less, like an assoc. degree
                         `Other first professional degree beyond B.A. (e.g., D.D., D.V.M.)`="Prof Degree",
                         `Ph.D.`="Ph.D."))

df$SELECTIVITY2=cut(df$SELECTIVITY, breaks=quantile(df$SELECTIVITY, probs = c(0,.9,1),na.rm=T))  # defined as median SAT math and verbal (or ACT composite) of 1st time freshmen
levels(df$SELECTIVITY2)<-c("Not","Selective")

df<- df %>%
  mutate(INSTDESCR03=recode(INSTDESCR03, `Not descriptive`="Not very",                                   
                         `Somewhat descriptive`="Not very",
                         `Very descriptive`="Very"))

df<- df %>%
  mutate(INSTDESCR08=recode(INSTDESCR08, `Not descriptive`="Not very",                                   
                            `Somewhat descriptive`="Not very",
                            `Very descriptive`="Very"))

df<- df %>%
  mutate(INSTOPN10=recode(INSTOPN10, `Agree somewhat`="Agree",                                   
                          `Agree strongly`="Agree",
                          `Disagree somewhat`="Disagree",
                          `Disagree strongly`="Disagree"))
df$INSTOPN10<-relevel(df$INSTOPN10,"Disagree")


df<- df %>%
  mutate(INSTOPN11=recode(INSTOPN11, `Agree somewhat`="Agree",                                   
                          `Agree strongly`="Agree",
                          `Disagree somewhat`="Disagree",
                          `Disagree strongly`="Disagree"))
df$INSTOPN11<-relevel(df$INSTOPN11,"Disagree")

df$HEALTHBENEFITS=df$SATIS02; levels(df$HEALTHBENEFITS)=c("Health Ins", "No Health Ins",rep("Health Ins",3)) #Not Applicable means "No insureance"
df$HEALTHBENEFITS<-relevel(df$HEALTHBENEFITS,"No Health Ins")

df$RETIREBENEFITS=df$SATIS03; levels(df$RETIREBENEFITS)=c("Retirement","No Retirement",rep("Retirement",3))
df$RETIREBENEFITS<-relevel(df$RETIREBENEFITS,"No Retirement")

df$PRINACT2<-df$PRINACT; levels(df$PRINACT2)[4]<-"Other"
df$PRINACT2<-factor(df$PRINACT2,levels = c("Teaching","Research","Administration","Other"))
