df$HEALTHBENEFITS=df$SATIS02; levels(df$HEALTHBENEFITS)=c("Health Ins", "No Health Ins",rep("Health Ins",3)) #Not Applicable means "No insureance"
df$RETIREBENEFITS=df$SATIS03; levels(df$RETIREBENEFITS)=c("Retirement","No Retirement",rep("Retirement",3))

df<-df %>%
 na_if("Not applicable") %>%
  droplevels()

satisvars<- df %>% select(starts_with("SATIS"),-SATIS_WORKPLACE,-SATIS_COMPENSATION) %>% names()
for (i in satisvars){df[,i]<-as.numeric(unlist(factor(df[,i], levels = c("Not satisfied","Marginally satisfied","Satisfied","Very satisfied"))))
} # Make them all numeric

df<-df %>% mutate(GAPPANTT=factor(if_else(FULLSTAT=="Yes", "FT NTT", 
                                   if_else(FULLSTAT=="No" & PTCHOICE=="No", "Aspiring Academic",
                                           if_else(FULLSTAT=="No" & PTCHOICE=="Yes" & PTCAREER=="Yes", "Expert",
                                                   if_else(FULLSTAT=="No" & PTCHOICE=="Yes" & PTCAREER=="No" & GENACT03=="Yes", "Career-Ender", "Freelancer"))))))
df$GAPPANTT<-relevel(df$GAPPANTT,"FT NTT")

df$YEAR<-NULL # 2010 for all R's
df$RESTYPE<-NULL # empty vector

df<- df %>% 
  mutate(MARRIED=MARITAL) %>% 
  mutate(MARRIED= car::recode(MARRIED, "c('Married','Unmarried, living with partner')='Married'; c('Single','Divorced','Widowed','Separated')='Unmarried'"))

df<- df %>% 
  mutate(RACE=RACEGROUP)  %>% 
  mutate(RACE= car::recode(RACE, "c('American Indian','Other','Two or more race/ethnicity')='Other'"))
df$RACE<-relevel(df$RACE,"White")

df<- df %>% 
 mutate(DEGEARN = car::recode(DEGEARN, "c('Bachelors (B.A., B.S., etc.)','None','Other degree')='BA or Less'; 'Masters (M.A., M.S., M.F.A., M.B.A., etc.)'='Masters'; c('Ed.D.','LL.B., J.D.','M.D., D.D.S. (or equivalent)','Other first professional degree beyond B.A. (e.g., D.D., D.V.M.)')='Professional'"))
df$DEGEARN<-factor(df$DEGEARN,levels=c("Ph.D.","Professional","Masters","BA or Less"))

df<- df %>% 
   mutate(NCHILD1=as.numeric(df$NCHILD1)-1)
   
df$SEX<-relevel(df$SEX,"Male")
 
df$INSTTYPE<-relevel(df$INSTTYPE,"4-year")

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
df$CARNEGIE<-relevel(df$CARNEGIE,"R1")

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

df$TIMEEMPLOYED<-2010-df$APPTYR

df$SUBJID<-factor(df$SUBJID)

df$PRINACT<-factor(df$PRINACT, levels = c("Teaching","Administration","Research","Services to clients and patients","Other"))

df$ACADRANK<-factor(df$ACADRANK, levels = c("Instructor","Lecturer","Assistant Professor","Associate Professor","Professor"))

df$INSTCONT<-relevel(df$INSTCONT,"Public")

df<- df %>%
  mutate(INSTOPN10N=as.numeric(INSTOPN10)) %>%
  group_by(ACE) %>%
  mutate(AVGINSTOPN10N = mean(INSTOPN10N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTOPN11N=as.numeric(INSTOPN11)) %>%
  group_by(ACE) %>%
  mutate(AVGINSTOPN11N = mean(INSTOPN11N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTDESCR03N=as.numeric(INSTDESCR03)) %>%
  group_by(ACE) %>%
  mutate(FACRESP = mean(INSTDESCR03N,na.rm=T)) %>%
  as.data.frame()

df<- df %>%
  mutate(INSTDESCR08N=as.numeric(INSTDESCR08)) %>%
  group_by(ACE) %>%
  mutate(ADMINRESP = mean(INSTDESCR08N,na.rm=T)) %>%
  as.data.frame()

df$ADJUNCT1<- factor(rep(NA, nrow(df)), levels=c("Professional Adjuncts","Itinerant Academic","Single Inst Adjunct","Full-time") )   
df$ADJUNCT1[df$FULLSTAT=="Yes"] <- "Full-time"
df$ADJUNCT1[df$PTCAREER=="Yes"] <- "Professional Adjuncts"
df$ADJUNCT1[df$PTCAREER=="No" & df$PTTEACH>1 ] <- "Itinerant Academic"
df$ADJUNCT1[df$PTCAREER=="No" & df$PTTEACH==1 ] <- "Single Inst Adjunct"

df$OFFSPRING<-as.factor(as.numeric(df$NCHILD1)-1+as.numeric(df$NCHILD2)-1); levels(df$OFFSPRING)=c("0","1","2","3",rep("4 or More",5))
df$PARENT<-df$OFFSPRING; levels(df$PARENT) <- c("No","Yes","Yes","Yes","Yes"); levels(df$PARENT)<-c("Non-Parent","Parent")
df$MINORITY<-df$RACE; levels(df$MINORITY) <- c("White", rep("Non-white", 4))

HPWvars<- df %>% select(starts_with("HPW")) %>% names()
for (i in HPWvars){
  df[,i]<-factor(df[,i], levels = c("None","1-4","5-8","9-12","13-16","17-20","21-34","35-44","45+"))
} 
hpws<-c(0,2.5,6.5,10.5,14.5,18.5,27.5, 39.5,50) # These are ~ the average value within each interval
for (i in HPWvars){
  tempvar<-as.numeric(df[,i])
  name <- paste(i, "Q", sep = "")
  for(ii in 1:length(hpws)){
    tempvar[tempvar==ii]<-hpws[ii]
  } # Assumes midpoint of age interval
  df[name]<-assign(name, tempvar)
}

# Salary and Income
df$SALARY=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # returns the max of the two
# Some R's did not tally their salary sources correctly
df$SSVALID<- factor(rep(NA, nrow(df)), levels=c("Valid Salary","Invalid Salary"))   
df$SSVALID[df$SALARYSOURCE01+df$SALARYSOURCE02+df$SALARYSOURCE03+df$SALARYSOURCE04>90 & df$SALARYSOURCE01+df$SALARYSOURCE02+df$SALARYSOURCE03+df$SALARYSOURCE04<110] <- "Valid Salary"
df$SSVALID[df$SALARYSOURCE01+df$SALARYSOURCE02+df$SALARYSOURCE03+df$SALARYSOURCE04<90 | df$SALARYSOURCE01+df$SALARYSOURCE02+df$SALARYSOURCE03+df$SALARYSOURCE04>110] <- "Invalid Salary"
# 68 people added salary sources very poorly.
df$SALARY[df$SSVALID=="Invalid Salary"]<-NA
df <- df %>% mutate(INCOME<-SALARY/(SALARYSOURCE01/100))
df$INCOME[df$INCOME=="Inf"]<-0
df$INCOMEF=cut(df$INCOME,breaks=c(0,35000,64999,94999,1000000))
df$LINCOME=log(abs(df$INCOME));df$LINCOME[df$LINCOME=="-Inf"]=0

df$PROFDEVOPP<- factor(rep(NA, nrow(df)), levels=c("Prof. Dev. Available","No Professional Development")) 
df$PROFDEVOPP[df$PROFDEV01 %in% c("Not available","Not eligible") & df$PROFDEV02 %in% c("Not available","Not eligible") & df$PROFDEV03 %in% c("Not available","Not eligible") & df$PROFDEV04 %in% c("Not available","Not eligible") & df$PROFDEV05 %in% c("Not available","Not eligible") & df$PROFDEV06 %in% c("Not available","Not eligible") & df$PROFDEV07 %in% c("Not available","Not eligible")] <- "No Professional Development"
df$PROFDEVOPP[df$PROFDEV01 %in% c("No","Yes") | df$PROFDEV02 %in% c("No","Yes") | df$PROFDEV03 %in% c("No","Yes") | df$PROFDEV04 %in% c("No","Yes") | df$PROFDEV05 %in% c("No","Yes") | df$PROFDEV06 %in% c("No","Yes") | df$PROFDEV07 %in% c("No","Yes")] <- "Prof. Dev. Available"

df$PROFDEVPART<- factor(rep(NA, nrow(df)), levels=c("Prof. Dev. Participant","Not a Prof. Dev. Participant")) 
df$PROFDEVPART[df$PROFDEV01 %in% c("No","Not available","Not eligible") & df$PROFDEV02 %in% c("No","Not available","Not eligible") & df$PROFDEV03 %in% c("No","Not available","Not eligible") &df$PROFDEV04 %in% c("No","Not available","Not eligible") &df$PROFDEV05 %in% c("No","Not available","Not eligible") &df$PROFDEV06 %in% c("No","Not available","Not eligible") &df$PROFDEV07 %in% c("No","Not available","Not eligible")] <- "Not a Prof. Dev. Participant"
df$PROFDEVPART[df$PROFDEV01 %in% c("Yes") | df$PROFDEV02 %in% c("Yes") | df$PROFDEV03 %in% c("Yes") | df$PROFDEV04 %in% c("Yes") | df$PROFDEV05 %in% c("Yes") | df$PROFDEV06 %in% c("Yes") | df$PROFDEV07 %in% c("Yes")] <- "Prof. Dev. Participant"






