# Salary and Income
df$SALARYALL=pmax(df$SALARY, df$PTSALARY, na.rm = TRUE) # returns the max of the two
df$AGE<-2010-df$BIRTHYR

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