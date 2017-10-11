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