df<- df %>% 
  mutate(SUBCLASS = recode(DEPTA, `Agriculture or Forestry (General Area 1)`="Sciences",
                         `Biological Sciences (General Area 5)`="Sciences",
                         `Business (General Area 6)`="Soft/Applied",
                         `Education (General Area 10 and Specific Discipline 2102)`="Soft/Applied",
                         `Engineering (General Area 11)`="Sciences",
                         `English (General Area 12)`="Humanities/Arts",
                         `Fine Arts (General Area 2,4,22)`="Humanities/Arts",
                         `Health-related (General Area 15)`="Health Sciences",
                         `History or Political Science (Specific Discipline 3007,3009)`="Social Sciences",
                         `Humanities (General Area 14,24)`="Humanities/Arts",
                         `Mathematics or Statistics (General Area 18)`="Sciences",
                         `Other Non-technical (General Area 7,9,13,16,17,20,23,29,31,32 and Specific Discipline 2101,2103)`="Soft/Applied",
                         `Other Technical (General Area 8,19,28)`="Sciences",
                         `Physical Sciences (General Area 25)`="Sciences",
                         `Social Sciences (General Area 3,26,27 and Specific Discipline 3001,3002,3003,3004,3005,3006,3008,3010,3011,3012)`="Social Sciences"))