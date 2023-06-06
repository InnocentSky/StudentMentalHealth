# So i learnt aboutt an exciting feature about R programming on how to use piping from
#your dataset to your ggplot graphics, so i decided to a pratice with this "Student Menta Health
# dataset.

# By the way Shoutout to Greg Martin for the tutorials. 


library(tidyverse)


StudentMentahealth <- read.csv("StudentMentahealth.csv")

View(StudentMentahealth)
glimpse(StudentMentahealth)
str(StudentMentahealth)
unique(StudentMentahealth$What_is_your_CGPA)
names(StudentMentahealth)

class(StudentMentahealth$What_is_your_CGPA) #Change CGPA variable to numeric variable
StudentMentahealth$What_is_your_CGPA <- as.factor(StudentMentahealth$What_is_your_CGPA)
View(StudentMentahealth)


StudentMentalHealth <- (StudentMentahealth)
glimpse(StudentMentalHealth)

StudentMentalHealth %>% 
  filter(Course_of_Study %in% c("Engineering","BIT","BCS","Laws","Kirkhs",
                                     "Pendidikan Islam","Biomedical science","koe") &
                        Current_year_of_Study >  "Year 2") %>% 
  select(Course_of_Study,Current_year_of_Study, 
         Gender,Age,What_is_your_CGPA, Marital_status) %>% 
  mutate(Marital_status = recode(Marital_status, 
                                 "Yes" = "Married",
                                 "No" = "Single")) %>% 
  arrange(-Age)



StudentMentalHealth %>% 
  filter(Course_of_Study %in% c("Engineering","BIT","BCS","Laws","Kirkhs",
                                "Pendidikan Islam","Biomedical science","koe") &
           Current_year_of_Study >  "Year 2") %>% 
  select(Course_of_Study,Current_year_of_Study, 
         Gender,Age,What_is_your_CGPA, Marital_status) %>% 
  mutate(Marital_status = recode(Marital_status, 
                                 "Yes" = "Married",
                                 "No" = "Single")) %>% 
  arrange(-Age) %>% 
  ggplot(aes(fct_infreq(Course_of_Study)))+
  geom_bar(fill = "#97B3C6")+
  coord_flip()+
  theme_bw()+
  labs(title = "Student Vs Course")
ggsave("StudentMentahealth plot.png",
       width = 10,
       height = 7,
       units = "cm",
       dpi = 300)



StudentMentalHealth %>% 
  filter(Course_of_Study %in% c("Engineering","BIT","BCS","Laws","Kirkhs",
                                "Pendidikan Islam","Biomedical science","koe") &
           Current_year_of_Study >  "Year 2") %>% 
  select(Course_of_Study,Current_year_of_Study, 
         Gender,Age,What_is_your_CGPA, Marital_status) %>% 
  mutate(Marital_status = recode(Marital_status, 
                                 "Yes" = "Married",
                                 "No" = "Single")) %>% 
  arrange(-Age) %>%  
  ggplot(aes(Course_of_Study,What_is_your_CGPA))+
  geom_point(color = "brown", size = 5)+
  geom_line()+
  theme_bw()+
  labs(title = "Student Course Vs CGPA")
  ggsave("StudentMentahealth plot.png",
         width = 10,
         height = 7,
         units = "cm",
         dpi = 300)