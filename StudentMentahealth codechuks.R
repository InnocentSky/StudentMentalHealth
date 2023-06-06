# So i learnt about an exciting feature about R programming on how to use piping from
#your dataset to your ggplot graphics, so i decided to a pratice with this "Student Menta Health
# dataset.

# By the way Shoutout to Greg Martin for the tutorials. 

 **Disclaimer:** The whole point of this work is not really about the Student mental Health, but rather it is basically about practicing piping in R programming from the dataset to ggplot visualization.

### About Dataset

* **A STATISTICAL RESEARCH ON THE EFFECTS OF MENTAL HEALTH ON STUDENTS’ CGPA** dataset
This Data set was collected by a survey conducted by Google forms from University student in order to examine their current academic situation and mental health.

* For more details about the Student Mental Health data set see [Link](https://www.kaggle.com/datasets/shariful07/student-mental-health).

Loading package

library(tidyverse)

Loading Dataset
StudentMentahealth <- read.csv("StudentMentahealth.csv")


# Explore and manipulate the dataset
View(StudentMentahealth) # To check the first six columns of the dataset
glimpse(StudentMentahealth) # To check variable string
str(StudentMentahealth) # To check the structure of the dataset
unique(StudentMentahealth$What_is_your_CGPA) # To check all the unique values on the dataset
names(StudentMentahealth) # To check columns heads

# to view the structure, observation and variables of the dataset
class(StudentMentahealth$What_is_your_CGPA) # Change CGPA variable to numeric variable
StudentMentahealth$What_is_your_CGPA <- as.factor(StudentMentahealth$What_is_your_CGPA)
View(StudentMentahealth)

# To convert the dataframe
StudentMentalHealth <- (StudentMentahealth)
glimpse(StudentMentalHealth)


# Reviewing selected data to work with and change marital status from "Yes" to Married and "No" to Single
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


# Ploting a bar chat to check the most courses student offer
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


# * **Ploting a point graph too check the student CGPA from selected courses from 3year and above student** 
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

 #* **Thank you for your time here**

#### By the way Shoutout to Greg Martin for the tutorials. 
