# Libraries
library(tidyverse)


# Importing the data and limiting it to relevant columns
df_orig <- read.csv("graduate_survey.csv", header=TRUE, sep=",")
df <- df_orig |>
  select(ResponseId, Campus, StudyField, Branch, Role, EduLevel,  ProgLang, Databases, Platform, WebFramework, Industry, AISearch, AITool, Employment)

#-------------------------------------------------------------------------------------------------------------------------------
# Question 1
#-------------------------------------------------------------------------------------------------------------------------------

# Data exploration and cleaning/tidying

head(df)
summary(df)
str(df)

unique(df$Campus)
df <- df |>
  mutate(Campus = case_when(
    Campus == "Mbombela Campus" ~ "Nelspruit Campus",
    Campus == "Nelson Mandela Bay Campus" ~ "Port Elizabeth Campus",
    Campus == "Umhlanga Campus" ~ "Durban Campus",
    TRUE ~ Campus
  ))
unique(df$Campus)


unique(df$StudyField)


unique(df$Branch)
table(df$Branch)
df <- df |>
  mutate(Branch = ifelse(Branch == "15", NA, Branch))
unique(df$Branch)


unique(df$Role)
table(df$Role)
df <- df |>
  mutate(Role = ifelse(Role %in% c("", "Clojure;Erlang;Go;Groovy;Haskell;HTML/CSS;Java;JavaScript;Kotlin;Python;Scala;SQL;TypeScript"), NA, Role))
df <- df |>
  mutate(Role = case_when(
    Role == "Other (please specify):" ~ "Other",
    TRUE ~ Role
  ))
unique(df$Role)
    

unique(df$EduLevel)   
df <- df |>
  mutate(EduLevel = case_when(
    EduLevel == "Bachelorâ€™s degree (B.A., B.S., B.Eng., etc.)" ~ "Bachelors degree (B.A., B.S., B.Eng., etc.)",
    EduLevel == "Masterâ€™s degree (M.A., M.S., M.Eng., MBA, etc.)" ~ "Masters degree (M.A., M.S., M.Eng., MBA, etc.)",
    TRUE ~ EduLevel
  ))
unique(df$EduLevel)


unique(df$ProgLang)
unique(df$Databases)
unique(df$Platform)
unique(df$WebFramework)


unique(df$Industry)


unique(df$AISearch)
unique(df$AITool)
unique(df$Employment)

# Removing null values
colSums(is.na(df))
df_clean <- na.omit(df)
colSums(is.na(df_clean))


# Subsetting data
sort(table(df$Campus), decreasing = TRUE)

top_campuses <- df_clean[df_clean$Campus %in% c("Port Elizabeth Campus", " Durban Campus", "Nelspruit Campus", "Midrand Campus", "Pretoria Campus"),]

# Removing null values
top_campuses_no_null <- top_campuses |>
  filter(ProgLang != "" & Databases != "" & Platform != "" & WebFramework != "" & AISearch != "")


#-------------------------------------------------------------------------------------------------------------------------------
# Question 2
#-------------------------------------------------------------------------------------------------------------------------------
#-----------
# 2.1
#-----------
# Top tools used by graduates in the tech space

# Programming Languages Plot
df_split1 <- top_campuses_no_null |>
  separate_rows(ProgLang, sep = ";")

plot1 <- ggplot(df_split1, aes(x= reorder(ProgLang, table(ProgLang)[ProgLang])))+
  geom_bar(fill = "royalblue") +
  coord_flip() +
  labs(y = "Count", x = "Programming Languages", title = "Programming Language Usage After Graduating") +
  theme_light()
plot1

table(top_campuses$ProgLang == "")

# Databases Plot
df_split2 <- top_campuses_no_null |>
  separate_rows(Databases, sep = ";")

plot2 <- ggplot(df_split2, aes(x= reorder(Databases, table(Databases)[Databases])))+
  geom_bar(fill = "royalblue") +
  coord_flip() +
  labs(y = "Count", x = "Databases", title = "Database Usage After Graduating") +
  theme_light()
plot2

table(top_campuses$Databases == "")

# Platform Plot
df_split3 <- top_campuses_no_null |>
  separate_rows(Platform, sep = ";")

plot3 <- ggplot(df_split3, aes(x= reorder(Platform, table(Platform)[Platform])))+
  geom_bar(fill = "royalblue") +
  coord_flip() +
  labs(y = "Count", x = "Platform", title = "Platform Usage After Graduating") +
  theme_light()
plot3

table(top_campuses$Platform == "")

# Web Framework Plot
df_split4 <- top_campuses_no_null |>
  separate_rows(WebFramework, sep = ";")

plot4 <- ggplot(df_split4, aes(x= reorder(WebFramework, table(WebFramework)[WebFramework])))+
  geom_bar(fill = "royalblue") +
  coord_flip() +
  labs(y = "Count", x = "Web Framework", title = "Web Framework Usage After Graduating") +
  theme_light()
plot4

table(top_campuses$WebFramework == "")

# AI Search Plot
df_split5 <- top_campuses_no_null |>
  separate_rows(AISearch, sep = ";")

plot5 <- ggplot(df_split5, aes(x= reorder(AISearch, table(AISearch)[AISearch])))+
  geom_bar(fill = "royalblue") +
  coord_flip() +
  labs(y = "Count", x = "AI Search Tool", title = "AI Search Tool Usage After Graduating") +
  theme_light()
plot5

table(top_campuses$AISearch == "")

# AI Tool Plot
df_split6 <- top_campuses_no_null |>
  separate_rows(AITool, sep = ";")

plot6 <- ggplot(df_split6, aes(x= reorder(AITool, table(AITool)[AITool])))+
  geom_bar(fill = "royalblue") +
  coord_flip() +
  labs(y = "Count", x = "AI Tool", title = "AI Tool Usage After Graduating") +
  theme_light()
plot6

table(top_campuses$AITool == "")

#-----------
# 2.2
#-----------
# Popular industries

df_split7 <- top_campuses_no_null |>
  separate_rows(Industry, sep = ";")

plot7 <- ggplot(df_split7, aes(x= reorder(Industry, table(Industry)[Industry]), fill = StudyField))+
  geom_bar(position = "dodge") +
  #coord_flip() +
  labs(y = "Count", x = "Industry", title = "Industry Entered by Study Field After Graduating") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
plot7

table(top_campuses$Industry == "")

#-----------
# 2.3
#-----------
# Top jobs 

plot8 <- ggplot(top_campuses_no_null, aes(x= reorder(Role, table(Role)[Role]), fill = StudyField))+
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(y = "Count", x = "Job Role", title = "Job Roles Entered by Study Field After Graduating") +
  theme_light() 
plot8

#-----------
# 2.4
#-----------
# Employment rate

df_split9 <- top_campuses_no_null |>
  separate_rows(Employment, sep = ";")

unique(df_split9$Employment)

df_emp <- df_split9 |>
  mutate(EmploymenStatus = case_when(
    Employment %in% c("Employed, full-time", "Independent contractor, freelancer, or self-employed", "Employed, part-time") ~ "Employed",
    Employment %in% c("Not employed, but looking for work", "Student, part-time", "Student, full-time", "Not employed, and not looking for work", "Retired") ~ "Unemployed",
    TRUE ~ Employment  # Default for values not listed
  ))

plot9 <- ggplot(df_emp, aes(x= reorder(EmploymenStatus, table(EmploymenStatus)[EmploymenStatus])))+
  geom_bar(aes(y = after_stat(prop), group = 1), fill = "royalblue") +
  coord_flip() +
  labs(y = "Rate", x = "Employment Type", title = "Employment Type After Graduating") +
  theme_light()
plot9
