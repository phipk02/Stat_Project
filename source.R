# --------------------------------------------
library(dplyr)

library(ggplot2)
library(ggrepel)
library(tidyverse)

setwd("Final/")

library(readr)

data <- read.csv("healthcare-dataset-stroke-data.csv", na = "N/A")
data <- na.omit(data)


data <- data[!(data$gender == "Other"),]

attach(data)

data $ hypertension <- factor(data $ hypertension)
levels(data $ hypertension) <- c("No", "Yes")

data $ heart_disease <- factor(data $ heart_disease)
levels(data $ heart_disease) <- c("No", "Yes")

data $ stroke <- factor(data $ stroke)
levels(data $ stroke) <- c("No", "Yes")

data $ gender <- factor(data $ gender)

data $ work_type <- factor(data $ work_type)

data $ Residence_type <- factor(data $ Residence_type)

data $ smoking_status <- factor(data $ smoking_status)

summary(data)

# --------------------------------------------

df.data <- data.frame(id = 1:nrow(data))

# --------------------------------------------

## Stroke distribution

df.data$stroke <- data$stroke

df.stroke <- df.data %>% group_by(stroke) %>%
             summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.stroke %>% 
  mutate(csum = rev(cumsum(rev(count))), 
         pos = count/2 + lead(csum, 1),
         pos = if_else(is.na(pos), count/2, pos))

ggplot(df.stroke, aes(x = "" , y = count, fill = fct_inorder(stroke))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Stroke")) +
  theme_void()
  
ggsave(filename = "stroke.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Gender distribution

df.data $ gender <- data$gender

df.gender <- df.data %>% group_by(gender) %>%
             summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df.gender

df2 <- df.gender %>% 
  mutate(csum = rev(cumsum(rev(df.gender$count))), 
         pos = df.gender$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.gender$count/2, pos))

ggplot(df.gender, aes(x = "" , y = count, fill = fct_inorder(gender))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Gender")) +
  theme_void()

ggsave(filename = "gender.png", device = "png",width = 19,height = 10,units="cm")

## Gender and stroke

## ------------------------------- Male

df.GenderVsStroke <- df.data %>% group_by(gender, stroke) %>%
                   summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df.MaleVsStroke <- df.GenderVsStroke[3:4,]

df2 <- df.MaleVsStroke %>% 
  mutate(csum = rev(cumsum(rev(df.MaleVsStroke$count))), 
         pos = df.MaleVsStroke$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.MaleVsStroke$count/2, pos))

ggplot(df.MaleVsStroke, aes(x = "" , y = count, fill = fct_inorder(stroke))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Stroke")) +
  ggtitle("Male") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "male.png", device = "png",width = 19,height = 10,units="cm")

## ------------------------------- Female

df.FemaleVsStroke <- df.GenderVsStroke[1:2,]

df2 <- df.FemaleVsStroke %>% 
  mutate(csum = rev(cumsum(rev(df.FemaleVsStroke$count))), 
         pos = df.FemaleVsStroke$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.FemaleVsStroke$count/2, pos))

ggplot(df.FemaleVsStroke, aes(x = "" , y = count, fill = fct_inorder(stroke))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Stroke")) +
  ggtitle("Female") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "female.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Age

df.age = 1:length(data$age)

for (i in 1:length(data$age)) {
  if (age[i] < 40) {
    df.age[i] = "Tre"
  }
  else if (age[i] >= 40 && age[i] <= 60) {
    df.age[i] = "Trung nien"
  }
  else {
    df.age[i] = "Gia"
  }
}

table(df.age)

## -------------------------------- Barchart
df.data $ age <- df.age

df.AgeVsStroke <- df.data %>% group_by(age, stroke) %>%
                  summarise(count = n())# %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df.AgeVsStroke

ggplot(df.AgeVsStroke, aes(x = age, y = count, fill = stroke)) +
  geom_bar(stat = "identity", color = 1) +
  xlab("Age") + ylab("Count") +
  scale_fill_discrete(name = "Stroke", labels = c("No", "Yes")) +
  theme_minimal()

ggsave(filename = "age_bar.png", device = "png",width = 19,height = 10,units="cm")

## --------------------------------------- Boxplot

df.data$oAge <- data$age

ggplot(df.data, aes(x = oAge, y = stroke, fill = stroke)) +
  geom_boxplot() +
  xlab("Age") +
  ylab("Target") +
  theme(legend.position = "none")

ggsave(filename = "age_boxplot.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Hypertension

df.data $ hypertension <- data$hypertension

df.hypertension <- df.data %>% group_by(hypertension) %>%
  summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.hypertension %>% 
  mutate(csum = rev(cumsum(rev(df.hypertension$count))), 
         pos = df.hypertension$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.hypertension$count/2, pos))


ggplot(df.hypertension, aes(x = "" , y = count, fill = fct_inorder(hypertension))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "hypertension")) +
  theme_void()

ggsave(filename = "hypertension_pie.png", device = "png",width = 19,height = 10,units="cm")

## -------------------------------------- VS stroke

df.HypertensionVsStroke <- df.data %>% group_by(hypertension, stroke) %>%
  summarise(count = n())

df.HypertensionVsStroke

ggplot(df.HypertensionVsStroke, aes(x = hypertension, y = count, fill = stroke)) +
  geom_col(position = "dodge", color = 1) +
  coord_flip() + xlab("Hypertension") + ylab("Frequency") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme_minimal()

ggsave(filename = "hypertension_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## HeartDisease

df.data $ heart_disease <- data$heart_disease

df.heart_disease <- df.data %>% group_by(heart_disease) %>%
  summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.heart_disease %>% 
  mutate(csum = rev(cumsum(rev(df.heart_disease$count))), 
         pos = df.heart_disease$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.heart_disease$count/2, pos))


ggplot(df.heart_disease, aes(x = "" , y = count, fill = fct_inorder(heart_disease))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "HeartDisease")) +
  theme_void()

ggsave(filename = "heart_disease_pie.png", device = "png",width = 19,height = 10,units="cm")

## -------------------------------------- VS stroke

df.HeartDiseaseVsStroke <- df.data %>% group_by(heart_disease, stroke) %>%
  summarise(count = n())

df.HeartDiseaseVsStroke

ggplot(df.HeartDiseaseVsStroke, aes(x = heart_disease, y = count, fill = stroke)) +
  geom_col(position = "dodge", color = 1) +
  coord_flip() + xlab("HeartDisease") + ylab("Frequency") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme_minimal()

ggsave(filename = "heart_disease_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## married

df.data $ married <- data$ever_married

df.married <- df.data %>% group_by(married) %>%
  summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.married %>% 
  mutate(csum = rev(cumsum(rev(df.married$count))), 
         pos = df.married$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.married$count/2, pos))


ggplot(df.married, aes(x = "" , y = count, fill = fct_inorder(married))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "married")) +
  theme_void()

ggsave(filename = "ever_married_pie.png", device = "png",width = 19,height = 10,units="cm")

## -------------------------------------- VS stroke

df.marriedVsStroke <- df.data %>% group_by(married, stroke) %>%
  summarise(count = n())

df.marriedVsStroke

ggplot(df.marriedVsStroke, aes(x = married, y = count, fill = stroke)) +
  geom_col(position = "dodge", color = 1) +
  coord_flip() + xlab("Ever married") + ylab("Frequency") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme_minimal()

ggsave(filename = "ever_married_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Work type

df.data$work_type <- data$work_type

df.work_type <- df.data %>% group_by(work_type) %>%
  summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.work_type %>% 
  mutate(csum = rev(cumsum(rev(df.work_type$count))), 
         pos = df.work_type$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.work_type$count/2, pos))


ggplot(df.work_type, aes(x = "" , y = count, fill = fct_inorder(work_type))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Work type")) +
  theme_void()

ggsave(filename = "work_type_pie.png", device = "png",width = 19,height = 10,units="cm")

## --------------------------------------- VS Stroke

df.WorktypeVsStroke <- df.data %>% group_by(work_type, stroke) %>% summarise(count = n())

df.WorktypeVsStroke

ggplot(df.WorktypeVsStroke, aes(x = work_type, y = count, fill = stroke)) +
  geom_bar(stat = "identity", color = 1) +
  xlab("Work type") + ylab("Count") +
  scale_fill_discrete(name = "Stroke", labels = c("No", "Yes")) +
  theme_minimal()

ggsave(filename = "work_type_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Residence type

df.data $ Residence_type <- data$Residence_type

df.residence <- df.data %>% group_by(Residence_type) %>%
  summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.residence %>% 
  mutate(csum = rev(cumsum(rev(df.residence$count))), 
         pos = df.residence$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.residence$count/2, pos))


ggplot(df.residence, aes(x = "" , y = count, fill = fct_inorder(Residence_type))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Residence type")) +
  theme_void()

ggsave(filename = "residence_type_pie.png", device = "png",width = 19,height = 10,units="cm")

## -------------------------------------- VS stroke

df.ResidenceVsStroke <- df.data %>% group_by(Residence_type, stroke) %>%
  summarise(count = n())

df.ResidenceVsStroke

ggplot(df.ResidenceVsStroke, aes(x = Residence_type, y = count, fill = stroke)) +
  geom_col(position = "dodge", color = 1) +
  coord_flip() + xlab("Residence type") + ylab("Frequency") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme_minimal()

ggsave(filename = "residence_type_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Smoking status

df.data$smoking_status <- data$smoking_status

df.smoking_status <- df.data %>% group_by(smoking_status) %>%
  summarise(count = n()) %>% mutate(count = round(count * 100 / sum(count), digits = 2))

df2 <- df.smoking_status %>% 
  mutate(csum = rev(cumsum(rev(df.smoking_status$count))), 
         pos = df.smoking_status$count / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), df.smoking_status$count/2, pos))


ggplot(df.smoking_status, aes(x = "" , y = count, fill = fct_inorder(smoking_status))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(count, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Smoking status")) +
  theme_void()

ggsave(filename = "smoking_status_pie.png", device = "png",width = 19,height = 10,units="cm")

## --------------------------------------- VS Stroke

df.SmokingVsStroke <- df.data %>% group_by(smoking_status, stroke) %>% summarise(count = n())

df.SmokingVsStroke

ggplot(df.SmokingVsStroke, aes(x = smoking_status, y = count, fill = stroke)) +
  geom_bar(stat = "identity", color = 1) +
  xlab("Smoking status") + ylab("Count") +
  scale_fill_discrete(name = "Stroke", labels = c("No", "Yes")) +
  theme_minimal()

ggsave(filename = "smoking_status_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Glucose

## boxplot

df.data$avg_glucose_level <- data$avg_glucose_level

ggplot(df.data, aes(x = avg_glucose_level, y = stroke, fill = stroke)) +
  geom_boxplot() +
  xlab("Average glucose level") +
  ylab("Target") +
  theme(legend.position = "none")

ggsave(filename = "glucose_boxplot.png", device = "png",width = 19,height = 10,units="cm")

## histogram

ggplot(df.data, aes(x = avg_glucose_level, fill = stroke)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = .2) +
  facet_grid(stroke ~ .) +
  xlab("Average glucose level") +
  ylab("Density") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme(legend.position = "none")

ggsave(filename = "glucose_histogram.png", device = "png",width = 19,height = 10,units="cm")

## Dinh tinh hoa

df.glucose <- 1:length(data$avg_glucose_level)

for (i in 1:length(data$avg_glucose_level)) {
  if (avg_glucose_level[i] < 140) {
    df.glucose[i] = "Binh thuong"
  }
  else if (avg_glucose_level[i] >= 140 && avg_glucose_level[i] <= 200) {
    df.glucose[i] = "Cao"
  }
  else df.glucose[i] = "Qua cao"
}

## Bar chart

df.data$glucose <- df.glucose

df.GluVsStroke <- df.data %>% group_by(glucose, stroke) %>%
  summarise(count = n())

df.GluVsStroke

ggplot(df.GluVsStroke, aes(x = glucose, y = count, fill = stroke)) +
  geom_col(position = "dodge", color = 1) +
  xlab("Glucose") + ylab("Frequency") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme_minimal()

ggsave(filename = "glucose_bar.png", device = "png",width = 19,height = 10,units="cm")


# --------------------------------------------

## bmi

## boxplot

df.data$bmi <- data$bmi

ggplot(df.data, aes(x = bmi, y = stroke, fill = stroke)) +
  geom_boxplot() +
  xlab("BMI") +
  ylab("Target") +
  theme(legend.position = "none")

ggsave(filename = "bmi_boxplot.png", device = "png",width = 19,height = 10,units="cm")

## histogram

ggplot(df.data, aes(x = bmi, fill = stroke)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = .2) +
  facet_grid(stroke ~ .) +
  xlab("BMI") +
  ylab("Density") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme(legend.position = "none")

ggsave(filename = "bmi_histogram.png", device = "png",width = 19,height = 10,units="cm")

## Dinh tinh hoa

df.bmi <- 1:length(data$bmi)

for (i in 1:length(data$bmi)) {
  if (bmi[i] < 18.5) {
    df.bmi[i] = "Gay"
  }
  else if (bmi[i] >= 18.5 && bmi[i] <= 24.99) {
    df.bmi[i] = "Binh thuong"
  }
  else if (bmi[i] >= 25 && bmi[i] <= 30) {
    df.bmi[i] = "Thua can"
  }
  else df.bmi[i] = "Beo phi"
}

## Bar chart

df.data$df.bmi <- df.bmi

df.BmiVsStroke <- df.data %>% group_by(df.bmi, stroke) %>%
  summarise(count = n())

df.BmiVsStroke

ggplot(df.BmiVsStroke, aes(x = df.bmi, y = count, fill = stroke)) +
  geom_col(position = "dodge", color = 1) +
  xlab("BMI") + ylab("Frequency") +
  scale_fill_discrete(name = "Stroke", labels = c("No","Yes")) +
  theme_minimal()

ggsave(filename = "bmi_bar.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

t.test(age~df.data$stroke, alternative = "less")

t.test(bmi~df.data$stroke, alternative = "less")

t.test(avg_glucose_level~df.data$stroke, alternative = "less")

heart_diseaseVSstroke <- table(df.data$heart_disease, df.data$stroke)

heart_diseaseVSstroke

prop.test(heart_diseaseVSstroke, correct = FALSE, alternative = "greater")

hypertensionVSstroke <- table(df.data$hypertension, df.data$stroke)

hypertensionVSstroke

prop.test(hypertensionVSstroke, correct = FALSE, alternative = "greater")

# --------------------------------------------

## age vs glucose

ggplot(data, aes(age, avg_glucose_level, color = age, size = stroke)) +
  geom_point(alpha = 0.7) +
  xlab("Tuoi") +
  ylab("Duong huyet trung binh")

ggsave(filename = "agevsglu.png", device = "png",width = 19,height = 10,units="cm")

## glucose vs bmi

ggplot(data, aes(avg_glucose_level, bmi, color = avg_glucose_level, size = stroke)) +
  geom_point(alpha = 0.7) +
  ylab("BMI") +
  xlab("Duong huyet trung binh")

ggsave(filename = "bmivsglu.png", device = "png",width = 19,height = 10,units="cm")

# --------------------------------------------

## Model glucose~age

model1 <- lm(avg_glucose_level~age)
model1

ggplot(data, aes(age, avg_glucose_level, color = age)) +
  geom_point(alpha = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Tuoi") +
  ylab("Duong huyet trung binh")

ggsave(filename = "model1.png", device = "png",width = 19,height = 10,units="cm")

summary(model1)

confint(model1)

age_test <- 40

predict(model1, data.frame(age = age_test), interval = 'prediction')
predict(model1, data.frame(age = age_test), interval = 'confidence')

x <- seq(min(age), max(age), length = 4908)
predint<- predict(model1,data.frame(age=x),interval="prediction")
confint<- predict(model1,data.frame(age=x),interval="confidence")
plot(avg_glucose_level~age,xlab="Tuoi",ylab="Duong huyet trung binh")
abline(model1)
rc <- cbind(predint,data.frame(age=x))
rb <- cbind(confint,data.frame(age=x))
lines(rc$fit ~ rc$age)
lines(rc$lwr ~ rc$age, col = 2)
lines(rc$upr ~ rc$age, col = 2)
lines(rb$lwr ~ rb$age, col = 4)
lines(rb$upr ~ rb$age, col = 4)
legend("bottomright",lty=c(2,3),lwd=c(2,1),c("confidence","prediction"),col=c("red","blue"))

# --------------------------------------------

model2 <- lm(avg_glucose_level~bmi)
model2

ggplot(data, aes(bmi, avg_glucose_level, color = avg_glucose_level)) +
  geom_point(alpha = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Bmi") +
  ylab("Duong huyet trung binh")

ggsave(filename = "model2.png", device = "png",width = 19,height = 10,units="cm")

summary(model2)

confint(model2)

bmi_test <- 22

predict(model2, data.frame(bmi = bmi_test), interval = 'prediction')
predict(model2, data.frame(bmi = bmi_test), interval = 'confidence')

x <- seq(min(bmi), max(bmi), length = 4908)
predint<- predict(model2,data.frame(bmi=x),interval="prediction")
confint<- predict(model2,data.frame(bmi=x),interval="confidence")
plot(avg_glucose_level~bmi,xlab="Bmi",ylab="Duong huyet trung binh")
abline(model2)
rc <- cbind(predint,data.frame(bmi=x))
rb <- cbind(confint,data.frame(bmi=x))
lines(rc$fit ~ rc$bmi)
lines(rc$lwr ~ rc$bmi, col = 2)
lines(rc$upr ~ rc$bmi, col = 2)
lines(rb$lwr ~ rb$bmi, col = 4)
lines(rb$upr ~ rb$bmi, col = 4)
legend("bottomright",lty=c(2,3),lwd=c(2,1),c("confidence","prediction"),col=c("red","blue"))

# --------------------------------------------

## Model glucose ~ age + bmi

pairs(avg_glucose_level ~ age + bmi)

ggsave(filename = "pairs.png", device = "png",width = 19,height = 10,units="cm")

model3 <- lm(avg_glucose_level ~ age + bmi)
model3

confint(model3)

summary(model3)

anova(model1, model2, model3)

model4 <- lm(bmi ~ age + avg_glucose_level)
model4

summary(model4)

model5 <- lm(bmi ~ age)
summary(model5)

model6 <- lm(bmi ~ avg_glucose_level)
summary(model6)

# --------------------------------------------

chisq.test(df.data $ married, df.data $ heart_disease, correct = FALSE)

# --------------------------------------------
