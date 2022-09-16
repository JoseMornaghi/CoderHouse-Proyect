
library(readr)
stroke_imputed <- read_csv("C:/Users/Jose/Desktop/CoderHouse/Trabajo Final/3 - Analisis Bivariado del proyecto/stroke_imputed.csv")
View(stroke_imputed)
head(stroke_imputed)
str(stroke_imputed)

glmS1 <- glm(stroke_imputed$stroke~ age+hypertension+heart_disease+avg_glucose_level+bmi, family=binomial(link="logit"), data = stroke_imputed)
glmS1
summary(glmS1)
glmS2 <- glm(stroke_imputed$stroke~ age+hypertension+heart_disease+avg_glucose_level, family=binomial(link="logit"), data = stroke_imputed)
glmS2
summary(glmS2)

#el mejor modelo es el n2 porque tiene un grado mayor de libertad(es decir estima un parametro menos), tiene
#una residual variance identica pero un AIC menor

newdata1 <- data.frame(age=67,hypertension=0,heart_disease=1,avg_glucose_level=228.69)
newdata2 <- data.frame(age=61,hypertension=0,heart_disease=0,avg_glucose_level=202.61)
predict.glm(glmS2, newdata1, type = "response")
predict.glm(glmS2, newdata2, type = "response")


response <- predict.glm(glmS2, stroke_imputed, type = "response")
dfresponse <- data.frame(response)
