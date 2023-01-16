library(ggplot2)
library(dplyr)
library(Hmisc)
library(cowplot)
library(WVplots)
library(MASS)
set.seed(1234)
Data<-read.csv("insurance (1).csv")
sample(Data,5)
x <- ggplot(Data, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()
y <- ggplot(Data, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()
p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
#########################
x <- ggplot(Data, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

y <- ggplot(Data, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#########################
x <- ggplot(Data, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

y <- ggplot(Data, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

###############################
n_train <- round(0.8 * nrow(Data))
train_indices <- sample(1:nrow(Data), n_train)
Data_train <- Data[train_indices, ]
Data_test <- Data[-train_indices, ]

formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")
#train and test model
model_0 <- lm(formula_0, data = Data_train)
summary(model_0)

r_sq_0 <- summary(model_0)$r.squared

#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

#new model
formula_1 <- as.formula("charges ~ age + bmi + children + smoker + region")

model_1 <- lm(formula_1, data = Data_train)
summary(model_1)
r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)

residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))


Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")


Data_test$residuals <- Data_test$charges - Data_test$prediction

ggplot(data = Data_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")
