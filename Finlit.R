#Необходимые библиотеки 
# install.packages("ordinal")
# install.packages("rcompanion")
# install.packages("MASS")
# install.packages("brant")
#install.packages("coefplot")
# install.packages("foreign")
# install.packages("Hmisc")
# install.packages("reshape2")
#install.packages("gofcat")
#install.packages("gridExtra")


library(ordinal)  
library(rcompanion) 
library(MASS) 
library(brant)
library(coefplot)
library(stargazer)
library(sandwich)
library(ggplot2)
library(ggcorrplot)
library(haven)
library(dplyr)
library(tableone)
library(survival)
library(tibble) 
library(forcats)
library(lmtest)
library(car)
library(foreign)
library(Hmisc)
library(reshape2)
library(lmtest)
library(gridExtra)

# Загрузка и обработка данных
data <- read.csv("dataset.csv", sep = ";")
data$optfinbeh <- as.numeric(gsub(",", ".", data$optfinbeh))
data$finlitindex <- as.numeric(gsub(",", ".", data$finlitindex))
stargazer(data, type = "text")
summary(data)

#Таблица описательной статистики
stargazer(data[, 2:19], title = "Описательная статистика",  type="html",out="ОписСтат.html",
          digits = 3, header = FALSE, rownames = TRUE, df = FALSE)

data1 <- data %>% subset(country == 1) #Россия
data2 <- data %>% subset(country == 0) #Другое - больше 95% Армения 

#Корреляционная матрица
Correl <- cor(data[, 2:8])
ggcorrplot(Correl, hc.order = TRUE, type = "lower",
           colors = c("white","yellow","purple" ), lab = TRUE)

# Графики разброса.

# График 1
p1 <- ggplot(data) + aes(recfined, finsat, color = education) +
  geom_point() + labs(x = "Финансовое образование",
                      y = "Финансовая удовлетворенность", color = 'education') + 
  stat_smooth(method = "lm", se = FALSE, size = 2)

# График 2
p2 <- ggplot(data) + aes(objfinlit, finsat, color = education) +
  geom_point() + labs(x = "Объективная финансовая грамотность",
                      y = "Финансовая удовлетворенность", color = 'education') + 
  stat_smooth(method = "lm", se = FALSE, size = 2)

# График 3
p3 <- ggplot(data) + aes(subfinlit, finsat, color = education) +
  geom_point() + labs(x = "Субъективная финансовая грамотность",
                      y = "Финансовая удовлетворенность", color = 'education') + 
  stat_smooth(method = "lm", se = FALSE, size = 2)

# График 4
p4 <- ggplot(data) + aes(optfinbeh, finsat, color = education) +
  geom_point() + labs(x = "Оптимальное финансовое поведение",
                      y = "Финансовая удовлетворенность", color = 'education') + 
  stat_smooth(method = "lm", se = FALSE, size = 2)

# График 5
p5 <- ggplot(data) + aes(finatt, finsat, color = education) +
  geom_point() + labs(x = "Финансовое отношение",
                      y = "Финансовая удовлетворенность", color = 'education') + 
  stat_smooth(method = "lm", se = FALSE, size = 2)

# График 6
p6 <- ggplot(data) + aes(finlitindex, finsat, color = education) +
  geom_point() + labs(x = "Индекс финансовой грамотности",
                      y = "Финансовая удовлетворенность", color = 'education') + 
  stat_smooth(method = "lm", se = FALSE, size = 2)

# Объединяем графики в матрицу 3x2
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)






#Обычный МНК
#Базовая модель - Описать изначальную проблему мультикор
model_1 <- lm(data = data, finsat ~ objfinlit + subfinlit + optfinbeh  + finatt + recfined +
               age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)

summary(model_1)
vif(model_1) # <10 
waldtest(model_1)
resettest(model_1) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
#crPlots(model_1) 
bptest(model_1, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_1) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма




#Для проверки структурных сдвигов - не ссылаемся на этот модель - так как очень большая мультиколлениарность
model_1.1 <- lm(data = data, finsat ~ objfinlit + subfinlit + optfinbeh  + finatt + recfined +
                  recfined*country +
                  (objfinlit + subfinlit + optfinbeh  + finatt)*country +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)

summary(model_1.1)
vif(model_1.1)
waldtest(model_1.1)
resettest(model_1.1) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_1.1, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_1.1) #Если Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

#МНК с использование индекса 
model_2 <- lm(data = data, finsat ~  subfinlit + finlitindex + recfined + 
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)

summary(model_2)
vif(model_2)
waldtest(model_2)
resettest(model_2) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_2, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_2) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма



#Для проверки структурных сдвигов 
model_2.1 <- lm(data = data, finsat ~  subfinlit + finlitindex + recfined +
                 recfined*country +
                  (subfinlit + finlitindex)*country + 
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_2.1)
vif(model_2.1)
waldtest(model_2.1)
resettest(model_2.1) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_2.1, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_2.1) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

#Базовые модели для экспорта 
stargazer(list(model_1, model_2), column.labels = c("Базовая модель", "Использвание индекса"),type="html",out="Base.html",
          title = 'Таблица X: Оценка влияния финансовой грамотности на финансовое благосостояние - МНК',
          df=FALSE, digits=3)

#Модели для проверки структурных сдвигов - экспорт

stargazer(list(model_1.1, model_2.1), column.labels = c("Базовая модель", "Использвание индекса"), type="html",out="Structure.html",
          title = 'Таблица X: МНК модели с учетом структурных сдвигов',
          df=FALSE, digits=3)

grid.arrange(coefplot(model_1),coefplot(model_2))

# Модели для первой гипотезы 
#Тут проверяю зависят ли переменны интереса от полученного фин образования 

model_3 <- lm(data = data, subfinlit ~  recfined + objfinlit +  optfinbeh  + finatt +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_3) #finatt значимый
vif(model_3)
waldtest(model_3)
resettest(model_3) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_3, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_3) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

model_4 <- lm(data = data, objfinlit ~  recfined + subfinlit +  optfinbeh  + finatt +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_4) #optfinbeh - значимо на 10%
vif(model_4)
waldtest(model_4)
resettest(model_4) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_4, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_4) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

model_5 <- lm(data = data, optfinbeh ~  recfined + subfinlit +  objfinlit  + finatt + 
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_5) # objfinlit -10% finatt -1%
vif(model_5)
waldtest(model_5)
resettest(model_5) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_5, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_5) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

model_6 <- lm(data = data, finatt ~  recfined + subfinlit +  objfinlit  + optfinbeh + 
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_6) #subfinlit - 1%, optfinbeh - 1%
vif(model_6)
waldtest(model_6)
resettest(model_6) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_6, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_6) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

model_7 <- lm(data = data, finlitindex ~  recfined  + subfinlit +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_7) # subfinlit не значимо 
vif(model_7)
waldtest(model_7)
resettest(model_7) #Тест Рамсея на линейность: Нулевая гипотез – отсутствие пропущенных членов 
bptest(model_7, studentize = FALSE) #Тест Бреуша-Пагона (H0: гетроскедастичность отсутствует)
boxCox(model_7) # Лямбда в окресости 1 - не требуется логаритмическая функциональная норма

#Модели для проверки первой гипотезы - Экспорт 
#Функция робастных стандартных ошибок
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(list(model_3, model_4, model_5, model_6, model_7),
          se=list(cse(model_3),cse(model_5),cse(model_6),cse(model_7)),
          column.labels = c("Обьективная финансовая грамотность", "Субьктивная финансовая грамотность", "Оптимальное финансовое поведение",
                            "Финансовое отношение", "Индекс финансовой грамотности"), type = "text",
          title = 'Таблица X: Оценка влияния финансового образовния на перемены интереса',
          df=FALSE, digits=3)


# Базовая модель на разных выборках (отдельно Россиия и Армения)
#Проблема: очень сильная мултиколл (так наблюдений меньше - бин переменных много)
#Выборка сужается, станд. ошибки увеличиваются + мултиколл - не будут использованы модели в работе
model_8 <- lm(data = data1, finsat ~ objfinlit + subfinlit + optfinbeh  + finatt + recfined +
                age18 +  age24 + age44  + female  +
                education + married +  
                income_high + income_low + country)
summary(model_8)
vif(model_8) # Error in vif.default(model_8) : there are aliased coefficients in the model
cor(data1)
waldtest(model_8)

model_9 <- lm(data = data2, finsat ~ objfinlit + subfinlit + optfinbeh  + finatt + recfined +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_9)
vif(model_9) #Тут тоже сильная мультиколлениарность 
waldtest(model_9)

model_10 <- lm(data = data1, finsat ~ subfinlit + finlitindex + recfined +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_10)
vif(model_10) # Error in vif.default(model_8) : there are aliased coefficients in the model
cor(data1)
waldtest(model_10)

model_9.1 <- lm(data = data2, finsat ~ objfinlit + subfinlit + optfinbeh  + finatt + recfined +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)
summary(model_9.1)
vif(model_9.1) #Тут тоже сильная мультиколлениарность 
waldtest(model_9.1)


#Порядковый логит модель
#Использую для проверки резульататов на устойчивость

data <- data %>% mutate(finsat_ord = ordered(as.factor(finsat)))
str(data)
summary(data)

#В этой модели не включены finatt и objfinlit, так как тест Wald показывает, что этот модель лучше
#Если в модели включить эти переменные (model_13 они включены), не выполяется предпосылка parallel regression assumptuion
model_11 <- polr(data = data, method = "logistic", Hess = TRUE,
                finsat_ord ~ subfinlit + optfinbeh  + recfined +
                age18 +  age24 + age44  + female  +
                education + married + children + 
                income_high + income_low + country)

summary(model_11)
coeftest(model_11)
coefplot(model_11)
brant(model_11) 
vif(model_11)
waldtest(model_11)
exp(coef(model_11))

model_13 <- polr(data = data, method = "logistic", Hess = TRUE,
                 finsat_ord ~ objfinlit + finatt + subfinlit + optfinbeh  + recfined +
                   age18 +  age24 + age44  + female  +
                   education + married + children + 
                   income_high + income_low + country)
summary(model_13)
coeftest(model_13)
coefplot(model_13)
brant(model_13) 
vif(model_13)
waldtest(model_13)
exp(coef(model_13))


waldtest(model_11, model_13)


#Ordered-logit - использование индекса 
model_12 <- polr(data = data, method = "logistic", Hess = TRUE,
                 finsat_ord ~ subfinlit + finlitindex + recfined +
                   age18 +  age24 + age44  + female  +
                   education + married + children + 
                   income_high + income_low + country)
summary(model_12)
coeftest(model_12)
coefplot(model_12)
brant(model_12) #parallel regression assumption - Это не выполяентся - критично? 
vif(model_12)
waldtest(model_12)
exp(coef(model_12))

#Импорт моделей порядковой логистической регресии

stargazer(list(model_11, model_12),
          column.labels = c("Базовая модель", "Использвание индекса"), type="html",out="Оrdlog.html",
          title = 'Таблица X: Модели порядковой логистической регрессии',
          df=FALSE, digits=3)

















