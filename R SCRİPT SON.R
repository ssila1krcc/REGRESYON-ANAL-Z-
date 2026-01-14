data
glikoz <- as.numeric(as.character(data$Glucose))
insulin <- as.numeric(as.character(data$Insulin))
vucut_kitle_ind <- as.numeric(as.character(data$BMI))
uyku_saati <- as.numeric(as.character(data$Sleep_Hours))
diyabet <- as.numeric(data$Diabetes_Status)
yas <- as.numeric(data$Age)
sigara_kullanimi <- data$Smoking
diyet_turu <- data$Diet_Type
alkol_kullanimi <- data$Alcohol
egzersiz_frekansi <- data$Exercise_Frequency
cinsiyet <- data$Gender
genetik_yatkinlik <- data$Heredity
uyku_saati<- log(uyku_saati)


colnames(data) <- c("glikoz","insulin","yas","cinsiyet","diyet_turu","egzersiz_sikligi","genetik_yatkinlik","sigara_kullanimi","alkol_kullanimi","uyku_saati","diyabet_statusu","vucut_kitle_ind")
str(data)

#eksik verilerin silinmesi
data_model <- na.omit(data[, c("Diabetes_Status", "Glucose", "Insulin",
                               "BMI", "Sleep_Hours","Gender", "Diet_Type")])

#modelin ilk kurulumu glikozun neden eklenmeyeceginin kan??t??
model_logit <- glm(
  diyabet ~ glikoz + insulin + yas + uyku_saati,
  family = binomial(link = "logit")
)

summary(model_logit)
#glikoz basit dogrusal reg modeli icin alternatif degiskendir o yuzden anlamsiz getirir digerlerini.

#modelin dogru kurulumu
model2 <- glm(
  diyabet ~ log(insulin) + vucut_kitle_ind + log(uyku_saati),
  data = data_model,
  family = binomial
)

summary(model2)

#vif degeri
install.packages("car")
library(car)
vif(model2)

#standartlastirilmis sapmalar
rstd <- rstandard(model2)
summary(rstd)
which(abs(rstd)>3)

#cooks degerleri
cooks <- cooks.distance(model2)
threshold <- 4 /length(cooks)
sum(cooks > threshold)
max(cooks.distance(model2))
plot(model2, which = 4, id.n = 3) #cooks distance grafigi

#orneklem buyuklugunun uygunlugu icin
table(model2$model$diyabet)

events <- sum(model2$model$diyabet == 1)
p <- 3
EPV <- events / p
EPV

#normallik aranmamasi icin
hist(residuals(model2),
          breaks = 30,
          probability = TRUE,
          main = "Hatalarin Histogram",
          xlab = "Hatalar")

curve(dnorm(x,
          mean = mean(residuals(model2)),
          sd   = sd(residuals(model2))),
          col = "red", lwd = 2, add = TRUE)


qqnorm(residuals(model2))
qqline(residuals(model2), col = "red")

shapiro.test(residuals(model2))

#anova
anova(model2, test = "Chisq")

#grafikler
frekans <- table(cinsiyet)
pie(frekans)
pie(frekans, main="cinsiyet_grafik",col=c("red","pink"),
    labels=etiket)
oran <- round((frekans/sum(frekans)*100),2)
etiket <- paste(names(frekans),"%",oran)
etiket

cinsiyet <- factor(data$Gender,
                   levels=c("Female","Male"),
                   labels=c("Kadin","Erkek"))

frekans1<- table(sigara_kullanimi)
pie(frekans1, main="Sigara Kullanimi" , etiket, col=c("darkblue","darkred"))
oran <- round((frekans1/sum(frekans1)*100),2)
etiket <- paste(names(frekans1),"%",oran)
etiket

sigara_kullanimi <- factor(
  data$Smoking,
  levels = c("No", "Yes"),
  labels = c("Hayir", "Evet")
)


boxplot(data_model$Diet_Type)


diyet_turu <- factor(
  data$Diet_Type,
  levels = c("Healthy", "Moderate", "Unhealthy"),
  labels = c("Saglikli", "Orta", "Sagliksiz")
)


diyet_freq <- table(diyet_turu)

# Renkler (orta = yesil, digerleri kirmizi)
bar_cols <- c("darkred", "darkgreen", "darkred")

# Barplot
barplot(diyet_freq,
        col = bar_cols,
        main = "diyet_turu",
        xlab = "Tur",
        ylab = "Kisi Sayisi",
        ylim = c(0, max(diyet_freq) * 1.75))


library(dplyr)

df_50_alt_diyabet <- data %>%
  filter(yas < 50, diyabet == 1)

oran_df <- df_50_alt_diyabet %>%
  mutate(
    Cinsiyet = factor(cinsiyet,
                      levels = c("Female", "Male"),
                      labels = c("Kadin", "Erkek"))
  ) %>%
  count(Cinsiyet) %>%
  mutate(Oran = n / sum(n) * 100)

barplot(oran_df$Oran,
        names.arg = oran_df$Cinsiyet,
        col = c("pink", "lightblue"),
        ylim = c(0, 60),
        main = "50 Yas Alti Diyabetli Bireylerin Cinsiyete Gore Oranlari",
        ylab = "Oran (%)")

# Yuzdeler
text(x = seq_along(oran_df$Oran),
     y = oran_df$Oran,
     labels = paste0(sprintf("%.1f", oran_df$Oran), "%"),
     pos = 3)


library(dplyr)

df_50_alt_diyabet <- data %>%
  filter(
    yas < 50,
    diyabet == 1,
    !is.na(cinsiyet)          
  )

# Gerekli paketler
library(readxl)
library(dplyr)

# 1) Veriyi okuma
data <- read_excel("data.xlsx")

# 2) Sayisal tipleri garanti altina alma
data <- data %>%
  mutate(
    Glucose      = as.numeric(Glucose),
    BMI          = as.numeric(BMI),
    Insulin      = as.numeric(Insulin),
    Sleep_Hours  = as.numeric(Sleep_Hours),
    Age          = as.numeric(Age),
    Diabetes_Status = as.numeric(Diabetes_Status)
  )

# 3) 50 ya?? alt?? + diyabetli + cinsiyet var 
df_f <- data %>%
  filter(
    Age < 50,
    Diabetes_Status == 1,
    !is.na(Gender)
  ) %>%
  mutate(
    # Cinsiyet 
    Cinsiyet = factor(Gender,
                      levels = c("Male","Female"),
                      labels = c("Erkek","Kadin")),
    # Log(Uyku) (0 veya negatif varsa NA yap)
    log_uyku = ifelse(!is.na(Sleep_Hours) & Sleep_Hours > 0,
                      log(Sleep_Hours),
                      NA_real_)
  )

# 4) Medyanlari cinsiyete gore hesaplama
medyan_tablosu <- df_f %>%
  group_by(Cinsiyet) %>%
  summarise(
    Glikoz_Medyan   = median(Glucose, na.rm = TRUE),
    BMI_Medyan      = median(BMI, na.rm = TRUE),
    Insulin_Medyan  = median(Insulin, na.rm = TRUE),
    LogUyku_Medyan  = median(log_uyku, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
medyan_tablosu


#standart hata qq tablo
std_res <- rstandard(model2)
boxplot(std_res,
        main="Standartla??t??r??lm???? Hatalar",
        ylab="standart hatalar",
        outline= FALSE,
        lwd=2,
        col="red",
        abline(h=0, col="blue",lwd=2)
        )


frekans3<- table(diyabet)
frekans3
pie(frekans3, main="Diyabet_Dag_Grf" , etiket, col=c("lightblue","pink"))
oran <- round((frekans3/sum(frekans3)*100),2)
etiket <- paste(names(frekans3),"%",oran)
etiket
legend("topright",legend=c("0=Diyabet yok","1=Diyabet var"),col=c("lightblue","pink"),bty="n")

