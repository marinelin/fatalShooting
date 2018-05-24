train <- read_csv("~/UCLA/201804 Spring/STAT 101C/Kaggle/train.csv")
test <- read_csv("~/UCLA/201804 Spring/STAT 101C/Kaggle/test.csv")


train$SubjectGender <- gsub("U", NA, train$SubjectGender)
train$SubjectRace <- gsub("U", NA, train$SubjectRace)
train$SubjectArmed <- gsub("U", NA, train$SubjectArmed)
train$Fatal <- gsub("U", NA, train$Fatal)


train$NumberOfShots <- gsub("U", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("Unknown", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("not clear", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("no information", NA, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=1", 1.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=20", 20.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=2", 2.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=3", 3.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=4", 4.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">/=5", 5.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">1", 1.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">2", 2.0, train$NumberOfShots)
train$NumberOfShots <- gsub(">4", 4.0, train$NumberOfShots)
train$NumberOfShots <- gsub("45 total", 45.0, train$NumberOfShots)
train$NumberOfShots <- gsub(";", NA, train$NumberOfShots)
train$NumberOfShots <- gsub("Multiple", NA, train$NumberOfShots)

train$NumberOfShots <- as.numeric(train$NumberOfShots)

train.na <- na.omit(train[-c(15, 17)])


set.seed(169)

train.sample <- train.na %>% sample_n(3000, replace = TRUE)

model <- glm(factor(Fatal) ~ SubjectArmed + SubjectGender + SubjectRace + NumberOfShots, data = train.sample, family = binomial)
summary(model)


test$SubjectGender <- gsub("U", NA, test$SubjectGender)
test$SubjectRace <- gsub("U", NA, test$SubjectRace)
test$SubjectRace <- gsub("O", NA, test$SubjectRace)
test$SubjectArmed <- gsub("U", NA, test$SubjectArmed)
test$Fatal <- gsub("U", NA, test$Fatal)


test$NumberOfShots <- gsub("U", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("Unknown", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("not clear", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("no information", NA, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=1", 1.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=20", 20.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=2", 2.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=3", 3.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=4", 4.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">/=5", 5.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">1", 1.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">2", 2.0, test$NumberOfShots)
test$NumberOfShots <- gsub(">4", 4.0, test$NumberOfShots)
test$NumberOfShots <- gsub("45 total", 45.0, test$NumberOfShots)
test$NumberOfShots <- gsub(";", NA, test$NumberOfShots)
test$NumberOfShots <- gsub("Multiple", NA, test$NumberOfShots)

test$NumberOfShots <- as.numeric(test$NumberOfShots)

test.new <- test[-c(14, 16)]

Fatal <- predict(model, newdata = test.new)

submission <- data.frame(test.new[1], Fatal)
submission$Fatal[is.na(submission$Fatal)] <- "0.5"
for (i in 1:1400) {
  if (submission$Fatal[i] >= 0.5) {
    submission$Fatal[i] = "No"
  }
  if (submission$Fatal[i] < 0.5) {
    submission$Fatal[i] = "Yes"
  }
}
head(submission)

write_csv(submission, "~/UCLA/201804 Spring/STAT 101C/Kaggle/submission.csv")
