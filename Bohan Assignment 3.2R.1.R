# Bohan Fu
# 14486504

remind_me <- function()
{
  print("Date of tomorrow:")
  print(as.Date(Sys.time() + 86400))
}

remind_me()

cheat <- function(q_number)
{
  switch(
    q_number,
    "1" = {
      print(
        cat(
          '# Q3.1.1
sim_grade <- rnorm(n = 60, mean = 7.5, sd = 1)
sim_grade[sim_grade > 10] <- 10
sim_grade[sim_grade < 0] <- 0
hist(sim_grade)'
        )
      )
    },
    "2" = {
      print(
        cat(
          '# Q3.1.2
schiphol <- read.csv("schiphol_data.csv")
plot(schiphol$DATE,
     schiphol$TMAX,
     xlab = "Year",
     ylab = "Maximum temperature"'
        )
      )
    },
    "3" = {
      print(
        cat(
          '# Q3.1.3
library(ggplot2)
library(titanic)
Sex <- factor(c("male", "male", "female", "female"))
count <- vector(length = 4)
Condition <- c("Alive", "Dead")
count[1] <- sum(titanic_train$Survived[titanic_train$Sex == "male"])
count[2] <-
  length(titanic_train$Survived[titanic_train$Sex == "male"]) - count[1]
count[3] <-
  sum(titanic_train$Survived[titanic_train$Sex == "female"])
count[4] <-
  length(titanic_train$Survived[titanic_train$Sex == "female"]) - count[3]
titanic_survival <- data.frame(Sex, Condition, count)
ggplot(titanic_survival, aes(fill = Condition, y = count, x = Sex)) +
  geom_bar(position = "stack", stat = "identity") +
  guides(fill = guide_legend(title = "How did it go?"))'
        )
      )
    },
    "4" = {
      print(
        cat(
          '# Q3.1.4
ggplot(titanic_survival, aes(fill = Condition, y = count, x = Sex)) +
  geom_bar(position = "stack", stat = "identity") +
  guides(fill = guide_legend(title = "How did it go?")) +
  theme_light()'
        )
      )
    },
    "5" = {
      print(
        cat(
          '# Q3.1.5
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ggplot(ToothGrowth, aes(x = supp, y = len)) +
  geom_boxplot(outlier.color = "red") +
  xlab("Supplement") +
  ylab("Length") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey")
  )
# Added axis names, removed grids and background colors'
        )
      )
    },
    "6" = {
      print(
        cat(
          '# Q3.1.6
chick <- c(1, 20, 3, 40, 5)
max_weight <- vector(length = 5)
for (i in 1:5)
{
  max_weight[i] <-
    max(ChickWeight$weight[ChickWeight$Chick == chick[i]])
}
chick <- as.character(chick)
chick_w <- data.frame(chick, max_weight)
ggplot(chick_w, aes(x = chick, y = max_weight)) +
  geom_bar(stat = "identity")'
        )
      )
    },
    "7" = {
      print(
        cat(
          'ggplot(ChickWeight, aes(x = Time, y = weight)) +
  geom_smooth(method = "lm", formula = y ~ x)'
        )
      )
    },
    "8" = {
      print(
        cat(
          '# Q3.1.8
chick <- c(1, 20, 3, 40, 5)
Time <- unique(ChickWeight$Time)
weight <-
  ChickWeight$weight[ChickWeight$Chick == 1 |
                       ChickWeight$Chick == 20 |
                       ChickWeight$Chick == 3 |
                       ChickWeight$Chick == 40 | ChickWeight$Chick == 5]
chick <- rep(chick, each = length(Time))
chick <- as.character(chick)
Time <- rep(Time, 5)
chick_318 <- data.frame(chick, Time, weight)
p316 <- ggplot(chick_w, aes(x = chick, y = max_weight)) +
  geom_bar(stat = "identity")
p318 <- ggplot(chick_318, aes(x = Time, y = weight)) +
  geom_line(aes(color = chick))
library(patchwork)
p316 + p318'
        )
      )
    }
  )
}