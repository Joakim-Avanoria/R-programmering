# vektorer
# listor
# matriser
# dataframes
# factors

df <- data.frame(
  name = c("Joakim", "Hans", "Emil"),
  age = c(39, 43, 29)
)
df

head(df)
str(df)
summary(df)


df$name
df["name"]
df[["name"]]
df[1]


df[1,]
df[,1]

df[1, 1:2]


data <- read.csv("data.csv")


head(data)
str(data)
summary(data)














