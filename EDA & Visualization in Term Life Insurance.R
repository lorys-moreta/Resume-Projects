#Task 1: Import
term.life <- read_csv("termlife.csv")

#Task 2: FACE and FACE/INCOME histograms
term.life.pos <- term.life |> filter(FACE > 0)

#view outliers
view(term.life.pos |> arrange(desc(FACE)))
#there are 71 values over 500,000 which will be left out
#20 uniform bins
ggplot(term.life.pos, aes(x = FACE)) +
  geom_histogram(binwidth = 25000) +
  coord_cartesian(xlim = c(0, 500000)) #sets x-axis range to 0 - 500,000

#adds FACE/INCOME column to view outliers
term.life.pos <- term.life.pos |> mutate(fc = FACE/INCOME) |> arrange(desc(fc))
view(term.life.pos)

#20 uniform bins
ggplot(term.life.pos, aes(x = fc)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0.1, 50))

#Task 3: Spouse age difference histogram
term.life <- term.life |> mutate(ad = AGE - SAGE)
ggplot(term.life, aes(x = ad)) +
  geom_histogram(binwidth = 5)

#removes non-married insureds and takes absolute value of age difference
term.life.marriage <- term.life |> filter(SAGE > 0)
term.life.marriage <- term.life.marriage |> mutate(ad = abs(AGE - SAGE))
ggplot(term.life.marriage, aes(x = ad)) +
  geom_histogram(binwidth = 1)

#Task 4: Spousal Differences in Education
ggplot(term.life.marriage, aes(x = EDUCATION, y = SEDUCATION)) +
  geom_count()

#adds absolute value of education difference
term.life.marriage <- term.life.marriage |> mutate(ed = abs(EDUCATION - SEDUCATION))
ggplot(term.life.marriage, aes(x = ed)) +
  geom_bar()

#Task 5: Multivariable simultaneous plotting with pairs()
term.life <- read.csv(file = "TermLife.csv", stringsAsFactors = TRUE)

#the numbers refer to the excel columns: AGE, FACE, EDUCATION, INCOME
pairs(term.life[, c(4, 1, 6, 10)])

term.life <- term.life |> mutate(face_log = log(FACE), income_log = log(INCOME))
pairs(term.life[, c(4, 12, 6, 13)])

# Task 6: Construct exploratory boxplots
termLife.new <- term.life[term.life$EDUCATION > 6, ]
termLife.new <- termLife.new[termLife.new$SEDUCATION > 6 |
                               termLife.new$SEDUCATION == 0, ]

#switch orientation by using y = INCOME instead of x = INCOME
plot1 <- ggplot(termLife.new, aes(y = INCOME)) + 
  geom_boxplot() + 
  labs(title = "INCOME")

plot2 <- ggplot(termLife.new, aes(y = CHARITY)) + 
  geom_boxplot() + 
  labs(title = "CHARITY")

plot3 <- ggplot(termLife.new, aes(y = FACE)) + 
  geom_boxplot() + 
  labs(title = "FACE")

grid.arrange(p1, p2, p3, ncol = 3)

#take out zeros for charity and face
plot1 <- ggplot(termLife.new, aes(y = INCOME)) + 
  geom_boxplot() + 
  labs(title = "INCOME")

plot2 <- ggplot(termLife.new[termLife.new$CHARITY > 0, ], aes(y = CHARITY)) + 
  geom_boxplot() + 
  labs(title = "CHARITY")

plot3 <- ggplot(termLife.new[termLife.new$CHARITY > 0, ], aes(y = FACE)) + 
  geom_boxplot() + 
  labs(title = "FACE")

grid.arrange(plot1, plot2, plot3, ncol = 3)

#log-transformed y-axis
plot1 <- ggplot(termLife.new, aes(y = INCOME)) + 
  geom_boxplot() + 
  scale_y_log10() +
  labs(title = "INCOME")

plot2 <- ggplot(termLife.new[termLife.new$CHARITY > 0, ], aes(y = CHARITY)) + 
  geom_boxplot() + 
  scale_y_log10() +
  labs(title = "CHARITY")

plot3 <- ggplot(termLife.new[termLife.new$CHARITY > 0, ], aes(y = FACE)) + 
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "FACE")

grid.arrange(plot1, plot2, plot3, ncol = 3)

#Task 7: Final histograms
termLife.new <- termLife.new[termLife.new$CHARITY < 1000000, ]

termLife.new$log_face <- ifelse(termLife.new$FACE == 0, 0, 
                                 log(termLife.new$FACE))
termLife.new$log_income <- log(termLife.new$INCOME)
termLife.new$log_charity <- ifelse(termLife.new$CHARITY == 0, 0, 
                                    log(termLife.new$CHARITY))

#histograms
plot1 <- ggplot(data = termLife.new) +
  geom_histogram() +
  aes(log_income, ..density..)
plot2 <- ggplot(data = termLife.new[termLife.new$log_charity > 0, ]) +
  geom_histogram() +
  aes(log_charity, ..density..)
plot3 <- ggplot(data = termLife.new[termLife.new$log_face > 0, ]) +
  geom_histogram() +
  aes(log_face, ..density..)
grid.arrange(plot1, plot2, plot3, ncol = 3)
