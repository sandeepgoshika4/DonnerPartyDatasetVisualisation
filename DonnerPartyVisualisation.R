# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, dplyr, tidyr, magrittr, knitr, ggplot2, ggpubr, ggsci, vcd, vcdExtra, stringr) 

#data(Donner)

#str(Donner)
#cat("Number of columns: ", ncol(Donner))

head(Donner, n=15L)

#str(Donner)

#names(Donner)

#Donner$sex

#colSums(is.na(Donner))
#Donner$death[is.na(Donner$death)] = median(Donner$death, na.rm = TRUE)

#colSums(is.na(Donner))
#head(Donner[c( "family")])

#P1.Donner data exploratory analysis.
#1)How many members were there in the named families?
summary(Donner$family)

#2)What was ratio of men to women?
gender_counts <- table(Donner$sex)
#gender_counts
men_to_women_ratio <- gender_counts["Male"]/gender_counts["Female"]
print(men_to_women_ratio)

#3)How many children below 18 years of age were there?
donner_child <- filter(Donner, age < 18)
# count number of observations
nrow(donner_child)

#4)What was the most common first name for the females?
females <- Donner %>% filter(sex == "Female")
name_freq <- females %>% count(females$family, sort = TRUE)
name_freq[1,"females$family"]

#P2)Data Preparation Step Totaling the survivors
survivors_df <- Donner %>%
  group_by(family) %>%
  summarize(Total_Survivors = sum(survived),
            Family_Size = n(),
            Percentage_Survived = round(Total_Survivors/Family_Size * 100, 1)) %>%
  arrange(desc(Total_Survivors)) %>%
  ungroup()
survivors_df

#Pie chart prelude
myDonner <- survivors_df %>%
  arrange (desc(Total_Survivors)) %>%
  mutate(prop = round(Total_Survivors * 100/sum(Total_Survivors), 1), lab.ypos = cumsum(prop) - 0.5 * prop)

head(myDonner)

#P3)Pie Chart of the Donner Survivors
par(xpd=TRUE)
ggpie(
  myDonner, x = "prop", label ="prop",
  lab.pos = "in", lab.font = list(color = "white"),
  fill = "family", color="white",
  palette = "jco",sort.val = "asc"
) + theme(
  legend.position = c(.99, .30),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.key.size = unit(0.2, "cm")
) + ggtitle("Pie Chart of the Donner Survivors")

#p4)Data Preparations for a Bar Chart of the Survivors
# create a factor variable indicating survival status
survival_factor <- factor(ifelse(Donner$survived == 1, "Survived", "Died"))
# create the 2x2 data frame
survival_summary <- data.frame(Survival_Status = levels(survival_factor), Count = table(survival_factor))
survival_summary

#P5)Bar Chart of the Survivors
# create the ggbarplot
ggbarplot(survival_summary, x = "Survival_Status", y = "Count.Freq",
          fill = "Survival_Status",
          xlab = "Survival Status", ylab = "Count") +
  ggtitle("Donner Party Survivor Data") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))

#P6)Age of the Donner Party (Bar Chart)
# Take a random sample of 20 records from Donner dataset
set.seed(123)
donner_sample <- sample_n(vcdExtra::Donner, 20)

# Define colors for survivors and non-survivors
survived_color <- "#33BB33"  # green
died_color <- "#BB3333"      # red
  
names = row.names(donner_sample)

# Create the bar plot
ggplot(donner_sample, aes(x = names, y = age), ) +
  geom_bar(stat = "identity", aes(fill = case_when(survived  == '1' ~ survived_color, survived == '0' ~ died_color))) +
  scale_fill_identity() +
  labs(title = "Age of the Donner Party Bar",
       x = "Name",
       y = "Age") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#P7)
donner_sample$name <- names
ggbarplot(donner_sample, x = "name", y = "age",sort.val = "asc") +
  geom_bar(stat = "identity", aes(fill = case_when(survived  == '1' ~ survived_color, survived == '0' ~ died_color))) +
  scale_fill_identity() +
  labs(title = "Age of the Donner Party Bar using ggbarplot",
       x = "Name",
       y = "Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#P8)Which families enjoyed the largest percentage of survivors?
# Create dot chart
ggplot(survivors_df, aes(x = reorder(family, Percentage_Survived), y = Percentage_Survived)) +
  geom_segment(aes(x = family, xend = family, y = 0, yend = Percentage_Survived), color = "grey70", size = 0.5) +
  geom_point(aes(color = Percentage_Survived), size = 3) +
  scale_color_gradient(low = "grey70", high = "steelblue") +
  labs(title = "Percentage of survivors by family",
       x = "Family",
       y = "Percentage of survivors")


#P9)z score of survivors by Families (Dot Chart)
# Calculate z-scores of survivorss per family
survivors_df <- survivors_df %>%
  mutate(z_score_Sepal_Length = (Total_Survivors - mean(Total_Survivors)) / sd(Total_Survivors)) %>%
           arrange(z_score_Sepal_Length)
 
# Create dot chart of z-scores
ggplot(survivors_df, aes(x = family, y = z_score_Sepal_Length)) +
  geom_segment(aes(x = family, xend = family, y = 0, yend = z_score_Sepal_Length), color = "grey70", size = 0.5) +
  geom_point(aes(color = z_score_Sepal_Length > 0), size = 3) +
  scale_color_manual(values = c("steelblue", "tomato")) +
  geom_text(aes(label = round(z_score_Sepal_Length, 2)), size = 3, vjust = 1.5) +
  labs(title = "Z-scores of survivors by family",
       x = "Family",
       y = "Z-score")


#P10)Survivors by Families (Cleveland)
survivors_df
ggplot(survivors_df, aes(x = z_score_Sepal_Length, y = family, xmin = 0, xmax = z_score_Sepal_Length)) +
  geom_point(aes(color = z_score_Sepal_Length > 0),size = 3) +  # Use a larger dot
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed")
  )+
  scale_color_manual(values = c("steelblue", "tomato")) +
  labs(title = "Total number of survivors z-scored by family",
       x = "z-scores",
       y = "Family Name")