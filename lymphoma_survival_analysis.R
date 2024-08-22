### STAT 486 Final Exam 
### Yuxin Kang


## Read the data
data_final <- read.table("/Users/kangyuxin/Downloads/Lymphoma.csv", sep = ",", header = TRUE, fill = TRUE, quote = "\"", stringsAsFactors = FALSE)


## Display the data
head(data_final)
"""
Output:

PERF_STA  AGE STAGE SEX PR_RAD B_SYM RACE r_score pr_resp pr_drug os_event os_time       trt
1        0 49.5   III   M      N     N    1    0, 1    DU<1       Y        0  45.437 treatment
2        0 30.6     I   M      Y          1    0, 1    DU<1       Y        1  10.645   control
3        2 60.5   III   F      N     Y    1     >=3    DU<1       Y        0 138.448   control
4        1 57.1   III   M      N     N    1       2    DU<1       N        0 156.452 treatment
5        1 45.8    II   M      N     N    1    0, 1    DU>1       N        0 151.721   control
6        1 51.9   III   M      Y     N    1    0, 1    DU<1       N        1  22.637 treatment
"""


## Count missing values
sum(is.na(data_final$PERF_STA) | data_final$PERF_STA == "") # Output is 0

sum(is.na(data_final$AGE) | data_final$AGE == "") # Output is 0

sum(is.na(data_final$STAGE) | data_final$STAGE == "") # Output is 0

sum(is.na(data_final$PR_RAD) | data_final$PR_RAD == "") # Output is 0

sum(is.na(data_final$B_SYM) | data_final$B_SYM == "") # Output is 26

sum(is.na(data_final$RACE) | data_final$RACE == "") # Output is 14

sum(is.na(data_final$r_score) | data_final$r_score == "") # Output is 0

sum(is.na(data_final$pr_resp) | data_final$pr_resp == "") # Output is 0

sum(is.na(data_final$pr_drug) | data_final$pr_drug == "") # Output is 0

sum(is.na(data_final$os_event) | data_final$os_event == "") # Output is 0

sum(is.na(data_final$os_time) | data_final$os_time == "") # Output is 0

sum(is.na(data_final$trt) | data_final$trt == "") # Output is 0


## Remove rows with missing values
data_final <- data_final[!is.na(data_final$B_SYM) & data_final$B_SYM != "", ]
data_final <- data_final[!is.na(data_final$RACE) & data_final$RACE != "", ]


## Distribution of PERF_STA
library(ggplot2)

# Count the occurrences of each category in PERF_STA
category_counts <- table(data_final$PERF_STA)

# Convert the table to a data frame
perf_sta_data <- as.data.frame(category_counts)
names(perf_sta_data) <- c("category", "count")

# Calculate percentages
perf_sta_data$percentage <- (perf_sta_data$count / sum(perf_sta_data$count)) * 100

# Create the plot
ggplot(perf_sta_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Performance Status", y = "Count", fill = "Performance Status", title = "Distribution of Performance Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of STAGE
category_counts <- table(data_final$STAGE)
stage_data <- as.data.frame(category_counts)
names(stage_data) <- c("category", "count")
stage_data$percentage <- (stage_data$count / sum(stage_data$count)) * 100
ggplot(stage_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Cancer Stage", y = "Count", fill = "STAGE", title = "Distribution of Cancer Stage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of SEX
category_counts <- table(data_final$SEX)
sex_data <- as.data.frame(category_counts)
names(sex_data) <- c("category", "count")
sex_data$percentage <- (sex_data$count / sum(sex_data$count)) * 100
ggplot(sex_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Sex", y = "Count", fill = "Sex", title = "Distribution of Sex") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of B_SYM
category_counts <- table(data_final$B_SYM)
b_sym_data <- as.data.frame(category_counts)
names(b_sym_data) <- c("category", "count")
b_sym_data$percentage <- (b_sym_data$count / sum(b_sym_data$count)) * 100
ggplot(b_sym_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "B-symptom", y = "Count", fill = "B-symptom", title = "Distribution of B-symptom") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of RACE
category_counts <- table(data_final$RACE)
race_data <- as.data.frame(category_counts)
names(race_data) <- c("category", "count")
race_data$percentage <- (race_data$count / sum(race_data$count)) * 100
ggplot(race_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Race", y = "Count", fill = "Race", title = "Distribution of Race") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of r_score
category_counts <- table(data_final$r_score)
r_score_data <- as.data.frame(category_counts)
names(r_score_data) <- c("category", "count")
r_score_data$percentage <- (r_score_data$count / sum(r_score_data$count)) * 100
ggplot(r_score_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "r-score", y = "Count", fill = "r-score", title = "Distribution of r-score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of PR_RAD
category_counts <- table(data_final$PR_RAD)
pr_rad_data <- as.data.frame(category_counts)
names(pr_rad_data) <- c("category", "count")
pr_rad_data$percentage <- (pr_rad_data$count / sum(pr_rad_data$count)) * 100
ggplot(pr_rad_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "PR-RAD", y = "Count", fill = "PR-RAD", title = "Distribution of PR-RAD") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of pr_drug
category_counts <- table(data_final$pr_drug)
pr_drug_data <- as.data.frame(category_counts)
names(pr_drug_data) <- c("category", "count")
pr_drug_data$percentage <- (pr_drug_data$count / sum(pr_drug_data$count)) * 100
ggplot(pr_drug_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "PR-DRUG", y = "Count", fill = "PR-DRUG", title = "Distribution of PR-DRUG") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of pr_resp
category_counts <- table(data_final$pr_resp)
pr_resp_data <- as.data.frame(category_counts)
names(pr_resp_data) <- c("category", "count")
pr_resp_data$percentage <- (pr_resp_data$count / sum(pr_resp_data$count)) * 100
ggplot(pr_resp_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "PR-RESP", y = "Count", fill = "PR-RESP", title = "Distribution of PR-RESP") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of os_event
category_counts <- table(data_final$os_event)
os_event_data <- as.data.frame(category_counts)
names(os_event_data) <- c("category", "count")
os_event_data$percentage <- (os_event_data$count / sum(os_event_data$count)) * 100
ggplot(os_event_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Overall Survival Status", y = "Count", fill = "Overall Survival Status", title = "Distribution of Overall Survival Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of trt
category_counts <- table(data_final$trt)
trt_data <- as.data.frame(category_counts)
names(trt_data) <- c("category", "count")
trt_data$percentage <- (trt_data$count / sum(trt_data$count)) * 100
ggplot(trt_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Treatment", y = "Count", fill = "Treatment", title = "Distribution of Treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## Distribution of Age
p_age <- ggplot(data_final, aes(x = AGE)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "slategray1", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p_age)


## Distribution of os_time
p_time <- ggplot(data_final, aes(x = os_time)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "slategray1", color = "black") +
  labs(title = "Distribution of Overall Survival Time", x = "Overall Survival Time", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p_time)


## Log time vs PERF_STA
ggplot(data_final, aes(x = factor(PERF_STA), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "Performance Status", y = "Log(Time)", title = "Scatter Plot of Log Time vs Performance Status") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type")) +
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs AGE
ggplot(data_final, aes(x = AGE, y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "Age", y = "Log(Time)", title = "Scatter Plot of Log Time vs Age") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))+
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs STAGE
ggplot(data_final, aes(x = factor(STAGE), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "Cancer Stage", y = "Log(Time)", title = "Scatter Plot of Log Time vs Cancer Stage") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))  +
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs SEX
ggplot(data_final, aes(x = factor(SEX), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "Sex", y = "Log(Time)", title = "Scatter Plot of Log Time vs Sex") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))  +
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs PR_RAD
ggplot(data_final, aes(x = factor(PR_RAD), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "PR-RAD", y = "Log(Time)", title = "Scatter Plot of Log Time vs PR-RAD") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))  +
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs B_SYM
ggplot(data_final, aes(x = factor(B_SYM), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "B-symptom", y = "Log(Time)", title = "Scatter Plot of Log Time vs B-symptom") +
  theme_minimal() +
  theme(legend.title = element_blank()) + 
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))  +
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs RACE
ggplot(data_final, aes(x = factor(RACE), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "Race", y = "Log(Time)", title = "Scatter Plot of Log Time vs Race") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))  +
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs r_score
ggplot(data_final, aes(x = factor(r_score), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "r-score", y = "Log(Time)", title = "Scatter Plot of Log Time vs r-score") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))+
  theme(plot.title = element_text(hjust = 0.5))


## Log time vs pr_resp
ggplot(data_final, aes(x = factor(pr_resp), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "PR-RESP", y = "Log(Time)", title = "Scatter Plot of Log Time vs PR-RESP") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type")) +
  theme(plot.title = element_text(hjust = 0.5)) 


## Log time vs pr_drug
ggplot(data_final, aes(x = factor(pr_drug), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "PR-DRUG", y = "Log(Time)", title = "Scatter Plot of Log Time vs PR-DRUG") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type")) +
  theme(plot.title = element_text(hjust = 0.5)) 


## Log time vs trt
ggplot(data_final, aes(x = factor(trt), y = log(os_time), color = factor(os_event), shape = factor(os_event))) +
  geom_point(alpha = 0.5, size = 3) +  
  scale_color_manual(values = c("red", "blue")) +  
  scale_shape_manual(values = c(1, 4)) +  
  labs(x = "Treatment Versus Control", y = "Log(Time)", title = "Scatter Plot of Log Time vs Treatment Versus Control") +
  theme_minimal() +
  theme(legend.title = element_blank()) +  
  guides(color = guide_legend(title = "Event Type"),
         shape = guide_legend(title = "Event Type"))+
  theme(plot.title = element_text(hjust = 0.5))  


## Reload the data
data_final <- read.table("/Users/kangyuxin/Downloads/Lymphoma.csv", sep = ",", header = TRUE, fill = TRUE, quote = "\"", stringsAsFactors = FALSE)


## Log rank test
library(survival)  
logrk <- survdiff(Surv(os_time, os_event)~as.factor(trt), data= data_final)
logrk
"""
Output:

survdiff(formula = Surv(os_time, os_event) ~ as.factor(trt), 
    data = data_final)

                           N Observed Expected (O-E)^2/E (O-E)^2/V
as.factor(trt)=control   309      201      202    0.0105    0.0208
as.factor(trt)=treatment 310      208      207    0.0103    0.0208

 Chisq= 0  on 1 degrees of freedom, p= 0.9 
"""


## Wilcoxon test
wilcx <- survdiff(Surv(os_time, os_event)~as.factor(trt), data = data_final, rho = 1)
wilcx
"""
Output:

Call:
survdiff(formula = Surv(os_time, os_event) ~ as.factor(trt), 
    data = data_final, rho = 1)

                           N Observed Expected (O-E)^2/E (O-E)^2/V
as.factor(trt)=control   309      136      135   0.00797    0.0218
as.factor(trt)=treatment 310      137      138   0.00779    0.0218

 Chisq= 0  on 1 degrees of freedom, p= 0.9 
"""


## K-M and N-A plot of trt
fit_NA <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$trt),ctype = 1, stype = 2)
plot(fit_NA,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("orange", "purple"))
legend(x = "topright",
       legend = c("N-A Estimated S(t) for control group",
                  "N-A Estimated S(t) for treatment group"),
       col = c("orange", "purple"), lty=c(1,1), cex = 0.9)

fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$trt),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for control group",
                  "K-M Estimated S(t) for treatment group"),
       col = c("hotpink", "green"), lty=c(1,1), cex = 0.9)


## K-M plot of PERF_STA
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$PERF_STA),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green","orange", "purple"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for Performance Status = 0",
                  "K-M Estimated S(t) for Performance Status = 1",
                  "K-M Estimated S(t) for Performance Status = 2",
                  "K-M Estimated S(t) for Performance Status = 3"),
       col = c("hotpink", "green", "orange", "purple"), lty=c(1,1), cex = 0.7)


## K-M plot of STAGE
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$STAGE),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green","orange", "purple"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for Cancer Stage = I",
                  "K-M Estimated S(t) for Cancer Stage = II",
                  "K-M Estimated S(t) for Cancer Stage = III",
                  "K-M Estimated S(t) for Cancer Stage = IV"),
       col = c("hotpink", "green", "orange", "purple"), lty=c(1,1), cex = 0.7)


## K-M plot of SEX
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$SEX),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for Female",
                  "K-M Estimated S(t) for Male"),
       col = c("hotpink", "green"), lty=c(1,1), cex = 0.7)


## K-M plot of r_score
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$r_score),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green","orange"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for r-score >=3",
                  "K-M Estimated S(t) for r-score = 0,1",
                  "K-M Estimated S(t) for r-score = 2"),
       col = c("hotpink", "green","orange"), lty=c(1,1), cex = 0.7)


## K-M plot of PR_RAD
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$PR_RAD),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for PR-RAD = N",
                  "K-M Estimated S(t) for PR-RAD = Y"),
       col = c("hotpink", "green"), lty=c(1,1), cex = 0.7)


## K-M plot of pr_drug
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$pr_drug),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for PR-DRUG = N",
                  "K-M Estimated S(t) for PR-DRUG = Y"),
       col = c("hotpink", "green"), lty=c(1,1), cex = 0.7)


## K-M plot of pr_resp
fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$pr_resp),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green","orange"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for PR_RESP DU < 1",
                  "K-M Estimated S(t) for PR_RESP DU > 1",
                  "K-M Estimated S(t) for PR_RESP SDPD"),
       col = c("hotpink", "green","orange"), lty=c(1,1), cex = 0.7)


## K-M plot of B_SYM
data_final$B_SYM[data_final$B_SYM == ""] <- "N"

fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$B_SYM),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for B-symptom = N",
                  "K-M Estimated S(t) for B-symptom = Y"),
       col = c("hotpink", "green"), lty=c(1,1), cex = 0.7)


## K-M plot of RACE
data_final <- data_final[!is.na(data_final$RACE) & data_final$RACE != "", ]

fit_KM <- survfit(Surv(data_final$os_time,data_final$os_event)~ as.factor(data_final$RACE),ctype = 1, stype = 1)
plot(fit_KM,xlab = "Time", ylab = "Estimated S(t)", mark.time = T, conf.int = F,
     col = c("hotpink", "green","orange", "purple","skyblue"))
legend(x = "topright",
       legend = c("K-M Estimated S(t) for Race = 1",
                  "K-M Estimated S(t) for Race = 3",
                  "K-M Estimated S(t) for Race = 5",
                  "K-M Estimated S(t) for Race = 6",
                  "K-M Estimated S(t) for Race = 99"),
       col = c("hotpink", "green","orange", "purple","skyblue"), lty=c(1,1), cex = 0.7)


## Cox Regression without Interaction: Full model
fit_1 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) 
                                          + factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + factor(trt)), data = data_final)
print(fit_1)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + 
    factor(trt)), data = data_final)

                          coef exp(coef)  se(coef)      z        p
factor(PERF_STA)1     0.427568  1.533524  0.119897  3.566 0.000362
factor(PERF_STA)2     0.289977  1.336397  0.197632  1.467 0.142307
factor(PERF_STA)3     0.609438  1.839397  0.280285  2.174 0.029679
AGE                   0.003030  1.003035  0.005303  0.571 0.567744
factor(STAGE)II       0.179987  1.197202  0.235159  0.765 0.444044
factor(STAGE)III     -0.186669  0.829718  0.254307 -0.734 0.462930
factor(STAGE)IV      -0.011602  0.988465  0.254692 -0.046 0.963668
factor(SEX)M          0.122952  1.130830  0.107780  1.141 0.253970
factor(PR_RAD)Y       0.283121  1.327265  0.119364  2.372 0.017696
factor(B_SYM)Y        0.325382  1.384560  0.107700  3.021 0.002518
factor(RACE)3         0.336355  1.399836  0.320694  1.049 0.294254
factor(RACE)5         0.297212  1.346100  0.186263  1.596 0.110565
factor(RACE)6         0.224383  1.251551  0.424169  0.529 0.596808
factor(RACE)99       -0.181653  0.833891  0.392284 -0.463 0.643318
factor(r_score)0, 1  -0.901401  0.406000  0.196803 -4.580 4.64e-06
factor(r_score)2     -0.209308  0.811145  0.141964 -1.474 0.140381
factor(pr_resp)DU>1  -0.998740  0.368343  0.152124 -6.565 5.19e-11
factor(pr_resp)SDPD   0.225920  1.253475  0.117182  1.928 0.053863
factor(pr_drug)Y      0.161289  1.175025  0.122124  1.321 0.186600
factor(trt)treatment -0.045746  0.955285  0.101756 -0.450 0.653025

Likelihood ratio test=198.2  on 20 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## Cox Regression without Interaction: Reduced model including r-score
fit_2 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + factor(PR_RAD) + factor(B_SYM)  + factor(r_score)
                                          + factor(pr_resp)), data = data_final)
print(fit_2)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    factor(PR_RAD) + factor(B_SYM) + factor(r_score) + factor(pr_resp)), 
    data = data_final)

                       coef exp(coef) se(coef)      z        p
factor(PERF_STA)1    0.4277    1.5337   0.1183  3.616 0.000299
factor(PERF_STA)2    0.3330    1.3951   0.1912  1.741 0.081623
factor(PERF_STA)3    0.6251    1.8684   0.2753  2.270 0.023190
factor(PR_RAD)Y      0.3054    1.3572   0.1174  2.602 0.009275
factor(B_SYM)Y       0.3013    1.3516   0.1059  2.844 0.004452
factor(r_score)0, 1 -0.7988    0.4499   0.1389 -5.752 8.82e-09
factor(r_score)2    -0.1945    0.8232   0.1315 -1.480 0.138932
factor(pr_resp)DU>1 -1.0278    0.3578   0.1461 -7.035 1.99e-12
factor(pr_resp)SDPD  0.2650    1.3035   0.1143  2.318 0.020446

Likelihood ratio test=185.4  on 9 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 2
fit_2$loglik # Output: -2353.049 -2260.349


## Cox Regression without Interaction: Reduced model without r-score
fit_3 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + factor(PR_RAD) + factor(B_SYM) + factor(pr_resp)), data = data_final)
print(fit_3)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    factor(PR_RAD) + factor(B_SYM) + factor(pr_resp)), data = data_final)

                       coef exp(coef) se(coef)      z        p
factor(PERF_STA)1    0.5923    1.8082   0.1153  5.136 2.81e-07
factor(PERF_STA)2    0.8005    2.2266   0.1724  4.644 3.42e-06
factor(PERF_STA)3    1.1165    3.0542   0.2613  4.273 1.93e-05
factor(PR_RAD)Y      0.3250    1.3840   0.1178  2.758  0.00582
factor(B_SYM)Y       0.2857    1.3307   0.1049  2.723  0.00647
factor(pr_resp)DU>1 -0.9449    0.3887   0.1453 -6.505 7.78e-11
factor(pr_resp)SDPD  0.2333    1.2628   0.1138  2.051  0.04028

Likelihood ratio test=148.1  on 7 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 3
fit_3$loglik # Output:  -2353.049 -2278.994


## LR test p-value for r-score
1-pchisq(37.29,2) # Output: 7.9906e-09


## Cox Regression with Interaction: Full model
fit_4 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) 
                                          + factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + factor(trt))^2, data = data_final)
print(fit_4)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + 
    factor(trt))^2, data = data_final)

                                               coef  exp(coef)   se(coef)      z        p
factor(PERF_STA)1                         1.927e+00  6.868e+00  1.429e+00  1.348 0.177513
factor(PERF_STA)2                         4.324e+00  7.550e+01  2.993e+00  1.445 0.148528
factor(PERF_STA)3                        -7.948e+00  3.535e-04  4.765e+00 -1.668 0.095299
AGE                                       9.572e-03  1.010e+00  5.828e-02  0.164 0.869545
factor(STAGE)II                          -8.477e-01  4.284e-01  3.671e+00 -0.231 0.817353
factor(STAGE)III                         -4.399e-01  6.441e-01  3.655e+00 -0.120 0.904190
factor(STAGE)IV                          -1.216e+00  2.965e-01  3.601e+00 -0.338 0.735683
factor(SEX)M                             -2.414e+00  8.946e-02  1.299e+00 -1.858 0.063112
factor(PR_RAD)Y                           5.451e+00  2.329e+02  1.562e+00  3.489 0.000485
factor(B_SYM)Y                           -4.170e-01  6.590e-01  1.362e+00 -0.306 0.759418
factor(RACE)3                             1.402e+01  1.223e+06  6.454e+03  0.002 0.998267
factor(RACE)5                             6.500e+00  6.651e+02  3.437e+00  1.891 0.058604
factor(RACE)6                             6.182e+00  4.838e+02  4.049e+04  0.000 0.999878
factor(RACE)99                            2.946e+01  6.206e+12  7.620e+03  0.004 0.996916
factor(r_score)0, 1                      -2.369e+00  9.357e-02  2.694e+00 -0.879 0.379255
factor(r_score)2                         -9.860e-01  3.731e-01  1.348e+00 -0.732 0.464361
factor(pr_resp)DU>1                      -1.814e+00  1.630e-01  1.893e+00 -0.958 0.337905
factor(pr_resp)SDPD                       2.427e+00  1.132e+01  1.454e+00  1.669 0.095105
factor(pr_drug)Y                          2.966e+00  1.942e+01  1.476e+00  2.009 0.044487
factor(trt)treatment                     -1.251e+00  2.861e-01  1.182e+00 -1.059 0.289771
factor(PERF_STA)1:AGE                    -7.072e-03  9.930e-01  1.730e-02 -0.409 0.682767
factor(PERF_STA)2:AGE                    -2.577e-03  9.974e-01  3.100e-02 -0.083 0.933749
factor(PERF_STA)3:AGE                     1.598e-01  1.173e+00  7.503e-02  2.129 0.033228
factor(PERF_STA)1:factor(STAGE)II        -1.650e+00  1.920e-01  6.275e-01 -2.630 0.008539
factor(PERF_STA)2:factor(STAGE)II        -4.977e+00  6.893e-03  2.475e+00 -2.011 0.044349
factor(PERF_STA)3:factor(STAGE)II         5.661e-02  1.058e+00  2.609e+00  0.022 0.982685
factor(PERF_STA)1:factor(STAGE)III       -4.874e-01  6.142e-01  7.557e-01 -0.645 0.518943
factor(PERF_STA)2:factor(STAGE)III       -4.259e+00  1.414e-02  2.264e+00 -1.881 0.059967
factor(PERF_STA)3:factor(STAGE)III        3.026e+00  2.061e+01  1.364e+00  2.219 0.026505
factor(PERF_STA)1:factor(STAGE)IV        -1.161e+00  3.133e-01  7.661e-01 -1.515 0.129808
factor(PERF_STA)2:factor(STAGE)IV        -4.569e+00  1.037e-02  2.220e+00 -2.058 0.039563
factor(PERF_STA)3:factor(STAGE)IV                NA         NA  0.000e+00     NA       NA
factor(PERF_STA)1:factor(SEX)M            7.634e-01  2.145e+00  3.220e-01  2.370 0.017772
factor(PERF_STA)2:factor(SEX)M            1.890e+00  6.622e+00  6.219e-01  3.040 0.002367
factor(PERF_STA)3:factor(SEX)M            5.503e-01  1.734e+00  9.510e-01  0.579 0.562801
factor(PERF_STA)1:factor(PR_RAD)Y        -3.298e-01  7.191e-01  3.606e-01 -0.915 0.360425
factor(PERF_STA)2:factor(PR_RAD)Y        -7.818e-01  4.576e-01  5.544e-01 -1.410 0.158467
factor(PERF_STA)3:factor(PR_RAD)Y         1.843e+00  6.316e+00  1.823e+00  1.011 0.311946
factor(PERF_STA)1:factor(B_SYM)Y          2.047e-01  1.227e+00  3.266e-01  0.627 0.530909
factor(PERF_STA)2:factor(B_SYM)Y          1.997e-01  1.221e+00  5.496e-01  0.363 0.716354
factor(PERF_STA)3:factor(B_SYM)Y          1.006e+00  2.735e+00  8.171e-01  1.231 0.218245
factor(PERF_STA)1:factor(RACE)3          -1.561e+01  1.663e-07  4.717e+03 -0.003 0.997360
factor(PERF_STA)2:factor(RACE)3          -8.431e+00  2.179e-04  2.701e+03 -0.003 0.997509
factor(PERF_STA)3:factor(RACE)3                  NA         NA  0.000e+00     NA       NA
factor(PERF_STA)1:factor(RACE)5           5.489e-01  1.731e+00  7.386e-01  0.743 0.457394
factor(PERF_STA)2:factor(RACE)5          -3.083e-01  7.347e-01  1.168e+00 -0.264 0.791857
factor(PERF_STA)3:factor(RACE)5           3.305e+00  2.726e+01  1.673e+00  1.975 0.048212
factor(PERF_STA)1:factor(RACE)6          -4.631e+00  9.745e-03  9.744e+03  0.000 0.999621
factor(PERF_STA)2:factor(RACE)6                  NA         NA  0.000e+00     NA       NA
factor(PERF_STA)3:factor(RACE)6          -4.399e+00  1.229e-02  2.450e+04  0.000 0.999857
factor(PERF_STA)1:factor(RACE)99          5.974e+01  8.832e+25  9.590e+03  0.006 0.995029
factor(PERF_STA)2:factor(RACE)99                 NA         NA  0.000e+00     NA       NA
factor(PERF_STA)3:factor(RACE)99                 NA         NA  0.000e+00     NA       NA
factor(PERF_STA)1:factor(r_score)0, 1    -4.098e-01  6.638e-01  6.363e-01 -0.644 0.519575
factor(PERF_STA)2:factor(r_score)0, 1    -1.795e+01  1.606e-08  3.732e+03 -0.005 0.996163
factor(PERF_STA)3:factor(r_score)0, 1            NA         NA  0.000e+00     NA       NA
factor(PERF_STA)1:factor(r_score)2       -5.208e-01  5.940e-01  4.467e-01 -1.166 0.243634
factor(PERF_STA)2:factor(r_score)2       -4.570e-01  6.332e-01  9.298e-01 -0.491 0.623089
factor(PERF_STA)3:factor(r_score)2        1.970e+00  7.174e+00  1.314e+00  1.499 0.133775
factor(PERF_STA)1:factor(pr_resp)DU>1    -1.345e-01  8.741e-01  4.747e-01 -0.283 0.776856
factor(PERF_STA)2:factor(pr_resp)DU>1    -1.113e-01  8.947e-01  7.843e-01 -0.142 0.887161
factor(PERF_STA)3:factor(pr_resp)DU>1    -2.028e+00  1.316e-01  1.810e+00 -1.121 0.262472
factor(PERF_STA)1:factor(pr_resp)SDPD     4.017e-01  1.494e+00  3.505e-01  1.146 0.251754
factor(PERF_STA)2:factor(pr_resp)SDPD    -2.447e-01  7.830e-01  6.090e-01 -0.402 0.687869
factor(PERF_STA)3:factor(pr_resp)SDPD    -1.851e+00  1.571e-01  1.532e+00 -1.208 0.226880
factor(PERF_STA)1:factor(pr_drug)Y       -1.078e-01  8.978e-01  3.556e-01 -0.303 0.761775
factor(PERF_STA)2:factor(pr_drug)Y        5.262e-01  1.693e+00  6.208e-01  0.848 0.396667
factor(PERF_STA)3:factor(pr_drug)Y       -1.017e+00  3.617e-01  1.430e+00 -0.711 0.476940
factor(PERF_STA)1:factor(trt)treatment   -8.357e-02  9.198e-01  3.020e-01 -0.277 0.781960
factor(PERF_STA)2:factor(trt)treatment    1.832e-01  1.201e+00  5.187e-01  0.353 0.723956
factor(PERF_STA)3:factor(trt)treatment    4.601e-01  1.584e+00  9.146e-01  0.503 0.614900
AGE:factor(STAGE)II                       1.950e-02  1.020e+00  4.289e-02  0.455 0.649450
AGE:factor(STAGE)III                      2.484e-02  1.025e+00  5.052e-02  0.492 0.622955
AGE:factor(STAGE)IV                       5.070e-02  1.052e+00  4.992e-02  1.016 0.309777
AGE:factor(SEX)M                         -7.429e-03  9.926e-01  1.554e-02 -0.478 0.632581
AGE:factor(PR_RAD)Y                      -4.214e-02  9.587e-01  1.572e-02 -2.681 0.007338
AGE:factor(B_SYM)Y                        1.724e-02  1.017e+00  1.547e-02  1.114 0.265120
AGE:factor(RACE)3                         3.278e-01  1.388e+00  8.382e+01  0.004 0.996880
AGE:factor(RACE)5                        -2.918e-02  9.712e-01  3.638e-02 -0.802 0.422594
AGE:factor(RACE)6                        -6.716e-02  9.350e-01  9.461e+02  0.000 0.999943
AGE:factor(RACE)99                       -4.732e-01  6.230e-01  2.354e-01 -2.010 0.044417
AGE:factor(r_score)0, 1                   4.305e-02  1.044e+00  3.286e-02  1.310 0.190091
AGE:factor(r_score)2                      1.172e-02  1.012e+00  2.083e-02  0.563 0.573556
AGE:factor(pr_resp)DU>1                   2.290e-02  1.023e+00  2.296e-02  0.997 0.318683
AGE:factor(pr_resp)SDPD                  -2.859e-02  9.718e-01  1.570e-02 -1.821 0.068650
AGE:factor(pr_drug)Y                     -4.393e-02  9.570e-01  1.897e-02 -2.316 0.020543
AGE:factor(trt)treatment                  2.959e-03  1.003e+00  1.394e-02  0.212 0.831874
factor(STAGE)II:factor(SEX)M              8.909e-01  2.437e+00  5.950e-01  1.497 0.134295
factor(STAGE)III:factor(SEX)M             2.377e+00  1.077e+01  7.881e-01  3.016 0.002561
factor(STAGE)IV:factor(SEX)M              2.323e+00  1.021e+01  7.990e-01  2.908 0.003638
factor(STAGE)II:factor(PR_RAD)Y           4.289e-01  1.536e+00  8.007e-01  0.536 0.592211
factor(STAGE)III:factor(PR_RAD)Y         -6.766e-01  5.083e-01  1.029e+00 -0.657 0.510955
factor(STAGE)IV:factor(PR_RAD)Y          -1.312e+00  2.692e-01  1.085e+00 -1.209 0.226574
factor(STAGE)II:factor(B_SYM)Y           -1.098e+00  3.336e-01  6.850e-01 -1.603 0.109000
factor(STAGE)III:factor(B_SYM)Y          -6.910e-01  5.011e-01  8.611e-01 -0.802 0.422275
factor(STAGE)IV:factor(B_SYM)Y            3.739e-01  1.453e+00  8.677e-01  0.431 0.666557
factor(STAGE)II:factor(RACE)3             2.257e+01  6.325e+09  9.018e+03  0.003 0.998003
factor(STAGE)III:factor(RACE)3            2.933e+01  5.467e+12  1.510e+04  0.002 0.998450
factor(STAGE)IV:factor(RACE)3             1.868e+01  1.297e+08  1.035e+04  0.002 0.998560
factor(STAGE)II:factor(RACE)5            -1.380e+00  2.517e-01  1.121e+00 -1.231 0.218271
factor(STAGE)III:factor(RACE)5           -3.752e+00  2.347e-02  1.967e+00 -1.907 0.056474
factor(STAGE)IV:factor(RACE)5            -2.830e+00  5.901e-02  1.661e+00 -1.703 0.088497
factor(STAGE)II:factor(RACE)6                    NA         NA  0.000e+00     NA       NA
factor(STAGE)III:factor(RACE)6            1.500e+00  4.484e+00  2.280e+04  0.000 0.999947
factor(STAGE)IV:factor(RACE)6                    NA         NA  0.000e+00     NA       NA
factor(STAGE)II:factor(RACE)99           -3.800e+01  3.124e-17  4.047e+03 -0.009 0.992507
factor(STAGE)III:factor(RACE)99          -1.573e+00  2.075e-01  1.486e+00 -1.058 0.290086
factor(STAGE)IV:factor(RACE)99                   NA         NA  0.000e+00     NA       NA
factor(STAGE)II:factor(r_score)0, 1       1.207e+00  3.344e+00  2.288e+00  0.527 0.597853
factor(STAGE)III:factor(r_score)0, 1     -2.538e+00  7.904e-02  1.621e+00 -1.565 0.117468
factor(STAGE)IV:factor(r_score)0, 1      -2.077e+00  1.253e-01  1.616e+00 -1.285 0.198895
factor(STAGE)II:factor(r_score)2          2.686e+00  1.468e+01  1.817e+00  1.478 0.139294
factor(STAGE)III:factor(r_score)2        -4.235e-01  6.548e-01  4.198e-01 -1.009 0.313136
factor(STAGE)IV:factor(r_score)2                 NA         NA  0.000e+00     NA       NA
factor(STAGE)II:factor(pr_resp)DU>1      -1.243e+00  2.885e-01  9.028e-01 -1.377 0.168600
factor(STAGE)III:factor(pr_resp)DU>1     -1.058e+00  3.473e-01  1.117e+00 -0.947 0.343797
factor(STAGE)IV:factor(pr_resp)DU>1      -1.229e+00  2.926e-01  1.108e+00 -1.109 0.267224
factor(STAGE)II:factor(pr_resp)SDPD       7.197e-01  2.054e+00  7.283e-01  0.988 0.323067
factor(STAGE)III:factor(pr_resp)SDPD      3.452e-01  1.412e+00  8.576e-01  0.403 0.687303
factor(STAGE)IV:factor(pr_resp)SDPD       9.609e-02  1.101e+00  8.588e-01  0.112 0.910913
factor(STAGE)II:factor(pr_drug)Y         -4.176e-01  6.586e-01  6.643e-01 -0.629 0.529546
factor(STAGE)III:factor(pr_drug)Y         1.328e-01  1.142e+00  8.300e-01  0.160 0.872883
factor(STAGE)IV:factor(pr_drug)Y         -8.882e-01  4.114e-01  8.298e-01 -1.070 0.284499
factor(STAGE)II:factor(trt)treatment     -3.405e-01  7.114e-01  5.802e-01 -0.587 0.557237
factor(STAGE)III:factor(trt)treatment     1.803e-01  1.198e+00  7.044e-01  0.256 0.798017
factor(STAGE)IV:factor(trt)treatment      7.465e-01  2.110e+00  7.003e-01  1.066 0.286399
factor(SEX)M:factor(PR_RAD)Y              1.491e-01  1.161e+00  3.674e-01  0.406 0.684917
factor(SEX)M:factor(B_SYM)Y              -3.846e-01  6.807e-01  2.959e-01 -1.300 0.193738
factor(SEX)M:factor(RACE)3               -3.057e+01  5.299e-14  1.098e+04 -0.003 0.997779
factor(SEX)M:factor(RACE)5                6.928e-01  1.999e+00  5.179e-01  1.338 0.180993
factor(SEX)M:factor(RACE)6                       NA         NA  0.000e+00     NA       NA
factor(SEX)M:factor(RACE)99              -7.260e+00  7.033e-04  7.620e+03 -0.001 0.999240
factor(SEX)M:factor(r_score)0, 1          1.456e+00  4.289e+00  6.416e-01  2.269 0.023240
factor(SEX)M:factor(r_score)2             5.257e-01  1.692e+00  3.795e-01  1.385 0.165949
factor(SEX)M:factor(pr_resp)DU>1         -6.556e-01  5.191e-01  4.009e-01 -1.636 0.101926
factor(SEX)M:factor(pr_resp)SDPD         -5.300e-01  5.886e-01  3.267e-01 -1.622 0.104787
factor(SEX)M:factor(pr_drug)Y             2.418e-01  1.274e+00  3.228e-01  0.749 0.453804
factor(SEX)M:factor(trt)treatment         3.891e-01  1.476e+00  2.757e-01  1.411 0.158202
factor(PR_RAD)Y:factor(B_SYM)Y           -6.755e-01  5.089e-01  3.593e-01 -1.880 0.060076
factor(PR_RAD)Y:factor(RACE)3             1.295e+01  4.199e+05  1.026e+04  0.001 0.998993
factor(PR_RAD)Y:factor(RACE)5            -7.989e-01  4.498e-01  6.655e-01 -1.200 0.229996
factor(PR_RAD)Y:factor(RACE)6            -1.383e+01  9.882e-07  7.280e+03 -0.002 0.998485
factor(PR_RAD)Y:factor(RACE)99           -2.947e+01  1.587e-13  3.903e+03 -0.008 0.993975
factor(PR_RAD)Y:factor(r_score)0, 1      -1.608e+00  2.003e-01  8.300e-01 -1.937 0.052736
factor(PR_RAD)Y:factor(r_score)2         -1.381e+00  2.514e-01  4.999e-01 -2.762 0.005749
factor(PR_RAD)Y:factor(pr_resp)DU>1      -3.140e-03  9.969e-01  4.051e-01 -0.008 0.993816
factor(PR_RAD)Y:factor(pr_resp)SDPD      -1.641e-01  8.487e-01  3.981e-01 -0.412 0.680270
factor(PR_RAD)Y:factor(pr_drug)Y         -3.444e-01  7.087e-01  3.504e-01 -0.983 0.325676
factor(PR_RAD)Y:factor(trt)treatment     -7.640e-01  4.658e-01  3.085e-01 -2.477 0.013264
factor(B_SYM)Y:factor(RACE)3              3.302e-01  1.391e+00  3.684e+03  0.000 0.999928
factor(B_SYM)Y:factor(RACE)5              1.035e+00  2.816e+00  7.368e-01  1.405 0.159949
factor(B_SYM)Y:factor(RACE)6              1.132e+00  3.102e+00  5.771e+03  0.000 0.999843
factor(B_SYM)Y:factor(RACE)99            -2.781e+01  8.384e-13  4.047e+03 -0.007 0.994518
factor(B_SYM)Y:factor(r_score)0, 1        1.148e+00  3.151e+00  6.876e-01  1.669 0.095104
factor(B_SYM)Y:factor(r_score)2           8.072e-01  2.242e+00  4.042e-01  1.997 0.045823
factor(B_SYM)Y:factor(pr_resp)DU>1        4.290e-01  1.536e+00  4.227e-01  1.015 0.310149
factor(B_SYM)Y:factor(pr_resp)SDPD       -3.808e-02  9.626e-01  3.189e-01 -0.119 0.904930
factor(B_SYM)Y:factor(pr_drug)Y          -4.839e-01  6.164e-01  3.016e-01 -1.604 0.108624
factor(B_SYM)Y:factor(trt)treatment       4.304e-02  1.044e+00  2.714e-01  0.159 0.874002
factor(RACE)3:factor(r_score)0, 1        -1.919e+01  4.614e-09  5.848e+03 -0.003 0.997381
factor(RACE)5:factor(r_score)0, 1        -3.336e+00  3.559e-02  2.136e+00 -1.561 0.118433
factor(RACE)6:factor(r_score)0, 1        -2.223e+00  1.082e-01  1.419e+03 -0.002 0.998750
factor(RACE)99:factor(r_score)0, 1        1.686e+01  2.101e+07  5.505e+03  0.003 0.997556
factor(RACE)3:factor(r_score)2            1.349e+01  7.211e+05  1.065e+04  0.001 0.998990
factor(RACE)5:factor(r_score)2           -2.086e+00  1.241e-01  1.233e+00 -1.693 0.090527
factor(RACE)6:factor(r_score)2                   NA         NA  0.000e+00     NA       NA
factor(RACE)99:factor(r_score)2                  NA         NA  0.000e+00     NA       NA
factor(RACE)3:factor(pr_resp)DU>1        -4.204e+01  5.527e-19  1.210e+04 -0.003 0.997227
factor(RACE)5:factor(pr_resp)DU>1        -8.327e-01  4.349e-01  1.008e+00 -0.826 0.408924
factor(RACE)6:factor(pr_resp)DU>1                NA         NA  0.000e+00     NA       NA
factor(RACE)99:factor(pr_resp)DU>1        3.218e+00  2.497e+01  7.620e+03  0.000 0.999663
factor(RACE)3:factor(pr_resp)SDPD        -2.244e+00  1.060e-01  4.316e+03 -0.001 0.999585
factor(RACE)5:factor(pr_resp)SDPD        -9.684e-01  3.797e-01  8.870e-01 -1.092 0.274941
factor(RACE)6:factor(pr_resp)SDPD                NA         NA  0.000e+00     NA       NA
factor(RACE)99:factor(pr_resp)SDPD       -2.991e+01  1.022e-13  6.545e+03 -0.005 0.996354
factor(RACE)3:factor(pr_drug)Y           -4.106e+01  1.473e-18  1.762e+04 -0.002 0.998141
factor(RACE)5:factor(pr_drug)Y           -8.472e-01  4.286e-01  6.904e-01 -1.227 0.219759
factor(RACE)6:factor(pr_drug)Y                   NA         NA  0.000e+00     NA       NA
factor(RACE)99:factor(pr_drug)Y                  NA         NA  0.000e+00     NA       NA
factor(RACE)3:factor(trt)treatment        2.650e-01  1.303e+00  3.590e+03  0.000 0.999941
factor(RACE)5:factor(trt)treatment       -2.425e-01  7.847e-01  5.703e-01 -0.425 0.670711
factor(RACE)6:factor(trt)treatment               NA         NA  0.000e+00     NA       NA
factor(RACE)99:factor(trt)treatment              NA         NA  0.000e+00     NA       NA
factor(r_score)0, 1:factor(pr_resp)DU>1  -2.291e-01  7.952e-01  8.731e-01 -0.262 0.792983
factor(r_score)2:factor(pr_resp)DU>1      7.964e-01  2.218e+00  5.189e-01  1.535 0.124816
factor(r_score)0, 1:factor(pr_resp)SDPD  -9.481e-01  3.875e-01  6.219e-01 -1.524 0.127385
factor(r_score)2:factor(pr_resp)SDPD      7.561e-01  2.130e+00  4.293e-01  1.761 0.078206
factor(r_score)0, 1:factor(pr_drug)Y     -1.337e-01  8.748e-01  6.490e-01 -0.206 0.836771
factor(r_score)2:factor(pr_drug)Y        -3.114e-01  7.324e-01  4.130e-01 -0.754 0.450814
factor(r_score)0, 1:factor(trt)treatment  1.078e+00  2.940e+00  5.537e-01  1.947 0.051493
factor(r_score)2:factor(trt)treatment     2.352e-01  1.265e+00  3.684e-01  0.639 0.523136
factor(pr_resp)DU>1:factor(pr_drug)Y      8.205e-01  2.272e+00  3.877e-01  2.117 0.034301
factor(pr_resp)SDPD:factor(pr_drug)Y     -3.398e-01  7.119e-01  3.808e-01 -0.892 0.372260
factor(pr_resp)DU>1:factor(trt)treatment  2.192e-01  1.245e+00  3.846e-01  0.570 0.568611
factor(pr_resp)SDPD:factor(trt)treatment -1.599e-01  8.523e-01  2.939e-01 -0.544 0.586427
factor(pr_drug)Y:factor(trt)treatment     3.476e-01  1.416e+00  3.019e-01  1.151 0.249527

Likelihood ratio test=425.1  on 177 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## Cox Regression with Interaction: Reduced model 1
fit_5 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + factor(RACE) 
                                          + factor(r_score) + factor(pr_resp) + factor(pr_drug) + factor(trt)+ factor(PERF_STA)*AGE + factor(PERF_STA)*factor(STAGE) 
                                          + factor(PERF_STA)*factor(SEX)+ factor(PERF_STA)*factor(RACE) + AGE*factor(PR_RAD) + AGE*factor(RACE) + AGE*factor(pr_drug) 
                                          + factor(STAGE)*factor(SEX)+ factor(SEX)*factor(r_score) + factor(PR_RAD)* factor(r_score) + factor(PR_RAD)* factor(trt) 
                                          + factor(B_SYM)*factor(r_score) + factor(pr_resp)*factor(pr_drug)), data = data_final)
print(fit_5)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + 
    factor(trt) + factor(PERF_STA) * AGE + factor(PERF_STA) * 
    factor(STAGE) + factor(PERF_STA) * factor(SEX) + factor(PERF_STA) * 
    factor(RACE) + AGE * factor(PR_RAD) + AGE * factor(RACE) + 
    AGE * factor(pr_drug) + factor(STAGE) * factor(SEX) + factor(SEX) * 
    factor(r_score) + factor(PR_RAD) * factor(r_score) + factor(PR_RAD) * 
    factor(trt) + factor(B_SYM) * factor(r_score) + factor(pr_resp) * 
    factor(pr_drug)), data = data_final)

                                           coef  exp(coef)   se(coef)      z        p
factor(PERF_STA)1                     1.6733405  5.3299427  0.7575986  2.209 0.027193
factor(PERF_STA)2                     2.0925792  8.1057947  1.6884158  1.239 0.215207
factor(PERF_STA)3                    -7.6897639  0.0004575  3.1667436 -2.428 0.015170
AGE                                   0.0287390  1.0291560  0.0141776  2.027 0.042654
factor(STAGE)II                       0.8879944  2.4302507  0.4011516  2.214 0.026855
factor(STAGE)III                     -0.6481323  0.5230217  0.4580027 -1.415 0.157031
factor(STAGE)IV                      -0.1231104  0.8841661  0.4250943 -0.290 0.772117
factor(SEX)M                         -0.9778336  0.3761251  0.6330860 -1.545 0.122455
factor(PR_RAD)Y                       0.7048007  2.0234434  0.6743565  1.045 0.295956
factor(B_SYM)Y                        0.3980741  1.4889543  0.1870815  2.128 0.033353
factor(RACE)3                        -1.2660640  0.2819392  1.3849645 -0.914 0.360639
factor(RACE)5                         0.1008923  1.1061575  0.8354127  0.121 0.903874
factor(RACE)6                         2.6086450 13.5806362  2.4522387  1.064 0.287428
factor(RACE)99                        0.4217534  1.5246325  2.1962538  0.192 0.847716
factor(r_score)0, 1                  -1.3465000  0.2601492  0.3646772 -3.692 0.000222
factor(r_score)2                     -0.4851797  0.6155865  0.2617705 -1.853 0.063817
factor(pr_resp)DU>1                  -1.4082027  0.2445825  0.2536296 -5.552 2.82e-08
factor(pr_resp)SDPD                   0.2890431  1.3351493  0.2644425  1.093 0.274381
factor(pr_drug)Y                      1.0208455  2.7755405  0.6571126  1.554 0.120296
factor(trt)treatment                  0.0718652  1.0745105  0.1261414  0.570 0.568868
factor(PERF_STA)1:AGE                -0.0161155  0.9840137  0.0112959 -1.427 0.153674
factor(PERF_STA)2:AGE                -0.0112960  0.9887676  0.0204809 -0.552 0.581266
factor(PERF_STA)3:AGE                 0.1435680  1.1543853  0.0531874  2.699 0.006949
factor(PERF_STA)1:factor(STAGE)II    -1.0927274  0.3353008  0.5161754 -2.117 0.034262
factor(PERF_STA)2:factor(STAGE)II    -4.2216822  0.0146739  1.4028357 -3.009 0.002618
factor(PERF_STA)3:factor(STAGE)II    -0.4050416  0.6669491  1.1531334 -0.351 0.725399
factor(PERF_STA)1:factor(STAGE)III   -0.2957451  0.7439771  0.5331661 -0.555 0.579103
factor(PERF_STA)2:factor(STAGE)III   -1.5823385  0.2054940  1.2007187 -1.318 0.187562
factor(PERF_STA)3:factor(STAGE)III    3.5289172 34.0870383  0.9352571  3.773 0.000161
factor(PERF_STA)1:factor(STAGE)IV    -0.4350062  0.6472606  0.5035626 -0.864 0.387666
factor(PERF_STA)2:factor(STAGE)IV    -1.5101567  0.2208754  1.1777612 -1.282 0.199763
factor(PERF_STA)3:factor(STAGE)IV            NA         NA  0.0000000     NA       NA
factor(PERF_STA)1:factor(SEX)M        0.3014062  1.3517583  0.2592183  1.163 0.244931
factor(PERF_STA)2:factor(SEX)M        1.1929544  3.2968068  0.4336743  2.751 0.005945
factor(PERF_STA)3:factor(SEX)M        0.4718136  1.6028985  0.6620798  0.713 0.476079
factor(PERF_STA)1:factor(RACE)3       0.4135140  1.5121221  0.7615445  0.543 0.587134
factor(PERF_STA)2:factor(RACE)3       0.8895358  2.4339994  1.0017701  0.888 0.374560
factor(PERF_STA)3:factor(RACE)3              NA         NA  0.0000000     NA       NA
factor(PERF_STA)1:factor(RACE)5       0.2060671  1.2288357  0.4473115  0.461 0.645029
factor(PERF_STA)2:factor(RACE)5      -0.3398869  0.7118508  0.6084208 -0.559 0.576409
factor(PERF_STA)3:factor(RACE)5       4.5086051 90.7950824  1.0452974  4.313 1.61e-05
factor(PERF_STA)1:factor(RACE)6      -2.3708107  0.0934050  0.9962427 -2.380 0.017324
factor(PERF_STA)2:factor(RACE)6              NA         NA  0.0000000     NA       NA
factor(PERF_STA)3:factor(RACE)6      -2.6975569  0.0673699  1.5175048 -1.778 0.075465
factor(PERF_STA)1:factor(RACE)99      0.2209254  1.2472304  0.9058865  0.244 0.807326
factor(PERF_STA)2:factor(RACE)99             NA         NA  0.0000000     NA       NA
factor(PERF_STA)3:factor(RACE)99             NA         NA  0.0000000     NA       NA
AGE:factor(PR_RAD)Y                  -0.0027065  0.9972972  0.0117286 -0.231 0.817504
AGE:factor(RACE)3                     0.0311544  1.0316447  0.0292404  1.065 0.286670
AGE:factor(RACE)5                     0.0034137  1.0034195  0.0144880  0.236 0.813727
AGE:factor(RACE)6                    -0.0184768  0.9816928  0.0552740 -0.334 0.738171
AGE:factor(RACE)99                   -0.0201032  0.9800975  0.0447262 -0.449 0.653091
AGE:factor(pr_drug)Y                 -0.0186385  0.9815341  0.0120146 -1.551 0.120824
factor(STAGE)II:factor(SEX)M          0.1807078  1.1980650  0.5244142  0.345 0.730403
factor(STAGE)III:factor(SEX)M         0.9765760  2.6553488  0.5816784  1.679 0.093173
factor(STAGE)IV:factor(SEX)M          0.6003460  1.8227494  0.5807915  1.034 0.301291
factor(SEX)M:factor(r_score)0, 1      0.5981289  1.8187125  0.4302402  1.390 0.164462
factor(SEX)M:factor(r_score)2         0.5319922  1.7023203  0.3031663  1.755 0.079296
factor(PR_RAD)Y:factor(r_score)0, 1   0.4924076  1.6362510  0.3053123  1.613 0.106788
factor(PR_RAD)Y:factor(r_score)2     -0.2112935  0.8095364  0.3031388 -0.697 0.485791
factor(PR_RAD)Y:factor(trt)treatment -0.6446215  0.5248612  0.2478791 -2.601 0.009308
factor(B_SYM)Y:factor(r_score)0, 1   -0.2572616  0.7731660  0.2817313 -0.913 0.361166
factor(B_SYM)Y:factor(r_score)2       0.2255984  1.2530724  0.2875833  0.784 0.432769
factor(pr_resp)DU>1:factor(pr_drug)Y  0.4977449  1.6450074  0.3145455  1.582 0.113552
factor(pr_resp)SDPD:factor(pr_drug)Y  0.0466090  1.0477123  0.2961322  0.157 0.874935

Likelihood ratio test=281.9  on 60 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## Cox Regression with Interaction: Reduced model 2
fit_6 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) +
                                            factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + factor(trt)
                                          + factor(PERF_STA)*factor(STAGE) + factor(PERF_STA)*factor(SEX)+ factor(PR_RAD)* factor(trt) ), data = data_final)
print(fit_6)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + 
    factor(trt) + factor(PERF_STA) * factor(STAGE) + factor(PERF_STA) * 
    factor(SEX) + factor(PR_RAD) * factor(trt)), data = data_final)

                                          coef exp(coef)  se(coef)      z        p
factor(PERF_STA)1                     0.702816  2.019432  0.439748  1.598 0.109993
factor(PERF_STA)2                     2.053349  7.793960  1.076490  1.907 0.056462
factor(PERF_STA)3                     0.817328  2.264442  0.433532  1.885 0.059392
AGE                                   0.004482  1.004493  0.005475  0.819 0.412929
factor(STAGE)II                       0.800167  2.225912  0.334112  2.395 0.016625
factor(STAGE)III                     -0.121148  0.885903  0.364285 -0.333 0.739464
factor(STAGE)IV                       0.117738  1.124949  0.350929  0.336 0.737246
factor(SEX)M                         -0.048911  0.952266  0.186558 -0.262 0.793186
factor(PR_RAD)Y                       0.575031  1.777185  0.169039  3.402 0.000670
factor(B_SYM)Y                        0.319016  1.375774  0.110859  2.878 0.004006
factor(RACE)3                         0.326383  1.385946  0.324611  1.005 0.314676
factor(RACE)5                         0.309455  1.362682  0.189397  1.634 0.102281
factor(RACE)6                        -0.057577  0.944049  0.449443 -0.128 0.898064
factor(RACE)99                       -0.194672  0.823105  0.395726 -0.492 0.622764
factor(r_score)0, 1                  -0.948439  0.387345  0.209176 -4.534 5.78e-06
factor(r_score)2                     -0.138926  0.870292  0.146448 -0.949 0.342803
factor(pr_resp)DU>1                  -1.071726  0.342417  0.153615 -6.977 3.02e-12
factor(pr_resp)SDPD                   0.258733  1.295288  0.120000  2.156 0.031075
factor(pr_drug)Y                      0.144440  1.155393  0.124123  1.164 0.244552
factor(trt)treatment                  0.092813  1.097256  0.121535  0.764 0.445063
factor(PERF_STA)1:factor(STAGE)II    -0.903370  0.405202  0.481945 -1.874 0.060872
factor(PERF_STA)2:factor(STAGE)II    -4.546233  0.010607  1.325455 -3.430 0.000604
factor(PERF_STA)3:factor(STAGE)II     0.339418  1.404131  1.116624  0.304 0.761152
factor(PERF_STA)1:factor(STAGE)III   -0.220967  0.801743  0.491616 -0.449 0.653092
factor(PERF_STA)2:factor(STAGE)III   -2.078046  0.125175  1.151801 -1.804 0.071204
factor(PERF_STA)3:factor(STAGE)III    2.253089  9.517085  0.756803  2.977 0.002910
factor(PERF_STA)1:factor(STAGE)IV    -0.318566  0.727191  0.461678 -0.690 0.490183
factor(PERF_STA)2:factor(STAGE)IV    -2.048087  0.128981  1.116151 -1.835 0.066512
factor(PERF_STA)3:factor(STAGE)IV           NA        NA  0.000000     NA       NA
factor(PERF_STA)1:factor(SEX)M        0.260212  1.297205  0.238745  1.090 0.275750
factor(PERF_STA)2:factor(SEX)M        0.997203  2.710688  0.375164  2.658 0.007859
factor(PERF_STA)3:factor(SEX)M       -0.642435  0.526010  0.573497 -1.120 0.262626
factor(PR_RAD)Y:factor(trt)treatment -0.616530  0.539814  0.242208 -2.545 0.010913

Likelihood ratio test=235  on 32 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 6
fit_6$loglik # Output: -2353.049 -2235.548


## Cox Regression with Interaction: Reduced model 2 without factor(PERF_STA)*factor(SEX)
fit_7 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) 
                                          + factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + factor(trt) 
                                          + factor(PERF_STA)*factor(STAGE)  + factor(PR_RAD)* factor(trt)), data = data_final)
print(fit_7)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + 
    factor(trt) + factor(PERF_STA) * factor(STAGE) + factor(PR_RAD) * 
    factor(trt)), data = data_final)

                                          coef exp(coef)  se(coef)      z        p
factor(PERF_STA)1                     0.804106  2.234697  0.422915  1.901  0.05726
factor(PERF_STA)2                     2.123356  8.359144  1.076943  1.972  0.04865
factor(PERF_STA)3                     0.475681  1.609110  0.331365  1.436  0.15114
AGE                                   0.005098  1.005111  0.005459  0.934  0.35036
factor(STAGE)II                       0.710250  2.034500  0.329915  2.153  0.03133
factor(STAGE)III                     -0.201227  0.817727  0.360617 -0.558  0.57684
factor(STAGE)IV                       0.043515  1.044476  0.348199  0.125  0.90055
factor(SEX)M                          0.171749  1.187380  0.109720  1.565  0.11750
factor(PR_RAD)Y                       0.523597  1.688089  0.167992  3.117  0.00183
factor(B_SYM)Y                        0.339927  1.404845  0.110310  3.082  0.00206
factor(RACE)3                         0.361600  1.435624  0.323932  1.116  0.26430
factor(RACE)5                         0.267661  1.306904  0.189018  1.416  0.15676
factor(RACE)6                        -0.066392  0.935764  0.452399 -0.147  0.88333
factor(RACE)99                       -0.165696  0.847304  0.393354 -0.421  0.67358
factor(r_score)0, 1                  -0.934519  0.392775  0.209401 -4.463 8.09e-06
factor(r_score)2                     -0.127994  0.879859  0.147149 -0.870  0.38440
factor(pr_resp)DU>1                  -1.042748  0.352485  0.152940 -6.818 9.23e-12
factor(pr_resp)SDPD                   0.234495  1.264270  0.118898  1.972  0.04858
factor(pr_drug)Y                      0.158551  1.171812  0.122722  1.292  0.19637
factor(trt)treatment                  0.081772  1.085208  0.121536  0.673  0.50106
factor(PERF_STA)1:factor(STAGE)II    -0.822062  0.439524  0.479559 -1.714  0.08649
factor(PERF_STA)2:factor(STAGE)II    -3.693687  0.024880  1.283076 -2.879  0.00399
factor(PERF_STA)3:factor(STAGE)II    -0.037584  0.963113  1.083046 -0.035  0.97232
factor(PERF_STA)1:factor(STAGE)III   -0.154397  0.856932  0.489851 -0.315  0.75262
factor(PERF_STA)2:factor(STAGE)III   -1.646462  0.192731  1.135784 -1.450  0.14716
factor(PERF_STA)3:factor(STAGE)III    1.957103  7.078792  0.738371  2.651  0.00804
factor(PERF_STA)1:factor(STAGE)IV    -0.255354  0.774642  0.460280 -0.555  0.57905
factor(PERF_STA)2:factor(STAGE)IV    -1.484984  0.226506  1.090818 -1.361  0.17340
factor(PERF_STA)3:factor(STAGE)IV           NA        NA  0.000000     NA       NA
factor(PR_RAD)Y:factor(trt)treatment -0.506900  0.602360  0.238600 -2.124  0.03363

Likelihood ratio test=225.1  on 29 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 7 
fit_7$loglik # Output: -2353.049 -2240.491


## LR test p-value for PERF_STA * SEX
1-pchisq(9.886,3) # Output: 0.01956045


## Cox Regression with Interaction: Reduced model 2 without factor(PERF_STA)*factor(STAGE)
fit_8 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) 
                                          + factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + factor(trt)  
                                          + factor(PERF_STA)*factor(SEX) + factor(PR_RAD)* factor(trt)), data = data_final)
print(fit_8)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    AGE + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(RACE) + factor(r_score) + factor(pr_resp) + factor(pr_drug) + 
    factor(trt) + factor(PERF_STA) * factor(SEX) + factor(PR_RAD) * 
    factor(trt)), data = data_final)

                                          coef exp(coef)  se(coef)      z        p
factor(PERF_STA)1                     0.299132  1.348688  0.189528  1.578  0.11450
factor(PERF_STA)2                    -0.047692  0.953427  0.310302 -0.154  0.87785
factor(PERF_STA)3                     0.787604  2.198124  0.419080  1.879  0.06019
AGE                                   0.003574  1.003581  0.005401  0.662  0.50815
factor(STAGE)II                       0.220059  1.246150  0.237766  0.926  0.35469
factor(STAGE)III                     -0.118801  0.887984  0.256970 -0.462  0.64385
factor(STAGE)IV                       0.048642  1.049845  0.256839  0.189  0.84979
factor(SEX)M                         -0.019267  0.980918  0.185874 -0.104  0.91744
factor(PR_RAD)Y                       0.543792  1.722526  0.169219  3.214  0.00131
factor(B_SYM)Y                        0.318273  1.374752  0.109094  2.917  0.00353
factor(RACE)3                         0.296117  1.344627  0.321550  0.921  0.35710
factor(RACE)5                         0.321404  1.379062  0.187344  1.716  0.08624
factor(RACE)6                         0.175654  1.192025  0.426466  0.412  0.68043
factor(RACE)99                       -0.196321  0.821749  0.394947 -0.497  0.61913
factor(r_score)0, 1                  -0.867224  0.420116  0.195843 -4.428 9.50e-06
factor(r_score)2                     -0.190780  0.826314  0.142815 -1.336  0.18160
factor(pr_resp)DU>1                  -1.017203  0.361605  0.152638 -6.664 2.66e-11
factor(pr_resp)SDPD                   0.240739  1.272189  0.118794  2.027  0.04271
factor(pr_drug)Y                      0.163248  1.177329  0.123313  1.324  0.18555
factor(trt)treatment                  0.076143  1.079117  0.119775  0.636  0.52496
factor(PERF_STA)1:factor(SEX)M        0.223520  1.250471  0.238853  0.936  0.34937
factor(PERF_STA)2:factor(SEX)M        0.596447  1.815656  0.363827  1.639  0.10114
factor(PERF_STA)3:factor(SEX)M       -0.364309  0.694676  0.538968 -0.676  0.49908
factor(PR_RAD)Y:factor(trt)treatment -0.487694  0.614041  0.238504 -2.045  0.04087

Likelihood ratio test=205.7  on 24 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 8 
fit_8$loglik # Output -2353.049 -2250.214


## LR test p-value for PERF_STA * STAGE
1-pchisq(29.332,9) #Output: 0.0005695373


## Cox Regression with Interaction: Reduced model 2 without insignificant individual covirates,  
fit_9 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) 
                                          + factor(r_score) + factor(pr_resp)+ factor(trt) + factor(PERF_STA)*factor(STAGE) 
                                          + factor(PERF_STA)*factor(SEX)+ factor(PR_RAD)* factor(trt) ), data = data_final)
print(fit_9)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(r_score) + factor(pr_resp) + factor(trt) + factor(PERF_STA) * 
    factor(STAGE) + factor(PERF_STA) * factor(SEX) + factor(PR_RAD) * 
    factor(trt)), data = data_final)

                                         coef exp(coef) se(coef)      z        p
factor(PERF_STA)1                     0.73275   2.08080  0.43841  1.671 0.094647
factor(PERF_STA)2                     1.89542   6.65531  1.07100  1.770 0.076765
factor(PERF_STA)3                     0.80673   2.24057  0.43181  1.868 0.061727
factor(STAGE)II                       0.79014   2.20371  0.33337  2.370 0.017782
factor(STAGE)III                     -0.16686   0.84632  0.36464 -0.458 0.647232
factor(STAGE)IV                       0.08186   1.08530  0.35071  0.233 0.815450
factor(SEX)M                         -0.03204   0.96846  0.18586 -0.172 0.863118
factor(PR_RAD)Y                       0.58046   1.78686  0.16643  3.488 0.000487
factor(B_SYM)Y                        0.29005   1.33650  0.10964  2.646 0.008156
factor(r_score)0, 1                  -1.03475   0.35532  0.19648 -5.266 1.39e-07
factor(r_score)2                     -0.17836   0.83664  0.13912 -1.282 0.199824
factor(pr_resp)DU>1                  -1.10461   0.33134  0.14842 -7.442 9.89e-14
factor(pr_resp)SDPD                   0.25901   1.29565  0.11870  2.182 0.029101
factor(trt)treatment                  0.08561   1.08938  0.12069  0.709 0.478132
factor(PERF_STA)1:factor(STAGE)II    -0.89196   0.40985  0.47977 -1.859 0.063006
factor(PERF_STA)2:factor(STAGE)II    -4.41266   0.01212  1.32298 -3.335 0.000852
factor(PERF_STA)3:factor(STAGE)II     0.45429   1.57506  1.11458  0.408 0.683576
factor(PERF_STA)1:factor(STAGE)III   -0.22368   0.79957  0.48863 -0.458 0.647117
factor(PERF_STA)2:factor(STAGE)III   -1.98830   0.13693  1.15024 -1.729 0.083882
factor(PERF_STA)3:factor(STAGE)III    2.21798   9.18875  0.72611  3.055 0.002253
factor(PERF_STA)1:factor(STAGE)IV    -0.35388   0.70196  0.45919 -0.771 0.440904
factor(PERF_STA)2:factor(STAGE)IV    -1.87035   0.15407  1.10916 -1.686 0.091744
factor(PERF_STA)3:factor(STAGE)IV          NA        NA  0.00000     NA       NA
factor(PERF_STA)1:factor(SEX)M        0.20699   1.22997  0.23565  0.878 0.379736
factor(PERF_STA)2:factor(SEX)M        0.96532   2.62563  0.37391  2.582 0.009832
factor(PERF_STA)3:factor(SEX)M       -0.75060   0.47208  0.57083 -1.315 0.188537
factor(PR_RAD)Y:factor(trt)treatment -0.59704   0.55044  0.23857 -2.503 0.012329

Likelihood ratio test=229.8  on 26 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 9
fit_9$loglik # Output -2353.049 -2238.150


## Fit 9 without r-score,  
fit_10 <- coxph(Surv(os_time, os_event) ~ (factor(PERF_STA) + factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) 
                                           + factor(pr_resp)+ factor(trt) + factor(PERF_STA)*factor(STAGE) 
                                          + factor(PERF_STA)*factor(SEX)+ factor(PR_RAD)* factor(trt) ), data = data_final)
print(fit_10)
"""
Output:

Call:
coxph(formula = Surv(os_time, os_event) ~ (factor(PERF_STA) + 
    factor(STAGE) + factor(SEX) + factor(PR_RAD) + factor(B_SYM) + 
    factor(pr_resp) + factor(trt) + factor(PERF_STA) * factor(STAGE) + 
    factor(PERF_STA) * factor(SEX) + factor(PR_RAD) * factor(trt)), 
    data = data_final)

                                          coef exp(coef)  se(coef)      z        p
factor(PERF_STA)1                     0.665296  1.945066  0.440661  1.510 0.131102
factor(PERF_STA)2                     2.817634 16.737208  1.057843  2.664 0.007732
factor(PERF_STA)3                     1.092196  2.980814  0.426527  2.561 0.010447
factor(STAGE)II                       0.753969  2.125420  0.331696  2.273 0.023022
factor(STAGE)III                      0.288993  1.335082  0.351895  0.821 0.411506
factor(STAGE)IV                       0.713384  2.040887  0.328019  2.175 0.029643
factor(SEX)M                         -0.051540  0.949766  0.184790 -0.279 0.780315
factor(PR_RAD)Y                       0.574470  1.776190  0.165849  3.464 0.000533
factor(B_SYM)Y                        0.250399  1.284538  0.107702  2.325 0.020076
factor(pr_resp)DU>1                  -1.038360  0.354035  0.147208 -7.054 1.74e-12
factor(pr_resp)SDPD                   0.254284  1.289538  0.118674  2.143 0.032136
factor(trt)treatment                  0.091333  1.095634  0.119612  0.764 0.445116
factor(PERF_STA)1:factor(STAGE)II    -0.744935  0.474765  0.477129 -1.561 0.118456
factor(PERF_STA)2:factor(STAGE)II    -4.556024  0.010504  1.321765 -3.447 0.000567
factor(PERF_STA)3:factor(STAGE)II     0.921015  2.511838  1.097602  0.839 0.401405
factor(PERF_STA)1:factor(STAGE)III    0.007091  1.007116  0.486573  0.015 0.988373
factor(PERF_STA)2:factor(STAGE)III   -2.444443  0.086774  1.146636 -2.132 0.033020
factor(PERF_STA)3:factor(STAGE)III    2.325892 10.235810  0.721743  3.223 0.001270
factor(PERF_STA)1:factor(STAGE)IV    -0.150779  0.860038  0.457163 -0.330 0.741540
factor(PERF_STA)2:factor(STAGE)IV    -2.480176  0.083728  1.100677 -2.253 0.024239
factor(PERF_STA)3:factor(STAGE)IV           NA        NA  0.000000     NA       NA
factor(PERF_STA)1:factor(SEX)M        0.256925  1.292948  0.234912  1.094 0.274084
factor(PERF_STA)2:factor(SEX)M        0.951767  2.590283  0.372889  2.552 0.010698
factor(PERF_STA)3:factor(SEX)M       -0.697073  0.498041  0.568742 -1.226 0.220334
factor(PR_RAD)Y:factor(trt)treatment -0.583923  0.557706  0.238077 -2.453 0.014180

Likelihood ratio test=197.5  on 24 df, p=< 2.2e-16
n= 605, number of events= 398 
"""


## log-likelihood of fit 10
fit_10$loglik # Output -2353.049 -2254.318


## LR test p-value for r-score
1-pchisq(32.336,2) # Output: 9.513204e-08


## Fit 2 (Model 2 in report): Residuals vs. risk score (predicted values)
fit_2_resids <- resid(fit_2, type = "deviance")
par(mfrow = c(1, 2))
plot(predict(fit_2), fit_2_resids , ylab = "Deviance Residuals", xlab = "Risk Score", main = "Model 2")


## Fit 9 (Model 8 in report): Residuals vs. risk score (predicted values)
fit_9_resids <- resid(fit_9, type = "deviance")
plot(predict(fit_9), fit_9_resids , ylab = "Deviance Residuals", xlab = "Risk Score", main = "Model 8")


## Normal Q-Q Plot
qqnorm(fit_2_resids , ylab = "Deviance Residuals", xlab = "N(0,1) Quantiles", main = "Model 2: Normal Q-Q Plot")
abline(0,1,lty = 2)

qqnorm(fit_9_resids , ylab = "Deviance Residuals", xlab = "N(0,1) Quantiles", main = "Model 8: Normal Q-Q Plot")
abline(0,1,lty = 2)


## Chi-square test
cox.zph(fit_2, terms = F)
"""
Output:
                       chisq df       p
factor(PERF_STA)1    0.00131  1  0.9711
factor(PERF_STA)2   18.93942  1 1.3e-05
factor(PERF_STA)3    8.95116  1  0.0028
factor(PR_RAD)Y      7.63027  1  0.0057
factor(B_SYM)Y       9.69411  1  0.0018
factor(r_score)0, 1  2.59940  1  0.1069
factor(r_score)2     4.00048  1  0.0455
factor(pr_resp)DU>1  9.49711  1  0.0021
factor(pr_resp)SDPD 10.14903  1  0.0014
GLOBAL              56.53970  9 6.2e-09
"""

cox.zph(fit_9, terms = F)
"""
Output:
                                        chisq df       p
factor(PERF_STA)1                    4.03e-01  1 0.52548
factor(PERF_STA)2                    1.25e+01  1 0.00041
factor(PERF_STA)3                    7.45e+00  1 0.00633
factor(STAGE)II                      2.54e-01  1 0.61445
factor(STAGE)III                     9.61e-05  1 0.99218
factor(STAGE)IV                      5.37e-01  1 0.46378
factor(SEX)M                         3.76e-01  1 0.53956
factor(PR_RAD)Y                      6.55e+00  1 0.01047
factor(B_SYM)Y                       7.68e+00  1 0.00558
factor(r_score)0, 1                  1.76e+00  1 0.18504
factor(r_score)2                     3.56e+00  1 0.05913
factor(pr_resp)DU>1                  8.41e+00  1 0.00373
factor(pr_resp)SDPD                  9.12e+00  1 0.00253
factor(trt)treatment                 1.34e+00  1 0.24646
factor(PERF_STA)1:factor(STAGE)II    4.84e-03  1 0.94452
factor(PERF_STA)2:factor(STAGE)II    6.40e+00  1 0.01142
factor(PERF_STA)3:factor(STAGE)II    2.55e+00  1 0.11023
factor(PERF_STA)1:factor(STAGE)III   1.28e+00  1 0.25846
factor(PERF_STA)2:factor(STAGE)III   4.63e-01  1 0.49636
factor(PERF_STA)3:factor(STAGE)III   2.31e+00  1 0.12869
factor(PERF_STA)1:factor(STAGE)IV    4.07e-01  1 0.52331
factor(PERF_STA)2:factor(STAGE)IV    8.87e+00  1 0.00290
factor(PERF_STA)1:factor(SEX)M       1.29e+00  1 0.25581
factor(PERF_STA)2:factor(SEX)M       2.10e+00  1 0.14732
factor(PERF_STA)3:factor(SEX)M       2.25e+00  1 0.13337
factor(PR_RAD)Y:factor(trt)treatment 7.25e+00  1 0.00710
GLOBAL                               7.70e+01 26 5.9e-07
"""
