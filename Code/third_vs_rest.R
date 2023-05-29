#### Compare socio-demographics ####

pl_rent <- ggplot() +
  geom_boxplot(data=database_all, aes(x="A: Respondi", y=MietePerSqm), fill="#66c2a5", outlier.shape = NA) +
  geom_boxplot(data=database_third, aes(x="B: IMUG", y=MietePerSqm), fill="#fc8d62", outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 20)) +
  ylab(expression("Rent per"~m^2)) +
  xlab("Sample")



pl_female <- ggplot() +
  geom_bar(data=database_all, aes(x="A: Respondi", y=mean(gender_female)), stat="identity", position="dodge", fill="#66c2a5", width=0.8) +
  geom_bar(data=database_third, aes(x="B: IMUG", y=mean(gender_female)), stat="identity", position="dodge", fill = "#fc8d62", width=0.8) +
  ylab("Share of Females") +
  xlab("Sample") 


# pl_wd <- ggplot() +
#   geom_boxplot(data=database_all, aes(x="A: Respondi", y=WalkingDistance), fill="#66c2a5", outlier.shape = NA) +
#   geom_boxplot(data=database_third, aes(x="B: IMUG", y=WalkingDistance), fill="#fc8d62", outlier.shape = NA) +
#   ylab("Walking Distance") +
#   xlab("Sample") +
#   coord_cartesian(ylim = c(0, 35))

pl_income <- ggplot() +
  geom_boxplot(data=database_all, aes(x="A: Respondi", y=Income_Dis_Present), fill="#66c2a5", outlier.shape = NA) +
  geom_boxplot(data=database_third, aes(x="B: IMUG", y=Income_Dis_Present), fill="#fc8d62", outlier.shape = NA) +
  ylab("Disposable Income") +
  xlab("Sample") +
  coord_cartesian(ylim = c(0, 7000))

pl_age <- ggplot() +
  geom_boxplot(data=database_all, aes(x="A: Respondi", y=Age), fill="#66c2a5", outlier.shape = NA) +
  geom_boxplot(data=database_third, aes(x="B: IMUG", y=Age), fill="#fc8d62", outlier.shape = NA) +
  ylab("Age") +
  xlab("Sample") +
  coord_cartesian(ylim = c(16, 72))

pl_kids <- ggplot() +
  geom_bar(data=database_all, aes(x="A: Respondi", y=mean(KidsDummy)), stat="identity", position="dodge", fill="#66c2a5", width=0.8) +
  geom_bar(data=database_third, aes(x="B: IMUG", y=mean(KidsDummy)), stat="identity", position="dodge", fill = "#fc8d62", width=0.8) +
  ylab("Share of Parents") +
  xlab("Sample") 

pl_educ <- ggplot() +
  geom_bar(data=database_all, aes(x="A: Respondi", y=mean(education)), stat="identity", position="dodge", fill="#66c2a5", width=0.8) +
  geom_bar(data=database_third, aes(x="B: IMUG", y=mean(education)), stat="identity", position="dodge", fill = "#fc8d62", width=0.8) +
  ylab("Share of Higher Education") +
  xlab("Sample") 

ggarrange(pl_rent, pl_income, pl_age, pl_female, pl_educ, pl_kids)  
ggsave("Figures/respondi_vs_imug_socio.png", dpi = "print", width=7, height = 5)


