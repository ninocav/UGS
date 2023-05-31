#### Load WTP models for different treatments #####

mxl_info1 <- apollo_loadModel("Estimation_results/MXL_info_1")
mxl_info2 <- apollo_loadModel("Estimation_results/MXL_info_2")
mxl_info3 <- apollo_loadModel("Estimation_results/MXL_info_3")


mxl_info_compare <- as.data.frame(mxl_info1$estimate)
mxl_info_compare[2] <- as.data.frame(mxl_info2$estimate)
mxl_info_compare[3] <- as.data.frame(mxl_info3$estimate)
mxl_info_compare[4] <- as.data.frame(mxl_12$estimate)

alpha = 0.1
mxl_info_compare$margin_of_error1 <- qnorm(1-alpha/2)*mxl_info1$robse
mxl_info_compare$margin_of_error2 <- qnorm(1-alpha/2)*mxl_info2$robse
mxl_info_compare$margin_of_error3 <- qnorm(1-alpha/2)*mxl_info3$robse
mxl_info_compare$margin_of_error_respondi <- qnorm(1-alpha/2)*mxl_12$robse
mxl_info_compare <- rownames_to_column(mxl_info_compare, "Coefficent")
colnames(mxl_info_compare) <- c("Coefficent", "Estimate1", "Estimate2", "Estimate3","Estimate_Respondi", "Margin_of_error1",
                                "Margin_of_error2", "Margin_of_error3", "Margin_of_error_Respondi")


mxl_melt_info <- melt(mxl_info_compare[1:5], id = "Coefficent")
mxl_melt_info$ME <- mxl_info_compare$Margin_of_error1
mxl_melt_info$ME[11:20] <- mxl_info_compare$Margin_of_error2
mxl_melt_info$ME[21:30] <- mxl_info_compare$Margin_of_error3
mxl_melt_info$ME[31:40] <- mxl_info_compare$Margin_of_error_Respondi

# Figure paper compare info treatments #
ggplot(data=mxl_melt_info, aes(x=Coefficent, y=abs(value), fill=variable)) +
  geom_bar(stat="identity",  position='dodge', width = 0.9) +
  geom_errorbar(aes(x=Coefficent, ymin=abs(value)-ME, ymax=abs(value)+ME), width=0.3, position=position_dodge(0.8)) +
  ylab("Absolute Value") +
  xlab("Coefficient") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Set2", labels = c("Always Info", "Optional Info", "No Info", "Respondi"), name="Treatment") +
  theme(legend.position = c(0.9, 0.8)) 
ggsave("Figures/info_treatment.png", dpi = "print",  width = 7, height = 5)

#### Test distribution of treatments among cities ####
test_treatment <- database_third %>% mutate(Info1 = case_when(treatment==1 ~ 1, TRUE ~0),
                                            Info2 = case_when(treatment==2 ~ 1, TRUE ~0),
                                            Info3 = case_when(treatment==3 ~ 1, TRUE ~0)) %>%
                                              group_by(City) %>% summarize(treatment = mean(treatment),
                                                                  Info1 = sum(Info1), Info2 = sum(Info2),
                                                                  Info3 = sum(Info3),
                                                                  Age = round(mean(Age), 2)) %>% 
  mutate(N = Info1 + Info2 + Info3, Share1 = round(Info1/N, 2), Share2 = round(Info2/N, 2), 
         Share3 = round(Info3/N, 2)) %>% filter(City != "-oth-")

# Figure appendix treatment shares per city 
ggplot(data = test_treatment) +
  geom_point(aes(x=City, y=Share3, col="No Info"), shape=16) +
  geom_point(aes(x=City, y=Share1, col="Always Info"), shape = 17) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_hline(yintercept=1/3) +
  scale_color_manual(values = c("red", "black")) +
  ylab("Share") +
  labs(colour= "Treatment") +
  theme(legend.position = "bottom")

ggsave("Figures/info_treatment_shares.png", dpi = "print",  width = 7, height = 5)
