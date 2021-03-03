####### DATA CLEANING #######################################################################################################

data_original <- read_excel('/Users/laurahuber/Desktop/data_analysisThai_zip/Reduce AMU_main file(5).xlsx', sheet = "Main file")
cost_updated <- read_excel('/Users/laurahuber/Desktop/data_analysisThai_zip/cost_updated.xlsx')

data_original <- as.data.frame(data_original %>% dplyr::select(ID...1, Correct_coordination, farm_size...4, drugstore_coordinates, number_sows, total_numberpigs, cost_antibiotics, manure_management, disease_count, used_drugs_recently, most_commondrug, most_commondrug_correctedLH, special_feed, District_code, contains('ad_'),
                                                               contains('adv_'), contains('P_'), contains('C_'), contains('U_') ))

names(data_original) <- str_replace(names(data_original), "ID...1", "ID") 
names(data_original) <- str_replace(names(data_original), "farm_size...4", "farm_size") 
data_original <- data_original %>% 
  separate(Correct_coordination, into = c("x_farm", "y_farm"), sep = ", ", remove = FALSE)
data_original <- data_original %>%  
  separate(Correct_coordination, into = c("x_farm", "y_farm"), sep = ",", remove = FALSE)


data_original$x_farm[data_original$ID == 1110] <- 16.7011386
data_original$y_farm[data_original$ID == 1110] <- 102.0934022

data_original$x_farm <- as.numeric(data_original$x_farm)
data_original$y_farm <- as.numeric(data_original$y_farm)

data_original$y_farm[data_original$ID == 4402] <- 102.1171944
data_original$y_farm[data_original$ID == 4403] <- 102.0951362

data_original <- data_original %>%  
  separate(drugstore_coordinates, into = c("lat_drugStore", "long_drugStore"), sep = ", ", remove = FALSE)
data_original <- data_original %>%  
  separate(drugstore_coordinates, into = c("lat_drugStore", "long_drugStore"), sep = ",", remove = FALSE)

names(data_original) <- str_replace(names(data_original), " ", "_")
data_original$lat_drugStore <- as.numeric(data_original$lat_drugStore)
data_original$long_drugStore <- as.numeric(data_original$long_drugStore)

#Received updated data from Ulf, merge data with updated data with cost with ATB
data <- merge(data_original,cost_updated,by="ID") #I will use this one to test my models
data$x_farm <- as.numeric(data$x_farm)
data$y_farm <- as.numeric(data$y_farm)

# Outcomes

#Sum proportion of resistance across all classes in pig samples 
pig <- as.data.frame(data %>% dplyr::select(contains('P_')))
pig$xp <- vector(length=nrow(pig))
pig$xp  <- rowSums(pig[,1:7] )
data$sumPropPigs<-pig$xp/7 

# Number of ATB resistance in pig samples
pig <- as.data.frame(data %>% dplyr::select(contains('P_')))
pig$xp <- vector(length=nrow(pig))
pig[is.na(pig)] <- " "
data$n_classes_pig<-apply(pig,1,function(x) length(x[x>0.0])) # I've decided to count how many classes of ATB, at least 1 of the isolates tested had resistance

# Number of ATB resistance in contact samples
contact <- as.data.frame(data %>% dplyr::select(contains('C_')))
contact$xc <- vector(length=nrow(contact))
contact[is.na(contact)] <- " "
data$n_classes_contact<-apply(contact,1,function(x) length(x[x>0.0]))

# Number of ATB resistance in non-contact samples
no_contact <- as.data.frame(data %>% dplyr::select(contains('U_')))
no_contact$xu <- vector(length=nrow(no_contact))
no_contact[is.na(no_contact)] <- " "
data$n_classes_no_contact<-apply(no_contact,1,function(x) length(x[x>0.0]))


# Assigning 1 if MDR, 0 if not for each source

data$mdr_pig <- ifelse(data$n_classes_pig>=3, 1, 
                       ifelse(data$n_classes_pig<3, 0, NA))

data$mdr_contact <- ifelse(data$n_classes_contact>=3, 1, 
                           ifelse(data$n_classes_contact<3, 0, NA))

data$mdr_no_contact <- ifelse(data$n_classes_no_contact>=3, 1, 
                              ifelse(data$n_classes_no_contact<3, 0, NA))

# Assigning 1 if AT LEAST ONE CLASS RESISTANCE, 0 if not for each source

data$ATLEASTONE_pig <- ifelse(data$n_classes_pig>=1, 1, 
                              ifelse(data$n_classes_pig<1, 0, NA))
data$ATLEASTONE_pig <- as.factor(data$ATLEASTONE_pig)
data$ATLEASTONE_contact <- ifelse(data$n_classes_contact>=1, 1, 
                                  ifelse(data$n_classes_contact<1, 0, NA))
data$ATLEASTONE_contact <- as.factor(data$ATLEASTONE_contact)
data$ATLEASTONE_no_contact <- ifelse(data$n_classes_no_contact>=1, 1, 
                                     ifelse(data$n_classes_no_contact<1, 0, NA))
data$ATLEASTONE_no_contact <- as.factor(data$ATLEASTONE_no_contact)

#proportion resistance

data$pro_resist_pigs <- data$n_classes_pig/7
data$pro_resist_contact <- data$n_classes_contact/7
data$pro_resist_no_contact <- data$n_classes_no_contact/7


#Canculating AMU variable

data$amu <- data$cost_updated/data$total_numberpigs

#Calculating minimun distance between farms and drugstores
library(fields)

drugStore_coord <- data %>% dplyr::select("lat_drugStore", "long_drugStore") %>% drop_na()#made a database only with the coordinates of the drug stores
drugStore_coord<-as.data.frame(drugStore_coord)
data$x_farm <- as.numeric(data$x_farm)
data$y_farm <- as.numeric(data$y_farm)

x1 <- as.matrix(cbind(data$x_farm, data$y_farm))
drugStore_coord$lat_drugStore <- as.numeric(drugStore_coord$lat_drugStore)
drugStore_coord$long_drugStore <- as.numeric(drugStore_coord$long_drugStore)
x2 <- as.matrix(cbind(drugStore_coord$lat_drugStore, drugStore_coord$long_drugStore))

min_dis_DS <- fields::rdist.earth(x2, x1, miles=F)



data$min_dis_DS<-apply(X = min_dis_DS,
                       MARGIN = 2,
                       FUN = min)







x1 <- as.matrix(cbind(data$x_farm, data$y_farm))
min_dis_farms <- fields::rdist.earth(x1, x1, miles=F)
data$min_dis_farms<-apply(X = min_dis_farms,
                          MARGIN = 2,
                          FUN = min)







# See if I have duplicate coordinates for farm
length(unique(data$y_farm))
length(unique(data$y_farm)) == nrow(data)
data_original[duplicated(data_original$y_farm),]

#There are 4 farms that the coordinates are very close together and it gives me an error when I run the risk map, so I will have to add some error factor
data$y_farm[data$ID == 8802] <- 103.1861 #added 0.2
data$y_farm[data$ID == 9973] <- 102.7733 #reduced 0.2
data$y_farm[data$ID == 8872] <- 103.3087 #added 0.3
data$y_farm[data$ID == 8877] <- 102.6679 #reduced 0.3

data$x_farm[data$ID == 8802] <- 16.69715 #added 0.2
data$x_farm[data$ID == 9973] <- 16.30203 #reduced 0.2
data$x_farm[data$ID == 8872] <- 16.82142 #added 0.3
data$x_farm[data$ID == 8877] <- 16.20540 #reduced 0.3
data$x_farm <- as.numeric(data$x_farm)
data$y_farm <- as.numeric(data$y_farm)


#Data to be used in the risk maps (before transforming varaibles)
XY_krige = data


#Calculate the min. dist, between farms
x1 <- as.matrix(cbind( data$x_farm, data$y_farm))
x2 <- as.data.frame(x1)

save(x = x2,
     file = "/Users/laurahuber/Desktop/x2.rda")

library(geodist)
farms <- x2
farms <- as.data.frame(farms)
colnames(farms) <- c("lat", "lon")
farms <- cbind("lon" = farms$lon,
               "lat" = farms$lat)
row.names(farms) <- paste0(rep(x = "FARM",
                               times = nrow(farms)),
                           seq(from = 1,
                               to = nrow(farms),
                               by = 1))
dist_matrix <- geodist(x = as.matrix(farms),
                       measure = "geodesic")
farms$distance <- apply(X = dist_matrix,
                        MARGIN = 1,
                        FUN = function(i) {
                          min(i[i > 0])
                        }                      
)
data$min_dist_farms <- farms$distance

## Now transform variables 
data$manure_management <- as.factor(data$manure_management)
data$disease_count <- as.factor(data$disease_count)
data$farm_size <- as.factor(data$farm_size)
data$used_drugs_recently <- as.factor(data$used_drugs_recently)
data$number_sows <- as.numeric(data$number_sows)
data$min_dis_DS <- as.numeric(data$min_dis_DS)
data$n_classes_contact<- as.numeric(data$n_classes_contact)
data$n_classes_no_contact<- as.numeric(data$n_classes_no_contact)
data$n_classes_pig<- as.numeric(data$n_classes_pig)
data$mdr_pig <- as.factor(data$mdr_pig)
data$mdr_contact <- as.factor(data$mdr_contact)
data$mdr_no_contact <- as.factor(data$mdr_no_contact)
data$company <- as.factor(data$company)
data$amu <- as.numeric(data$amu)
data$consensus <- as.numeric(data$x)
data$ATLEASTONE_pig <- as.factor(data$ATLEASTONE_pig)
data$ATLEASTONE_contact <- as.factor(data$ATLEASTONE_contact)
data$ATLEASTONE_no_contact <- as.factor(data$ATLEASTONE_no_contact)
data$special_feed <- as.factor(data$special_feed)
data$ad_vet <- as.factor(data$ad_vet)


################ DISCRIPTIVE STATISTICS AND GRAPHS ############################################################################
smallFarms <- data[ which(data$farm_size==0), ]
largeFarms <- data[ which(data$farm_size==1), ]

smallFarms$ad_vet <- as.factor(smallFarms$ad_vet)
smallFarms$ad_aahw <- as.factor(smallFarms$ad_aahw)
smallFarms$adv_pharmacies <- as.factor(smallFarms$adv_pharmacies)
smallFarms$adv_otherfarmer <- as.factor(smallFarms$adv_otherfarmer)
smallFarms$adv_package <- as.factor(smallFarms$adv_package)
smallFarms$adv_company_staff <- as.factor(smallFarms$adv_company_staff)
smallFarms$adv_drugstore <- as.factor(smallFarms$adv_drugstore)
smallFarms$adv_drugstore_pharm <- as.factor(smallFarms$adv_drugstore_pharm)

summary(smallFarms$adv_drugstore_pharm)


n = length(smallFarms$mdr_no_contact)    # valid responses count 
k = sum(smallFarms$mdr_no_contact== "0") 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE
E = qnorm(.975)*SE; E
pbar + c(-E, E) 

#Difference of proportions
prop.test(x = c(24,1), n = c(51, 113), alternative = "greater") # number of success and total number of observations

tapply(data$disease_count, data$farm_size, summary) #to get median and range

prop.test(x = c(48,26), n = c(51, 113), alternative = "less") # number of success and total number of observations


prop.test(x = c(39,26), n = c(164, 164), alternative = "greater") # number of success and total number of observations



####### PIGS #######
library(rstatix)
library(ggplot2)
tapply(data$number_sows, data$farm_size, summary) #to get median and range
wilcox_test(amu~farm_size, data=data) #to get test statistics
wilcox_effsize(n_classes_pig~special_feed,data=data) #to get effect size


tapply(data$number_sows, data$farm_size, summary) #to get median and range
wilcox_test(amu~farm_size, data=data) #to get test statistics
wilcox_effsize(n_classes_pig~special_feed,data=data) #to get effect size



amu_pigs <- ggplot(aes(x=mdr_pig, y=amu),data = data)+
  geom_boxplot(color="#AA4499")+
  #annotate("text", x = 2, y = 535, label = "*", size = 5) +
  annotate("text", x = 1, y =600, label = "n=60", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 600, label = "n=104", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 600), oob = scales::squish)+
  ylab("Antimicrobial use") + xlab(expression(paste("R"[pig])))+ scale_x_discrete (labels = c("<3",">=3"))+
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 13),
        legend.position = "none", 
        panel.background = element_blank())+
  ggtitle("A")
# 0.00000132; ES= 0.378 moderate


n_classes_contact_pigs <- ggplot(aes(x=mdr_pig, y=n_classes_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y =7.5, label = "n=60", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=104", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[pig]))) + 
  xlab(expression("R"[contact],"<3"))+ scale_x_discrete (labels = c("<3",">=3"))+
  #theme_bw()+
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  ggtitle("G")
#p=0.318; ES=0.0782 small

n_classes_no_contact_pigs <- ggplot(aes(x=mdr_pig, y=n_classes_no_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y =7.5, label = "n=60",size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=104", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(1,8), oob = scales::squish)+
  ylab(expression(paste("R"[pig]))) + 
  xlab(expression("R"[no-contact],"<3"))+ scale_x_discrete (labels = c("<3",">=3"))+
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 13),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  ggtitle("H")
#p=0.45; ES=0.0593 small

min_dis_DS_pigs <- ggplot(aes(x=mdr_pig, y=min_dis_DS),data = data)+
  geom_boxplot(color="#332288")+
  annotate("text", x = 2, y = 22, label = "*", size = 5) +
  annotate("text", x = 1, y = 25, label = "n=60", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 25, label = "n=104", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 25), oob = scales::squish)+
  ylab("Distance drugstores") +xlab(expression(paste("R"[pig])))+ scale_x_discrete (labels = c("<3",">=3"))+
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 13), 
        legend.position = "none",
        panel.background = element_blank()) +
  ggtitle("B")
#p=0.000283; ES= 0.284 small

#cat variables 
tapply(data$amu, data$farm_size, summary) #to get median and range
wilcox_test(amu~farm_size,data=data) #to get test statistics
wilcox_effsize(amu~farm_size,data=data) #to get effect size

data$disease_count <- as.factor(data$disease_count)
disease_count_pigs <- ggplot(aes(x=disease_count, y=n_classes_pig),data = data)+
  geom_boxplot(color="#88CCEE")+
  #annotate("text", x = 2, y = 6.6, label = "*", size = 5) +
  annotate("text", x = 1, y = 7.5, label = "n=90", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=74", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4',color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[pig]))) + 
  xlab("Herd disease")+ scale_x_discrete (labels = c("No", "Yes"))+
  #theme_bw()+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(size = 13), 
        legend.position = "none", 
        panel.background = element_blank())+
  ggtitle("C")
#p=4.96e-10; ES=0.486 small

data$used_drugs_recently <- as.factor(data$used_drugs_recently)
used_atb_recently_pigs <- ggplot(aes(x=used_drugs_recently, y=n_classes_pig),data = data)+
  geom_boxplot(color="#CC6677")+
  #annotate("text", x = 2, y = 6.65, label = "*", size = 5) +
  annotate("text", x = 1, y = 7.5, label = "n=34", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=130", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[pig]))) + 
  xlab("Used antimicrobials")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(size = 13), 
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  ggtitle("D")
#p<0.001; ES=0.337 moderate


special_feed_pigs <- ggplot(aes(x=special_feed, y=n_classes_pig),data = data)+
  geom_boxplot(color="#DDCC77")+
  #annotate("text", x = 2, y = 6.6, label = "*", size = 5) +
  annotate("text", x = 1, y = 7.5, label = "n=139", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=25", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[pig]))) + 
  xlab("Feed additive")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(size = 13), 
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) +
  ggtitle("E")
#p<0.001; ES=0.337 moderate


data$manure_management <- as.factor(data$manure_management)
manure_management_pigs <- ggplot(aes(x=manure_management, y=n_classes_pig),data = data)+
  geom_boxplot(color="#117733")+
  #annotate("text", x = 4, y = 6.6, label = "*", size = 5) +
  annotate("text", x = 1, y = 7.5, label = "n=38", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=89", size = 3, color="dimgray")+
  annotate("text", x = 3, y = 7.5, label = "n=2", size = 3, color="dimgray")+
  annotate("text", x = 4, y = 7.5, label = "n=35", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[pig]))) + 
  xlab("Manure management")+ scale_x_discrete (labels = c("Discard", "Fertilizer", "Fuel", "Sold"), guide = guide_axis(n.dodge = 2))+
  theme(text = element_text(size=13),
        axis.text.x = element_text(size = 13),  
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())+
  ggtitle("F")

data$farm_size <- as.factor(data$farm_size)
farm_size_pigs <- ggplot(aes(x=farm_size, y=n_classes_pig),data = data)+
  geom_boxplot()+
  annotate("text", x = 2, y = 6.2, label = "*", size = 5) +
  annotate("text", x = 1, y = 7.5, label = "n=113", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=51", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 7.5), oob = scales::squish)+
  ylab("mResPigs") + xlab("Farm Size")+
  stat_summary(aes(y = n_classes_pig,group=1), fun.y=mean, colour="brown4", geom="line",group=1)+
  stat_summary(aes(y = n_classes_pig,group=1), fun.data=mean_se, colour="brown4", geom="linerange",group=1)+
  theme_bw()
#p=0.0765; ES=0.138 small
farm_size_pigs_amu <- ggplot(aes(x=farm_size, y=amu),data = data)+
  geom_boxplot()+
  annotate("text", x = 1, y = 550, label = "a", size = 4) +
  annotate("text", x = 2, y = 550, label = "b", size = 4) +
  annotate("text", x = 1, y = 590, label = "n=113", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 590, label = "n=51", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 600), oob = scales::squish)+
  ylab("AMU") + xlab("Farm Size")+
  stat_summary(aes(y = amu,group=1), fun.y=mean, colour="brown4", geom="line",group=1)+
  stat_summary(aes(y = amu,group=1), fun.data=mean_se, colour="brown4", geom="linerange",group=1)+
  theme_bw()
#p=0.0765; ES=0.138 small


ARRANGE_pigs <- ggarrange( disease_count_pigs,  used_atb_recently_pigs,special_feed_pigs, manure_management_pigs, n_classes_contact_pigs, n_classes_no_contact_pigs,nrow=1, ncol = 6)
ARRANGE_pigs2 <- ggarrange(amu_pigs,min_dis_DS_pigs,nrow=1, ncol = 2)
ARRANGE_pigs3 <- ggarrange(ARRANGE_pigs2, ARRANGE_pigs, nrow=2, ncol = 1)

####### CONTACT #######
#normality test
ggdensity(data$min_dis_DS)
ggqqplot(data$n_classes_pig)
shapiro_test(data$amu) # p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. 
#none of my variables are normally distributed, so I will do wilcoxon

tapply(data$amu, data$mdr_contact, summary) #to get median and range
wilcox_test(min_dis_DS~mdr_contact,data=data) #to get test statistics
wilcox_effsize(min_dis_DS~mdr_contact,data=data) #to get effect size

amu_contact <- ggplot(aes(x=mdr_contact, y=amu),data = data)+
  geom_boxplot(color="dimgray")+
  annotate("text", x = 1, y =600, label = "n=125", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 600, label = "n=39", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 610), oob = scales::squish)+
  ylab("Antimicrobial use") + xlab("Resistance - contact humans")+ scale_x_discrete (labels = c(expression(paste("R"[contact],"<3")),expression(paste("R"[contact],">=3"))))+
  theme(legend.position = "none", 
        panel.background = element_blank())

summary(data$mdr_contact)

number_sows_contact <- ggplot(aes(x=mdr_contact, y=number_sows),data = data)+
  geom_boxplot(color="dimgray")+
  annotate("text", x = 1, y =400, label = "n=125", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 400, label = "n=39", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 400), oob = scales::squish)+
  ylab("Number of sows") + xlab("Resistance - contact humans")+ scale_x_discrete (labels = c(expression(paste("R"[contact],"<3")),expression(paste("R"[contact],">=3"))))+
  theme_bw()
#p=0.376

n_classes_pigs_contact <- ggplot(aes(x=mdr_contact, y=n_classes_pig),data = data)+
  geom_boxplot(color="dimgray")+
  annotate("text", x = 1, y =7.5, label = "n=125", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=39", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(1,8), oob = scales::squish)+
  ylab(expression(paste("R"[contact]))) + 
  xlab(expression("R"[pig],"<3"))+ scale_x_discrete (labels = c(expression(paste("R"[pig],"<3")),expression(paste("R"[pig],">=3"))))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p=0.939;

n_classes_no_contact_contact <- ggplot(aes(x=mdr_contact, y=n_classes_no_contact),data = data)+
  geom_boxplot(color="dimgray")+
  annotate("text", x = 1, y =7.5, label = "n=125", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=39", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(1,8), oob = scales::squish)+
  ylab(expression(paste("R"[contact]))) + 
  xlab(expression("R"[pig],"<3"))+ scale_x_discrete (labels = c(expression(paste("R"[no-contact],"<3")),expression(paste("R"[no-contact],">=3"))))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p=0.575

min_dis_DS_contact <- ggplot(aes(x=mdr_contact , y=min_dis_DS),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 25, label = "n=125", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 25, label = "n=39", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 28), oob = scales::squish)+
  ylab("Min. dist. drug stores") +
  xlab("Resistance - contact humans")+ scale_x_discrete (labels = c(expression(paste("R"[contact],"<3")),expression(paste("R"[contact],">=3"))))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p=0.699;

#cat variables 
tapply(data$n_classes_contact, data$disease_count, summary) #to get median and range
wilcox_test(n_classes_contact~used_drugs_recently,data=data) #to get test statistics
wilcox_effsize(n_classes_contact~farm_size,data=data) #to get effect size


disease_count_contact <- ggplot(aes(x=disease_count, y=n_classes_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=90", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=74", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4',color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[contact]))) + 
  xlab("Herd disease")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(legend.position = "none", 
        panel.background = element_blank())
#p=0.554

used_atb_recently_contact <- ggplot(aes(x=used_drugs_recently, y=n_classes_contact),data = data)+
  geom_boxplot()+
  annotate("text", x = 1, y = 7.5, label = "n=34", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=130", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[contact]))) + 
  xlab("Used drugs recently")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p<0.001; ES=0.337 moderate
special_feed_contact <- ggplot(aes(x=special_feed, y=n_classes_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=139", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=25", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[contact]))) + 
  xlab("Feed additive")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p<0.001; ES=0.337 moderate

manure_management_contact <- ggplot(aes(x=manure_management, y=n_classes_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=38", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=89", size = 3, color="dimgray")+
  annotate("text", x = 3, y = 7.5, label = "n=2", size = 3, color="dimgray")+
  annotate("text", x = 4, y = 7.5, label = "n=35", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[contact]))) + 
  xlab("Manure management")+ scale_x_discrete (labels = c("Discard", "Fertilizer", "Fuel", "Sold"), guide = guide_axis(n.dodge = 2))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p= all NS; ES= all small

farm_size_contact <- ggplot(aes(x=farm_size, y=n_classes_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=113", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=51", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color='lightsteelblue2', alpha=0.4)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab("mResC") + xlab("Farm Size")+
  stat_summary(aes(y = n_classes_contact,group=1), fun.y=mean, colour="brown4", geom="line",group=1)+
  stat_summary(aes(y = n_classes_contact,group=1), fun.data=mean_se, colour="brown4", geom="linerange",group=1)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())
#p=0.398;
ARRANGE_contact <- ggarrange( disease_count_contact, used_atb_recently_contact,special_feed_contact, manure_management_contact,n_classes_pigs_contact, n_classes_no_contact_contact, nrow=1, ncol=6)
ARRANGE_contact2 <- ggarrange(amu_contact, min_dis_DS_contact, nrow=1, ncol=2)
ARRANGE_contact3 <- ggarrange(ARRANGE_contact2, ARRANGE_contact, nrow=2, ncol=1)

####### NO CONTACT #######

tapply(data$amu, data$mdr_no_contact, summary) #to get median and range
wilcox_test(min_dis_DS~mdr_no_contact,data=data) #to get test statistics
wilcox_effsize(n_classes_pig~mdr_no_contact,data=data) #to get effect size

amu_no_contact <- ggplot(aes(x=mdr_no_contact, y=amu),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y =595, label = "n=138", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 595, label = "n=26", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 610), oob = scales::squish)+
  ylab("Antimicrobial use") +
  xlab("Resistance - no-contact humans")+ scale_x_discrete (labels = c(expression(paste("R"[no-contact],"<3")),expression(paste("R"[no-contact],">=3"))))+
  theme(legend.position = "none", 
        panel.background = element_blank())

# p=0.875;
summary(data$mdr_contact)

number_sows_no_contact <- ggplot(aes(x=mdr_no_contact, y=number_sows),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y =405, label = "n=138", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 405, label = "n=26", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 410), oob = scales::squish)+
  ylab("Number of sows") + xlab("Resistance - no-contact humans")+ scale_x_discrete (labels = c(expression(paste("R"[contact],"<3")),expression(paste("R"[contact],">=3"))))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())

#p=0.971;

n_classes_pigs_no_contact <- ggplot(aes(x=mdr_no_contact, y=n_classes_pig),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y =7.5, label = "n=138", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=26", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(1,8), oob = scales::squish)+
  ylab(expression(paste("R"[no-contact]))) + 
  xlab(expression("R"[pig],"<3"))+ scale_x_discrete (labels = c(expression(paste("R"[pig],"<3")),expression(paste("R"[pig],">=3"))))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())

#p=0.286; ES=0.0835 small


n_classes_contact_no_contact <- ggplot(aes(x=mdr_no_contact, y=n_classes_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y =7.5, label = "n=138", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=26", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(1,8), oob = scales::squish)+
  ylab(expression(paste("R"[no-contact]))) + 
  xlab(expression("R"[contact],"<3"))+ scale_x_discrete (labels = c(expression(paste("R"[contact],"<3")),expression(paste("R"[contact],">=3"))))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())

#p=0.877;

min_dis_DS_no_contact <- ggplot(aes(x=mdr_no_contact , y=min_dis_DS),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 25, label = "n=138", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 25, label = "n=26", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 28), oob = scales::squish)+
  ylab("Min. dist. drug stores") +
  xlab("Resistance - no-contact humans")+ scale_x_discrete (labels = c(expression(paste("R"[no-contact],"<3")),expression(paste("R"[no-contact],">=3"))))+
  theme(legend.position = "none", 
        panel.background = element_blank())

#p=0.803; 

#cat variables 
tapply(data$n_classes_contact, data$disease_count, summary) #to get median and range
wilcox_test(n_classes_no_contact~farm_size,data=data) #to get test statistics
wilcox_effsize(n_classes_no_contact~disease_count,data=data) #to get effect size


disease_count_no_contact <- ggplot(aes(x=disease_count, y=n_classes_no_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=90", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=74", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4',color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[no-contact]))) + 
  xlab("Herd disease")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(legend.position = "none", 
        panel.background = element_blank())

#p=0.0703; 

used_atb_recently_no_contact <- ggplot(aes(x=used_drugs_recently, y=n_classes_no_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=34", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=130", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[no-contact]))) + 
  xlab("Used drugs recently")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())


special_feed_no_contact <- ggplot(aes(x=special_feed, y=n_classes_no_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=139",size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=25", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[no-contact]))) + 
  xlab("Feed additive")+ scale_x_discrete (labels = c("No", "Yes"))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())


manure_management_no_contact <- ggplot(aes(x=manure_management, y=n_classes_no_contact),data = data)+
  geom_boxplot(color="dimgrey")+
  annotate("text", x = 1, y = 7.5, label = "n=38", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 7.5, label = "n=89", size = 3, color="dimgray")+
  annotate("text", x = 3, y = 7.5, label = "n=2", size = 3, color="dimgray")+
  annotate("text", x = 4, y = 7.5, label = "n=35", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color=NA, alpha=0.3)+
  scale_y_continuous(limits = c(0, 8), oob = scales::squish)+
  ylab(expression(paste("R"[no-contact]))) + 
  xlab("Manure management")+ scale_x_discrete (labels = c("Discard", "Fertilizer", "Fuel", "Sold"), guide = guide_axis(n.dodge = 2))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())

#p= all NS; 

farm_size_no_contact <- ggplot(aes(x=farm_size, y=n_classes_no_contact),data = data)+
  geom_boxplot()+
  annotate("text", x = 1, y = 7.6, label = "a", size = 4) +
  annotate("text", x = 2, y = 7.6, label = "a", size = 4) +
  annotate("text", x = 1, y = 8.2, label = "n=113", size = 3, color="dimgray")+
  annotate("text", x = 2, y = 8.2, label = "n=51", size = 3, color="dimgray")+
  geom_violin(fill='steelblue4', color='lightsteelblue2', alpha=0.4)+
  scale_y_continuous(limits = c(0, 9), oob = scales::squish)+
  ylab("mResNC") + xlab("Farm Size")+
  stat_summary(aes(y = n_classes_no_contact,group=1), fun.y=mean, colour="brown4", geom="line",group=1)+
  stat_summary(aes(y = n_classes_no_contact,group=1), fun.data=mean_se, colour="brown4", geom="linerange",group=1)+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank())

#p=0.0686;

ARRANGE_no_contact <- ggarrange(disease_count_no_contact, used_atb_recently_no_contact, special_feed_no_contact, manure_management_no_contact, n_classes_pigs_no_contact, n_classes_contact_no_contact,nrow=1, ncol=6)
ARRANGE_no_contact2 <- ggarrange(amu_no_contact, min_dis_DS_no_contact, nrow=1, ncol=2)
ARRANGE_no_contact3 <- ggarrange(ARRANGE_no_contact2,ARRANGE_no_contact , nrow=2, ncol=1)



######### LOADING MY SHAPE FILE FOR MAPS #################################################################################
shape = shapefile('/Users/laurahuber/Desktop/data_analysisThai_zip/THA_L1_UTM47.shp')
spgeo <- spTransform(shape, CRS("+proj=longlat +datum=WGS84"))
shapeDD = shapefile('/Users/laurahuber/Desktop/data_analysisThai_zip/THA_L3_DD.shp')

polygon_thai <- subset(x = spgeo,
                       subset = ADL1CC == "THA40") # this is specifically the KK province where my real data was collected

polygon_thai2<- fortify(polygon_thai) # then I can use this one that contains only the polygon of interest to do the plotting
new_spatial_object <- intersect(x = polygon_thai, y = shapeDD)
new_spatial_object2<- fortify(new_spatial_object)

# Thai map with the districts
thaimap3 <- ggplot() +
  geom_polygon(data = new_spatial_object,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = "312",
               fill = NA,
               size=0.1) +
  geom_polygon(data = polygon_thai2,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = "black", size=0.5,
               fill = NA)






######## MAPS ###################################################################################
#Originals

pigs <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 8, size=0.4, colour = "blue", alpha = 0.85, position=position_jitter(width=0.01, height=0.01)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, fill = as.factor(n_classes_pig)), 
             shape = 21, alpha = 0.7, size=2, position=position_jitter(width=0.01, height=0.01)) + 
  theme_void() + 
  scale_fill_brewer(palette = "Reds") + 
  coord_fixed(xlim =c(101.7, 103.2), ylim = c(15.7, 17.1)) + 
  labs(fill="Number of ATB classes",  title="Pigs - Overall")

zoompigs <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 8, size=2, colour = "blue", alpha = 0.85) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, fill = as.factor(n_classes_pig)), 
             shape = 21, alpha = 0.7, size=2, position=position_jitter(width=0.01, height=0.01)) + 
  theme_void() + 
  scale_fill_brewer(palette = "Reds") + 
  labs(fill="Number of ATB classes",  title="Pigs - Overall") +
  coord_fixed(xlim =c(102.8, 103.05), ylim = c(16.35, 16.75))


pigsMDR <- 
  thaimap3 +
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=3, colour = "blue", alpha = 0.6,position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_pig)), 
             size = 3, alpha = 0.5) +  
  theme_void() +  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(101.7, 103.2), ylim = c(15.7, 17.1))



ZoompigsMDR <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=4, colour = "blue", position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_pig)), 
             size = 6, alpha = 0.4) +  
  theme_void() +  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(102.92, 103.04), ylim = c(16.45, 16.53)) 


ZoompigsMDR2 <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=4, colour = "blue", position=position_jitter(width=0.01, height=0.01)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_pig)), 
             size = 6, alpha = 0.4) +  
  theme_void() +  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(102.3, 102.4), ylim = c(15.84, 15.96))


contact <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 3, size=2, colour = "black", alpha = 0.85, position=position_jitter(width=0.01, height=0.01)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, fill = as.factor(n_classes_contact)), 
             shape = 21, alpha = 0.7, size=2, position=position_jitter(width=0.01, height=0.01)) + 
  theme_void() + 
  scale_fill_brewer(palette = "Reds") + 
  coord_fixed(xlim =c(101.7, 103.2), ylim = c(15.7, 17.1)) + 
  labs(fill="Number of ATB classes",  title="Contact - Overall")

contactMDR <- 
  thaimap3 +
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=3, colour = "blue", alpha = 0.6, position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_contact)), 
             size = 3, alpha = 0.5) +  
  theme_void() +  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(101.7, 103.2), ylim = c(15.7, 17.1))

contactMDRZoom1 <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=4, colour = "blue", position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_contact)), 
             size = 6, alpha = 0.4) +  
  theme_void() +  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(102.91, 103.04), ylim = c(16.45, 16.54)) 


contactMDRZoom2 <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=4, colour = "blue", position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_contact)), 
             size = 6, alpha = 0.4) +  
  theme_void()+  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,22))+
  coord_fixed(xlim =c(102.3, 102.4), ylim = c(15.84, 15.96))

NOcontact <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=3, colour = "blue", alpha = 0.6, position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_no_contact)), 
             size = 3, alpha = 0.5) +  
  theme_void() +  
  scale_fill_brewer(palette = "Reds") + 
  coord_fixed(xlim =c(101.7, 103.2), ylim = c(15.7, 17.1)) + 
  labs(fill="Number of ATB classes",  title="No Contact - Overall")

NOcontactMDR <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=3, colour = "blue", alpha = 0.6, position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_no_contact)), 
             size = 3, alpha = 0.5) +  
  theme_void() + 
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(101.7, 103.2), ylim = c(15.7, 17.1))


NOcontactMDRZoom1 <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=4, colour = "blue", position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_no_contact)), 
             size = 6, alpha = 0.4) +  
  theme_void()+  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(102.93, 103.04), ylim = c(16.45, 16.54)) 

NOcontactMDRZoom2 <- 
  thaimap3 + 
  geom_point(drugStore_coord, mapping=aes(x = as.numeric(long_drugStore), y = as.numeric(lat_drugStore)), 
             shape = 18, size=4, colour = "blue", position=position_jitter(width=0.005, height=0.005)) +
  geom_point(data, mapping=aes(x = y_farm, y = x_farm, shape = as.factor(farm_size), fill = as.factor(mdr_no_contact)), 
             size = 6, alpha = 0.4) +  
  theme_void()+  
  scale_fill_manual(values = c("dimgray", "red"))+
  scale_shape_manual(values = c(21,24))+
  coord_fixed(xlim =c(102.3, 102.4), ylim = c(15.84, 15.96))


#### Models #####

# number of bootstraps
nBS = 10 

# number of replications of the datasets, to transform into binary
nReps = 5

# create a list to store bootstrapped coefficients
pigs_bootstrap_list = list()


#make an empty DB that will be 5 times bigger than my original dataframe that each time will evaluate if my proportion of resistance is higher than the random (1) or lower (0).

for (j in 1:nBS){
  
  DFbrt = NULL
  for (i in 1:nReps){
    DFbrt=rbind(DFbrt,data)
  }
  
  DFbrt$HasRes_pigs = ifelse(DFbrt$pro_resist_pigs > runif(nrow(DFbrt)),1,0)
  DFbrt$HasRes_pigs <- as.factor(DFbrt$HasRes_pigs)
  #insert my model
  bino <-glm(HasRes_pigs ~  amu + used_drugs_recently+min_dis_DS+
               farm_size,
             DFbrt, family=binomial) 
  #save coefficients of the model
  Vcov <- vcov(bino, useScale = FALSE)
  betas <- coefficients(bino)
  se <- sqrt(diag(Vcov))
  zval <- betas / se
  pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
  
  dat <- data.frame(cbind(betas,se, zval, pval))
  dat$j <- j  # track of which iteration produced it
  pigs_bootstrap_list[[j]] <- dat # add it to list
}


all_pigs_bootstrap = do.call(rbind,  pigs_bootstrap_list)

###### Model considering farm_size as a factor, because it is likely a confounder factor, it is associates with both explanatory variables 
# (AMU, number of pigs, used drugs recently, presence of disease, minimal distance), and outcome 
#outcome will be either the porportion of resistant classses/7 or the count number of classes  - both binomial distributions

y= data.matrix(data$pro_resist_pigs)
x = data.matrix(data[,c("amu", "used_drugs_recently","farm_size","manure_management","special_feed",
                        "total_numberpigs","pro_resist_contact","pro_resist_no_contact","min_dis_DS")])


LASSO_CLASSES_PIG = cv.glmnet(x,y,alpha =1, foldid = data$folds)
lambda_CLASSES_PIG = LASSO_CLASSES_PIG$lambda.min
CoefsLASSO_CLASSES_PIG = coef(LASSO_CLASSES_PIG, s = lambda_CLASSES_PIG) #drops all variables

mOriginal <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+min_dis_DS+
                  farm_size+min_dis_DS*farm_size,
                data, family=binomial)
summary(mOriginal)

full <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+farm_size+special_feed+min_dis_DS+
             amu*farm_size+total_numberpigs*farm_size+min_dis_DS*farm_size+special_feed*farm_size,
           data, family=binomial) 
summary(full)
m1 <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+farm_size+special_feed+min_dis_DS+
           total_numberpigs*farm_size+min_dis_DS*farm_size+special_feed*farm_size,
         data, family=binomial)
summary(m1)

anova(full,m1, test="Chisq") #Not significant, so dropping is adequate

m2 <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+farm_size+special_feed+min_dis_DS+
           total_numberpigs*farm_size+special_feed*farm_size,
         data, family=binomial)

anova(m1, m2, test="Chisq")#Not significant, so dropping is adequate
m3 <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+farm_size+special_feed+
           total_numberpigs*farm_size+special_feed*farm_size,
         data, family=binomial)

anova(m2, m3, test="Chisq")#Not significant, so dropping is adequate

m4 <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+farm_size+special_feed+
           total_numberpigs*farm_size,
         data, family=binomial)

anova(m3, m4, test="Chisq")#Not significant, so dropping is adequate

m5 <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+farm_size+
           total_numberpigs*farm_size,
         data, family=binomial)

summary(m5)
anova(m4, m5, test="Chisq")#Not significant, so dropping is adequate

mOriginal <-glm(pro_resist_pigs ~  amu + used_drugs_recently+total_numberpigs+min_dis_DS+
                  farm_size+min_dis_DS*farm_size,
                data, family=binomial)

summary(mOriginalC <-glm(pro_resist_pigs ~  amu + used_drugs_recently+min_dis_DS+
                           farm_size+min_dis_DS*farm_size,
                         data, family=binomial))
summary(mOriginalD <-glm(pro_resist_pigs ~  amu + used_drugs_recently+min_dis_DS+
                           farm_size,
                         data, family=binomial))

anova(mOriginalNull, mOriginalD, test="Chisq")#Not significant, so dropping is adequate


plot(fitted(mOriginalD), residuals(mOriginalD), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(mOriginalD), residuals(mOriginalD)))

plot(mOriginalD)
plot_summs(mOriginalD)

#calculate RAC
pred_mOriginalD = predict(mOriginalD, type = "response") #Calculate RAC
res_mOriginalD = pred_mOriginalD-(data$pro_resist_pigs)
data2 = cbind(data,res_mOriginalD)
data2$RAC_mOriginalD = rep(NA,nrow(data2))
for (i in 1:nrow(data2)){
  data2$RAC_mOriginalD[i] = sum(data2$res_mOriginalD *(1 / data2$min_dis[i]))
}
mOriginalD_RAC = glm(pro_resist_pigs ~  amu + used_drugs_recently+min_dis_DS+
                       farm_size+RAC_mOriginalD,
                     data2, family=binomial)

plot(fitted(mOriginalD), residuals(mOriginalD), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(mOriginalD), residuals(mOriginalD)))

#By adding RAC, model fit is not improved and RAC is not significant.



















