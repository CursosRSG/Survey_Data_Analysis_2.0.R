# Data analysis for the RSG-Brazil Educational Committee Survey 2024
#
# Maira Neves
# maira.rn@gmail.com 
#
# Nilson Antonio da Rocha Coimbra
# nilson.coimbra@fm.usp.br 
#
# Lucas Azevedo
# lucas.aleixoleal17@gmail.com
#
################################################################################

# Libraries

###############################################################################
library(readr)
library(sf)
library(geobr)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(RColorBrewer)
library(ggpubr)
library(viridis)
library(ggalluvial)
library(cowplot)

################################################################################

# Setup main directory 

setwd("/Users/coimbra/Desktop/RSG-BRAZIL/COIMBRA_et_al")

################################################################################

# CSV parser

###############################################################################

resp_2023 = read.csv(file = "demanda_april.csv")
colnames(resp_2023) = c("timestamp","aceptance","color","gender",
                        "state","academic.level","profile",
                        "profile.user","profile.scientist","others",
                        "profile.developer","quant.bio","quant.stat",
                        "quant.comp","quant.etica","quant.usability",
                        "quant.comunication","quant.development",
                        "topics","knowledge.level")
################################################################################

# Data cleaning

###############################################################################

resp_2023 = resp_2023[8:length(resp_2023$timestamp),]
resp_2023 <- resp_2023[!grepl("No", resp_2023$aceptance),]

################################################################################

# Color pallet definition

###############################################################################

paleta = colorRampPalette(brewer.pal(12, "Paired"))
paleta25 = paleta(25)
paleta10 = paleta(10)

################################################################################
#Gender Distribution Analysis

GENDER <-
  ggplot(resp_2023) +
  X11(width=4,height=3) + 
  geom_bar( mapping = aes(x = gender, fill = profile), 
            show.legend = TRUE, width = 0.9, angle=-30) +
  theme_bw()+xlab("Gender Identification")+ ylab("Count")+
  scale_fill_viridis("Profile",option="viridis",discrete=TRUE)+
  #scale_fill_manual("Profile")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Gender Identification", fill="") 
ggsave("Gender_profile.png")
dev.off()


#Profile Carrer Distribution Analysis per Academic Level

PDA <-
  ggplot(resp_2023) +
  X11(width=4,height=3) + 
  geom_bar( mapping = aes(x = academic.level, fill = profile), 
            show.legend = TRUE, width = 0.9, angle=-30) +
  theme_bw()+xlab("Academic level")+ ylab("Count")+
  #scale_fill_manual("Profile", values =COR6)+
  scale_fill_viridis("Profile",option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Academic Level", fill="Academic Level") 
ggsave("Distribuition_profile.png")
dev.off()

#Carrer Profile Analysis per Academic Level

#Developers#
DEV <- 
  ggplot(data = resp_2023[resp_2023$profile == "Developer",]) +
  geom_bar(mapping = aes(profile.developer, fill = academic.level)) +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme(axis.title.x = element_blank(),  # Remove x axis title
        axis.title.y = element_blank(),  # Remove y axis title
        axis.text.x = element_text(size = 7, angle = -30, hjust = 0.1),
        plot.title = element_text(size = 12, hjust = 0.5))+
  labs(title = "Developers", fill = "Academic Level") + 
  coord_polar("y", start = 0, direction = +1) +
  xlab("Career Profile") + ylab("Distribution") +
  theme_bw()+xlab("Academic Level")+ ylab("Distribuition")+
  theme(aspect.ratio = 1)
ggsave("Distribuition_Developers.png", DEV, width=8, height=10, units="in")
dev.off()

#Carrer Profile - Users#
USER <- ggplot(data = resp_2023[resp_2023$profile == "User",]) +
  geom_bar(mapping = aes(academic.level, fill = profile.user)) +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme(axis.title.x = element_blank(),  # Remove x axis title
        axis.title.y = element_blank(),  # Remove y axis title
        axis.text.x = element_text(size = 7, angle = -30, hjust = 0.1),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  labs(title = "Users", fill = "Carrer Profile") + 
  coord_polar("y", start = 0, direction = +1) +
  xlab("Academic Level") + ylab("Distribution") +
  theme_bw()+xlab("Academic Level")+ ylab("Distribuition")+
  theme(aspect.ratio = 1)
ggsave("Distribution_Users.png", USER, width = 8, height = 10, units = "in")
dev.off()

#Carrer Profile - Scientist#
# Plot 4: Career Distribution by Academic Level (Scientists)
SCIENTIST <- ggplot(data = resp_2023[resp_2023$profile == "Scientist",]) +
  geom_bar(mapping = aes(academic.level, fill = profile.scientist)) +
  theme_bw() +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme(axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.x = element_text(size = 7, angle = -30, hjust = 0.1),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  labs(title = "Scientists", fill = "Career Profile") + 
  coord_polar("y", start = 0, direction = +1) +
  xlab("Academic Level") + ylab("Distribution") +
  theme_bw()+xlab("Academic Level")+ ylab("Distribuition")+
  theme(aspect.ratio = 1)
ggsave("Distribution_Scientists.png", SCIENTIST, width = 8, height = 10, units = "in")
dev.off()

GROUP_PROFILE <- plot_grid(USER, SCIENTIST,DEV, nrow=3, ncol=1, 
                           labels=LETTERS[1:3], axis = "r", align="hv")
ggsave("FIGURE_01.png",GROUP_PROFILE)

################################################################################

#Bachelor / Undergraduate Student#
d.academic.level =
  ggplot(data=resp_2023[resp_2023$academic.level == "Bachelor / Undergraduate Student ",])
GRADUATE <- d.academic.level +
  geom_bar(mapping=aes(state, fill=profile)) +
  theme_bw() +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Bachelor / Undergraduate Student ", fill="Profile")
ggsave("Academic_level_Graduate.png", width=8, height=10, units="in")
dev.off()

#Master / Master Student #
d.academic.level =
  ggplot(data=resp_2023[resp_2023$academic.level == "Master / Master Student" ,])
MASTERS <- d.academic.level +
  geom_bar(mapping=aes(state, fill=profile)) +
  theme_bw() +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Master / Master Student" , fill="Profile")
ggsave("Academic_level_Masters.png", width=8, height=10, units="in")
dev.off()

#PhD / PhD Student#
d.academic.level =
  ggplot(data=resp_2023[resp_2023$academic.level == "PhD / PhD Student" ,])
DOCTORS <- d.academic.level +
  geom_bar(mapping=aes(state, fill=profile)) +
  theme_bw() +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="PhD / PhD Student" , fill="Profile")
ggsave("Academic_level_PhD.png", width=8, height=10, units="in")
dev.off()

#Professor / PI#
d.academic.level =
  ggplot(data=resp_2023[resp_2023$academic.level == "Professor / PI" ,])
PROFESSOR <- d.academic.level +
  geom_bar(mapping=aes(state, fill=profile)) +
  theme_bw() +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Professor / PI" , fill="Profile")
ggsave("Academic_level_PI.png", width=8, height=10, units="in")
dev.off()

#Posdoc#
d.academic.level =
  ggplot(data=resp_2023[resp_2023$academic.level == "Posdoc"  ,])
POSDOC <- d.academic.level +
  geom_bar(mapping=aes(state, fill=profile)) +
  theme_bw() +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Posdoc", fill="Profile")
ggsave("Academic_level_Posdoc.png", width=8, height=10, units="in")
dev.off()

# Technician / Specialist #
d.academic.level =
  ggplot(data=resp_2023[resp_2023$academic.level == "Technician / Specialist"  ,])
TECHNICIAN <-d.academic.level +
  geom_bar(mapping=aes(state, fill=profile)) +
  theme_bw() +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=7, angle=-30, hjust=0.1),
        plot.title=element_text(size=12, hjust=0.5)) +
  labs(title="Technician / Specialist", fill="Profile")
ggsave("Academic_level_Technicien.png", width=8, height=10, units="in")
dev.off()

GROUP_ACADEMIC <- plot_grid(GRADUATE, MASTERS, DOCTORS, POSDOC, TECHNICIAN, 
                            PROFESSOR, nrow=3, ncol=2, labels=LETTERS[1:6])
ggsave("FIGURE_02.png",GROUP_ACADEMIC)


################################################################################

# Build geographical heatmap

###############################################################################

# set axis 
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# read states
states <- read_state(year = 2020, showProgress = FALSE)

#collect collumn states from data 
fstate = resp_2023$state
count_state <- plyr::count(fstate)
count_state$x <- tolower(count_state$x)
states$name_state <- tolower(states$name_state)
count_state = count_state[2:length(count_state$x),]
#simple regular expression to remove (*) from answers
count_state$x <- gsub("\\s*\\([^\\)]+\\)","",as.character(count_state$x))
#identify non reached states within states
non_reach <- setdiff(states$name_state,count_state$x)
#add non reached states within the dataframe
for(i in non_reach){
  count_state[nrow(count_state) + 1,] = c(i,0)
}
#remove rows marked with "resido fora do brasil" from dataframe
count_state <- count_state[!grepl("resido fora do brasil", count_state$x),]
count_state$freq <- as.numeric(count_state$freq)*1.1
#merging dataframe and preparasion for plot 
states <- dplyr::left_join(states, count_state, by = c("name_state" = "x"))
BIOINFOBR <- ggplot() +
  geom_sf(data=states, aes(fill=freq), color= "Black", size=.15) +
  labs(subtitle="Number of Participants per Brazilian State" , size=12) +
  #scale_fill_viridis_c(name = "N", limits = c(1, 100)) +  # Set palette to "viridis"
  #scale_fill_viridis_c(name = "N", option = "turbo", limits = c(1, 100)) + 
  scale_fill_distiller(palette = "Spectral", name="N", limits = c(1,100)) +
  theme_bw()
ggsave("FIGURE_XX-Participants_per_state.png", width=8, height=10, units="in")
dev.off()

################################################################################

# Domain Topics to be considered to tailor and design training/course

###############################################################################

# T1 - Knowledge in Biology
g.quant.bio = ggplot(data=resp_2023)
T1 <- g.quant.bio +
  geom_bar(mapping=aes(quant.bio), fill = "#CC3333", colour = "black") +
  ggtitle("T1 - Knowledge in Biology") +
  theme_bw() +
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=25)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants") 
ggsave("T1_Knowledge_in_Biology.png")
dev.off()

# T2 - Statistical Methods
g.quant.stat = ggplot(data=resp_2023)
T2 <- g.quant.stat +
  geom_bar(mapping=aes(quant.stat), fill = "#cc6600", colour = "black") + 
  theme_bw() +
  ggtitle("T2 - Statistics and Data Science") +
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=20)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants")
ggsave("T2_Methods_Statistics_Data_Science.stat.png")
dev.off()

# T3 - knowledge in computing and interdisciplinary programming in the area
g.quant.comp = ggplot(data=resp_2023)
T3 <- g.quant.comp +
  geom_bar(mapping=aes(quant.comp),fill = "#FFCC66", colour = "black") + 
  theme_bw() +
  ggtitle("T4 - Computing and Programing") +
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=20)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants")
ggsave("T3_Computation_Programing.stat.png")
dev.off()

# T4 - Ethical Implications and Repercussions of Bioinformatics in Society
g.quant.etica = ggplot(data=resp_2023)
T4 <-g.quant.etica +
  geom_bar(mapping=aes(quant.etica), fill = "#00CC66", colour = "black") + 
  theme_bw() +
  ggtitle("T4 - Ethichs ands Society") +
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=20)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants")
ggsave("T4_Ethics_Society.png")
dev.off()

# T5 - Uses of Bioinformatics
g.quant.usability = ggplot(data=resp_2023)
T5 <- g.quant.usability +
  geom_bar(mapping=aes(quant.usability),fill = "#66CCCC", colour = "black") + 
  theme_bw() +
  ggtitle("T5 - Uses of Bioinformatics") +
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=20)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants")
ggsave("T5_usability.png")
dev.off()

# T6 - Communication in bioinformatics
g.quant.comunication = ggplot(data=resp_2023)
T6 <- g.quant.comunication +
  geom_bar(mapping=aes(quant.comunication), fill = "#0066CC", colour = "black") + 
  theme_bw() +
  ggtitle("T6 - Communication in Bioinformatics") +
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=20)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants")
ggsave("T6_comunication.png")
dev.off()

# T7 - Continuous development in Bioinformatics
g.quant.development = ggplot(data=resp_2023)
T7 <-g.quant.development +
  geom_bar(mapping=aes(quant.development), fill = "#9933CC", colour = "black") + 
  theme_bw() +
  ggtitle(" T7 - Continuum Development") + 
  facet_grid(rows=vars(profile), scales="free", labeller=label_wrap_gen(width=20)) +
  theme(axis.text.x=element_text(size=8, hjust=0.1),
        plot.title=element_text(size=10, hjust=0.5)) +
  labs(x="CF-Score", y="Number of Participants")
ggsave("T7_development.png")
dev.off()

ggarrange(T1,T2,T3,T4,T5,T6,T7,
          labels = c("A", "B", "C", "D", "F", "G", "H")) + bgcolor("White")
ggsave("Domains_grouped_T2.png")

################################################################################

# Track demands per profile

###############################################################################
resp_2023$topics = as.character(resp_2023$topics)

# Data transformation
resp_2023.topics <- resp_2023 %>%
  select(profile, topics) %>%
  mutate(
    genomics = str_count(topics, "DNA and Genomics"),
    evolution = str_count(topics, "Phylogeny and Evolution"),
    metagenomics = str_count(topics, "Metagenomics and Microbiome"),
    transcriptomics = str_count(topics, "RNA and Transcriptomics"),
    epigenomics = str_count(topics, "Epigenomics"),
    proteomics = str_count(topics, "Proteins and Proteomics"),
    systembiology = str_count(topics, "Metabolomics and Systems Biology"),
    datascienceIA = str_count(topics, "Artificial Intelligence and Data Science in Bioinformatics"),
    databaseftengsoft = str_count(topics, "Database and Software Development"),
    cancerbases = str_count(topics, "Molecular Basis of Cancer"),
    personalizedmedicine = str_count(topics, "Personalized Medicine"),
    communication = str_count(topics, "Didactics and Communication in Bioinformatics"),
    structuralbiology = str_count(topics, "Structural Biology and Modeling"),
    mentorship = str_count(topics, "Career Mentoring in Bioinformatics")
  )

# Formating table 
resp_2023.g.t = data.frame(matrix(ncol=1, nrow=42))
colnames(resp_2023.g.t) = "topics"
topics_2023 = colnames(resp_2023.topics)[4:17]
resp_2023.g.t$topics = rep(topics_2023, 3)
resp_2023.g.t$profile = c(rep("User", 14),							
                          rep("Scientist", 14),							
                          rep("Developer", 14))							

resp_2023.g.t$counts = c(rep(0, 42))							

# Populating table according profile and topics							
resp_2023.g.t$counts <- 0  
for (profile in c("User", "Scientist", "Developer")) {
  for (i in 1:14) {
    index <- (i + (14 * (which(c("User", "Scientist", "Developer") == profile) - 1)))
    resp_2023.g.t[index, 3] <- sum(resp_2023.topics[resp_2023.topics$profile == profile, i + 2])
  }
}
print(resp_2023.g.t)
# Create the data.frame directly
resp_2023.df <- data.frame(
  topics = rep(df_topics_2023, each = 3),
  knowledge = sort(rep(df_knowledge_2023, each = 42)),
  profile = rep(c("User", "Scientist", "Developer"), each = 42),
  freq = rep(0, 126)
)
print(resp_2023.df)

# # side by side
# g.temas = ggplot(data=resp_2023.g.t)
# g.temas +
#   geom_col(mapping=aes(topics, counts, fill=profile), position="dodge") +
#   labs(title="Demands in Training", fill="Profiles") +theme_bw()+coord_flip() +
#   labs(x="Tracks", y="Votes") +
#   scale_fill_viridis(option="viridis",discrete=TRUE)+
#   theme(axis.text.x=element_text(size=5, hjust=0, vjust=0.5),
#         plot.title=element_text(size=10, hjust=0.5))
# ggsave("Demands_needs_profiles.png")
# dev.off()


# piled up 
g.temas2 = ggplot(data=resp_2023.g.t)
g.temas2 +
  geom_col(mapping=aes(topics, counts, fill=profile)) + coord_flip()+
  labs(title="Demands in Training", fill="Profile") + theme_bw() + 
  labs(x="Tracks", y="Votes") +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.text.x=element_text(size=5, hjust=0, vjust=0.5),
        plot.title=element_text(size=10, hjust=0.5))
ggsave("Demands_needs_profile_pilleup.png")
dev.off()




################################################################################

# Tailoring Needs

###############################################################################
resp_2023.knowledge = resp_2023[,c(1, 5, 7)]
resp_2023$knowledge.level = as.character(resp_2023$knowledge.level)
resp_2023.knowledge$basic = str_count(resp_2023$knowledge.level, "Basic")
resp_2023.knowledge$intermediate = str_count(resp_2023$knowledge.level, "Intermediate")
resp_2023.knowledge$advanced = str_count(resp_2023$knowledge.level, "Advanced")

resp_2023.kl = data.frame(matrix(ncol=1, nrow=9))
colnames(resp_2023.kl) = "knowledge"
knowledge_2023 = colnames(resp_2023.knowledge)[4:6]
resp_2023.kl$knowledge = rep(knowledge_2023, 3)
resp_2023.kl$profile = c(rep("User", 3),							
                         rep("Scientist", 3),							
                         rep("Developer", 3))		
resp_2023.kl$counts = c(rep(0, 9))							
resp_2023.kl[1,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "User", "basic"])
resp_2023.kl[2,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "User", "intermediate"])
resp_2023.kl[3,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "User", "advanced"])
resp_2023.kl[4,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "Scientist", "basic"])
resp_2023.kl[5,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "Scientist", "intermediate"])
resp_2023.kl[6,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "Scientist", "advanced"])
resp_2023.kl[7,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "Developer", "basics"])
resp_2023.kl[8,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "Developer", "intermediate"])
resp_2023.kl[9,3] = sum(resp_2023.knowledge[resp_2023.knowledge$profile == "Developer", "advanced"])

ALLUVIAL <-
  ggplot(data = resp_2023.kl,
         aes(axis1 = profile, 
             axis2 = knowledge,
             y = counts)) +
  geom_alluvium(aes(fill = profile)) +
  geom_stratum() +
  theme_bw()+
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Profile", "Level of Training"),
                   expand = c(0.25, 0.05)) +
  labs(title="Tailoring in Training")+
  labs(x="Needs", y="Demand") +
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(axis.text.x=element_text(size=10, hjust=0, vjust=0.5),
        plot.title=element_text(size=10, hjust=0.5))
ggsave("Alluvial_needs.png")
dev.off()
theme_void() 




