#This script reports the codes used to generate the graphs for the paper on harke forms, Bagli (2023)

freq <- read.csv("form frequency.csv", header = TRUE, sep = ";")
library(tidyr)
library(dplyr)
library(ggstream)
library(ggplot2)
library(tidyverse)
library(gridExtra)

#The first graph charts the frequency of the different forms of the imperative form of the verb. 
#Let's upload the data.

df <- freq %>%
  select(year, harke, hearke, hark, heark) %>%
  gather(key = "variable", value = "value", -year)
ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable))+ 
  scale_color_manual(values = c("#d1b304", "#193fa8", "#d40dca", "#235204"))+
  theme_bw()+
  labs(title = "Frequency of forms", x = "year", y = "Frequency")+
  scale_y_continuous(breaks=seq(0, 600, by = 100)) +
  scale_x_continuous(breaks=seq(1500, 1690, by= 20))
  
#Let's have a look at the Frequency of infinitive forms in EEBO. 

dff<- freq %>% 
  select(year, harken, hearken) %>% 
  gather(key= "variable", value="value", -year)
ggplot(dff, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values=c("#d1b304", "#d40dca"))+
  theme_bw()+
  labs(title= "Frequency of infinitive forms", x= "year", y="Frequency")+
  scale_y_continuous(breaks=seq(0,4000, by= 500))+
  scale_x_continuous(breaks=seq(1470, 1690, by=20))

#Let's compare the frequency of hark(e) with other words that display a similar structure, namely dark(e) and bark(e).
#The aim is to show that final (e) drops in different forms in the first half of the 17th century.
#Let's start with the couple barke/bark

ark_df <- ark %>% 
  select(year, barke, bark) %>% 
  gather(key="variable", value= "value", -year)
ggplot(ark_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#09097d", "#800858", "#197806", "#0e0eed","#fa0aaa",  "#35ff0d"))+
  theme_bw()+
  labs(title = "bark(e) forms", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 6000, by=500))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20)) 

#Let's make a graph for the couple darke/dark as well.

ark_df <- ark %>% 
  select(year, darke, dark) %>% 
  gather(key="variable", value= "value", -year)
ggplot(ark_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#09097d", "#800858", "#197806", "#0e0eed","#fa0aaa",  "#35ff0d"))+
  theme_bw()+
  labs(title = "dark(e) forms", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 6000, by=500))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20)) 


#Let's move now to the analysis of individual forms and their syntactic configurations. 
#The variables  are called S (subordinate with subordinator), O (subrodinate with zero subordinator, Direct Object, relative clause), P (parenthetical/absolute usage), V (vocative construction, i.e. NP after verb)
#The data are stored in different data-sets. Let's start with the less frequent: hearke. 

hearke <- read.csv("Hearke_R.csv", header = TRUE, sep = ";")

#Now let's make it readable for ggplot.
hearke_df <- hearke %>% 
  select(year, O, P, S, V) %>% 
  gather(key="variable", value= "value", -year)

#Now let's create a line graph that shows us the development of the different syntactic usages through time. 

ggplot(hearke_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#FEB326", "#E84D8A", "#64C5EB", "#7F58AF"))+
  theme_bw()+
  labs(title = "Hearke", x="year", y= "Frequency")+
  scale_y_continuous(breaks = seq(0, 50, by=10))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20))

#Now let's create a line graph for heark. 

heark <- read.csv("Heark_R.csv", header = TRUE, sep = ";")

heark_df <- heark %>% 
  select(year, O, P, S, V) %>% 
  gather(key="variable", value= "value", -year)
ggplot(heark_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#FEB326", "#E84D8A", "#64C5EB", "#7F58AF"))+
  theme_bw()+
  labs(title = "Heark", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 100, by=10))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20))

# Now let's create one for harke. 

harke <- read.csv("Harke_R.csv", header = TRUE, sep = ";")

harke_df <- harke %>% 
  select(year, O, P, S, V) %>% 
  gather(key="variable", value= "value", -year)
ggplot(harke_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#FEB326", "#E84D8A", "#64C5EB", "#7F58AF"))+
  theme_bw()+
  labs(title = "Harke", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 200, by=10))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20))

#Now let's create one for hark. 

hark <- read.csv("Hark_R.csv", header = TRUE, sep = ";")

hark_df <- hark %>% 
  select(year, O, P, S, V) %>% 
  gather(key="variable", value= "value", -year)
ggplot(hark_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#FEB326", "#E84D8A", "#64C5EB", "#7F58AF"))+
  theme_bw()+
  labs(title = "Hark", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 400, by=50))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20))

#Let's have a look at the proliferation of forms

ee <- read.csv("ee_forms_tot.csv", header = TRUE, sep = ";")

ee %>%
  arrange(value) %>%    
  mutate(name=factor(name, levels=name)) %>%  
  ggplot( aes(x=name, y=value)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  labs(title="Spelling variations of harkee", y="Overall frequency")+
  scale_y_continuous(breaks = seq(0, 50, by=5))

#Let's try and map it over time. 

ee_time <- read.csv("ee_forms.csv", header = TRUE, sep = ";")

ee_time_df <- ee_time %>% 
  select(year, harkee, hark.e, hearkee, hark.ee, harkey) %>% 
  gather(key="variable", value= "value", -year)
ggplot(ee_time_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  theme_bw()+
  labs(title = "Harkee forms > 10", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 40, by=10))+
  scale_x_continuous(breaks = seq(1160, 1690, by=10))

#Objects of heark in V configuration

heark_V <- read.csv("heark_V_coll.csv", header = TRUE, sep = ";")

heark_V_df <- heark_V %>% 
  select(year,you, ye, thee, other) %>% 
  gather(key="variable", value = "value", -year)
ggplot(heark_V_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  theme_bw()+
  labs(title = "Heark V construction", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 40, by=10))+
  scale_x_continuous(breaks = seq(1550, 1690, by=10))

#Objects of hark in V configuration

hark_V <- read.csv("hark_V_coll.csv", header = TRUE, sep = ";")

hark_V_df <- hark_V %>% 
  select(year,you, ye, thee, other) %>% 
  gather(key="variable", value = "value", -year)
ggplot(hark_V_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  theme_bw()+
  labs(title = "Hark V construction", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 100, by=10))+
  scale_x_continuous(breaks = seq(1550, 1690, by=10))

#Development of hark_ee forms
hark_ee <- read.csv("hark_ee_forms.csv", header = TRUE, sep = ";")

hark_ee_df <- hark_ee %>% 
  select(year, heark.ye, hark.ye, hark.you, harkee, hearkee) %>% 
  gather(key="variable", value = "value", -year)
ggplot(hark_ee_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  theme_bw()+
  labs(title = "H(e)ark_ee forms", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 100, by=10))+
  scale_x_continuous(breaks = seq(1550, 1690, by=10))


#THE END! I HOPE YOU ENJOYED IT. It's super basic but it's the first piece of code I wrote ;-) so I'm still very proud.



#This is a bit of code that I wrote to compare the different spelling variants of the couples harke/hark, darke/dark, barke/bark. 
#It resulted in a Spaghetti plot so I did not use it. I decided to leave it here because it could serve as a basis for future graphs?


ark <- read.csv("similar forms.csv", header=TRUE, sep=";")

ark_df <- ark %>% 
  select(year, darke, dark, barke, bark, harke, hark) %>% 
  gather(key="variable", value= "value", -year)
ggplot(ark_df, aes(x=year, y=value))+
  geom_line(aes(color=variable))+
  scale_color_manual(values = c("#09097d", "#800858", "#197806", "#0e0eed","#fa0aaa",  "#35ff0d"))+
  theme_bw()+
  labs(title = "<ark(e)> forms", x = "year", y="Frequency")+
  scale_y_continuous(breaks = seq(0, 6000, by=500))+
  scale_x_continuous(breaks = seq(1500, 1690, by=20))





