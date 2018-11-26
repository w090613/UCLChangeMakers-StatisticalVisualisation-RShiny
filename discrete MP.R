#install.packages('markovchain')
library(markovchain)
weather<-c('sunny','windy','snowy')
TM<-matrix(c(0.9,0.05,0.05,0.6,0.25,0.15,0.25,0.25,0.5),nrow = 3,byrow = TRUE, dimnames = list(weather))
TM
MC<-new("markovchain",states= weather, byrow=TRUE, transitionMatrix= TM,
                    name='Da Jia Hao')
MC
install.packages('diagram')
library('diagram')

plot(MC, pos = c(1,2), lwd=3, box.lwd = 1, cwx.txt=0.5, box.size=0.1, 
        box.type = "circle", box.prop = 0.5, box.col = "green", arr.length = 1, arr.width = 1,
        self.cex = .4, self.shifty = -.01, self.shiftx = .13, main = "Transition diagram")
initialstate<-c(0.7,0.2,0.1)
transitionProbability(MC,'sunny','windy')
print(MC)
days<-2
weather_after_days<-initialstate*(MC^days)
weather_after_days
steadyStates(MC)
summary(MC)
