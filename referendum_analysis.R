getwd()
uk.map <- readOGR("topo_lad.json", "lad")
## prepare map 
head(uk.map@data$id)
uk.map.df <- fortify(uk.map)
head(uk.map.df)
lad.map <- data.frame(id=0:(length(uk.map@data$id)-1),
                                 LAD12CD=as.character(uk.map@data$id))
head(lad.map)
### Make the maps
p <- ggplot(data=uk.map.df,
            aes(x=long, y=lat,
                group=group))

p1 <- p + geom_map(data = uk.map.df,
                   map = uk.map.df,
                   aes(map_id=id, x=long, y=lat),
                   color="white", size=0.2)

p2 <- p1 + coord_map(projection="albers", at0 = 51, lat1 = 0) + labs(x=NULL, y=NULL, fill="") +
  theme(panel.grid=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        axis.text=element_blank(),
        legend.position=c(0.8, 0.55))
p2

### already looking good

## referendum results
#http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/upcoming-elections-and-referendums/eu-referendum/electorate-and-count-information
referendum<-read.csv("data/EU-referendum-result-data.csv")
referendum<-referendum[,-1]
head(referendum)
referendum$outcome[1:100]
referendum$outcome[referendum$Pct_Leave>50]<-"out"
referendum$outcome[referendum$Pct_Leave<50]<-"in"
str(referendum)

str(uk.map.df)

uk.map.df2<-merge(uk.map.df, lad.map, by="id")
head(uk.map.df2)
uk.map.df3 <- merge(uk.map.df2, referendum, by.x="LAD12CD", by.y="Area_Code")
head(uk.map.df3)
p <- ggplot(data=uk.map.df3,
            aes(x=long, y=lat,
                group=group))

p1 <- p + geom_map(data = uk.map.df3,
                   map = uk.map.df3,
                   aes(map_id=id, x=long, y=lat, group=group, fill=Pct_Leave),
                   color="white", size=0.2)+
  coord_map(projection="albers", at0 = 51, lat1 = 0) + labs(x=NULL, y=NULL, fill="")

p2<-p1 + scale_fill_gradient2(name="% Leave",low = 'darkblue',high = 'darkred', midpoint = 50) +theme_map()
p2
ggsave("figures/referendum.png",
       p2,
       height=7.5,
       width=5,
       dpi=300)
theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.margin=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0), 
          legend.position=c(0.8, 0.55)
    )
}

# for spliting the data by Region, to summarise the vote
region<-referendum %>% group_by(Region) %>% summarise(total = length(outcome),
                                                  pct_in = sum(outcome=="in")/total,
                                                  pct_out = sum(outcome=="out")/total)

region                                                                 
library(reshape2)                                         
region2<-melt(region, id.vars = "Region", measure.vars = c("pct_in", "pct_out"))
region2
region2$Region<-as.character(region2$Region)
region2$Region[region2$Region=="Yorkshire and The Humber"]<-"Yorkshire"

ggplot(region2, aes(x=Region, y=value, fill=variable))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("steelblue", "red"),
                    name="Referendum", 
                    breaks=c("pct_in", "pct_out"),
                    labels=c("remain", "leave"))+
  theme_classic()+
  ylab("rel.value")+
  theme(line=element_line(size=1, color="black"),
        text = element_text(size=20, face="bold"),
        axis.title.y = element_text(vjust=1.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90))
ggsave("figures/regions_referendum.png")
## general election results 2015
# http://www.electoralcommission.org.uk/our-work/our-research/electoral-data
ge<-read.csv("RESULTS.csv")
head(ge)
class(ge$Constituency.ID)
# https://data.gov.uk/dataset/local-authority-districts-2012-to-westminster-parliamentary-constituencies-2012-uk-lookup
lad<-read.csv("data/LAD12_PCON12_UK_LU.csv")
head(lad1)
lad1<-lad[,c(1,4)]
colnames(lad1)<-c("LAD12CD", "Constituency.ID")
lad1$Constituency.ID<-as.character(lad1$Constituency.ID)
ge$Constituency.ID<-as.character(ge$Constituency.ID)
ge1<-merge(x=ge, y=lad1, by="Constituency.ID")
ge1[1:30,]

by.party <- ge1 %>% group_by(LAD12CD) %>% filter(Votes==max(Votes))  %>%
  ungroup() %>% data.frame(.)
head(by.party)
by.party2<-by.party[,c("Party.name.identifier", "LAD12CD")]
head(by.party2)
head(referendum)
referendum_ge<- merge(referendum, by.party2, by.x="Area_Code", by.y="LAD12CD")
head(referendum_ge)

# for spliting the data by Party, to summarise the vote
library(dplyr)
# Don't load plyr second (after dplyr) or at all. The problem is that it's using plyr::summarise not dplyr::summarise:
party_referendum<-referendum_ge %>% group_by(Party.name.identifier) %>% dplyr::summarise(total = length(outcome),
                                                      pct_in = sum(outcome=="in")/total,
                                                      pct_out = sum(outcome=="out")/total)

party_referendum<-party_referendum[c(-4,-8),]
party_referendum$Party.name.identifier<-as.character(party_referendum$Party.name.identifier)
party_referendum$Party.name.identifier[party_referendum$Party.name.identifier=="Scottish National Party"]<-"SNP"
party_referendum
library(reshape2)                                         
party_referendum2<-melt(party_referendum, id.vars = "Party.name.identifier", measure.vars = c("pct_in", "pct_out"))
head(party_referendum2)
ggplot(party_referendum2, aes(x=Party.name.identifier, y=value, fill=variable))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("steelblue", "red"),
                    name="Referendum", 
                    breaks=c("pct_in", "pct_out"),
                    labels=c("remain", "leave"))+
  theme_classic()+
  ylab("rel.value")+
  theme(line=element_line(size=1, color="black"),
        text = element_text(size=20, face="bold"),
        axis.title.y = element_text(vjust=1.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, vjust = 0.5))
ggsave("figures/party_referendum.png")
