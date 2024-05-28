
library(tidyverse)
library(ggplot2)
library(stringr)
library(maps)
library(dplyr)


pres<-read_csv("president_polls.csv")
presids<-c("19368","16651","31042") # Kennedy should probably be in this
biden_trump_ids<-c("19368","16651")

diff<-as.numeric(range(as.Date(pres$end_date, format = "%m/%d/%y")))
max_diff<-as.numeric(diff[2]-diff[1])


check_biden_vs_trump<-pres%>%
  filter(candidate_id%in%biden_trump_ids)%>%
  group_by(poll_id,question_id)%>%
  summarise(candidate_id=sum(as.numeric(candidate_id)))%>%
  filter(candidate_id==sum(as.numeric(biden_trump_ids)))

poll_data<-pres%>%
  mutate(end_date=as.Date(pres$end_date, format = "%m/%d/%y"))%>% # make the dates date type objects
  filter(candidate_id%in%biden_trump_ids)%>%
  filter(question_id%in%check_biden_vs_trump$question_id 
         & poll_id%in%check_biden_vs_trump$poll_id)%>%
  mutate(pct=as.numeric(pct)/100)%>%
  mutate(time_weight=1-(diff[2]-as.numeric(end_date))/max_diff)%>% # add a linear weight to the polls by recency. 
  mutate(weight = ifelse(is.na(sample_size),111,sample_size) *
           ifelse(is.na(numeric_grade),0.5,numeric_grade) *
           time_weight * time_weight)

biden_data<-poll_data%>%
  filter(candidate_name=="Joe Biden")
trump_data<-poll_data%>%
  filter(candidate_name=="Donald Trump")


# ggplot()+
#   geom_density(data=biden_data,aes(x=pct),color="blue")+
#   geom_density(data=trump_data,aes(x=pct),color="red")+
#   geom_vline(xintercept =mean(trump_data$pct),color="red")+
#   geom_vline(xintercept =mean(biden_data$pct),color="blue")

diff_mean<-mean(biden_data$pct) - mean(trump_data$pct)

prob_trump_beats_biden <- function(biden_data, trump_data) {

# Weighted difference
weighted_difference <- sum((biden_data$pct - trump_data$pct) * biden_data$weight) / sum(biden_data$weight)

normalized_weights <- biden_data$weight / sum(biden_data$weight)


# Weighted variance
var_biden <- sum(normalized_weights * (biden_data$pct - weighted_difference)^2)
var_trump <- sum(normalized_weights * (trump_data$pct - weighted_difference)^2)
weighted_var_difference <- var_biden + var_trump
se_difference <- sqrt(weighted_var_difference)

# P-value from the Z-score
z_score <- weighted_difference / se_difference
p_value <- pnorm(-z_score, lower.tail = TRUE)

prob_trump_beats_biden<-p_value
return(prob_trump_beats_biden)

}

states = sort(unique(pres$state))

pvals<-c()
for (statename in states){
  bdat<-poll_data%>%
    filter(candidate_name=="Joe Biden",state==statename)
  tdat<-poll_data%>%
    filter(candidate_name=="Donald Trump",state==statename)
  pvals<-c(pvals,prob_trump_beats_biden(bdat,tdat))
}

state_probs<-as.data.frame(cbind(states,pvals))
summary(pvals)

#turns out there's no polling data for Delaware (outside of RFK's janky metric) so
# I just set it equal to Oregon.
pvals[8]=pvals[40]
state_probs[8,2]=state_probs[40,2]

#now define a softmax style forcing function bc there is not a 30% chance trump wins MA/CA
tanh_transform <- function(x, a = 5) {
  return((tanh(a * (x - 0.5)) + 1) / 2)
}


#### Begin simulating ####

electoral_college<-read_csv("Electoral_College.csv") 
colnames(electoral_college)[1]="states"

# Add in DoC as the same as Maryland
state_probs<-rbind(state_probs,c("District of Columbia",0.283858067668726))

state_probs<-state_probs%>%
  filter(!str_detect(states,"CD-"))

state_probs<-merge(state_probs,electoral_college,by="states")


bidenwins<-rep(0,51)
trumpwins<-rep(0,51)
outcome<-as.data.frame(cbind(state_probs$states,bidenwins,trumpwins))
outcome<-rbind(outcome,c("Election",0,0))


numsim<-10000
for(i in 1:numsim){
  
  # make a random systematic polling/prediction error
  # choose a=10 bc it gives me a 95% chance trump takes Wyoming
  dep_pct<-rnorm(1,mean=0,sd=0.01)
  state_probs$new_pvals<-tanh_transform(as.numeric(state_probs$pvals)+dep_pct,a=10)
  
  t_ec_count<-0
  
  for(j in 1:nrow(state_probs)){
    res<-as.numeric(rbinom(1,1,state_probs$new_pvals[j]))
    t_ec_count = t_ec_count + res*state_probs$Electoral_College_Votes[j]
    
    outcome[j,3]=as.numeric(outcome[j,3])+res
    outcome[j,2]=as.numeric(outcome[j,2])+1-res
  }
  
  ifelse(t_ec_count>=270,outcome[52,3]<-(as.numeric(outcome[52,3])+1),
         ifelse(t_ec_count<270,outcome[52,2]<-(as.numeric(outcome[52,2])+1),NULL)
  )

}

#### Simulation Complete ####

#### Beginning map plot ####

us_map <- map_data("state")

# Merge with your data
results<-outcome[1:51,]
results$V1=tolower(results$V1)
results$percentage=as.numeric(results$bidenwins)/numsim
results$st<-state_probs$Abb_State

election_result<-c(as.numeric(outcome[52,2])/numsim,as.numeric(outcome[52,3])/numsim)



map_data <- us_map %>%
  left_join(results, by = c("region" = "V1"))

map_data$bins <- cut(map_data$percentage, breaks = breaks <- c(0, 0.1, 0.25, 0.45, 0.55, 0.75, 0.9, 1), 
                     right = TRUE,include.lowest = TRUE)

# Calculate centroid positions for state labels
label_positions <- map_data %>%
  group_by(st) %>%
  summarize(long = mean(long, na.rm = TRUE), lat = mean(lat, na.rm = TRUE), .groups = 'drop')  # Ensure grouping is dropped

# Plot the map
ggplot() +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = bins), color = "white") +
  geom_text(data = label_positions, aes(x = long, y = lat, label = st), size = 3, hjust = 0.5, vjust = 0.5) +
  # scale_fill_gradient2(low="#E81B23",mid="white",high="#00AEF3",midpoint = 0.5,
  #                      breaks = c(0.2187736,0.9086382),labels = c("Republican","Democrat"))+
  scale_fill_manual(values=c("#A03232","#FF5864","#FF8B98","#C9C09B","#89AFFF","#577CCC","#244999"),
                    labels=c("Safe R (90-100%)","Likely R (75-90%)","Lean R (55-75%)","Toss-Up (45-55%)","Lean D (55-75%)","Likely D (75-90%)","Safe D (90-100%)"))+
  labs(title = "Forecast Mock-Up (nsim = 10000)", fill = "Percentage",subtitle = paste("Biden's Odds: ",election_result[1]*100,"%, Trump's Odds: ",election_result[2]*100,"%",sep="")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())




# c("#A03232","#FF5864","#FF8B98","#C9C09B","#89AFFF","#577CCC","#244999")



# write_csv(outcome,"sim_2_n_10000")
# results2<-rbind(results3,c("Election",5980,4020,0.598,"-","-","-"))
# raw_pvals<-state_probs$pvals
# transformed_pvals<-tanh_transform(as.numeric(state_probs$pvals),a=10)
# 
# results3<-cbind(results,raw_pvals, transformed_pvals)
# colnames(results3)[1]="states"
# 
# write_csv(results2,"Forecast Results.csv")


# biden_data$pct[0:10]
# trump_data$pct[0:10]
# trump_data$weight[0:10]
# weighted_difference
# var_biden
# var_trump
# weighted_var_difference

