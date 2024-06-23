

library(ggplot2)
library(grid)


pal = c("#A03232","#FF5864","#FF8B98","#C9C09B","#89AFFF","#577CCC","#244999")


election_bar_long<-data_frame(ec_votes=merged_data$Electoral_College_Votes,percentage=merged_data$percentage)
ecdat<-election_bar_long%>%
  mutate(class=ifelse(percentage<0.1,-3,
                      ifelse(percentage< 0.25, -2,
                             ifelse(percentage<0.45,-1,
                                    ifelse(percentage<0.55, 0,
                                           ifelse(percentage<0.75,1,
                                                  ifelse(percentage<0.9,2,3)
                                           )
                                    )
                             )
                      )
  )
  )%>%
  group_by(class)%>%
  summarise(ec_votes=sum(ec_votes,na.rm=TRUE))%>%
  filter(class<5)


trump_votes = sum(ecdat$ec_votes[1:3])
biden_votes = sum(ecdat$ec_votes[5:7])

# Create the base plot
p<-ggplot()+xlim(0,538)+ylim(0,1)+
  geom_rect(aes(xmin = 0, xmax = 538,ymin = 0.4,ymax=0.6),color="black")+
  geom_rect(aes(xmin = 0, xmax = sum(ecdat[0:1,2]), ymin = 0.4, ymax = 0.6),color="black", fill = pal[1])+
  geom_rect(aes(xmin = sum(ecdat[0:1,2]), xmax = sum(ecdat[0:2,2]), ymin = 0.4, ymax = 0.6), fill = pal[2])+
  geom_rect(aes(xmin = sum(ecdat[0:2,2]), xmax = sum(ecdat[0:3,2]), ymin = 0.4, ymax = 0.6), fill = pal[3])+
  geom_rect(aes(xmin = sum(ecdat[0:3,2]), xmax = sum(ecdat[0:4,2]), ymin = 0.4, ymax = 0.6), fill = pal[4])+
  geom_rect(aes(xmin = sum(ecdat[0:4,2]), xmax = sum(ecdat[0:5,2]), ymin = 0.4, ymax = 0.6), fill = pal[5])+
  geom_rect(aes(xmin = sum(ecdat[0:5,2]), xmax = sum(ecdat[0:6,2]), ymin = 0.4, ymax = 0.6), fill = pal[6])+
  geom_rect(aes(xmin = sum(ecdat[0:6,2]), xmax = sum(ecdat[0:7,2]), ymin = 0.4, ymax = 0.6), fill = pal[7])+
  geom_text(aes(x = 0.5*(0+ecdat$ec_votes[0:1]), y = 0.5, label = ecdat$ec_votes[1]), color = "white", size = 7,vjust=0.5,hjust=0.5)+
  geom_text(aes(x = 0.5*(sum(ecdat$ec_votes[0:1])+sum(ecdat$ec_votes[0:2])), y = 0.5, label = ecdat$ec_votes[2]), color = "white", size = 7,vjust=0.5,hjust=0.5)+
  geom_text(aes(x = 0.5*(sum(ecdat$ec_votes[0:2])+sum(ecdat$ec_votes[0:3])), y = 0.5, label = ecdat$ec_votes[3]), color = "black", size = 7,vjust=0.5,hjust=0.5)+
  geom_text(aes(x = 0.5*(sum(ecdat$ec_votes[0:3])+sum(ecdat$ec_votes[0:4])), y = 0.5, label = ecdat$ec_votes[4]), color = "black", size = 7,vjust=0.5,hjust=0.5)+
  geom_text(aes(x = 0.5*(sum(ecdat$ec_votes[0:4])+sum(ecdat$ec_votes[0:5])), y = 0.5, label = ecdat$ec_votes[5]), color = "black", size = 7,vjust=0.5,hjust=0.5)+
  geom_text(aes(x = 0.5*(sum(ecdat$ec_votes[0:5])+sum(ecdat$ec_votes[0:6])), y = 0.5, label = ecdat$ec_votes[6]), color = "white", size = 7,vjust=0.5,hjust=0.5)+
  geom_text(aes(x = 0.5*(sum(ecdat$ec_votes[0:6])+sum(ecdat$ec_votes[0:7])), y = 0.5, label = ecdat$ec_votes[7]), color = "white", size = 7,vjust=0.5,hjust=0.5)+
  


  # Add the "270 to win" line
  # geom_vline(xintercept = votes_to_win, linetype = "dashed", color = "gray") +
  geom_segment(aes(x=270, xend=270, y=0.4, yend=0.64),color="#3b3b3b",linewidth=0.5)+
  geom_text(aes(x = votes_to_win, y = 0.67, label = "270"), size = 7, color = "#3b3b3b")+

  # Add Biden's vote count
  geom_text(aes(x = 538, y = 0.7, label = biden_votes), hjust=1, size = 8, color = pal[7]) +
  # Add Trump's vote count
  geom_text(aes(x = 0, y = 0.7, label = trump_votes), hjust=0, size = 8, color = pal[1]) +
  
  # Add labels for candidates
  annotate("text", x = 538, y = 0.8, label = "Joe Biden",hjust=1, size = 10, color = "black") +
  annotate("text", x = 0, y = 0.8, label = "Donald Trump",hjust=0, size = 10, color = "black") +
  # Theme adjustments to remove axis labels and ticks
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())
p

# print(p)

# Create a grid layout to overlay the images
grid.newpage()
print(p, vp = viewport(width = 0.9, height = 0.9))

ggsave("gptdemo/www/election_results.png", plot = last_plot(), width = 25, height = 4)


# 
# # Add Biden image
# grid.raster(biden_img, x = 2, y = 0.85, width = 0.1, height = 0.1)
# 
# # Add Trump image
# grid.raster(trump_img, x = 0, y = 0.85, width = 0.1, height = 0.1)
# 



library(png)
# Load candidate images
biden_img <- readPNG("Biden_Circle.png")
trump_img <- readPNG("Trump_Circle.png")







