source("Load_Aframomum_data.r")

plotty<-function(datary, coloury){
  barplot(datary, col="couloury")
  legend("topleft", legend="cleverplot", fill=TRUE)
}
plotty(perform_types$LeafBredth, coloury="forestgreen")
library(ggplot2)
ggtry_sizes<-ggplot(perform_types,
              aes(x=Long_leaf, y=LeafBredth, size=LeavesWeight))+
  geom_point()+           
  geom_smooth(show.legend = FALSE)+
               labs(x="Length of longest leaf", y="Width of widest leaf")
ggtry_sizes
ggtry_sizes+scale_x_log10()
ggtry_sizes+theme_classic()
ggsave("ggtry_sizes.pdf")

ggtry_weights<-ggplot(perform_types,
                    aes(x=Above, y=RootsWeight, colour=Category))+
  geom_point()+
  geom_smooth(show.legend = FALSE)+
  labs(x="Above ground biomass", y="Below Ground biomass")
ggtry_weights+theme_classic()
ggsave("ggtry_weights.pdf")
