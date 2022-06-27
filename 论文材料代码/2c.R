ggplot(boxplot_data_betweenness,mapping=aes(x=group,y=value,fill = group))+ ##设置图形的纵坐标横坐标和分组
  stat_boxplot(mapping=aes(x=group,y=value,fill = group),
               geom ="errorbar",                           ##添加箱子的bar为最大、小值
               width=0.15,position=position_dodge(0.8))+     ##bar宽度和组间距
  geom_boxplot(aes(fill=group),                             ##分组比较的变量
               position=position_dodge(0.8),                 ##因为分组比较，需设组间距
               width=0.6,                                    ##箱子的宽度
               outlier.color = "black",outlier.size = 0.7,outlier.alpha = 0.8)+  
  stat_summary(mapping=aes(group=group),                    ##分组计算的变量
               fun="mean",                                   ##箱线图添加均值
               geom="point",shape=23,size=3,fill="white",    ##均值图形的设置
               position=position_dodge(0.8))+             ##因为分组比较，需设置两组间距
  scale_fill_manual(values=c('#104E8B',"#CD2626"))+scale_y_continuous(limits=c(0,25))+theme_bw()+theme(panel.grid = element_blank()
                          ,axis.text=element_text(face = "bold",size = 11,colour = "black"))+
  stat_compare_means(aes(group=group),method = "t.test",label="p.signif",label.x = 1.45)
  
  
  my_comparisons=list(c("单次辐照","多次辐照"))
  

  
  
  library(ggplot2)
  
  
  

stat_compare_means(comparisons = my_comparisons),label.y =0.9,label =  "p.signif",method = "t.test",,fun="mean",geom="errorbar")+theme_bw()+ 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=12, angle=0),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=12, angle=0),panel.grid=element_blank())
