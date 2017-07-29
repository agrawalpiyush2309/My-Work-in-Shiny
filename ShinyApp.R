---
title: "Customer Churn Snapshot"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
runtime: shiny
---
<style>                     
.navbar {
  background-color:red;
  border-color:black;
}
.navbar-brand {
color:white!important;
}
.section.sidebar {
background-color: white;
}
</style>  

```{r setup, include=FALSE}
library(flexdashboard)
library(shiny)
library(DT)
library(readxl)
library(sunburstR)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)
```

```{r global, include=FALSE}
# load data in 'global' chunk so it can be shared by all users of the dashboard
churn <-read.csv("C:\\Users\\Admin\\Documents\\MMA 865 Big Data\\test.csv")
churn <- churn %>% filter((churned==1 & prob_1 >= 0.5) | (churned==0 & prob_0 >= 0.5))
ftitle <- list(
  family = "Times New Roman",
  size = 18,
  color = "black"
)
flabels <- list(
  family = "Times New Roman",
  size = 13,
  color = "black"
)
```
```{r}
sunburnData <- reactive({
  input$Go
  province <- isolate(input$Province)
  churnstatus <- isolate(input$ChurnStatus)
  e <-churn
  if (province=='ALL' & churnstatus=="ALL"){
    e <-churn
  }else if(province=='ALL' & churnstatus != "ALL"){
    if(churnstatus=="Churned"){
      e <-churn %>% filter(churned==1)
    }else{
      e <-churn %>% filter(churned==0)
    }
  }else if(province !='ALL' & churnstatus == "ALL"){
      e <-churn %>% filter(psnl_prov_state_cd==province)
  }else if(province != 'ALL' & churnstatus != "ALL"){
    if(churnstatus=="Churned"){
      e <-churn %>% filter(psnl_prov_state_cd==province & churned==1)
    }else{
      e <-churn %>% filter(psnl_prov_state_cd==province & churned==0)
    }
  }else{
    e <-churn
  }
  e
})

scatterData <- reactive({
  input$Go
  points <- input$Points
  province <- isolate(input$Province)
  churnstatus <- selectedChurn()
  if(is.null(points)) points <- 500
  f <-churn %>% filter(row_number() <= points)
  if (province=='ALL' & churnstatus=="ALL"){
    f <-churn %>% filter(row_number() <= points)
  }else if(province=='ALL' & churnstatus != "ALL"){
    if(churnstatus=="Churned"){
      # f <-churn %>% filter(churned==1) %>% filter(row_number() <= points)
      f <-churn %>% filter(churned==1)
    }else{
      # f <-churn %>% filter(churned==0) %>% filter(row_number() <= points)
      f <-churn %>% filter(churned==0)
    }
  }else if(province!='ALL' & churnstatus == "ALL"){
      # f <-churn %>% filter(psnl_prov_state_cd==province) %>% filter(row_number() <= points)
    f <-churn %>% filter(psnl_prov_state_cd==province)
  }else if(province != 'ALL' & churnstatus != "ALL"){
    if(churnstatus=="Churned"){
      # f <-churn %>% filter(psnl_prov_state_cd==province & churned==1) %>% filter(row_number() <= points)
      f <-churn %>% filter(psnl_prov_state_cd==province & churned==1)
    }else{
      # f <-churn %>% filter(psnl_prov_state_cd==province & churned==0) %>% filter(row_number() <= points)
      f <-churn %>% filter(psnl_prov_state_cd==province & churned==0)
    }
  }else{
    # f <-churn %>% filter(row_number() <= points)
    f <-churn %>% filter(row_number())
  }
  f
})

tableData <- reactive({
  x<-scatterData()
  rownames(x) <- x$scene_mbr_key
  key <- row.names(x)  
  g <- event_data("plotly_selected")
  if (is.null(g)) x[,c(2:ncol(x))] else x[g$key,c(2:ncol(x))]
})

selectedChurn<- reactive({
  input$Go
  isolate(input$ChurnStatus)
})

selectedPoints<- reactive({
  input$Points
})
```

Inputs {.sidebar}
-----------------------------------------------------------------------

<style>
.js-irs-0 .irs-bar {
border-top-color: red;
border-bottom-color: red;
} 

.js-irs-0 .irs-bar-edge {
border-color: red;
}

.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
background: red;
}

</style>

```{r}
 selectInput("Province", label = h3("Province"), 
     choices = append(as.character(unique(churn$psnl_prov_state_cd)),"ALL",0),
    selected = "ALL")
 selectInput("ChurnStatus", label = h3("Predicted Churn Status"),
    choices = list("ALL","Churned","NotChurned"),
    selected = "ALL")
 sliderInput("Points", "No. Of Points", 500, 10000, 1000)
 actionButton(inputId = "Go", label = "Go!" ,icon = icon("fa fa-refresh"),style="color: black; background-color: red;border-color: red")
```

Rows {data-height=700}
-------------------------------------

### Summary of data points in various categories
    
```{r}
renderSunburst({
temp <- sunburnData() %>% group_by(churned,psnl_prov_state_cd,gndr_desc,ed_lvl_desc,mrtl_stat_desc) %>% summarise(count=n()) %>% mutate(event = paste(ifelse(churned==0,"NotChurned","Churned"),'-',psnl_prov_state_cd,"-",gndr_desc,"-",ed_lvl_desc,"-",mrtl_stat_desc,sep=""),depth=5) %>% ungroup() %>% select(event,count,depth) %>% as.data.frame()

temp <- temp %>% arrange(desc(depth),event)
legend_items <- unique(unlist(strsplit(temp$event, '-')))
cols <- sample(colorRampPalette(brewer.pal(8, 'Dark2'))(length(legend_items)))

add_shiny(sunburst(temp,count = TRUE,colors = list(range=cols,domain=legend_items),legendOrder=legend_items,width="100%"))
})

```

### Customers age and their length of stay 
    
```{r}
renderPlotly({
churn2 <- scatterData()
rownames(churn2) <- churn2$scene_mbr_key
key <- row.names(churn2)
Points <- selectedPoints()
if (selectedChurn() == "ALL"){
plot_ly(data = churn2, x = ~age, y = ~lifetime_mnths, key=key, color=~factor(churned),colors=c("green","red"), text = ~paste(psnl_prov_state_cd, ",",gndr_desc),marker = list(size=5,opacity=0.5)) %>% layout(dragmode = "select",title = "",
       xaxis = list(title = "Age",tickfont=flabels),
       yaxis = list(title = "Lifetime in Month",tickfont=flabels))
}else if((selectedChurn() == "Churned")){
churn2 <- churn2 %>% filter(row_number() %in% sample(1:n(),Points))
p2 <- churn2 %>% filter(prob_1>=0.5 & prob_1<=0.6) %>% as.data.frame()
rownames(p2) <- p2$scene_mbr_key
keyp2 <- row.names(p2)
p3 <- churn2 %>% filter(prob_1>0.6 & prob_1<=0.7) %>% as.data.frame()
rownames(p3) <- p3$scene_mbr_key
keyp3 <- row.names(p3)
p4 <- churn2 %>% filter(prob_1>0.7 & prob_1<=0.8) %>% as.data.frame()
rownames(p4) <- p4$scene_mbr_key
keyp4 <- row.names(p4)
p5 <- churn2 %>% filter(prob_1>0.8 & prob_1<=0.9) %>% as.data.frame()
rownames(p5) <- p5$scene_mbr_key
keyp5 <- row.names(p5)
p6 <- churn2 %>% filter(prob_1>0.9) %>% as.data.frame()
rownames(p6) <- p6$scene_mbr_key
keyp6 <- row.names(p6)
plot_ly(showlegend=TRUE) %>% 
add_markers( data = p2, x = ~age, y = ~lifetime_mnths, key=keyp2,marker = list(size=5,color='red'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.9") %>%
add_markers( data = p3, x = ~age, y = ~lifetime_mnths, key=keyp3,marker = list(size=5,color='#F56741'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.8 <=0.9") %>%
add_markers( data = p4, x = ~age, y = ~lifetime_mnths, key=keyp4,marker = list(size=5,color='#F58241'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.7 <=0.8") %>%
add_markers( data = p5, x = ~age, y = ~lifetime_mnths, key=keyp5,marker = list(size=5,color='#F5D741'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.6 <=0.7") %>%
add_markers( data = p6, x = ~age, y = ~lifetime_mnths, key=keyp6,marker = list(size=5,color='#F2F541'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">=0.5 <=0.6") %>%
layout(dragmode = "select",title = "",
       xaxis = list(title = "Age",tickfont=flabels),
       yaxis = list(title = "Lifetime in Month",tickfont=flabels))  
}else{
churn2 <- churn2 %>% filter(row_number() %in% sample(1:n(),Points))
p2 <- churn2 %>% filter(prob_0>=0.5 & prob_0<=0.6) %>% as.data.frame()
rownames(p2) <- p2$scene_mbr_key
keyp2 <- row.names(p2)
p3 <- churn2 %>% filter(prob_0>0.6 & prob_0<=0.7) %>% as.data.frame()
rownames(p3) <- p3$scene_mbr_key
keyp3 <- row.names(p3)
p4 <- churn2 %>% filter(prob_0>0.7 & prob_0<=0.8) %>% as.data.frame()
rownames(p4) <- p4$scene_mbr_key
keyp4 <- row.names(p4)
p5 <- churn2 %>% filter(prob_0>0.8 & prob_0<=0.9) %>% as.data.frame()
rownames(p5) <- p5$scene_mbr_key
keyp5 <- row.names(p5)
p6 <- churn2 %>% filter(prob_0>0.9) %>% as.data.frame()
rownames(p6) <- p6$scene_mbr_key
keyp6 <- row.names(p6)
plot_ly(showlegend=TRUE) %>% 
add_markers( data = p2, x = ~age, y = ~lifetime_mnths, key=keyp2,marker = list(size=5,color='#F5CF41'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">=0.5 <=0.6") %>%
add_markers( data = p3, x = ~age, y = ~lifetime_mnths, key=keyp3,marker = list(size=5,color='#F5E541'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.6 <=0.7") %>%
add_markers( data = p4, x = ~age, y = ~lifetime_mnths, key=keyp4,marker = list(size=5,color='#F0F541'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.7 <=0.8") %>%
add_markers( data = p5, x = ~age, y = ~lifetime_mnths, key=keyp5,marker = list(size=5,color='#C9F541'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.8 <=0.9") %>%
add_markers( data = p6, x = ~age, y = ~lifetime_mnths, key=keyp6,marker = list(size=5,color='green'),type='scatter',text = ~paste(psnl_prov_state_cd, ",",gndr_desc),name=">0.9") %>%
layout(dragmode = "select",title = "",
       xaxis = list(title = "Age",tickfont=flabels),
       yaxis = list(title = "Lifetime in Month",tickfont=flabels))  
}
})
```

Rows {.tabset data-width=300}
-----------------------------------------------------------------------
    
### Gender
    
```{r}
renderPlotly({
temp <- sunburnData()
p1<-temp %>% mutate(ranges1 = ifelse(prob_1<0.5,"<0.5",
                              ifelse(prob_1>=0.5 & prob_1<=0.6,">=0.5<=0.6",
                              ifelse(prob_1>0.6 & prob_1<=0.7,">0.6<=0.7",
                              ifelse(prob_1>0.7 & prob_1<=0.8,">0.7<=0.8",
                              ifelse(prob_1>0.8 & prob_1<=0.9,">0.8<=0.9",">0.9"))))),
                    ranges0 = ifelse(prob_0<0.5,"<0.5",
                              ifelse(prob_0>=0.5 & prob_0<=0.6,">=0.5<=0.6",
                              ifelse(prob_0>0.6 & prob_0<=0.7,">0.6<=0.7",
                              ifelse(prob_0>0.7 & prob_0<=0.8,">0.7<=0.8",
                              ifelse(prob_0>0.8 & prob_0<=0.9,">0.8<=0.9",">0.9"))))))


if (selectedChurn() == "ALL"){
  p2 <- p1 %>% filter(churned==1) %>% group_by(gndr_desc) %>% summarise(count=n()) %>% as.data.frame()
  p3 <- p1 %>% filter(churned==0) %>% group_by(gndr_desc) %>% summarise(count=n()) %>% as.data.frame()
  v1 <- plot_ly(width = 800,height=400,showlegend = FALSE) %>% 
               add_bars(data=p2,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = 'red')) %>% 
               add_bars(data=p3,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = 'green')) %>%
               layout(title = "Customer churn by Gender",titlefont = ftitle,
               xaxis = list(title = ""),
               yaxis = list(title = "",tickfont=flabels),barmode = 'stack')
  p2<- p1 %>% filter(churned==1) %>% group_by(ranges1) %>% summarise(count=n())
  p3<- p1 %>% filter(churned==0) %>% group_by(ranges0) %>% summarise(count=n())
  
  v2 <- plot_ly(showlegend = FALSE,width = 750, height = 200) %>%
    add_pie(data = p2,labels = ~ranges1, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.5, 0.72), y = c(0,0.6)),
            marker = list(colors = c('#F2F541','#F5D741','#F58241','#F56741','red'))) %>% 
    add_pie(data = p3,labels = ~ranges0, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.78, 1), y = c(0, 0.6)),
            marker = list(colors = c('#F5CF41','#F5E541','#F0F541','#C9F541','green'))) %>% 
    layout(title = "",titlefont = ftitle,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  subplot(v1, v2, nrows = 1) %>% layout(title = "Customer churn by Gender and corresponding probabilities")
}else if(selectedChurn() == "Churned") {
  p2 <- p1 %>% group_by(gndr_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(gndr_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(gndr_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(gndr_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(gndr_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F2F541'),name="<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F5D741'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F58241'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F56741'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = 'red'),name=">0.9") %>%
  layout(title = "Customer churn by Gender",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
  
}else{
  p2 <- p1 %>% group_by(gndr_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(gndr_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(gndr_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(gndr_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(gndr_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F5CF41'),name=">=0.5<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F5E541'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#F0F541'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = '#C9F541'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~gndr_desc,orientation = 'h', marker = list(color = 'green'),name=">0.9") %>%
  layout(title = "Customer churn by Gender",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
}
})

```

### Marital Status
    
```{r}
renderPlotly({
temp <- sunburnData()
p1<-temp %>% mutate(ranges1 = ifelse(prob_1<0.5,"<0.5",
                              ifelse(prob_1>=0.5 & prob_1<=0.6,">=0.5<=0.6",
                              ifelse(prob_1>0.6 & prob_1<=0.7,">0.6<=0.7",
                              ifelse(prob_1>0.7 & prob_1<=0.8,">0.7<=0.8",
                              ifelse(prob_1>0.8 & prob_1<=0.9,">0.8<=0.9",">0.9"))))),
                    ranges0 = ifelse(prob_0<0.5,"<0.5",
                              ifelse(prob_0>=0.5 & prob_0<=0.6,">=0.5<=0.6",
                              ifelse(prob_0>0.6 & prob_0<=0.7,">0.6<=0.7",
                              ifelse(prob_0>0.7 & prob_0<=0.8,">0.7<=0.8",
                              ifelse(prob_0>0.8 & prob_0<=0.9,">0.8<=0.9",">0.9"))))))


if (selectedChurn() == "ALL"){
  p2 <- p1 %>% filter(churned==1) %>% group_by(mrtl_stat_desc) %>% summarise(count=n()) %>% as.data.frame()
  p3 <- p1 %>% filter(churned==0) %>% group_by(mrtl_stat_desc) %>% summarise(count=n()) %>% as.data.frame()
  v1 <- plot_ly(width = 800,height=400,showlegend = FALSE) %>% 
               add_bars(data=p2,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = 'red')) %>% 
               add_bars(data=p3,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = 'green')) %>%
               layout(title = "Customer churn by Marital Status",titlefont = ftitle,
               xaxis = list(title = ""),
               yaxis = list(title = "",tickfont=flabels),barmode = 'stack')
  p2<- p1 %>% filter(churned==1) %>% group_by(ranges1) %>% summarise(count=n())
  p3<- p1 %>% filter(churned==0) %>% group_by(ranges0) %>% summarise(count=n())
  
  v2 <- plot_ly(showlegend = FALSE,width = 750, height = 200) %>%
    add_pie(data = p2,labels = ~ranges1, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.5, 0.72), y = c(0,0.6)),
            marker = list(colors = c('#F2F541','#F5D741','#F58241','#F56741','red'))) %>% 
    add_pie(data = p3,labels = ~ranges0, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.78, 1), y = c(0, 0.6)),
            marker = list(colors = c('#F5CF41','#F5E541','#F0F541','#C9F541','green'))) %>% 
    layout(title = "",titlefont = ftitle,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  subplot(v1, v2, nrows = 1) %>% layout(title = "Customer churn by Marital Status and corresponding probabilities")
}else if(selectedChurn() == "Churned") {
  p2 <- p1 %>% group_by(mrtl_stat_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(mrtl_stat_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(mrtl_stat_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(mrtl_stat_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(mrtl_stat_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F2F541'),name="<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F5D741'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F58241'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F56741'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = 'red'),name=">0.9") %>%
  layout(title = "Customer churn by Marital Status",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
  
}else{
  p2 <- p1 %>% group_by(mrtl_stat_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(mrtl_stat_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(mrtl_stat_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(mrtl_stat_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(mrtl_stat_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F5CF41'),name=">=0.5<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F5E541'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#F0F541'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = '#C9F541'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~mrtl_stat_desc,orientation = 'h', marker = list(color = 'green'),name=">0.9") %>%
  layout(title = "Customer churn by Marital Status",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
}
})

```

### Language
    
```{r}
renderPlotly({
temp <- sunburnData()
p1<-temp %>% mutate(ranges1 = ifelse(prob_1<0.5,"<0.5",
                              ifelse(prob_1>=0.5 & prob_1<=0.6,">=0.5<=0.6",
                              ifelse(prob_1>0.6 & prob_1<=0.7,">0.6<=0.7",
                              ifelse(prob_1>0.7 & prob_1<=0.8,">0.7<=0.8",
                              ifelse(prob_1>0.8 & prob_1<=0.9,">0.8<=0.9",">0.9"))))),
                    ranges0 = ifelse(prob_0<0.5,"<0.5",
                              ifelse(prob_0>=0.5 & prob_0<=0.6,">=0.5<=0.6",
                              ifelse(prob_0>0.6 & prob_0<=0.7,">0.6<=0.7",
                              ifelse(prob_0>0.7 & prob_0<=0.8,">0.7<=0.8",
                              ifelse(prob_0>0.8 & prob_0<=0.9,">0.8<=0.9",">0.9"))))))


if (selectedChurn() == "ALL"){
  p2 <- p1 %>% filter(churned==1) %>% group_by(lang_desc) %>% summarise(count=n()) %>% as.data.frame()
  p3 <- p1 %>% filter(churned==0) %>% group_by(lang_desc) %>% summarise(count=n()) %>% as.data.frame()
  v1 <- plot_ly(width = 800,height=400,showlegend = FALSE) %>% 
               add_bars(data=p2,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = 'red')) %>% 
               add_bars(data=p3,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = 'green')) %>%
               layout(title = "Customer churn by Language",titlefont = ftitle,
               xaxis = list(title = ""),
               yaxis = list(title = "",tickfont=flabels),barmode = 'stack')
  p2<- p1 %>% filter(churned==1) %>% group_by(ranges1) %>% summarise(count=n())
  p3<- p1 %>% filter(churned==0) %>% group_by(ranges0) %>% summarise(count=n())
  
  v2 <- plot_ly(showlegend = FALSE,width = 750, height = 200) %>%
    add_pie(data = p2,labels = ~ranges1, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.5, 0.72), y = c(0,0.6)),
            marker = list(colors = c('#F2F541','#F5D741','#F58241','#F56741','red'))) %>% 
    add_pie(data = p3,labels = ~ranges0, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.78, 1), y = c(0, 0.6)),
            marker = list(colors = c('#F5CF41','#F5E541','#F0F541','#C9F541','green'))) %>% 
    layout(title = "",titlefont = ftitle,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  subplot(v1, v2, nrows = 1) %>% layout(title = "Customer churn by Language and corresponding probabilities")
}else if(selectedChurn() == "Churned") {
  p2 <- p1 %>% group_by(lang_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(lang_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(lang_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(lang_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(lang_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F2F541'),name="<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F5D741'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F58241'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F56741'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = 'red'),name=">0.9") %>%
  layout(title = "Customer churn by Language",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
  
}else{
  p2 <- p1 %>% group_by(lang_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(lang_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(lang_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(lang_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(lang_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F5CF41'),name=">=0.5<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F5E541'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#F0F541'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = '#C9F541'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~lang_desc,orientation = 'h', marker = list(color = 'green'),name=">0.9") %>%
  layout(title = "Customer churn by Language",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
}
})
```
    
### Education Level

```{r}
renderPlotly({
temp <- sunburnData()
p1<-temp %>% mutate(ranges1 = ifelse(prob_1<0.5,"<0.5",
                              ifelse(prob_1>=0.5 & prob_1<=0.6,">=0.5<=0.6",
                              ifelse(prob_1>0.6 & prob_1<=0.7,">0.6<=0.7",
                              ifelse(prob_1>0.7 & prob_1<=0.8,">0.7<=0.8",
                              ifelse(prob_1>0.8 & prob_1<=0.9,">0.8<=0.9",">0.9"))))),
                    ranges0 = ifelse(prob_0<0.5,"<0.5",
                              ifelse(prob_0>=0.5 & prob_0<=0.6,">=0.5<=0.6",
                              ifelse(prob_0>0.6 & prob_0<=0.7,">0.6<=0.7",
                              ifelse(prob_0>0.7 & prob_0<=0.8,">0.7<=0.8",
                              ifelse(prob_0>0.8 & prob_0<=0.9,">0.8<=0.9",">0.9"))))))


if (selectedChurn() == "ALL"){
  p2 <- p1 %>% filter(churned==1) %>% group_by(ed_lvl_desc) %>% summarise(count=n()) %>% as.data.frame()
  p3 <- p1 %>% filter(churned==0) %>% group_by(ed_lvl_desc) %>% summarise(count=n()) %>% as.data.frame()
  v1 <- plot_ly(width = 800,height=400,showlegend = FALSE) %>% 
               add_bars(data=p2,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = 'red')) %>% 
               add_bars(data=p3,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = 'green')) %>%
               layout(title = "Customer churn by Education Level",titlefont = ftitle,
               xaxis = list(title = ""),
               yaxis = list(title = "",tickfont=flabels),barmode = 'stack')
  p2<- p1 %>% filter(churned==1) %>% group_by(ranges1) %>% summarise(count=n())
  p3<- p1 %>% filter(churned==0) %>% group_by(ranges0) %>% summarise(count=n())
  
  v2 <- plot_ly(showlegend = FALSE,width = 750, height = 200) %>%
    add_pie(data = p2,labels = ~ranges1, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.5, 0.72), y = c(0,0.6)),
            marker = list(colors = c('#F2F541','#F5D741','#F58241','#F56741','red'))) %>% 
    add_pie(data = p3,labels = ~ranges0, values = ~count,hole = 0.6,textposition = 'auto',
            textinfo = 'percent',
            domain = list(x = c(0.78, 1), y = c(0, 0.6)),
            marker = list(colors = c('#F5CF41','#F5E541','#F0F541','#C9F541','green'))) %>%
    layout(title = "",titlefont = ftitle,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  subplot(v1, v2, nrows = 1) %>% layout(title = "Customer churn by Education Level and corresponding probabilities")
}else if(selectedChurn() == "Churned") {
  p2 <- p1 %>% group_by(ed_lvl_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(ed_lvl_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(ed_lvl_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(ed_lvl_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(ed_lvl_desc, ranges1) %>% summarise(count=n()) %>% filter(ranges1==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F2F541'),name="<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F5D741'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F58241'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F56741'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = 'red'),name=">0.9") %>%
  layout(title = "Customer churn by Education Level",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
  
}else{
  p2 <- p1 %>% group_by(ed_lvl_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">=0.5<=0.6") %>% as.data.frame()
  p3 <- p1 %>% group_by(ed_lvl_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.6<=0.7") %>% as.data.frame()
  p4 <- p1 %>% group_by(ed_lvl_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.7<=0.8") %>% as.data.frame()
  p5 <- p1 %>% group_by(ed_lvl_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.8<=0.9") %>% as.data.frame()
  p6 <- p1 %>% group_by(ed_lvl_desc, ranges0) %>% summarise(count=n()) %>% filter(ranges0==">0.9") %>% as.data.frame()
  plot_ly(width = 800,height=200,showlegend = TRUE) %>% 
  add_bars(data=p2,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F5CF41'),name=">=0.5<=0.6") %>%
  add_bars(data=p3,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F5E541'),name=">0.6<=0.7") %>%
  add_bars(data=p4,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#F0F541'),name=">0.7<=0.8") %>%  
  add_bars(data=p5,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = '#C9F541'),name=">0.8<=0.9") %>%
  add_bars(data=p6,x = ~count,y = ~ed_lvl_desc,orientation = 'h', marker = list(color = 'green'),name=">0.9") %>%
  layout(title = "Customer churn by Education Level",titlefont = ftitle, barmode = 'stack',
  xaxis = list(title = ""),
  yaxis = list(title = "",tickfont=flabels))
}
})
```

### Customer Details
    
```{r}
renderTable({
  tableData()
})
```
