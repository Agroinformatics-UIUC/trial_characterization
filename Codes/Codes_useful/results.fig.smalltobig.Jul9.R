#---- RESULTS ----#
# colors http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# Hexadecimal color code chart http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#hexadecimal-color-code-chart
# PLOT DIFFERENT PLOT TYPES IN FACETS https://statbandit.wordpress.com/2011/07/29/a-ggplot-trick-to-plot-different-plot-types-in-facets/
# Small to big approach
# ggplot(aes(x = interaction(C, A),  y = B, fill = A)) +
  # geom_col(position = "dodge")


#--------------------------
#Fig. 3: SQUARES, PLOTS, CELLS
#--------------------------

checkb.app.map.whole.sf <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/checkb.app.map.whole.sf.RDS')
plot.one <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/plot.one.RDS')
wendte.C.sf <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/wendte.C.sf.rds')
nrow(plot.one)

# install.packages('shinyjs')
# library('shinyjs')
# tmaptools::palette_explorer()

tm_shape(wendte.C.sf) + tm_polygons(c('OM', 'elev', 'N.initial', 'Depth'), legend.show = TRUE) +
  tm_layout(legend.text.size = 0.7,
            main.title = paste('Initial conditions of the Field'),
            main.title.position = "center",
            main.title.size = 1)

wendte.C.sf$id.X <- as.numeric(wendte.C.sf$id.X)
wendte.C.sf$id.X <- c(rep('-', 3), 32, rep('-', 24), 1, rep('-', 3))
(id.x <- tm_shape(wendte.C.sf) + tm_borders() + tm_text('id.X', size = 0.5)+ #tm_polygons('id.X', legend.show = FALSE, palette = 'Pastel1' )
  tm_legend(
    main.title = paste('Squares (c=1,...,32)'),
    main.title.size = 1.1,
    main.title.position = "left"))

checkb.app.map.whole.sf$id.plot <- c(1, rep('-', 108), 110, '-')
(id.plot <- tm_shape(checkb.app.map.whole.sf) + tm_borders() + tm_text('id.plot', size = 0.5)+ #tm_polygons('block', legend.show = FALSE)
  tm_legend(
    main.title = paste('Plots (x = 1,...,110)'),
    main.title.size = 1.1,
    main.title.position = "left"))

#plot.one$id.all.random <- as.character(sample(1:length(plot.one$id.all), length(plot.one$id.all)))
plot.one$id.all[!plot.one$id.all %in% c(17,237)] <- '-'
plot.one$id.all[plot.one$id.all %in% c(17,237)] <- c(1,256)

(id.all <- tm_shape(plot.one) + tm_borders() + tm_text('id.all', size = 0.5) + #tm_polygons(col = 'id.all.random', legend.show = FALSE)
  tm_legend(
    main.title = paste('Cells (j=1,...,256)'),
    main.title.size = 1.1,
    main.title.position = "left")) 

(trial <- tm_shape(plot.one) + tm_polygons(col = 'N', palette = 'Greys') + #tm_text('id.all', size = 0.5) +
  tm_legend(legend.position = c('right', 'top'),
            legend.height = 0.16,
            main.title.size = 1.1,
            main.title = paste('Trial, with rates (by plot)'),
            main.title.position = "left")) 

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig3.tiff', units="in", width=12, height=5, res=300, compression = 'lzw')
tmap_arrange(id.x, id.plot, id.all,trial, nrow = 1)
dev.off()

#--------------------------
#Fig. 4: Weather
#--------------------------
yearly.weather.extra2.long <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/yearly.weather.extra2.long.rds')

julpp.mean <- yearly.weather.extra2.long[variable == 'Julpp' & year.real > 1979, mean(value)]
seassonpp.mean <- yearly.weather.extra2.long[variable %in% c('Julpp', 'Seasonpp') & year.real > 1979, .(Seasonpp = sum(value)),by=year.real][,mean(Seasonpp)]
annualpp.mean <- yearly.weather.extra2.long[variable %in% c('Julpp', 'Seasonpp', 'Annualpp') & year.real > 1979, .(Annualpp = sum(value)),by=year.real][,mean(Annualpp)]


(fig4 <- ggplot(yearly.weather.extra2.long,aes(year.real,value,fill=variable))+
    geom_bar(stat="identity",position="stack") + 
    scale_fill_manual(name='Weather variable', 
                      labels=c(Annualpp=expression(paste('pp'^'A')),
                               Seasonpp=expression(paste('pp'^'S')),
                               Julpp=expression(paste('pp'^'J'))),
                      values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
    geom_hline(yintercept = julpp.mean, color = "#000000") + 
    annotate("text",x=2010,y=julpp.mean+20,size=3,label=c('Historic'), color = "#000000") +
    annotate("text",x=2010,y=julpp.mean-15,size=3,label=c('mean'), color = "#000000") +
    geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
    annotate("text",x=2010,y=seassonpp.mean+20,size=3,label=c('Historic'), color = "#000000") +
    annotate("text",x=2010,y=seassonpp.mean-15,size=3,label=c('mean'), color = "#000000") +
    geom_hline(yintercept = annualpp.mean, color = "#000000") +
    annotate("text",x=2010,y=annualpp.mean+20,size=3,label=c('Historic'), color = "#000000") +
    annotate("text",x=2010,y=annualpp.mean-15,size=3,label=c('mean'), color = "#000000") +
    scale_x_continuous(breaks = 1979:2010,'year') +
    # scale_color_discrete(name='Wather Variable') +
    # scale_fill_manual(values = c("MRTN" = "#CCCCCC", "Stg. 1" = "#CC9900", "Stg. 2" = "#996600", "Stg. 3" = "#663300", "Stg. 4" = "black"))+
    xlab('Year') +
    ylab('Precipitation (mm)') +
    theme_bw() +
    theme(legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          legend.title = element_blank(),
          strip.text = element_blank()))

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig4.tiff', units="in", width=11, height=6, res=300, compression = 'lzw')
fig4
dev.off()

#--------------------------
#Fig. 5: Costs and Revenue for one set
#--------------------------
one.set <- profits.summary[starting.year == 1990 & L %in% c(0,2) & stg.id %in% c(0,3)]
# one.set[,revenue.ha := revenue.dol / area.field.ha]
# one.set[,fert.cost.ha := fert.cost.dol / area.field.ha]
# one.set[,revenue.ha := revenue.dol / area.field.ha]
# one.set[,c_app.ha := c_app.dol / area.field.ha]

one.set

DT.m1 = melt(one.set, id.vars = c('stg.id', 'year'),
             measure.vars = c('profits.dol', "revenue.dol","c_app.dol", "c_soilsamp.dol", "c_trial.dol", 'fert.cost.dol')) #"total.cost.dol", 
class(DT.m1)
# DT.m1[,type := factor(ifelse(variable %in% c('revenue.dol', 'profits.dol'), 'Revenue and Profits', 'Cost'), 
#                       levels = c('Revenue and Profits', 'Cost'))]

# DT.m1[stg.id == 0, stg.id := 'MRTN']
# DT.m1[stg.id == 3, stg.id := 'Stg.3']
DT.m1[, stg.id := factor(stg.id, levels = c('0', '3'))]

DT.m1[,type := factor(ifelse(variable %in% c('revenue.dol', 'profits.dol'), 'Revenue and Profits', ifelse(variable  == 'fert.cost.dol', 'Fert.Cost', 'Other Cost')), 
                      levels = c('Revenue and Profits', 'Fert.Cost', 'Other Cost'))]

#with facet
p <- ggplot(data=DT.m1) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2)+
  scale_linetype_discrete(name=element_blank(),
                          labels=c('0'=expression(paste('S'^'no,URA')),
                                   '1'=expression(paste('S'^'low,URA')),
                                   '2'=expression(paste('S'^'low,VRA')),
                                   '3'=expression(paste('S'^'high,URA', '(L=2)')),
                                   '4'=expression(paste('S'^'high,VRA'))))+
  labs(y = "$/ha",
       x = "year")+
  scale_color_manual(name=element_blank(),
                  labels=c(profits.dol=expression(paste(Pi)),
                           c_trial.dol=expression(paste('C'^'F')),
                           c_app.dol=expression(paste('C'^'A')),
                           revenue.dol="R",
                           c_soilsamp.dol = expression(paste('C'^'SS')),
                           fert.cost.dol = expression(paste('C'^'N'))),
                  values = c("profits.dol" = "#339900", 
                             "c_trial.dol" = "#CC9900", 
                             "c_app.dol" = "#996600", 
                             "revenue.dol" = "#663300", 
                             "c_soilsamp.dol" = "#CCCCCC",
                             "fert.cost.dol" = "#3366FF")) +
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.text.align = 0)+
  scale_x_continuous(breaks=1:10) 
p
p2 <- p + facet_grid(type~. , scales="free", space="free") +
  theme(#legend.position = 'bottom',
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text.align = 0,
        strip.text = element_blank())

p2
# https://stackoverflow.com/questions/49110877/how-to-adjust-facet-size-manually

gt = ggplot_gtable(ggplot_build(p2))
grid.draw(gt)
gtable_show_layout(gt)
gt$heights[10] = 7*gt$heights[10]
gt$heights[6] = 0.2*gt$heights[6]
gt$heights[14] = 0.5*gt$heights[14]

grid.draw(gt)

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig5.tiff', units="in", width=6.5, height=5, res=300, compression = 'lzw')
grid.draw(gt)
dev.off()


#--------------------------
#Fig. 6: PV by L for one starting year
#--------------------------
one.starting.year <- profits.summary.set4[starting.year == 1990]
one.starting.year <- one.starting.year[L == 0 & stg.id != 3, PV.dol := 0] #Keep only one L=0
one.starting.year[L == 0 & stg.id == 3, stg.id := '0']

one.starting.year[,stg.id := factor(stg.id)]
(one.starting.year[PV.dol == max(PV.dol),PV.dol] - one.starting.year[stg.id == 0,PV.dol])/318.6

(fig6 <- ggplot() +
    geom_bar(data=one.starting.year, aes(x=L,y=PV.dol/318.6, fill=stg.id),position='dodge',stat='identity') +
    scale_fill_manual(name = element_blank(),
                      #labels = c("MRTN" = "MRTN","1" = "Stg.1", "2" = "Stg.2", "3" = "Stg.3", "4" = "Stg.4"),
                      labels=c('0'=expression(paste('S'^'no,URA')),
                               '1'=expression(paste('S'^'low,URA')),
                               '2'=expression(paste('S'^'low,VRA')),
                               '3'=expression(paste('S'^'high,URA')),
                               '4'=expression(paste('S'^'high,VRA'))),
                      values = c("0" = "#CCCCCC", "1" = "#CC9900", "2" = "#996600", "3" = "#663300", "4" = "black")) +
    coord_cartesian(ylim=c(one.starting.year[L > 0,min(PV.dol)]/318.6-5,
                           one.starting.year[,max(PV.dol)]/318.6)+5) +
  scale_x_continuous(breaks=0:5) +
  xlab('L') +
  ylab('Annual PV ($/ha)') +
  theme_bw() +
  theme(#legend.position='bottom',
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.text.align = 0,
        strip.text = element_blank()))

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig6.tiff', units="in", width=6.5, height=4, res=300, compression = 'lzw')
fig6
dev.off()

#--------------------------
#Fig. 7: Showing variability in optimal stopping time
#--------------------------
profits.summary.set4
profits.summary.set4[starting.year == 2000 & L %in% 0:4]

DT.m2 = melt(profits.summary.set4, id.vars = c('starting.year', 'L', 'stg.id'),
             measure.vars = c('PV.dol.diff.MRTN')) #"total.cost.dol", 

DT.m2[,starting.year := as.character(starting.year)]
DT.m2[starting.year == 1980, starting.year := 'z = 1980']
DT.m2[, starting.year := factor(starting.year, levels = c('z = 1980', 1985,1990,1995,2000))]
DT.m2[, stg.id := factor(stg.id)]
#DT.m2[, stg.id := factor(paste('Stg.', stg.id, sep = ''))]

(p3 <- ggplot(data=DT.m2) + 
  geom_point(aes(x=L, y= value/318.6, colour = stg.id)) + 
  # scale_color_manual(, values = c("0" = "black", "1" = "red", "2" = "#FF9900", "3" = "#0066CC", "4" = "#660099"))+
    scale_color_manual(name=element_blank(), 
                       labels=c('0'=expression(paste('S'^'no,URA')),
                                '1'=expression(paste('S'^'low,URA')),
                                '2'=expression(paste('S'^'low,VRA')),
                                '3'=expression(paste('S'^'high,URA')),
                                '4'=expression(paste('S'^'high,VRA'))),
                       values = c("0" = "#CCCCCC", "1" = "#CC9900", "2" = "#996600", "3" = "#663300", "4" = "black"))+
    geom_line(aes(x=L, y= value/318.6, colour = stg.id), size = 1.2)+
  # geom_smooth(aes(x=L, y= value, colour = starting.year), se = FALSE, size = 1.2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
        #legend.title = element_blank())+
    #scale_color_discrete(name='Strategy ')+
    scale_x_continuous(breaks=0:5) +
  labs(y = expression(paste("Annual ", Delta, "PV ($/ha)")),
       x = "L"))

(fig7 <- p3 + facet_grid( starting.year~. , scales="free") +
  theme(#legend.position = 'bottom',
        strip.background = element_blank(),
        legend.text.align = 0))#+
        
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig7.tiff', units="in", width=5, height=4, res=300, compression = 'lzw')
fig7
dev.off()

#--------------------------
# TABLE 2: GETTING THE VALUE OF INFO AND TECN
#--------------------------
optimal.stopping.time <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/optimal.stopping.time.rds')

optimal.stopping.time.sum <- optimal.stopping.time[,.(PV.ha.diff.MRTN = mean(PV.ha.diff.MRTN)),by=.(starting.year,stg.id)][order(starting.year, stg.id)]

optimal.stopping.time.sum2 <- dcast(optimal.stopping.time.sum, starting.year ~ stg.id, value.var = "PV.ha.diff.MRTN", subset = .(stg.id != "MRTN"))
#setnames(optimal.stopping.time.sum2, c('starting.year', '1','2','3','4'), c('Starting year', paste("Stg", 1:4, sep = '.')))

optimal.stopping.time.sum2[,V.I.OFT := `1`]
optimal.stopping.time.sum2[,V.T.LOW := `2`-`1`]
optimal.stopping.time.sum2[,V.I.SS := `3`-`1`]
optimal.stopping.time.sum2[,V.T.HIGH := `4`-`3`]
#setnames(optimal.stopping.time.sum2, 'V.I.OFT',text(expression(paste('V'^'I.OFT'))))
avg <- optimal.stopping.time.sum2[, lapply(.SD, mean)][,'starting.year' := 'AVERAGE']
optimal.stopping.time.sum3 <- rbind(optimal.stopping.time.sum2, avg)
cols <- names(optimal.stopping.time.sum3)[-1]
optimal.stopping.time.sum3[,(cols) := round(.SD,1), .SDcols=cols]

table <- optimal.stopping.time.sum3 %>% data.frame()

tt <- ttheme_default(colhead=list(fg_params = list(parse = TRUE))) #fontface=2, 

cols2=c(starting.year= 'z',
        '1'= expression(paste('S'^'low,URA')),
        '2'= expression(paste('S'^'low,VRA')),
        '3'= expression(paste('S'^'high,URA')),
        '4'= expression(paste('S'^'high,VRA')),
        V.I.OFT=expression(paste('V'^'I.OFT')),
       V.T.LOW=expression(paste('V'^'T.LOW')),
       V.I.SS=expression(paste('V'^'I.SS')),
       V.T.HIGH=expression(paste('V'^'T.HIGH')))

colnames(table) <- cols2

table.tg.left <- tableGrob(table[,1:5], rows = NULL, 
                theme=tt)
table.tg.right <- tableGrob(table[,6:9], rows = NULL, 
                           theme=tt)
grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/table2.tiff', units="in", width=7, height=2, res=300, compression = 'lzw')
grid.arrange(table.tg.left, table.tg.right, nrow = 1, ncol = 3,    
             widths = c(0.5, 3,3),
             layout_matrix = rbind(c(NA, 1, 2),
                                   c(NA, 1, 2)))
dev.off()

#--------------------------
#Fig. 8: Showing N leaching for different L that are not L.OPT
#--------------------------
profits.summary.set4
profits.summary.set4[starting.year == 2000 & L %in% 0:4]


DT.m2 = melt(profits.summary.set4, id.vars = c('starting.year', 'L', 'stg.id'),
             measure.vars = c('Nleaching.annual.kg.ha')) #"total.cost.dol", 
DT.m2[, starting.year := as.character(starting.year)]
DT.m2[starting.year == 1980, starting.year := 'z = 1980']
DT.m2[, starting.year := factor(starting.year, levels = c('z = 1980', 1985,1990,1995,2000))]
DT.m2[, stg.id := factor(stg.id)]
DT.m2[L ==0]

(p4 <- ggplot(data=DT.m2) + 
    geom_point(aes(x=L, y= value, colour = stg.id)) + 
    # scale_color_manual(, values = c("0" = "black", "1" = "red", "2" = "#FF9900", "3" = "#0066CC", "4" = "#660099"))+
    geom_line(aes(x=L, y= value, colour = stg.id), size = 1.2)+
    scale_color_manual(name=element_blank(), 
                       labels=c('0'=expression(paste('S'^'no,URA')),
                                '1'=expression(paste('S'^'low,URA')),
                                '2'=expression(paste('S'^'low,VRA')),
                                '3'=expression(paste('S'^'high,URA')),
                                '4'=expression(paste('S'^'high,VRA'))),
                       values = c("0" = "#CCCCCC", "1" = "#CC9900", "2" = "#996600", "3" = "#663300", "4" = "black")) +
    # geom_smooth(aes(x=L, y= value, colour = starting.year), se = FALSE, size = 1.2)+
    theme_bw()+
    theme(panel.grid = element_blank())+
    scale_x_continuous(breaks=0:5) +
    ylim(min(DT.m2$value),max(DT.m2$value))+
    labs(y = "ANL (kg/ha)",
         x = "L"))

(fig8 <- p4 + facet_grid( starting.year~. , scales="free") +
    theme(#legend.position = 'bottom',
          strip.background = element_blank(),
          legend.text.align = 0))#+

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig8.tiff', units="in", width=5, height=4, res=300, compression = 'lzw')
fig8
dev.off()

#--------------------------
#Fig. 9: Nleaching.tn at optimal stopping time. THis shows the total values
#--------------------------
optimal.stopping.time <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/optimal.stopping.time.rds')
optimal.stopping.time2 <- optimal.stopping.time[,.SD[1], by=c('starting.year', 'stg.id')]
optimal.stopping.time2[stg.id == 'MRTN',stg.id := '0']
#optimal.stopping.time2[stg.id != 'MRTN',stg.id := paste('Stg.',stg.id, sep = ' ')]
optimal.stopping.time2[,stg.id := factor(stg.id)]

(fig9 <- ggplot() +
  #geom_bar(aes(x=starting.year,y=PV.dol,fill=stg.id),position='dodge',stat='identity') +
  geom_bar(data=optimal.stopping.time2, aes(x=starting.year,y=Nleaching.annual.kg.ha,fill=stg.id),position='dodge',stat='identity') +
  #scale_fill_discrete(name='Stg.') +values=c("#CC6666", "#9999CC", "#66CC99")
  #scale_fill_manual("legend", values = c("MRTN" = "black", "Stg. 1" = "red", "Stg. 2" = "#9999CC", "Stg. 3" = "#0066CC", "Stg. 4" = "darkgreen"))+
  #scale_fill_manual(values = c("MRTN" = "#CCCCCC", "Stg. 1" = "#CC9900", "Stg. 2" = "#996600", "Stg. 3" = "#663300", "Stg. 4" = "black"))+ 
  scale_fill_manual(name=element_blank(), 
                    labels=c('0'=expression(paste('S'^'no,URA')),
                             '1'=expression(paste('S'^'low,URA')),
                             '2'=expression(paste('S'^'low,VRA')),
                             '3'=expression(paste('S'^'high,URA')),
                             '4'=expression(paste('S'^'high,VRA'))),
                       values = c("0" = "#CCCCCC", "1" = "#CC9900", "2" = "#996600", "3" = "#663300", "4" = "black"))+
  labs(y = "ANL (kg/ha)",
         x = "z")+
  theme_bw() +
  theme(#legend.position='bottom',
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.text.align = 0,
        legend.title = element_blank(),
        strip.text = element_blank()))

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig9.tiff', units="in", width=6.5, height=4, res=300, compression = 'lzw')
fig9
dev.off()

#--------------------------
# TABLE 3: GETTING THE N LEACHING OF INFO AND TECN
#--------------------------

optimal.stopping.time <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/optimal.stopping.time.rds')
Nleaching.sum <- optimal.stopping.time[,.(Nleaching.diff.MRTN.kg.ha = mean(Nleaching.diff.MRTN.kg.ha)),by=.(starting.year,stg.id)][order(starting.year, stg.id)]

Nleaching.sum2 <- dcast(Nleaching.sum, starting.year ~ stg.id, value.var = "Nleaching.diff.MRTN.kg.ha", subset = .(stg.id != "MRTN"))


Nleaching.sum2[,NLI.I.OFT := `1`]
Nleaching.sum2[,NLI.T.LOW := `2`-`1`]
Nleaching.sum2[,NLI.I.SS := `3`-`1`]
Nleaching.sum2[,NLI.T.HIGH := `4`-`3`]
#setnames(optimal.stopping.time.sum2, 'V.I.OFT',text(expression(paste('V'^'I.OFT'))))
avg <- Nleaching.sum2[, lapply(.SD, mean)][,'starting.year' := 'AVERAGE']
Nleaching.sum3 <- rbind(Nleaching.sum2, avg)
cols <- names(Nleaching.sum3)[-1]
Nleaching.sum3[,(cols) := round(.SD,1), .SDcols=cols]

table3 <- Nleaching.sum3 %>% data.frame()

tt <- ttheme_default(colhead=list(fg_params = list(parse = TRUE))) #fontface=2, 

cols2=c(starting.year= 'z',
        '1'= expression(paste('S'^'low,URA')),
        '2'= expression(paste('S'^'low,VRA')),
        '3'= expression(paste('S'^'high,URA')),
        '4'= expression(paste('S'^'high,VRA')),
        V.I.OFT=expression(paste('NLI'^'I.OFT')),
        V.T.LOW=expression(paste('NLI'^'T.LOW')),
        V.I.SS=expression(paste('NLI'^'I.SS')),
        V.T.HIGH=expression(paste('NLI'^'T.HIGH')))

colnames(table3) <- cols2


table3.tg.left <- tableGrob(table3[,1:5], rows = NULL, 
                           theme=tt)
table3.tg.right <- tableGrob(table3[,6:9], rows = NULL, 
                            theme=tt)
grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/table3.tiff', units="in", width=7.5, height=2, res=300, compression = 'lzw')
grid.arrange(table3.tg.left, table3.tg.right, nrow = 1, ncol = 3,    
             widths = c(0.5, 3,3),
             layout_matrix = rbind(c(NA, 1, 2),
                                   c(NA, 1, 2)))
dev.off()

#--------------------------
#Fig. 10: Understanding 2000 negative PV (Case 1)
#--------------------------

sets.case1<- profits.summary[starting.year == 2000 & L %in% c(0,2) & stg.id %in% c(0,3)]
sets.case1[year == 1, profits.dol][1] - sets.case1[year == 1, profits.dol][2]
4631.37/31.86

yearly.weather.case1 <- yearly.weather.extra2.long[year.real %in% 2000:2009] %>% .[,year := year.real - 1999]

#https://stackoverflow.com/questions/11585954/varying-axis-labels-formatter-per-facet-in-ggplot-r
#Individual plots, arranged later

DT.m1.case1 = melt(sets.case1, id.vars = c('stg.id', 'year'),
                   measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol', 'PV.dol')) #"total.cost.dol", 

PV.diff <- sets.case1[stg.id == 3, PV.dol] - sets.case1[stg.id == 0, PV.dol]
PV.diff.cum <- cumsum(PV.diff)

sets.case1.pv <- rbind(DT.m1.case1[variable != 'PV.dol'],
                       data.table(stg.id = 3, year = 1:10, variable = 'PV.diff.cum', value = PV.diff.cum))

DT.comb.case1 <- rbind(sets.case1.pv, yearly.weather.case1[,-c('year.real')], fill = TRUE)

unique(DT.comb.case1$stg.id)
unique(DT.comb.case1$variable)
DT.comb.case1[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                     ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','PV')), 
                              levels = c('Weather', 'Economic', 'PV'))]

DT.comb.case1[, variable := factor(variable)]

p1.case1 <- ggplot(data = subset(DT.comb.case1,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),
                             Seasonpp=expression(paste('pp'^'S')),
                             Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case1


p2.case1 <- ggplot(data = subset(DT.comb.case1,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2)+
  #geom_bar(data = subset(Economic.data,type == 'Weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='Strategy line type',
                        values = c("0" = "dotted", 
                                   "1" = "dashed", 
                                   "3" = "solid"),
                        labels=c('0'=expression(paste('S'^'no,URA')),
                                 '1'=expression(paste('S'^'low,URA')),
                                 '2'=expression(paste('S'^'low,VRA')),
                                 '3'=expression(paste('S'^'high,URA', ', L=2')),
                                 '4'=expression(paste('S'^'high,VRA')))) +
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p2.case1

p3.case1 <- ggplot(data = subset(DT.comb.case1,type == 'PV')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2)+
  scale_linetype_manual(name='Strategy line type',
                        values = c("0" = "dotted", 
                                   "1" = "dashed", 
                                   "3" = "solid"),
                        guide = 'none')+
  labs(y = expression(paste('Cumulative ',Delta, "PV ($/ha)")),
       x = "year")+
  scale_color_manual(name=element_blank(),
                     labels=c(PV.diff.cum = expression(paste('Cumulative ',Delta, "PV"))),
                     values = c("PV.diff.cum" = "#660000")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case1

#First 3 plots, with no legend  

g1.case1=ggplotGrob(p1.case1)
g2.case1=ggplotGrob(p2.case1)
g3.case1=ggplotGrob(p3.case1)

case1.tg = rbind(g1.case1,g2.case1,g3.case1,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)
title.case1 <- textGrob("a) Case 1 (z=2000)",gp=gpar(fontsize=16))
padding <- unit(2,"mm")

case1.tg <- gtable_add_rows(case1.tg, heights = grobHeight(title.case1) + padding, pos = 0)
#gtable_show_layout(table)
case1.tit.tg <- gtable_add_grob(case1.tg, title.case1, 1, 1, 1, ncol(case1.tg))
case1.tit.tg$heights[19] = 1.5*case1.tit.tg$heights[19]
grid.newpage()
grid.draw(case1.tit.tg)

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig10.tiff', units="in", width=7, height=8, res=300, compression = 'lzw')
# grid.arrange(case1.tit.tg, case2a.tit.tg, case2b.tit.tg, ncol = 4,    
#              widths = c(0.5, 0.5, 0.5, 0.15),
#              layout_matrix = rbind(c(1, 2, 3, 3),
#                                    c(1, 2, 3, 3)))
grid.draw(case1.tit.tg)
dev.off()

#--------------------------
#Fig. 11 and 12: Trial profitability
#--------------------------

#--- Trial Oportunity cost by L
trial.profitability <- profits.summary[L %in% c(0,1)  & stg.id %in% c(0,3) & year < 6]
# trial.profitability[stg.id == 0, stg.id := 'MRTN']
# trial.profitability[stg.id == 3, stg.id := 'Stg.3']
trial.profitability[, N.profits := revenue.dol - fert.cost.dol] #what can be improved by selecting better the treatments

trial.profitability.comp <- dcast(trial.profitability, starting.year + year ~ stg.id, value.var = "N.profits")
trial.profitability.comp[,N.profits.diff.ha :=  (`3` - `0`)/31.86]
# precipitations <- wendte.complete.dt[,.(Seasonpp = mean(Seasonpp), Julpp = mean(Julpp)), by=.(starting.year, year)]
# trial.profitability.comp.2 <- merge(trial.profitability.comp, precipitations)

trial.profitability.comp[,starting.year := factor(starting.year)]

trial.profitability.comp.mean <- trial.profitability.comp[,.(N.profits.diff.ha = mean(N.profits.diff.ha)), by = year][,starting.year := 'AVERAGE']

(fig11 <- ggplot() +
    geom_line(data=trial.profitability.comp, aes(x=year,y=N.profits.diff.ha, colour=starting.year), size = 1.3) +
    geom_line(data=trial.profitability.comp.mean, aes(x=year,y=N.profits.diff.ha, colour=starting.year), size = 1.7) +
    # geom_bar(data=one.starting.year[L == 0,], aes(x=L,y=PV.dol,fill=stg.id),position='dodge',stat='identity', width = 0.2, fill = "black") +
    # scale_fill_manual(values = c("MRTN" = "#996600", "Stg. 1" = "#CCCCCC", "Stg. 2" = "#999999", "Stg. 3" = "#666666", "Stg. 4" = "#333333"))+
    # scale_fill_manual(values = c("MRTN" = "#CCCCCC", "Stg. 1" = "#CC9900", "Stg. 2" = "#996600", "Stg. 3" = "#663300", "Stg. 4" = "black"))+
    # coord_cartesian(ylim=c(one.starting.year[L > 0,min(PV.dol)]-2000,
    #                        one.starting.year[,max(PV.dol)]+2000)) +
    scale_x_continuous(breaks=0:5) +
    #scale_color_brewer(name = 'Starting year', palette = "Accent") + 
    scale_color_manual(name = 'z', 
                       values = c("AVERAGE" = '#CC0000',"1980" = "#CCCCCC", "1985" = "#CC9900", "1990" = "#996600", "1995" = "#663300", "2000" = "black"))+
    xlab('Trial year') +
    ylab('Opportunity cost ($/ha)') +
    theme_bw() +
    theme(#legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          strip.text = element_blank()))

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig11.tiff', units="in", width=5, height=3, res=300, compression = 'lzw')
fig11
dev.off()


(fig12 <- ggplot(wendte.complete.dt[year == 1]) + 
  geom_point(aes(x = N, y= Y, colour = factor(starting.year)))+
  geom_smooth(aes(x = N, y= Y, colour = factor(starting.year)), method = "lm", formula = y ~ x + I(x^2), se = FALSE)+
  scale_color_manual(name = 'z', values = c("1980" = "#CCCCCC", "1985" = "#CC9900", "1990" = "#996600", "1995" = "#663300", "2000" = "black"))+
  xlab('N (kg/ha)') +
  ylab('Y (kg/ha)') +
  theme_bw() +
  theme(#legend.position='bottom',
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.text.align = 0,
        strip.text = element_blank()))

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig12.tiff', units="in", width=5, height=3, res=300, compression = 'lzw')
fig12
dev.off()
#--------------------------
#Fig. 13a: Understanding 1985 low and high info (Case 2a)
#--------------------------
sets.case2a <- profits.summary[starting.year == 1985 & L ==1 & stg.id %in% c(1,3)]
yearly.weather.case2a <- yearly.weather.extra2.long[year.real %in% 1985:1994] %>% .[,year := year.real - 1984]

#https://stackoverflow.com/questions/11585954/varying-axis-labels-formatter-per-facet-in-ggplot-r
#Individual plots, arranged later

DT.m1.case2a = melt(sets.case2a, id.vars = c('stg.id', 'year'),
                   measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol','N.initial', 'Y.N.prev')) #"total.cost.dol", 

DT.comb.case2a <- rbind(DT.m1.case2a, yearly.weather.case2a[,-c('year.real')], fill = TRUE)

# DT.comb.case2a[stg.id == 0, stg.id := 'MRTN']
# DT.comb.case2a[stg.id == 1, stg.id := 'Stg.1']
# DT.comb.case2a[stg.id == 3, stg.id := 'Stg.3']
# DT.comb.case2a[, stg.id := factor(stg.id, levels = c('MRTN', 'Stg.1', 'Stg.3'))]
DT.comb.case2a[, stg.id := factor(stg.id)]
unique(DT.comb.case2a$stg.id)
unique(DT.comb.case2a$variable)
DT.comb.case2a[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                     ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','soil')), 
                              levels = c('Weather', 'Economic', 'soil'))]

DT.comb.case2a[, variable := factor(variable)]

p1.case2a <- ggplot(data = subset(DT.comb.case2a,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),
                             Seasonpp=expression(paste('pp'^'S')),
                             Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-30,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-30,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-30,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case2a

p2.case2a <- ggplot(data = subset(DT.comb.case2a,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2)+
  #geom_bar(data = subset(Economic.data,type == 'Weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='Strategy line type',
                        values = c("0" = "dotted", 
                                   "1" = "dashed", 
                                   "3" = "solid"),
                        labels=c('0'=expression(paste('S'^'no,URA')),
                                 '1'=expression(paste('S'^'low,URA',', L=1')),
                                 '2'=expression(paste('S'^'low,VRA')),
                                 '3'=expression(paste('S'^'high,URA',', L=1')),
                                 '4'=expression(paste('S'^'high,VRA')))) +
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p2.case2a

p3.case2a <- ggplot(data = subset(DT.comb.case2a,type == 'soil')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2)+
  scale_linetype_manual(name='Strategy line type',
                        values = c("0" = "dotted", 
                                   "1" = "dashed", 
                                   "3" = "solid"),
                        labels=c('0'=expression(paste('S'^'no,URA')),
                                 '1'=expression(paste('S'^'low,URA')),
                                 '2'=expression(paste('S'^'low,VRA')),
                                 '3'=expression(paste('S'^'high,URA')),
                                 '4'=expression(paste('S'^'high,VRA'))),
                        guide = 'none')+
  scale_color_manual(name='Soil variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "kg/ha",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case2a

#First 3 plots, with no legend  

g1.case2a=ggplotGrob(p1.case2a+theme(legend.position="none"))
g2.case2a=ggplotGrob(p2.case2a+theme(legend.position="none"))
g3.case2a=ggplotGrob(p3.case2a+theme(legend.position="none"))

case2a.tg = rbind(g1.case2a,g2.case2a,g3.case2a,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)
title.case2a <- textGrob("a) Case 2a (z = 1985)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")

case2a.tg <- gtable_add_rows(case2a.tg, heights = grobHeight(title.case2a) + padding, pos = 0)
#gtable_show_layout(table)
case2a.tit.tg <- gtable_add_grob(case2a.tg, title.case2a, 1, 1, 1, ncol(case2a.tg))
case2a.tit.tg$heights[19] = 1.5*case2a.tit.tg$heights[19]
grid.newpage()
grid.draw(case2a.tit.tg)

#--------------------------
#Fig. 13b: Understanding 1985 low and high info (Case 2b)
#--------------------------
sets.case2b<- profits.summary[starting.year == 2000 & L ==1 & stg.id %in% c(1,3)]
yearly.weather.case2b <- yearly.weather.extra2.long[year.real %in% 2000:2009] %>% .[,year := year.real - 1999]

#https://stackoverflow.com/questions/11585954/varying-axis-labels-formatter-per-facet-in-ggplot-r
#Individual plots, arranged later

DT.m1.case2b = melt(sets.case2b, id.vars = c('stg.id', 'year'),
                    measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol','N.initial', 'Y.N.prev')) #"total.cost.dol", 

DT.comb.case2b <- rbind(DT.m1.case2b, yearly.weather.case2b[,-c('year.real')], fill = TRUE)

# DT.comb.case2b[stg.id == 0, stg.id := 'MRTN']
# DT.comb.case2b[stg.id == 1, stg.id := 'Stg.1']
# DT.comb.case2b[stg.id == 3, stg.id := 'Stg.3']
# DT.comb.case2b[, stg.id := factor(stg.id, levels = c('MRTN', 'Stg.1', 'Stg.3'))]
DT.comb.case2b[, stg.id := factor(stg.id)]
unique(DT.comb.case2b$stg.id)
unique(DT.comb.case2b$variable)
DT.comb.case2b[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                      ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','soil')), 
                               levels = c('Weather', 'Economic', 'soil'))]

DT.comb.case2b[, variable := factor(variable)]

p1.case2b <- ggplot(data = subset(DT.comb.case2b,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),
                             Seasonpp=expression(paste('pp'^'S')),
                             Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-30,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-30,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-30,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case2b

p2.case2b <- ggplot(data = subset(DT.comb.case2b,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2)+
  #geom_line(data = subset(DT.comb.case1,type  == 'Economic'), aes(x=year,y=value/31.86, colour = variable, linetype = stg.id), size = 1.2, alpha=0)+ #to get the legend
  #geom_bar(data = subset(econ.data,type == 'Weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='Strategy line type',
                     values = c("0" = "dotted", 
                                   "1" = "dashed", 
                                   "3" = "solid"),
                        labels=c('0'=expression(paste('S'^'no,URA')),
                                 '1'=expression(paste('S'^'low,URA',', L=1')),
                                 '2'=expression(paste('S'^'low,VRA')),
                                 '3'=expression(paste('S'^'high,URA',', L=1')),
                                 '4'=expression(paste('S'^'high,VRA')))) +
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(1, 'cm'),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p2.case2b

p3.case2b <- ggplot(data = subset(DT.comb.case2b,type == 'soil')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value, colour = variable, linetype = stg.id), size = 1.2)+
  scale_linetype_manual(name='Strategy line type',
                        values = c("0" = "dotted", 
                                   "1" = "dashed", 
                                   "3" = "solid"),
                        guide = 'none')+
  scale_color_manual(name='Soil variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "kg/ha",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case2b

#Last 3 plots, with legend  

g1.case2b=ggplotGrob(p1.case2b)
g2.case2b=ggplotGrob(p2.case2b)
g3.case2b=ggplotGrob(p3.case2b)

case2b.tg = rbind(g1.case2b,g2.case2b,g3.case2b,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)
title.case2b <- textGrob("b) Case 2b (z = 2000)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")

case2b.tg <- gtable_add_rows(case2b.tg, heights = grobHeight(title.case2b) + padding, pos = 0)
#gtable_show_layout(table)
case2b.tit.tg <- gtable_add_grob(case2b.tg, title.case2b, 1, 1, 1, ncol(case2b.tg))
case2b.tit.tg$heights[19] = 1.5*case2b.tit.tg$heights[19]
grid.newpage()
grid.draw(case2b.tit.tg)

#--------------------------
#Fig. 13: PUT TOGETHER CASE 2a AND 2b
#--------------------------
grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig13.tiff', units="in", width=12, height=9, res=300, compression = 'lzw')
grid.arrange(case2a.tit.tg, case2b.tit.tg, ncol = 3,    
             widths = c(0.4, 0.4, 0.10),
             layout_matrix = rbind(c(1, 2, 2),
                                   c(1, 2, 2)))
dev.off()

#--------------------------
#Fig. 14a: Understanding L.OPT = 1 (and drop in L=2) - Case 3
#--------------------------
sets.case3<- profits.summary[starting.year %in% c(1980) & L %in% c(1,2) & stg.id == 3]
yearly.weather.case3 <- yearly.weather.extra2.long[year.real %in% c(1980:1989)] %>% .[,year := year.real - 1979]

#https://stackoverflow.com/questions/11585954/varying-axis-labels-formatter-per-facet-in-ggplot-r
#Individual plots, arranged later

DT.m1.case3 = melt(sets.case3, id.vars = c('L', 'year'),
                   measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol', 'PV.dol')) #"total.cost.dol", 

PV.diff <- sets.case3[L == 1, PV.dol] - sets.case3[L == 2, PV.dol]
PV.diff.cum <- cumsum(PV.diff)

sets.case3.pv <- rbind(DT.m1.case3[variable != 'PV.dol'],
                       data.table(L = 1, year = 1:10, variable = 'PV.diff.cum', value = PV.diff.cum))

DT.comb.case3 <- rbind(sets.case3.pv, yearly.weather.case3[,-c('year.real')], fill = TRUE)
DT.comb.case3[, L := factor(L)]

unique(DT.comb.case3$stg.id)
unique(DT.comb.case3$variable)
DT.comb.case3[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                     ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','PV')), 
                              levels = c('Weather', 'Economic', 'PV'))]

DT.comb.case3[, variable := factor(variable)]

p1.case3 <- ggplot(data = subset(DT.comb.case3,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),
                             Seasonpp=expression(paste('pp'^'S')),
                             Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case3


p2.case3 <- ggplot(data = subset(DT.comb.case3,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  #geom_line(data = subset(DT.comb.case1,type  == 'econ'), aes(x=year,y=value, colour = variable, linetype = stg.id), size = 1.2, alpha=0)+ #to get the legend
  #geom_bar(data = subset(econ.data,type == 'Weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted"))+
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.text.align = 0)
p2.case3


p3.case3 <- ggplot(data = subset(DT.comb.case3,type == 'PV')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted"),
                        guide = 'none')+
  labs(y = expression(paste('Cumulative ',Delta, "PV ($/ha)")),
       x = "year")+
  scale_color_manual(name=element_blank(),
                     labels=c(PV.diff.cum = expression(paste('Cumulative ',Delta, "PV"))),
                     values = c("PV.diff.cum" = "#660000")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case3

#First 3 plots, with no legend  

g1.case3=ggplotGrob(p1.case3+theme(legend.position="none"))
g2.case3=ggplotGrob(p2.case3+theme(legend.position="none"))
g3.case3=ggplotGrob(p3.case3+theme(legend.position="none"))

case3.tg = rbind(g1.case3,g2.case3,g3.case3,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)
#title.case3 <- textGrob("a) Case 3 (w=1980, Stg.3)",gp=gpar(fontsize=16))
title.case3 <- textGrob(expression(paste('a) Case 3 (z=1980, S'^'high,URA',')')),gp=gpar(fontsize=16))
padding <- unit(2,"mm")

case3.tg <- gtable_add_rows(case3.tg, heights = grobHeight(title.case3) + padding, pos = 0)
#gtable_show_layout(table)
case3.tit.tg <- gtable_add_grob(case3.tg, title.case3, 1, 1, 1, ncol(case3.tg))
case3.tit.tg$heights[19] = 1.5*case3.tit.tg$heights[19]
grid.newpage()
grid.draw(case3.tit.tg)

#--------------------------
#Fig. 14b: Understanding L.OPT = 1 (and drop in L=2) - Case 4
#--------------------------

sets.case4<- profits.summary[starting.year %in% c(1985) & L %in% c(1,2) & stg.id == 3]
yearly.weather.case4 <- yearly.weather.extra2.long[year.real %in% c(1985:1994)] %>% .[,year := year.real - 1984]

#https://stackoverflow.com/questions/11585954/varying-axis-labels-formatter-per-facet-in-ggplot-r
#Individual plots, arranged later

DT.m1.case4 = melt(sets.case4, id.vars = c('L', 'year'),
                   measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol', 'PV.dol')) #"total.cost.dol", 

PV.diff <- sets.case4[L == 1, PV.dol] - sets.case4[L == 2, PV.dol]
PV.diff.cum <- cumsum(PV.diff)
PV.diff.cum/31.86

sets.case4.pv <- rbind(DT.m1.case4[variable != 'PV.dol'],
                       data.table(L = 1, year = 1:10, variable = 'PV.diff.cum', value = PV.diff.cum))

DT.comb.case4 <- rbind(sets.case4.pv, yearly.weather.case4[,-c('year.real')], fill = TRUE)
DT.comb.case4[, L := factor(L)]

unique(DT.comb.case4$variable)
DT.comb.case4[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                     ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','PV')), 
                              levels = c('Weather', 'Economic', 'PV'))]

DT.comb.case4[, variable := factor(variable)]

p1.case4 <- ggplot(data = subset(DT.comb.case4,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),
                             Seasonpp=expression(paste('pp'^'S')),
                             Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case4


p2.case4 <- ggplot(data = subset(DT.comb.case4,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  #geom_line(data = subset(DT.comb.case1,type  == 'econ'), aes(x=year,y=value, colour = variable, linetype = stg.id), size = 1.2, alpha=0)+ #to get the legend
  #geom_bar(data = subset(econ.data,type == 'weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted"))+
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(1, 'cm'),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p2.case4


p3.case4 <- ggplot(data = subset(DT.comb.case4,type == 'PV')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted"),
                        guide = 'none')+
  labs(y = expression(paste('Cumulative ',Delta, "PV ($/ha)")),
       x = "year")+
  scale_color_manual(name=element_blank(),
                     labels=c(PV.diff.cum = expression(paste('Cumulative ',Delta, "PV"))),
                     values = c("PV.diff.cum" = "#660000")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case4

#Last 3 plots, with legend  

g1.case4=ggplotGrob(p1.case4)
g2.case4=ggplotGrob(p2.case4)
g3.case4=ggplotGrob(p3.case4)

case4.tg = rbind(g1.case4,g2.case4,g3.case4,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)
# title.case4 <- textGrob("II) Case 4 (w=1985, Stg.3)",gp=gpar(fontsize=16))
title.case4 <- textGrob(expression(paste('b) Case 4 (z=1985, S'^'high,URA',')')),gp=gpar(fontsize=16))
padding <- unit(2,"mm")

case4.tg <- gtable_add_rows(case4.tg, heights = grobHeight(title.case4) + padding, pos = 0)
#gtable_show_layout(table)
case4.tit.tg <- gtable_add_grob(case4.tg, title.case4, 1, 1, 1, ncol(case4.tg))
case4.tit.tg$heights[19] = 1.5*case4.tit.tg$heights[19]
grid.newpage()
grid.draw(case4.tit.tg)

#--------------------------
#Fig. 14: PUT TOGETHER CASE 3 and 4
#--------------------------
grid.newpage()

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig14.tiff', units="in", width=12, height=9, res=300, compression = 'lzw')
grid.arrange(case3.tit.tg, case4.tit.tg, ncol = 3,    
             widths = c(0.4, 0.4, 0.10),
             layout_matrix = rbind(c(1, 2, 2),
                                   c(1, 2, 2)))
dev.off()

#--------------------------
#Fig. 15a: Understanding L.OPT = 2 (and increase from L=1) in 1990  - Case 5
#--------------------------
sets.case5<- profits.summary[starting.year %in% c(1990) & L %in% c(1,2) & stg.id == 3]
yearly.weather.case5 <- yearly.weather.extra2.long[year.real %in% c(1990:1999)] %>% .[,year := year.real - 1989]

#https://stackoverflow.com/questions/11585954/varying-axis-labels-formatter-per-facet-in-ggplot-r
#Individual plots, arranged later

DT.m1.case5 = melt(sets.case5, id.vars = c('L', 'year'),
                   measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol', 'PV.dol')) #"total.cost.dol", 

PV.diff <- sets.case5[L == 2, PV.dol] - sets.case5[L == 1, PV.dol]
PV.diff.cum <- cumsum(PV.diff)
PV.diff.cum/31.86

sets.case5.pv <- rbind(DT.m1.case5[variable != 'PV.dol'],
                       data.table(L = 2, year = 1:10, variable = 'PV.diff.cum', value = PV.diff.cum))

DT.comb.case5 <- rbind(sets.case5.pv, yearly.weather.case5[,-c('year.real')], fill = TRUE)
DT.comb.case5[, L := factor(L)]

unique(DT.comb.case5$variable)
DT.comb.case5[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                     ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','PV')), 
                              levels = c('Weather', 'Economic', 'PV'))]

DT.comb.case5[, variable := factor(variable)]

p1.case5 <- ggplot(data = subset(DT.comb.case5,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),                                Seasonpp=expression(paste('pp'^'S')),                                Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case5


p2.case5 <- ggplot(data = subset(DT.comb.case5,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  #geom_line(data = subset(DT.comb.case1,type  == 'econ'), aes(x=year,y=value, colour = variable, linetype = stg.id), size = 1.2, alpha=0)+ #to get the legend
  #geom_bar(data = subset(econ.data,type == 'weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted")) +
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(1, 'cm'),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p2.case5


p3.case5 <- ggplot(data = subset(DT.comb.case5,type == 'PV')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted"),
                        guide = 'none')+
  labs(y = expression(paste('Cumulative ',Delta, "PV ($/ha)")),
       x = "year")+
  scale_color_manual(name=element_blank(),
                     labels=c(PV.diff.cum = expression(paste('Cumulative ',Delta, "PV"))),
                     values = c("PV.diff.cum" = "#660000")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case5

#First 3 plots, with no legend  

g1.case5=ggplotGrob(p1.case5+theme(legend.position="none"))
g2.case5=ggplotGrob(p2.case5+theme(legend.position="none"))
g3.case5=ggplotGrob(p3.case5+theme(legend.position="none"))

case5.tg = rbind(g1.case5,g2.case5,g3.case5,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)

# title.case5 <- textGrob("I) Case 5 (z=1990, Stg.3)",gp=gpar(fontsize=16))
title.case5 <- textGrob(expression(paste('a) Case 5 (z=1990, S'^'high,URA',')')),gp=gpar(fontsize=16))
padding <- unit(2,"mm")

case5.tg <- gtable_add_rows(case5.tg, heights = grobHeight(title.case5) + padding, pos = 0)
#gtable_show_layout(table)
case5.tit.tg <- gtable_add_grob(case5.tg, title.case5, 1, 1, 1, ncol(case5.tg))
case5.tit.tg$heights[19] = 1.5*case5.tit.tg$heights[19]
grid.newpage()
grid.draw(case5.tit.tg)
#--------------------------
#Fig. 15b: Understanding L.OPT = 2 (and increase from L=1) in 1995  - Case 6
#--------------------------
sets.case6 <- profits.summary[starting.year %in% c(1995) & L %in% c(1,2) & stg.id == 3]

yearly.weather.case6 <- yearly.weather.extra2.long[year.real %in% c(1995:2004)] %>% .[,year := year.real - 1994]


DT.m1.case6 = melt(sets.case6, id.vars = c('L', 'year'),
                   measure.vars = c('profits.dol', "revenue.dol",'fert.cost.dol', 'PV.dol')) #"total.cost.dol", 

PV.diff <- sets.case6[L == 2, PV.dol] - sets.case6[L == 1, PV.dol]
PV.diff.cum <- cumsum(PV.diff)
PV.diff.cum/31.86

sets.case6.pv <- rbind(DT.m1.case6[variable != 'PV.dol'],
                       data.table(L = 2, year = 1:10, variable = 'PV.diff.cum', value = PV.diff.cum))

DT.comb.case6 <- rbind(sets.case6.pv, yearly.weather.case6[,-c('year.real')], fill = TRUE)
DT.comb.case6[, L := factor(L)]

unique(DT.comb.case6$variable)
DT.comb.case6[,type := factor(ifelse(variable %in% c('Julpp', 'Seasonpp', 'Annualpp'), 'Weather',
                                     ifelse(variable %in% c('profits.dol' ,'revenue.dol', 'fert.cost.dol'), 'Economic','PV')), 
                              levels = c('Weather', 'Economic', 'PV'))]

DT.comb.case6[, variable := factor(variable)]

p1.case6 <- ggplot(data = subset(DT.comb.case6,type == 'Weather')) + facet_wrap(~type) +
  geom_bar(aes(year,value,fill=variable),  stat="identity",position="stack") + 
  scale_fill_manual(name='Weather variable', 
                    labels=c(Annualpp=expression(paste('pp'^'A')),                                
                             Seasonpp=expression(paste('pp'^'S')),                                
                             Julpp=expression(paste('pp'^'J'))),
                    values = c("Annualpp" = "#CCCCCC", "Seasonpp" = "#999999", "Julpp" = "#666666")) + 
  geom_hline(yintercept = julpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=julpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=julpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = seassonpp.mean, color = "#000000") + 
  annotate("text",x=10.4,y=seassonpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=seassonpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  geom_hline(yintercept = annualpp.mean, color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean+50,size=3,label=c('Hist'), color = "#000000") +
  annotate("text",x=10.4,y=annualpp.mean-40,size=3,label=c('mean'), color = "#000000") +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "Precipitation (mm/year)",
       x = "year")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p1.case6


p2.case6 <- ggplot(data = subset(DT.comb.case6,type  == 'Economic')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  #geom_line(data = subset(DT.comb.case1,type  == 'econ'), aes(x=year,y=value, colour = variable, linetype = stg.id), size = 1.2, alpha=0)+ #to get the legend
  #geom_bar(data = subset(econ.data,type == 'weather'), aes(year,value,fill=variable), alpha = 0, stat="identity",position="stack")+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted")) +
  scale_color_manual(name='Economic variables',
                     labels=c(profits.dol=expression(paste(Pi)),
                              revenue.dol="R",
                              fert.cost.dol = expression(paste('C'^'N')),
                              N.initial= expression(paste('N'^'Apr')),
                              Y.N.prev= expression(paste('Y/N'['t-1']))),
                     values = c("profits.dol" = "#339900", 
                                "revenue.dol" = "#663300", 
                                "fert.cost.dol" = "#3366FF",
                                #"N.initial" = "#CCCCCC",
                                "Y.N.prev" = "#666666", 
                                "N.initial" = "#996600")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  labs(y = "$/ha",
       x = "year")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.key.size = unit(1, 'cm'),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p2.case6


p3.case6 <- ggplot(data = subset(DT.comb.case6,type == 'PV')) + facet_wrap(~type) +
  geom_line(aes(x=year,y=value/31.86, colour = variable, linetype = L), size = 1.2)+
  scale_linetype_manual(name='L line type',
                        labels=c("1"= "L=1",
                                 "2"="L=2"),
                        values = c("1" = "solid",
                                   "2" = "dashed",
                                   "3" = "dotted"),
                        guide = 'none')+
  labs(y = expression(paste('Cumulative ',Delta, "PV ($/ha)")),
       x = "year")+
  scale_color_manual(name=element_blank(),
                     labels=c(PV.diff.cum = expression(paste('Cumulative ',Delta, "PV"))),
                     values = c("PV.diff.cum" = "#660000")) +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5),'year')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        #legend.position='bottom',
        #strip.background = element_blank(),
        #legend.title = element_blank(),
        #strip.text = element_blank(),
        legend.text.align = 0)
p3.case6

#Last 3 plots, with legend  

g1.case6=ggplotGrob(p1.case6)
g2.case6=ggplotGrob(p2.case6)
g3.case6=ggplotGrob(p3.case6)

case6.tg = rbind(g1.case6,g2.case6,g3.case6,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)

# title.case6 <- textGrob("b) Case 6 (w=1995, Stg.3)",gp=gpar(fontsize=16))
title.case6 <- textGrob(expression(paste('b) Case 6 (z=1995, S'^'high,URA',')')),gp=gpar(fontsize=16))
padding <- unit(2,"mm")
case6.tg <- gtable_add_rows(case6.tg, heights = grobHeight(title.case6) + padding, pos = 0)

case6.tit.tg <- gtable_add_grob(case6.tg, title.case6, 1, 1, 1, ncol(case6.tg))
gtable_show_layout(case6.tit.tg)
case6.tit.tg$widths[3] = 1.5*case6.tit.tg$widths[3]
case6.tit.tg$heights[19] = 1.5*case6.tit.tg$heights[19]

grid.newpage()
grid.draw(case6.tit.tg)

#--------------------------
#Fig. 15: PUT TOGETHER CASE 5 and 6
#--------------------------
grid.newpage()

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig15.tiff', units="in", width=12, height=9, res=300, compression = 'lzw')
grid.arrange(case5.tit.tg, case6.tit.tg, ncol = 3,    
             widths = c(0.4, 0.4, 0.10),
             layout_matrix = rbind(c(1, 2, 2),
                                   c(1, 2, 2)))
dev.off()
#--------------------------
#Fig. 16: Trial CHARACTERIZATION 1995
#--------------------------
# trial.data.aggregated.1995.L2.dt <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/trial.data.aggregated.1995.L2.dt.rds')
trial.data.1995.L2 <- wendte.complete.dt[starting.year == 1995 & stg.id == 'stage1' & year <=2]
trial.data.1995.L2[,year := factor(year)]

trial.data.1995.L2[,N.total.ss.gross := N.total.ss * area]
trial.data.1995.L2[,Y.gross :=  Y * area]
trial.data.1995.L2.plot <- trial.data.1995.L2[,.(area = sum(area), N.total.ss = sum(N.total.ss.gross) / sum(area), Y =  sum(Y.gross) / sum(area)), by = .(year, id.plot)][id.plot > 0]


(fig16 <- ggplot() +
    geom_smooth(data=trial.data.1995.L2.plot, aes(x=N.total.ss,y=Y, linetype = year, colour = year), size = 1.3, method = "lm", formula = y ~ x + I(x^2))+
    geom_point(data=trial.data.1995.L2.plot, aes(x=N.total.ss,y=Y, colour = year), size = 1.3) +
    scale_linetype_manual(name='L line type',
                          labels=c("1"= "L=1",
                                   "2"="L=2"),
                          values = c("1" = "solid",
                                     "2" = "dashed",
                                     "3" = "dotted"),
                          guide = 'none')+
    scale_color_manual(name = 'Trial year', values = c("1" = "#996600", "2" = "#663300"))+
    xlab(expression(paste('N'^'Apr'))) +
    ylab('Yield (kg/ha)') +
    theme_bw() +
    theme(#legend.position='bottom',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.text.align = 0,
          strip.text = element_blank()))




tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/fig16.tiff', units="in", width=5, height=3, res=300, compression = 'lzw')
fig16
dev.off()

#--------------------------
#TABLE 4: TRYING TO FIND L.OPT
#--------------------------

Seasonpp.mean <- 454 #from APSIM run from 1979:2015
Seasonpp.sd <- 153 #from APSIM run from 1979:2015
Julpp.mean <- 105 #from APSIM run from 1979:2015
Julpp.sd <- 62 #from APSIM run from 1979:2015

mrtn.data <- wendte.complete.dt[stg.id == 0 & year < 3 & starting.year < 2000]
mrtn.data.sum <- mrtn.data[,.(Y = mean(Y),
                              N.initial = mean(N.initial),
                              Seasonpp = ((mean(Seasonpp)-454)/153),
                              Julpp = ((mean(Julpp)-105)/62)), by = .(starting.year, year)]
mrtn.data.sum[,Y.r := Y/mean(Y)][]

trial.data <- wendte.complete.dt[stg.id == 'stage1' & year < 3 & starting.year <= 2000]
trial.data.sum <- trial.data[,.(Y.max = round(max(Y),0),
                                #Y.min = min(Y),
                                N.initial = mean(N.initial),
                                Seasonpp = mean(Seasonpp),
                                Annualpp = mean(Annualpp),
                                Julpp = mean(Julpp),
                                Seasonpp.Z = ((mean(Seasonpp)-454)/153),
                                Julpp.Z = ((mean(Julpp)-105)/62)), by = .(starting.year, year)]

trial.data.sum[,P.S.z := pnorm(-abs(Seasonpp.Z))]
trial.data.sum[,P.J.z := pnorm(-abs(Julpp.Z))]
trial.data.sum[,P.C.z := P.S.z * P.J.z]

trial.data.sum[,Y.max.r := Y.max/mrtn.data[,.(Y = mean(Y))]$Y][]#relative to Historic mean using MRTN
#trial.data.sum[,Y.max.min.diff := Y.max - Y.min][]
setcolorder(trial.data.sum, c('starting.year', 'year', 'Y.max', 'Y.max.r','N.initial','Annualpp', 'Seasonpp', 'Julpp', 'Seasonpp.Z', 'Julpp.Z'))

#first.N.rec <- wendte.complete.dt[stg.id == 3 & year == L+1, .(N.total.ss.first = round(mean(N.total.ss),0)), by = c('starting.year', 'L') ]
trial.data.sum2 <- merge(trial.data.sum, profits.summary.set4[stg.id ==3 ,.(starting.year, year=L, Y.N.prev.overlap)], by = c('starting.year', 'year'))
#trial.data.sum3 <-merge(trial.data.sum2, first.N.rec, by.x = c('starting.year', 'year'), by.y = c('starting.year', 'L'))
cols <- setdiff(names(trial.data.sum2), c('starting.year', 'year'))
trial.data.sum2[,(cols) := round(.SD,2), .SDcols=cols][]
trial.data.sum2[, year := as.character(year)]
trial.data.sum2[starting.year %in% c(1980,1985) & year ==1, year := '*1']
trial.data.sum2[starting.year %in% c(1990,1995,2000) & year ==2, year := '*2']

table4 <- trial.data.sum2[,-c('Annualpp','Seasonpp.Z', 'Julpp.Z')] %>% data.frame()

tt <- ttheme_default(colhead=list(fg_params = list(parse = TRUE))) #fontface=2, 

cols3=c(starting.year= 'z',
        year= 'year',
        Y.max = expression(paste('Y'^'max')),
        Y.max.r = expression(paste('Y'^'max.r')),
        N.initial = expression(paste('N'^'Apr')),
        #Annualpp=expression(paste('pp'^'A')),
        Seasonpp=expression(paste('pp'^'S')),
        Julpp=expression(paste('pp'^'J')),
        # Seasonpp.Z=expression(paste('Z'^'S')),
        # Julpp.Z=expression(paste('Z'^'J')),
        P.S.z=expression(paste('PZ'^'S')),
        P.J.z=expression(paste('PZ'^'J')),
        P.C.z = 'CP',
        Y.N.prev.overlap=expression(paste('Y/N'^'overlap')))

colnames(table4) <- cols3



table4.tg <- tableGrob(table4, rows = NULL,
                       theme=tt)
grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/table4.tiff', units="in", width=6.5, height=3.5, res=300, compression = 'lzw')
grid.draw(table4.tg)
dev.off()
#--------------------------
#PPT 1: FIELD CREATION
#--------------------------


checkb.app.map.whole.sf <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/checkb.app.map.whole.sf.RDS')
plot.one <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/plot.one.RDS')
wendte.C.sf <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/wendte.C.sf.rds')
nrow(plot.one)

# install.packages('shinyjs')
# library('shinyjs')
# tmaptools::palette_explorer()

tm_shape(wendte.C.sf) + tm_polygons(c('OM', 'elev', 'N.initial', 'Depth'), legend.show = TRUE) +
  tm_layout(legend.text.size = 0.7,
            main.title = paste('Initial conditions of the Field'),
            main.title.position = "center",
            main.title.size = 1.8)

wendte.C.sf$id.X <- as.numeric(wendte.C.sf$id.X)
wendte.C.sf$id.X <- c(rep('-', 3), 32, rep('-', 24), 1, rep('-', 3))
(id.x <- tm_shape(wendte.C.sf) + tm_polygons('elev', legend.show = FALSE) + tm_text('id.X', size = 0.5)+ #tm_polygons('id.X', legend.show = FALSE, palette = 'Pastel1' )
    tm_legend(
      main.title = paste('Squares (x=1,...,32)'),
      main.title.position = "left"))

checkb.app.map.whole.sf$id.plot <- c(1, rep('-', 108), 110, '-')
(id.plot <- tm_shape(checkb.app.map.whole.sf) +  tm_polygons('block', legend.show = FALSE) + tm_text('id.plot', size = 0.5)+ #tm_polygons('block', legend.show = FALSE)
    tm_legend(
      main.title = paste('Plots (i = 1,...,110)'),
      main.title.position = "left"))

#plot.one$id.all.random <- as.character(sample(1:length(plot.one$id.all), length(plot.one$id.all)))
plot.one$id.all[!plot.one$id.all %in% c(17,237)] <- '-'
plot.one$id.all[plot.one$id.all %in% c(17,237)] <- c(1,256)

(id.all <- tm_shape(plot.one) + tm_polygons(col = 'block', legend.show = FALSE) + tm_text('id.all', size = 0.5) + #tm_polygons(col = 'id.all.random', legend.show = FALSE)
    tm_legend(
      main.title = paste('Cells (j=1,...,256)'),
      main.title.position = "left")) 

(trial <- tm_shape(plot.one) + tm_polygons(col = 'N') + #tm_text('id.all', size = 0.5) +
    tm_legend(legend.position = c('right', 'top'),
              legend.height = 0.16,
              main.title = paste('Trial - Treat (by plot)'),
              main.title.position = "left")) 



tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/ppt.1.tiff', units="in", width=12, height=5, res=300, compression = 'lzw')
tmap_arrange(id.x, id.plot, id.all, trial,nrow = 1)
dev.off()

#--------------------------
#PPT 2: PREDICTIONS
#--------------------------
#HIGH INFO PROFITS GRAPH 1990 L = 5 - Predictions for year 6
data.n.acum.sum <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/data.n.acum.sum.equallyw.1990L5.RDS')
data.n.acum.sum.pp <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/data.n.acum.sum.ppw.1990L5.RDS')
data.n.all <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/data.n.all.1990L5.RDS')
combined.dt <- readRDS('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/combined.dt.1990L5.RDS')
#saveRDS(combined.dt, './Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Bigfiles/combined.dt.1990L5.RDS')


area.id20.conv <- 10000/data.n.all[id.all == 20, ][1,area]
data.n.acum.sum[,p.hi := p.hi.gross*area.id20.conv]
data.n.acum.sum.pp[,p.hi := p.hi.gross*area.id20.conv]
data.n.all[,p.hi := p.hi.gross/area*10000]
combined.dt2 <- merge(combined.dt, data.n.all[N==255 & id.all == 20,.(year,p.hi, N)], by = 'year')
combined.dt2[,P := round(P*100,1)]
data.n.all[,year := factor(year)]

ppt.2 <- ggplot2::ggplot() +
  geom_line(data = data.n.all[id.all == 20], aes(x = N, y = p.hi, colour = year), size = 1) + #model fit for the each year dummy
  geom_line(data = data.n.acum.sum[id.all == 20], aes(x = N, y = p.hi, linetype = 'Equally weighted'), size = 2)+ #prediction for year 5 weighting each year equally
  geom_line(data = data.n.acum.sum.pp[id.all == 20], aes(x = N, y = p.hi, linetype = 'Weather weighted'), size = 2)+  #prediction for year 5 weighting by weather P
   scale_linetype_discrete(name=element_blank())+
  annotate("text",x=combined.dt2$N,y=combined.dt2$p.hi-15,size=3,label=paste(combined.dt2$P, '%', sep = ''), color = "#000000") +
  ylab('P ($/ha)') +
  xlab('N (kg/ha)') +
  #ylim(0,max(data.n.all[id.all %in% 20,p.hi])+5)+
  theme_bw() +
  theme(legend.position='bottom',
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.text.align = 0,
        strip.text = element_blank())

tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/ppt.2.tiff', units="in", width = 6, height=4, res=300, compression = 'lzw')
ppt.2
dev.off()

#--------------------------
#PPT 3: CASE 1
#--------------------------
g1.case1=ggplotGrob(p1.case1)
g2.case1=ggplotGrob(p2.case1)
g3.case1=ggplotGrob(p3.case1)

case1.tg = rbind(g1.case1,g2.case1,g3.case1,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
#gtable_show_layout(g)
title.case1 <- textGrob("Case 1 (w = 2000, L = 2)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")



case1.tg <- gtable_add_rows(case1.tg, heights = grobHeight(title.case1) + padding, pos = 0)
#gtable_show_layout(table)
case1.tit.tg <- gtable_add_grob(case1.tg, title.case1, 1, 1, 1, ncol(case1.tg))
case1.tit.tg$heights[19] = 1.5*case1.tit.tg$heights[19]

grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/ppt3.case1.tiff', units="in", width=5.95, height=6.8, res=300, compression = 'lzw')
grid.draw(case1.tit.tg)
dev.off()

#--------------------------
#PPT 4: PUT TOGETHER CASE 2a AND 2b
#--------------------------
#First put a nice title
case2a.tg = rbind(g1.case2a,g2.case2a,g3.case2a,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
title.case2a <- textGrob("Case 2a (w = 1985, L = 1)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")
case2a.tg <- gtable_add_rows(case2a.tg, heights = grobHeight(title.case2a) + padding, pos = 0)
#gtable_show_layout(table)
case2a.tit.tg <- gtable_add_grob(case2a.tg, title.case2a, 1, 1, 1, ncol(case2a.tg))
case2a.tit.tg$heights[19] = 1.5*case2a.tit.tg$heights[19]

#First put a nice title
case2b.tg = rbind(g1.case2b,g2.case2b,g3.case2b,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
title.case2b <- textGrob("Case 2b (w = 2000, L = 1)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")
case2b.tg <- gtable_add_rows(case2b.tg, heights = grobHeight(title.case2b) + padding, pos = 0)
#gtable_show_layout(table)
case2b.tit.tg <- gtable_add_grob(case2b.tg, title.case2b, 1, 1, 1, ncol(case2b.tg))
case2b.tit.tg$heights[19] = 1.5*case2b.tit.tg$heights[19]

grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/ppt4.case2.tiff', units="in", width=9, height=6.8, res=300, compression = 'lzw')
grid.arrange(case2a.tit.tg, case2b.tit.tg, ncol = 3,    
             widths = c(0.5, 0.5, 0.15),
             layout_matrix = rbind(c(1, 2, 2),
                                   c(1, 2, 2)))
dev.off()


#--------------------------
#PPT 5: PUT TOGETHER CASE 3 and 4
#--------------------------
#First put a nice title
case3.tg = rbind(g1.case3,g2.case3,g3.case3,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
title.case3 <- textGrob("Case 3 (w = 1980, Stg.3)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")
case3.tg <- gtable_add_rows(case3.tg, heights = grobHeight(title.case3) + padding, pos = 0)
#gtable_show_layout(table)
case3.tit.tg <- gtable_add_grob(case3.tg, title.case3, 1, 1, 1, ncol(case3.tg))
case3.tit.tg$heights[19] = 1.5*case3.tit.tg$heights[19]

#First put a nice title
case4.tg = rbind(g1.case4,g2.case4,g3.case4,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
title.case4 <- textGrob("Case 4 (w = 2000, Stg.3)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")
case4.tg <- gtable_add_rows(case4.tg, heights = grobHeight(title.case4) + padding, pos = 0)
#gtable_show_layout(table)
case4.tit.tg <- gtable_add_grob(case4.tg, title.case4, 1, 1, 1, ncol(case4.tg))
case4.tit.tg$heights[19] = 1.5*case4.tit.tg$heights[19]


grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/ppt5.case3and4.tiff', units="in", width=9, height=6.8, res=300, compression = 'lzw')
grid.arrange(case3.tit.tg, case4.tit.tg, ncol = 3,    
             widths = c(0.5, 0.5, 0.15),
             layout_matrix = rbind(c(1, 2, 2),
                                   c(1, 2, 2)))
dev.off()

#--------------------------
#PPT 6: PUT TOGETHER CASE 5 and 6
#--------------------------
#First put a nice title
case5.tg = rbind(g1.case5,g2.case5,g3.case5,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
title.case5 <- textGrob("Case 5 (w = 1990, Stg.3)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")
case5.tg <- gtable_add_rows(case5.tg, heights = grobHeight(title.case5) + padding, pos = 0)
#gtable_show_layout(table)
case5.tit.tg <- gtable_add_grob(case5.tg, title.case5, 1, 1, 1, ncol(case5.tg))
case5.tit.tg$heights[19] = 1.5*case5.tit.tg$heights[19]

#First put a nice title
case6.tg = rbind(g1.case6,g2.case6,g3.case6,size="first")#https://stackoverflow.com/questions/28255101/aligning-and-arranging-charts-in-ggplot
title.case6 <- textGrob("Case 6 (w = 1995, Stg.3)",gp=gpar(fontsize=18))
padding <- unit(2,"mm")
case6.tg <- gtable_add_rows(case6.tg, heights = grobHeight(title.case6) + padding, pos = 0)
#gtable_show_layout(table)
case6.tit.tg <- gtable_add_grob(case6.tg, title.case6, 1, 1, 1, ncol(case6.tg))
case6.tit.tg$heights[19] = 1.5*case6.tit.tg$heights[19]


grid.newpage()
tiff('./Box Sync/DIFM just me/APSIM_R/APSIM_SimulatedData/Graphs/ppt6.case5and6.tiff', units="in", width=9, height=6.8, res=300, compression = 'lzw')
grid.arrange(case5.tit.tg, case6.tit.tg, ncol = 3,    
             widths = c(0.5, 0.5, 0.15),
             layout_matrix = rbind(c(1, 2, 2),
                                   c(1, 2, 2)))
dev.off()


