#install libraries

install.packages("lpSolveAPI")
install.packages("ggplot2")
install.packages("reshape")
install.packages("gridExtra")


library(lpSolveAPI)

#used for result visualization
library(ggplot2)
library(reshape)
library(gridExtra)

#define the datasets
 
transportTYPE<-data.frame(mode=c('KQ','Fly540','Vehicle'), weightcapacity=c(24,8,5), spacecapacity=c(10000,4000,8000))

newspaperTYPE<-data.frame(type=c('DailyNation','EastAfrican','BusinessDaily','TaifaLeo'), available=c(18,10,5,10), volume=c(400,300,100,200),profit=c(2000,2500,2250,1000))

transportTYPE

newspaperTYPE


#create an LP model with 10 constraints and 12 decision variables

lpmodel<-make.lp(2*NROW(transportTYPE)+NROW(newspaperTYPE),12)

#I used this to keep count within the loops, I admit that this could be done a lot neater
column<-0
row<-0

#build the model column per column
for(wg in transportTYPE$mode){
	row<-row+1
	for(type in seq(1,NROW(newspaperTYPE$type))){
	column<-column+1
	
	#this takes the arguments 'column','values' & 'indices' (as in where these values should be placed in the column)
	set.column(lpmodel,column,c(1, newspaperTYPE[type,'volume'],1), indices=c(row,NROW(transportTYPE)+row, NROW(transportTYPE)*2+type))
	}}

#set rhs weight constraints
set.constr.value(lpmodel, rhs=transportTYPE$weightcapacity, constraints=seq(1,NROW(transportTYPE)))

#set rhs volume constraints
set.constr.value(lpmodel, rhs=transportTYPE$spacecapacity, constraints=seq(NROW(transportTYPE)+1,NROW(transportTYPE)*2))


#set rhs volume constraints
set.constr.value(lpmodel, rhs=newspaperTYPE$available, constraints=seq(NROW(transportTYPE)*2+1,NROW(transportTYPE)*2+NROW(newspaperTYPE)))

#set objective coefficients
set.objfn(lpmodel, rep(newspaperTYPE$profit,NROW(transportTYPE)))

#set objective direction
lp.control(lpmodel,sense='max')

#I in order to be able to visually check the model, I find it useful to write the model to a text file
write.lp(lpmodel,'model.lp',type='lp')


#solve the model, if this return 0 an optimal solution is found
solve(lpmodel)

#this return the proposed solution
get.objective(lpmodel)




#plots

results<-data.frame(newspaperTYPE=rep(newspaperTYPE$type, 3), mode=as.vector(sapply(transportTYPE$mode, FUN=function(x) rep(x, NROW(newspaperTYPE)))), solution=get.variables(lpmodel))

results

r1<-ggplot(results, aes(x=newspaperTYPE, y=solution, fill=mode, ymax=11)) + geom_bar(color='black', position='dodge', stat='identity') + geom_text(aes(label=solution), size=2.5, position=position_dodge(width=1), vjust=-.4) + scale_fill_brewer(palette='Set1') + facet_grid(.~mode) + labs(title='Planning Result', legend.position='none') + ylab('Solution (tonnes)') + xlab('Newspaper Type')

r1

financialresult<-data.frame(newspaperTYPE=rep(newspaperTYPE$type, 3), mode=as.vector(sapply(transportTYPE$mode, FUN=function(x) rep(x, NROW(newspaperTYPE)))), solution=get.variables(lpmodel), profit_unit=rep(newspaperTYPE$profit, 3))
financialresult$profit<-financialresult$profit_unit*financialresult$solution

financialresult

r2<-ggplot(financialresult, aes(x=newspaperTYPE, y=profit, fill=mode, ymax=30000)) + geom_bar(color='black', position='dodge',stat='identity') + geom_text(aes(label=profit), size=2.5, position=position_dodge(width=1), vjust=-.4) + scale_fill_brewer(palette='Set1') + facet_grid(.~mode) + labs(title='Financial Result', legend.position='none') + ylab('Solution (KES)') + xlab('Newspaper Type')

r2

spacecapacity<-data.frame(mode=transportTYPE$mode, capacity=transportTYPE$spacecapacity, used=get.constraints(lpmodel)[4:6])
weightcapacity<-data.frame(mode=transportTYPE$mode, capacity=transportTYPE$weightcapacity, used=get.constraints(lpmodel)[1:3])

spacecapacity
weightcapacity

DailyNation<-ggplot(melt(spacecapacity, id='mode'), aes(x=variable, y=value, fill=variable)) + geom_bar(color='black', stat='identity') + facet_grid(.~mode) + labs(legend.position='none', title='Volume Capacity vs. Used') + scale_fill_brewer(palette='Set1') + geom_text(aes(label=value), size=2.5, vjust=-.4) + ylab('Volume (m2)') + xlab('Daily Nation Space Capacity')

DailyNation

EastAfrican<-ggplot(melt(weightcapacity, id='mode'), aes(x=variable, y=value, fill=variable)) + geom_bar(color='black', stat='identity') + facet_grid(.~mode) + labs(legend.position='none', title='Weight Capacity vs. Used') + scale_fill_brewer(palette='Set1') + geom_text(aes(label=value), size=2.5, vjust=-.4) + ylab('Weight (tonnes)') + xlab('East African Weight Capacity')

EastAfrican

png('model_results.png', width=8, height=8, units='in', res=90)
grid.arrange(r1, r2, DailyNation, EastAfrican, ncol=2)
