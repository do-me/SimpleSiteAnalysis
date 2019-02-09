##############################  
##   Simple Site Analysis   ##  IMPORTANT: TO EXECUTE 
##   by do-me 12.01.2019    ##  WITH SOURCE BUTTON
##############################   

if (!require(geojsonio)) {
  install.packages("geojsonio")
  library(geojsonio)
}
if (!require(raster)) {
  install.packages("raster")
  library(raster)
}
if (!require(tcltk)) {
  install.packages("tcltk")
  library(tcltk)
}

# define function for information window with user input 
getFactors <- function(){ 
  ## getFactors function Based on code by Barry Rowlingson found on following pages
  ## http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
  ## https://magesblog.com/post/2014-07-15-simple-user-interface-in-r-to-get-login/
  tt <- tktoplevel()
  tkwm.title(tt, "Simple Site Analysis ")
  Fac1 <- tclVar("Importance point file 1") 
  Fac2 <- tclVar("Importance point file 2")
  Fac3 <- tclVar("Path to C:/.../yourdistrictfile.json")
  Fac4 <- tclVar("Path to C:/.../yourpointfile1.json")
  Fac5 <- tclVar("Path to C:/.../yourpointfile2.json")
  Fac6 <- tclVar("Directory for plot saving C:/.../yourplot.png")
  
  entry.Fac1 <- tkentry(tt, width="20", textvariable=Fac1)
  entry.Fac2 <- tkentry(tt, width="20", textvariable=Fac2)
  entry.Fac3 <- tkentry(tt, width="40", textvariable=Fac3)
  entry.Fac4 <- tkentry(tt, width="40", textvariable=Fac4)
  entry.Fac5 <- tkentry(tt, width="40", textvariable=Fac5)
  entry.Fac6 <- tkentry(tt, width="40", textvariable=Fac6)
  
  tkgrid(tklabel(tt, text=
                   "   This is the Simple Site Analysis tool.
                 It works with one polygon file containing districts and two point files in .json format.   
                 The files must be in the same coordinate system.
                 
                 Please enter the paths to your files:   "
  ))
  tkgrid(entry.Fac3)
  tkgrid(entry.Fac4)
  tkgrid(entry.Fac5)
  
  tkgrid(tklabel(tt, text="
                 
                 Please enter a directory to save your plot in:"))
  tkgrid(entry.Fac6)
  
  tkgrid(tklabel(tt, text="
                 
                 Please enter your importance factors between 0 and 1 (i.e. 0.35)."))
  
  tkgrid(entry.Fac1)
  tkgrid(entry.Fac2)
  
  OnOK <- function()
  { 
    tkdestroy(tt) 
  }
  OK.but <-tkbutton(tt,text=" OK ", command=OnOK)
  tkbind(entry.Fac2, "<Return>", OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  
  invisible(c(Fac1=tclvalue(Fac1), Fac2=tclvalue(Fac2),Fac3=tclvalue(Fac3),Fac4=tclvalue(Fac4),Fac5=tclvalue(Fac5),Fac6=tclvalue(Fac6)))
}

# execute function and actually get user´s input
userFactors <- getFactors()

# the importance factors
f1<-as.numeric(userFactors[1])
f2<-as.numeric(userFactors[2])

# the file input
f3<-userFactors[3]
f4<-userFactors[4]
f5<-userFactors[5]
f6<-userFactors[6]

rm(userFactors) # removes input values from global environment

# define all input polygon and point files
Ortsteile <- geojson_read(f3, what = "sp")
Schulen <- geojson_read(f4, what = "sp")
Wohnheime <- geojson_read(f5, what = "sp")

plot(Ortsteile) # plot district borders (plot enginge doesn´t work in the end without this step)

###############################################################################

counter<-0
valla<-c() # list to store all valscore values to get max(valla)

# initiate loop only to get max(valla)
for(i in Ortsteile$ortsteil_bez) # for every "ortsteil_bez" means Auerberg, Bonn-Castell etc.
{
  counter<-counter+1
  
  s<-Schulen[Ortsteile[Ortsteile$ortsteil_bez == i, ],] # s includes only Schule points in polygon i (1.:Auerberg)
  NrSchul<-length(s) # length means the number of features, here points in polygon i 
  
  w<-Wohnheime[Ortsteile[Ortsteile$ortsteil_bez == i, ],] # same for Wohnheime
  NrWohn<-length(w)
  
  # calculate color shade (gray0-gray100) by user´s input factors
  # endscore = relationaler Zielerreichungswert zwischen den Ortsteilen, between 0 and 1
  valla<-c(valla,((NrSchul*f1+NrWohn*f2)/(area(Ortsteile)[counter])))
}            
################

counter<-0

# initiate second loop to plot one district after another
for(i in Ortsteile$ortsteil_bez) # for every "ortsteil_bez" means Auerberg, Bonn-Castell etc.
{
  counter<-counter+1 # add 1 to counter 
  
  s<-Schulen[Ortsteile[Ortsteile$ortsteil_bez == i, ],] # s includes only Schule points in polygon i (1.:Auerberg)
  NrSchul<-length(s) # length means the number of features, here points in polygon i 
  
  w<-Wohnheime[Ortsteile[Ortsteile$ortsteil_bez == i, ],] # same for Wohnheime
  NrWohn<-length(w)
  
  # valscore: absolute scorepoints for district i 
  valscore<-((NrSchul*f1+NrWohn*f2)/(area(Ortsteile)[counter]))
  
  # endscore: relational fulfilment degree in percent points
  endscore<-((valscore-min(valla))/(max(valla)-min(valla))) # if values between 0 and 1, easier formula applicable: endscore<-(valscore/max(valla))
  
  # calculate color shade (gray0-gray100) 
  m <-round(100-100*endscore)
  Farbton<-paste("gray",m,sep="")
  
  # plot map part and print district information to console
  plot(Ortsteile[Ortsteile$ortsteil_bez == i, ],col=Farbton, add=TRUE) # plot polygon i one after another
  print(paste(sep="",i,": E = ",round(endscore,4)*100,"%",", Z = ",round(valscore*1000000,2),"km^-2")) # print final scores for user
}

# add points to plot
plot(Schulen, pch=20, col="Red", add=TRUE)    # plot Schule and Wohnheim points
plot(Wohnheime, pch=20,col="Blue", add=TRUE)

# for quick saving
dev.print(png, f6, width=800, height=800)

# final user window with interpretation information
tt<-tktoplevel()
tkwm.title(tt, "Important Information")
tkgrid(tklabel(tt,text=
                 "The values seen in the lower black window (`console´) represent the following: 
               E = relative degree of fulfilment for particular district where 0% is worst and 100% is best district.
               Z = scorepoints per squarekilometer for particular district 
               
               The plot in the lower right window is based on E-values. 
               The higher the degree of fulfilment in relation to other districts the darker the district.
               `Schulen´ are represented by red dots, `Studentenwohnheime´ by blue dots."
))
OnOK <- function()
{
  tkdestroy(tt)
}

tkgrid(tklabel(tt,text="Plot saved to: "))
tkgrid(tklabel(tt,text=f6))

OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkfocus(tt)
