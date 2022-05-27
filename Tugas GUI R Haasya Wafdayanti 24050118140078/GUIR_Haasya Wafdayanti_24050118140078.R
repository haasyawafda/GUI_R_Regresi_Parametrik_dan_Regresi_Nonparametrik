library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(openintro)
library(plotrix)
library(shinythemes)
library(openxlsx)
library(car)
library(stats)
library(lmtest)

#Source
#Fungsi Kernel 
Gaussian<-function(X)
{
  gauss <-(1/sqrt(2*pi))*exp((-1/2)*X^2)
  return(gauss)
}
Uniform<-function (X)
{
  n = length(X)
  unif = NULL
  for (i in 1:n) {
    if (abs(X[i]) <= 1) {
      unifi = 1/2
    }
    else {
      unifi = 0
    }
    unif = c(unif,unifi)
  }
  return(unif)
}
cvkernel<-function(X,Y,a,b,c,ker)
{
  #X : variabel prediktor
  #Y : variabel respon
  #a : nilai bandwidth terkecil yang dicobakan
  #b : nilai bandwidth terbesar yang dicobakan
  #c : penambahan bandwidth
  #h : barisan bandwidth yang dicobakan
  h <- seq(a, b, by = c)
  s <- length(h)
  CV=matrix(nrow=s,ncol=1)
  for(m in 1:s){
    g<-length(X)
    Ghat<-rep(0,g)
    n<-length(Y)
    for(j in 1:g){
      ghat<-0
      faktor<-0
      for (i in 1:n){
        faktor<-faktor+ker((X[j]-X[i])/h[m])
        ghat<-ghat+Y[i]*ker((X[j]-X[i])/h[m])
      }
      if((faktor-ker(0))==0)
        Ghat[j]<-0
      else Ghat[j]<-(ghat-ker(0)*Y[j])/(faktor-ker(0))
    }
    cv.h<-1/n*sum((Y-Ghat)^2)
    CV[m]=cv.h
  }
  R<-matrix(c(h,CV),nrow=s)
  sort.R<-R[order(R[,2]),]
  S<-sort.R[1:10,]
  cat('Berikut 10 nilai CV minimum dan bandwidthnya:\n')
  cat('===========================\n')
  cat(' No h CV \n')
  cat('===========================\n')
  print(S)
  cat('===========================\n')
  cat('nilai bandwidth optimal=',S[1,1],'
dengan nilai CV minimum=',S[1,2],'\n')
}

gcvkernel=function(X, Y, a, b, c,ker)
{
  #X : variabel prediktor
  #Y : variabel respon
  #a : nilai bandwidth terkecil yang dicobakan
  #b : nilai bandwidth terbesar yang dicobakan
  #c : penambahan bandwidth
  #h : barisan bandwidth yang dicobakan
  h = seq(a, b, by = c)
  s = length(h)
  GCV=matrix(nrow=s,ncol=1)
  MSE=matrix(nrow=s,ncol=1)
  n=length(X)
  e2=matrix(nrow=n,ncol=1)
  e1=matrix(nrow=n,ncol=1)
  hi=matrix(ncol=1)
  for(m in 1:s) {
    g <- length(X)
    Ghat <- rep(0, g)
    n <- length(Y)
    gcv.h=0
    mse=0
    gcv=0
    for(i in 1:g) {
      e22=0
      e11=0
      ghat <- 0
      faktor <- 0
      for(j in 1:n) {
        faktor <- faktor + ker((X[i] - X[j])/h[m])
        ghat <- ghat + Y[j] * ker((X[i] - X[j])/h[m])
      }
      if(faktor== 0)
        Ghat [i]<- 0
      else Ghat[i] <- (ghat/faktor)
      hi[i]=ker(0)/faktor
      hii=hi[i]^2
      e11=(Y[i]-Ghat[i])^2
      e1[i]=e11
      gcv.h=gcv.h+e11
    }
    mse=(sum(e1))/n
    GCV[m]=(n*gcv.h)/(n-sum(hi))^2
    MSE[m]=mse
  }
  R<-matrix(c(h,GCV,MSE),ncol=3)
  sort.R1<-R[order(R[,3]),]
  S<-sort.R1[1:10,]
  cat("\nh opt=",S[1,1],"dengan GCV=",S[1,2],"dan MSE minimal=",S[1,3],"\nBerikut 10
nilai GCV dan MSE terkecil beserta nilai bandwidh h:\n")
  cat("\n============================")
  cat("\n No h GCV MSE ")
  cat("\n============================\n")
  S
}


#untuk UI GUI#

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  
  #Menampilkan gambar
  HTML('<center><img src="picture9.png" width="1900"></center>'),
  
  #Judul&Nama NIM
  
  h1("ANALISIS REGRESI PARAMETRIK DAN NONPARAMETRIK", style= "font-family: 'Arial Black', cursive;
color:navy; text-align:left"),
  
  h4("Disusun Oleh : Haasya Wafdayanti (24050118140078)", style= "font-family: 'Arial', cursive;
color:navy; text-align:left"),
  
  h5("Diajukan Guna Memenuhi Ujian Akhir Semester Komputasi Statistika Lanjut", style= "font-family: 'Arial', cursive;
color:navy; text-align:left"),
  
  navbarPage("", 
             
             #TabPanel 1 : Regresi Parametrik dan Nonparametrik
             tabPanel("Analisis Regresi",
                      
                      h4("Analisis regresi merupakan analisis hubungan antara variabel variabel respon (Y) 
dengan prediktor (X). Pendekatan regresi dibedakan menjadi dua yaitu pendekatan secara 
parametrik dan pendekatan nonparametrik. Pendekatan parametrik merupakan pemodelan 
regresi yang terikat dengan asumsi-asumsi dalam regresi. Asumsi-asumsi tersebut antara 
lain multikolinieritas, residual normalitas, homokedastisitas residual, dan nonautokorelasi. 
Sedangkan pendekatan regresi nonparametrik tidak ada asumsi-asumsi yang harus 
dipenuhi dalam pemodelan. Regresi parametrik dilakukan apabila bentuk kurva regresinya 
diketahui. Sedangkan regresi nonparametrik dilakukan jika bentuk kurva regresinya tidak 
diketahui. Regresi semiparametrik digunakan jika sebagian bentuk kurva regresinya tidak 
diketahui sedangkan yang sebagian lainnya diketahui. Kurva regresi nonparametrik 
diasumsikan smooth (mulus/halus) yang termuat dalam suatu ruang fungsi tertentu 
misalnya ruang sobolev (Eubank et al, 2004).", style= "font-family: 'Arial', cursive;
     color:dark; text-align:left"),
                      
                      h4("Analisis regresi linier sederhana adalah analisis terhadap hubungan satu variabel tak bebas (Y) 
dengan satu variabel bebas (X). Estimasi parameter biasanya diselesaikan dengan metode kuadrat terkecil. 
Salah satu asumsi yang harus dipenuhi dalam metode kuadrat terkecil adalah kenormalan dari error, 
yaitu error berdistribusi normal dengan rata-rata nol dan simpangan baku konstan. 
Jika asumsi kenormalan error tidak dapat dipenuhi maka metode kuadrat terkecil tidak dapat 
digunakan untuk mengestimasi parameter-parameternya, karena akan menghasilkan kesimpulan yang bias. 
Untuk mengatasi penyimpangan asumsi kenormalan error dapat digunakan prosedur nonparametrik.Berikut ini
adalah model regresi linier sederhana:", style= "font-family: 'Arial', cursive;
     color:dark; text-align:left"), 
                      
                      HTML('<center><img src="model1.png" width="800"></center>'),
                      
                      h4("Regresi Kernel adalah teknik statistika nonparametrik untuk memperkirakan ekspektasi bersyarat dari 
   variabel acak. Tujuannya adalah untuk menemukan hubungan antara variabel acak X dan Y dengan menggunakan 
   bobot fungsi kernel. Beberapa contoh fungsi kernel (Hardle, 1991) antara lain adalah uniform, segitiga, 
   epanecnikov, dan gauss. Keefektifan fungsi bobot dari penghalus kernel ditentukan oleh kernel K dan 
   barisan bandwith h. Sehingga ketepatan estimasi kurva regresi bukan hanya tergantung pada bandwith saja 
   tetapi bergantung juga dari pasangan (K,h). Pemilihan bandwith yang optimum dilakukan dengan 3 metode 
   yaitu metode MSE, CV, dan GCV.")
                      
             ),
             
             #Input Data
             tabPanel("Input Data",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("dataku","Input data dalam (.txt) dengan desimal memakai (.)", accept = c(".txt"))),
                        mainPanel(
                          tabsetPanel(type ="pills",id = "navbar",
                                      tabPanel("Data",verbatimTextOutput("statdes"),tableOutput("tabeldataku")),
                                      tabPanel("Plot Data",plotOutput("scatterplot"))
                          )))),
             
             #Regresi Parametrik
             tabPanel("Regresi Parametrik",
                      sidebarLayout(
                        sidebarPanel(
                          h4('Analisis regresi linier sederhana haruslah memenuhi asumsi normalitas, homoskedastisitas, non autokorelasi ')),
                        mainPanel(
                          tabsetPanel(type="pills",id="navbar",
                                      tabPanel("Regresi Linier Sederhana",verbatimTextOutput("regresi")),
                                      tabPanel("Korelasi",verbatimTextOutput("korelasi")),
                                      tabPanel("Uji Normalitas",verbatimTextOutput("normalitas"),plotOutput("qqplot")),
                                      tabPanel("Uji Homoskedastisitas",verbatimTextOutput("homoskedastisitas")),
                                      tabPanel("Uji Non Autokorelasi",verbatimTextOutput("nonautokorelasi"))
                          )))),
             
             #Regresi Nonparametrik
             tabPanel("Regresi Nonparametrik",
                      sidebarLayout(
                        sidebarPanel(
                          h4('Pencarian Bandwidth Optimal'),
                          selectInput("kernel","Pilih Kernel",choices = c('Gaussian','Uniform')),
                          textInput("band","Bandwidth Minimum"),
                          textInput("width","Bandwidth Maksimum"),
                          textInput("space","Penambahan Bandwidth yang Dicobakan"),
                          actionButton("hitung",'Run',class="btn-success")),
                        mainPanel(
                          tabsetPanel(type ="pills",id = "navbar",
                                      tabPanel("Bandwidth Optimal dengan CV",verbatimTextOutput('h_opt')),
                                      tabPanel("Bandwidth Optimal dengan GCV",verbatimTextOutput('h_opt2'))
                          ))))))

#Membuat Server
server<-function(input,output) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  output$tabeldataku<-renderTable({
    data<-input$dataku
    if(is.null(data)){return()}
    read.table(data$datapath, sep = '\t', header = T)})
  output$statdes<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.table(data$datapath, sep = '\t', header = T )
    X<-file[,1]
    Y<-file[,2]
    des_x=summary(X)
    des_y=summary(Y)
    cat("============================\n")
    cat("Statistika Deskriptif x:\n")
    print(des_x)
    cat("============================\n")
    cat("Statistika Deskriptif y:\n")
    print(des_y)
    cat("============================\n")})
  
  output$scatterplot<-renderPlot({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.table(data$datapath, sep = '\t', header = T )
    X<-file[,1]
    Y<-file[,2]
    plot(X,Y,main="scatterplot",col="red")})
  
  output$regresi=renderPrint({
    data=input$dataku
    if(is.null(data)){return()}
    file=read.table(data$datapath,sep= '\t', header = T)
    x=file[,1]
    y=file[,2]
    reg=lm(y~x)
    cat("===========================\n")
    cat(" Regresi Linier Sederhana \n")
    cat("===========================\n")
    print(summary(reg))
    cat("===========================\n")})
  
  output$korelasi=renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file=read.table(data$datapath,sep= '\t', header = T)
    x=file[,1]
    y=file[,2]
    kor=cor.test(x,y)
    cat("===========================\n")
    cat(" Korelasi Pearson \n")
    cat("===========================\n")
    print(kor)
    cat("===========================\n")})
  
  output$normalitas=renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file=read.table(data$datapath,sep= '\t', header = T)
    x=file[,1]
    y=file[,2]
    reg=lm(y~x)
    normal<-function(x,y) #Sumber : Modul Kompstat Pertemuan 14
    {
      n = length(y)
      p = ncol(x)
      bo= rep(1,n)
      X = cbind(bo,x)
      X = as.matrix(x)
      beta=solve(t(x)%*%x)%*%t(x)%*%y
      ytopi = x%*%beta
      error = y-ytopi
      ks.test(error,"pnorm",mean(error),sd(error))
    }
    normalis=normal(x,y)
    cat("===========================\n")
    cat(" Uji Normalitas \n")
    cat("===========================\n")
    print(normalis)
    cat("===========================\n")})
  
  output$qqplot <- renderPlot({
    data<-input$dataku
    if(is.null(data)){return()}
    file=read.table(data$datapath,sep= '\t', header = T)
    x=file[,1]
    y=file[,2]
    reg=lm(y~x)
    reg <- lm(y ~ x)
    qqPlot(summary(reg)$residuals, dist="norm", main="Normal QQ Plot of Residual")
  })
  
  output$homoskedastisitas<- renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file=read.table(data$datapath,sep= '\t', header = T)
    x=file[,1]
    y=file[,2]
    reg=lm(y~x)
    homoskes=bptest(reg)
    cat("===========================\n")
    cat(" Uji Homoskedastisitas \n")
    cat("===========================\n")
    print(homoskes)
    cat("===========================\n")})

  output$nonautokorelasi<- renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file=read.table(data$datapath,sep= '\t', header = T)
    x=file[,1]
    y=file[,2]
    reg=lm(y~x)
    nonauto=dwtest(reg)
    cat("===========================\n")
    cat(" Uji Nonautokorelasi \n")
    cat("===========================\n")
    print(nonauto)
    cat("===========================\n")})
 
   observeEvent(input$hitung,{
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.table(data$datapath, sep = '\t', header = T )
    X<-file[,1]
    Y<-file[,2]
    band<-as.numeric(input$band)
    width<-as.numeric(input$width)
    space<-as.numeric(input$space)
    if(input$kernel=='Gaussian')
    {
      output$h_opt<-renderPrint({cvkernel(X,Y,band,width,space,Gaussian)})
    }
    if(input$kernel=='Uniform')
    {
      output$h_opt<-renderPrint({cvkernel(X,Y,band,width,space,Uniform)})
    }
    if(input$kernel=='Gaussian')
    {
      output$h_opt2<-renderPrint({gcvkernel(X,Y,band,width,space,Gaussian)})
    }
    if(input$kernel=='Uniform')
    {
      output$h_opt2<-renderPrint({gcvkernel(X,Y,band,width,space,Uniform)})
    }})
}

#Running
shinyApp(ui = ui, server = server)


