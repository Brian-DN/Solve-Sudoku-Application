library(shiny)

check <- function(x){
    y<-vector()
    for(i in c(1:9)){
        if(sum(x[i,]==0)==1){y<-c(y,TRUE)}else{y}
    }
    for(i in c(1:9)){
        if(sum(x[,i]==0)==1){y<-c(y,TRUE)}else{y}
    }
    for(i in c(0:2)){
        for(j in c(0:2)){
            if(sum(x[i*3+c(1:3),j*3+c(1:3)]==0)==1){y<-c(y,TRUE)}else{y}
        }
    }
    if(sum(y)>0){FALSE}else{TRUE}
}

sdk1 <- function(x){
    x0<-0
    while(!identical(x0,x)){
        x0<-x
        # Theo o
        for(i in c(0:2)){
            for(j in c(0:2)){
                for(o in c(1:9)){
                    y<-vector(mode="numeric")
                    for(m in c(1:3)){
                        for(n in c(1:3)){
                            ifelse(x[i*3+m,j*3+n]==0&sum(x[i*3+m,]==o)==0&sum(x[,j*3+n]==o)==0&sum(x[i*3+c(1:3),j*3+c(1:3)]==o)==0,y<-c(y,m*10+n),y)
                        }
                    }
                    ifelse(length(y)==1,x[i*3+y[1]%/%10,j*3+y[1]%%10]<-o,y)
                }
            }
        }
        # Theo hang
        for(i in c(1:9)){
            for(o in c(1:9)){
                y<-vector(mode="numeric")
                for(j in c(1:9)){
                    ifelse(x[i,j]==0&sum(x[i,]==o)==0&sum(x[,j]==o)==0&sum(x[((i-1)%/%3)*3+c(1:3),((j-1)%/%3)*3+c(1:3)]==o)==0,y<-c(y,i*10+j),y)
                }
                ifelse(length(y)==1,x[y[1]%/%10,y[1]%%10]<-o,y)
            }
        }
        # Theo cot
        for(i in c(1:9)){
            for(o in c(1:9)){
                y<-vector(mode="numeric")
                for(j in c(1:9)){
                    ifelse(x[j,i]==0&sum(x[j,]==o)==0&sum(x[,i]==o)==0&sum(x[((j-1)%/%3)*3+c(1:3),((i-1)%/%3)*3+c(1:3)]==o)==0,y<-c(y,j*10+i),y)
                }
                ifelse(length(y)==1,x[y[1]%/%10,y[1]%%10]<-o,y)
            }
        }
        # Loai tru
        for(i in c(1:9)){
            for (j in c(1:9)){
                y<-vector(mode="numeric")
                for(o in c(1:9)){
                    ifelse(x[i,j]==0&sum(x[i,]==o)==0&sum(x[,j]==o)==0&sum(x[((i-1)%/%3)*3+c(1:3),((j-1)%/%3)*3+c(1:3)]==o)==0,y<-c(y,o),y)
                }
                ifelse(length(y)==1,x[i,j]<-y[1],y)
            }
        }
    }
    x
}

sdk2 <- function(x){
    x0<-0
    while(!identical(x0,x)){
        x0<-x
        # Theo o
        for(i in c(0:2)){
            for(j in c(0:2)){
                for(o in c(1:9)){
                    y<-vector(mode="numeric")
                    for(m in c(1:3)){
                        for(n in c(1:3)){
                            ifelse(x[i*3+m,j*3+n]==0&sum(x[i*3+m,]==o)==0&sum(x[,j*3+n]==o)==0&sum(x[i*3+c(1:3),j*3+c(1:3)]==o)==0,y<-c(y,m*10+n),y)
                        }
                    }
                    if(length(y)==2){
                        x1<-x
                        x2<-x
                        x1[i*3+y[1]%/%10,j*3+y[1]%%10]<-o
                        x2[i*3+y[2]%/%10,j*3+y[2]%%10]<-o
                        if(check(sdk1(x1))){
                            if(check(sdk1(x2))){x} else {x<-x1}
                        } else {if(check(sdk1(x2))){x<-x2} else {x}}
                    } else {y}
                }
            }
        }
        # Theo hang
        for(i in c(1:9)){
            for(o in c(1:9)){
                y<-vector(mode="numeric")
                for(j in c(1:9)){
                    ifelse(x[i,j]==0&sum(x[i,]==o)==0&sum(x[,j]==o)==0&sum(x[((i-1)%/%3)*3+c(1:3),((j-1)%/%3)*3+c(1:3)]==o)==0,y<-c(y,i*10+j),y)
                }
                if(length(y)==2){
                    x1<-x
                    x2<-x
                    x1[y[1]%/%10,y[1]%%10]<-o
                    x2[y[2]%/%10,y[2]%%10]<-o
                    if(check(sdk1(x1))){
                        if(check(sdk1(x2))){x} else {x<-x1}
                    } else {if(check(sdk1(x2))){x<-x2} else {x}}
                } else {y}
            }
        }
        # Theo cot
        for(i in c(1:9)){
            for(o in c(1:9)){
                y<-vector(mode="numeric")
                for(j in c(1:9)){
                    ifelse(x[j,i]==0&sum(x[j,]==o)==0&sum(x[,i]==o)==0&sum(x[((j-1)%/%3)*3+c(1:3),((i-1)%/%3)*3+c(1:3)]==o)==0,y<-c(y,j*10+i),y)
                }
                if(length(y)==2){
                    x1<-x
                    x2<-x
                    x1[y[1]%/%10,y[1]%%10]<-o
                    x2[y[2]%/%10,y[2]%%10]<-o
                    if(check(sdk1(x1))){
                        if(check(sdk1(x2))){x} else {x<-x1}
                    } else {if(check(sdk1(x2))){x<-x2} else {x}}
                } else {y}
            }
        }
        # Loai tru
        for(i in c(1:9)){
            for (j in c(1:9)){
                y<-vector(mode="numeric")
                for(o in c(1:9)){
                    ifelse(x[i,j]==0&sum(x[i,]==o)==0&sum(x[,j]==o)==0&sum(x[((i-1)%/%3)*3+c(1:3),((j-1)%/%3)*3+c(1:3)]==o)==0,y<-c(y,o),y)
                }
                if(length(y)==2){
                    x1<-x
                    x2<-x
                    x1[i,j]<-y[1]
                    x2[i,j]<-y[2]
                    if(check(sdk1(x1))){
                        if(check(sdk1(x2))){x} else {x<-x1}
                    } else {if(check(sdk1(x2))){x<-x2} else {x}}
                } else {y}
            }
        }
    }
    x
}

checkout <- function(x){
    y<-vector()
    for(i in c(1:9)){
        if(sum(x[i,])!=45){y<-c(y,TRUE)}else{y}
    }
    for(i in c(1:9)){
        if(sum(x[,i])!=45){y<-c(y,TRUE)}else{y}
    }
    for(i in c(0:2)){
        for(j in c(0:2)){
            if(sum(x[i*3+c(1:3),j*3+c(1:3)])!=45){y<-c(y,TRUE)}else{y}
        }
    }
    if(sum(y)>0){FALSE}else{TRUE}
}

solveSudoku <- function(x){
    y <- 0
    x <- sdk1(x)
    while(sum(x==0)>0&y<5){
        x <- sdk2(x)
        x <- sdk1(x)
        y <- y + 1
    }
    if(checkout(x)){x}else{"Can't solve! Please check and fill in your sudoku again."}
}

shinyServer(function(input, output) {
    
    currentResult <- reactive({solveSudoku(input$df)})
    output$solve <- renderTable(currentResult(), colnames = FALSE, digits = 0)
    
})