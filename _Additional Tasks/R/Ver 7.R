sol7.1=function(q=100,n=100,p=.5){
    m=n*p; s=m*(1-p)
    par(mfrow=c(1,2),oma=c(0,0,2,0))
    for(i in 1:2){
        X=rbinom(q,size=n,prob=p)
        X=(X-m)/sqrt(s)
        hist(X, prob=T, col="gold", main=paste("Simulation:",i))
        lines(density(X, adjust=2), col="purple2", lwd=2)
    }
    title("Binomial Simulation Comparison", outer=T)
}

# В двата случая, множеството X ще се запълни с различни числа,
# следователно и тяхната гъстота/плъстнос на графиките би трябвало да е различна,
# размера и формата трябва да са подобни, но...

sol7.2=function(n=10){
    m=c(10,100); s=c(10,100)
    par(mfrow=c(1,2),oma=c(0,0,2,0))
    for(i in 1:2){
        X=rnorm(n,mean=m[i], sd=s[i])
        qqnorm(X,col="darkgreen", pch=15, main=paste("Simulation:",i), xlab="")
	qqline(X,col="purple")
    }
    title("Normal Simulation Comparison", outer=T)
}

# Извадката е n=10 и е твърде малка и освен това е нормално разпределение 
# за това ще използваме qqnorm

sol7.3=function(n=100){
    p=c(.25,.05,.01)
    f=function(n=100,p=.5){
        X=rbinom(1,n,p)
        (X-n*p)/sqrt(n*p*(1-p))
    }
    par(mfrow=c(1,3), oma=c(0,0,2,0))
    for(i in 1:3){
        S=simple.sim(1000,f,n,p[i])
        hist(S,prob=T,col="gold",main=paste("Simulation p =",p[i]))
        lines(density(S,adjust=2),col="purple2",lwd=1.5)
    }
    title("Bernulli Simulations: p=.25, .05, .01", outer=T)
}


sol7.4=function(n=100,m=0,s=1){
    X=rnorm(n, mean=m, sd=s)
    cat("Thumb Rule Normal Dist. Check\n")
    for(i in 1:3){
        ans=sum(-i*s+m<X & X<m+s*i)/length(X)*100
        print(paste('Data in',i,"standart deviation",ans,"%"))
    }
}


sol7.5function(n=100,l=0,r=1){
    nums=c(1,5,10,25)
    mains=paste(rep("n =",4),nums)
    f=function(n,a=l,b=r){
        m=bitwShiftR(a+b,1)
        s=(r-l)/sqrt(12)
        (mean(runif(n,a,b))-m)/(s/sqrt(n))
    }
    par(mfrow=c(2,2), oma=c(0,0,2,0))
    for(i in 1:4){
        X=simple.sim(n,f,nums[i])
        hist(X, prob=T, col="gold", main=mains[i], xlab="")
        lines(density(X, adjust=2), col="purple2", lwd=2)
    }
    title("Uniform Dist. Simulations for n=1,5,10 and 25", outer=T)
}


sol7.6function(){
    for (n in 1:50) {
        results = c()
        mu = 10;sigma = mu
        for (i in 1:200) {
            X = rexp(200,1/mu)
            results[i] = (mean(X)-mu)/(sigma/sqrt(n))
        }
        hist(results, 
             prob=T, 
             col=gray(.9),
             xlim=c(-2, 2),
             ylim=c(0, 4),
             main=paste("7.6 - exp distribution: n=", as.character(n), sep=""))
        lines(density(results, adjust=2), col="blue")
        Sys.sleep(1)
    }
}


sol7.7=function(n=100){
    dfs=c(4,50,4,50)
    par(mfrow=c(2,2))
    for(i in 1:4){
        if(i<3) X=rt(n,dfs[i])
        else X=rchisq(n,dfs[i])
        qqnorm(X,col="darkblue",pch=8,main="",xlab="",ylab=""); qqline(X, col='red4', lwd=2)
    }
    title("Normal plots?",outer=T)
}


sol7.8=function(){
    data(faithful)
    bootstrap=function(data,n=length(data)){
        boot.sample=sample(data,n,replace=T)
        median(boot.sample)
    }
    S=simple.sim(100,bootstrap,faithful$eruptions)
    par(mfrow=c(1,2),oma=c(0,0,2,0))
    
    qqnorm(S)
    qqline(S,col="purple2", lwd=2)
    
    hist(S,prob=T,col="gold")
    lines(density(S),col="purple2",lwd=2)
    
    title("Bootstrap Technique: Sample median from eruptions data", outer=T)
}

