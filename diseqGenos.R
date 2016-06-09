p <- rbeta(100,0.5,0.5)
dat1 <- sim_reads(p, 100, 25, 4, 0.005)
dat2 <- simDiseq(p, rep(0.2,100),4,100,25,errorDenom = 200)
dat3 <- simDiseq(p, rep(0.5,100),4,100,25,errorDenom = 200)
denom=5*25*100

v1 <- c(sum(dat1$genos==0),
        sum(dat1$genos==1),
        sum(dat1$genos==2),
        sum(dat1$genos==3),
        sum(dat1$genos==4))

v2 <- c(sum(dat2$genos==0),
        sum(dat2$genos==1),
        sum(dat2$genos==2),
        sum(dat2$genos==3),
        sum(dat2$genos==4))

v3 <- c(sum(dat3$genos==0),
        sum(dat3$genos==1),
        sum(dat3$genos==2),
        sum(dat3$genos==3),
        sum(dat3$genos==4))

df <- data.frame(genotypes=rep(0:4,3), Diseq=c(rep("F=0",5),rep("F=0.2",5),rep("F=0.5",5)), num=c(v1/sum(v1),v2/sum(v2),v3/sum(v2)))

ggplot(df, aes(genotypes, num, color=Diseq, fill=Diseq)) + geom_bar(stat="identity", position=position_dodge()) + xlab("Genotypes") + ylab("Frequency") + ggtitle("Genotype frequencies") + theme_minimal(base_size = 16)
