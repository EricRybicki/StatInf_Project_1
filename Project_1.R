### Statistical Inference Project 1
    lambda <- 0.2
         n <- 1000
    s.size <- 40
simulation <- matrix(rexp(n*s.size, rate=lambda), n, s.size)
 sample.means <- rowMeans(simulation)

# Set as data frame for use with ggplot2
df <- data.frame(sample.means)
library(ggplot2)
g <- ggplot(df, aes(sample.means)) 
g <- g + geom_histogram(binwidth = 0.13, fill = "white", colour = "firebrick", aes(y = ..density..)) 
g <- g + stat_function(geom="line", fun=dnorm, arg=list(mean=mean(sample.means), sd=sd(sample.means)), size = 1.25)
g <- g + geom_density(colour = "seagreen", size = 1.25) + theme_bw() + xlab("")
g <- g + geom_vline(xintercept = mean(sample.means), colour = "seagreen", size = 1.25)
g <- g + annotate("text", x = 7.25, y = 0.55, label = "—Sample     ", colour = " seagreen", size = 7)
g <- g + annotate("text", x = 7.25, y = 0.5, label = "—Theoretical", size = 7)
g

# Find the sample and theoretical standard deviations
sd(sample.means)
(1/lambda)/sqrt(s.size)
# Find the sample and theoretical variances
var(sample.means)
((1/lambda)^2)/s.size
    
### qqnorm in ggplot
#ggplot(df, aes(sample = sample.means)) + 
#        geom_point(stat="qq", colour = "seagreen", size = 6, shape = 1) + 
#        theme_bw() + geom_line(stat="qq", sample = sample.means)
    
### qqline in ggplot   
    gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                      labels = names(x)){
        q.function <- eval(parse(text = paste0("q", distribution)))
        d.function <- eval(parse(text = paste0("d", distribution)))
        x <- na.omit(x)
        ord <- order(x)
        n <- length(x)
        P <- ppoints(length(x))
        df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
        
        if(is.null(line.estimate)){
            Q.x <- quantile(df$ord.x, c(0.25, 0.75))
            Q.z <- q.function(c(0.25, 0.75), ...)
            b <- diff(Q.x)/diff(Q.z)
            coef <- c(Q.x[1] - b * Q.z[1], b)
        } else {
            coef <- coef(line.estimate(ord.x ~ z))
        }
        
        zz <- qnorm(1 - (1 - conf)/2)
        SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
        fit.value <- coef[1] + coef[2] * df$z
        df$upper <- fit.value + zz * SE
        df$lower <- fit.value - zz * SE
        
        if(!is.null(labels)){ 
            df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
        }
        
        p <- ggplot(df, aes(x=z, y=ord.x)) +
            geom_point(colour = "seagreen", size = 7, shape = 1) + theme_bw() +
            geom_abline(intercept = coef[1], slope = coef[2], colour = "firebrick", size = 2) +
            geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
            xlab("Theoretical") + ylab("Sample")
        if(!is.null(labels)) p <- p + geom_text( aes(label = label))
        print(p)
        coef
    }