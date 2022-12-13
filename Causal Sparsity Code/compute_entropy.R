library(tidyverse)

rm(list=ls())

#Should be run before strategy analysis
load('./switches_unprocessed.rdata',verbose = T)

computed_earlier<-df.l


for (i in 1:nrow(df.l))
{
  switch_state = c(df.l$s1[i], df.l$s2[i], df.l$s3[i], df.l$s4[i], df.l$s5[i], df.l$s6[i])
  
  if (df.l$test[i]==1)
  {
    #On the first test its uniform
    prior<-rep(1/6, 6)
    prior_entropy<--6*(1/6*log2(1/6))
  } else {
    #Subsequently the prior is the previous test's posterior
    prior<-c(df.l$p1[i-1], df.l$p2[i-1], df.l$p3[i-1], df.l$p4[i-1], df.l$p5[i-1], df.l$p6[i-1])
    prior_entropy<-df.l$entropy[i-1]
  }
  
  if (df.l$condition.f[i]=='Sparse')
  {
    post.un<-post_alt.un<-prior
    post.un[switch_state!=df.l$outcome[i]]<-0#Unnormalised probability of each switch after the outcome (positive or negative outcome)
    post_alt.un[switch_state==df.l$outcome[i]]<-0#The probability of the alternative thing happening
    if (df.l$outcome[i]==1)
    {
      marg<-sum(prior[switch_state==1])#The likelihood of the actual outcome  
    } else {
      marg<-sum(prior[switch_state==0])#The likelihood of the actual outcome
    }

    
  } else if (df.l$condition.f[i]=='Dense')
  {
    post.un<-post_alt.un<-prior
    #If you switched exactly one
    if (sum(switch_state)==1)
    {
      #...then you got the outcome, 
      if (df.l$outcome[i]==1)
      {
        #....then you've ruled out that switch
        post.un[switch_state==1]<-0
        #...if you hadn't gotten the outcome, you'd have identified the true switch
        post_alt.un[switch_state==0]<-0
        #The chance of this happening is just the probability that this is the wrong switch in the prior
        marg<-sum(prior[switch_state!=1])
        
      } else if (df.l$outcome[i]==0)
      {
        #If you didn't get the outcome, then you've identified the true switch
        post.un[switch_state!=1]<-0
        #If you had gotten the outcome, you would just have ruled out that switch
        post_alt.un[switch_state==1]<-0
        #The chance of this happening is the chance that you selected teh correct switch (1/N remaining)
        marg<-sum(prior[switch_state=1])
      }
    } else {
      #Check outcome should always be 1
      # cat('\nDense: multiple switches pushed, outcome should be 1: ', df.l$outcome[i])
      marg = 1
    }
  }
  post=post.un/sum(post.un)
  post_alt<-post_alt.un/sum(post_alt.un)
  df.l$prior1[i]<-prior[1]
  df.l$prior2[i]<-prior[2]
  df.l$prior3[i]<-prior[3]
  df.l$prior4[i]<-prior[4]
  df.l$prior5[i]<-prior[5]
  df.l$prior6[i]<-prior[6]
  
  df.l$p1[i]<-post[1]
  df.l$p2[i]<-post[2]
  df.l$p3[i]<-post[3]
  df.l$p4[i]<-post[4]
  df.l$p5[i]<-post[5]
  df.l$p6[i]<-post[6]
  
  df.l$prior_entropy[i]<-prior_entropy
  df.l$entropy[i] = -sum(post * log2(post), na.rm=T)
  df.l$information[i] = prior_entropy - df.l$entropy[i]
  ent_alt<- -sum(post_alt * log2(post_alt), na.rm=T)

  df.l$eig[i]<-marg*df.l$information[i] + (1-marg)*(prior_entropy - ent_alt)
  
  n_left<-sum(prior!=0)
  if (df.l$condition.f[i]=='Sparse')
  {
    if (n_left%in%c(6,4,2))
    {
      df.l$eigmax[i]<-1
    } else if (n_left==5)
    {
      df.l$eigmax[i]<-(-log2(1/5) - -log2(1/2))*2/5 +  (-log2(1/5) - -log2(1/3))*3/5#.97
    } else if (n_left==3)
    {
      df.l$eigmax[i]<-(-log2(1/3) - 0)*1/3 +  (-log2(1/3) - -log2(1/2))*2/3#.91
    } else {
      df.l$eigmax[i]<-0
    }
    
  } else if (df.l$condition[i]=='Dense')
  {
    if (n_left>1)
    {
      df.l$eigmax[i]<- (-log2(1/n_left))/n_left +  (-log2(1/n_left) - -log2(1/(n_left-1)))*(n_left-1)/n_left
    } else {
      df.l$eigmax[i]<-0
    }
    
  }
  
  df.l$guess_p[i] = post[df.l$guess[i]]
  df.l$max_p[i]<-max(post)
  df.l$positive_outcome_p[i] = marg[1]
}

#How much do you stand to make with an optimal guess after this test
df.l$ev = (3.0 - (df.l$test * 0.5)) * apply(df.l %>% select(p1:p6), 1, max)

round(df.l$entropy - computed_earlier$entropy, 5)#Should all be zero (after a little csv rounding error)

df.s<-df.s %>% mutate(entropy = NA, information = NA, eig = NA, guess_p = NA, max_p = NA, positive_outcome_p = NA, ev = NA)
for (i in unique(df.l$id))
{
  df.s$entropy[df.s$id==i]<-df.l$entropy[df.l$id==i & df.l$last_test_corrected]
  df.s$information[df.s$id==i]<-df.l$information[df.l$id==i & df.l$last_test_corrected]
  df.s$eig[df.s$id==i]<-df.l$eig[df.l$id==i & df.l$last_test_corrected]
  df.s$guess_p[df.s$id==i]<-df.l$guess_p[df.l$id==i & df.l$last_test_corrected]
  df.s$max_p[df.s$id==i]<-df.l$max_p[df.l$id==i & df.l$last_test_corrected]
  df.s$positive_outcome_p[df.s$id==i]<-df.l$positive_outcome_p[df.l$id==i & df.l$last_test_corrected]
  df.s$ev[df.s$id==i]<-df.l$ev[df.l$id==i & df.l$last_test_corrected]
}


save(file='./switches.rdata', df.s, df.l)
