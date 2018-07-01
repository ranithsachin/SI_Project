#import data
survey_data = c(read.csv(file.path("D:","SI Project.csv")))

#convert imported data to framework
df = data.frame(survey_data)
fix(df)

#new columns with numbers
df["Frequency_of_usage"]= factor(df$Frequency.of.social.media.usage.while.at.work,levels =c('Never','Once a month','Once a Week','Daily'),labels = c(0,1,2,3))
df["Usage_Rate"]= factor(df$If.you.are.a.daily.user..how.often.do.you.access.social.media.sites.while.at.work.,levels =c('Not Applicable','Once a Day','2-4 times a day','more than 5 times','Within every hour'),labels = c(0,1,2,3,4))
df["Usage_time"]= factor(df$Usually..How.much.time.do.you.spend.on.social.media.sites.with.each.log.in,levels=c('Not Applicable','Less than a minute','1-5 minutes','6-10 minutes','more than 10 minutes'),labels = c(0,1,2,3,4))

#check the dataframe and change above new columns to numerical 
fix(df)

#convert to numaricals and score for Social media Usage (manually convert columns to numerical via Fix(df))
#df["Social_Media_Usage_sum"]= (as.numeric(unlist(df["Frequency_of_usage"]))+as.numeric(unlist(df["Usage_Rate"]))+as.numeric(unlist(df["Usage_time"]))
df["Social_Media_Usage_sum"]= (df["Frequency_of_usage"])+(df["Usage_Rate"])+(df["Usage_time"])
fix(df)

#histogram of social media usage
hist(as.numeric(unlist(df["Social_Media_Usage_sum"])))


#to understand the distribution
max(df["Social_Media_Usage_sum"])
min((df["Social_Media_Usage_sum"]))
table(df["Social_Media_Usage_sum"])



#define new column for Usage level "High" and "Low"
df = transform(df,Usage_level=ifelse(df$Social_Media_Usage_sum>6,"High","Low"))
fix(df)


#productivity score
df["productivity_sum"] = as.numeric(unlist(df$I.almost.always.perform.better.than.an.accepted.level)+as.numeric(unlist(df$I.often.manage.workload.during.office.hours))+as.numeric(unlist(df$I.often.Choose.to.take.on.additional.tasks.at.work))+as.numeric(unlist(df$The.Quality.of.my.work.is.Top.grade))+as.numeric(unlist(df$I.often.introduce.innovative.approaches.to.traditional.work.practices)))
fix(df)

new_df = subset(df,select = c("Usage_level","productivity_sum"))
fix(new_df)

#subsetting social media usage "High" to a new vector
df_High = new_df[new_df["Usage_level"]=="High",]
fix(df_High)
df_H = c(df_High[2])
#convert to numaric
df_H_N =unlist(df_H)

#subsetting social media usage "Low" to a new vector
df_Low = new_df[new_df["Usage_level"]=="Low",]
fix(df_Low)
df_L =c(df_Low[2])
#convert to numeric
df_L_N =unlist(df_L)

#calculate means of 2 vectors
mean(df_H_N)
mean(df_L_N)

#productivity mean_difference between social media usage "High" and "Low"
mean_dif_H_L =mean(df_L_N)- mean(df_H_N)
mean_dif_H_L

#assume null Hypothisis is True
adj_df_H_N = df_H_N+mean_dif_H_L
mean(adj_df_H_N)

boot_mean_diff =c()
H_mean = c()
L_mean =c()

for(i in 1:10000){
  
  sample_L =sample(df_L_N,39,replace = TRUE)
  sample_H =sample(adj_df_H_N,103,replace = TRUE)
  
  L_mean =c(L_mean,mean(sample_L))
  H_mean =c(H_mean,mean(sample_H))
  
  boot_mean_diff = (L_mean - H_mean)
}

#histogram of Mean difference of randomized samples
hist(boot_mean_diff)
mean(boot_mean_diff)
sd(boot_mean_diff)

#p-value calculation
length(boot_mean_diff[boot_mean_diff>1.051033])/1000

CI = c(0-2*sd(boot_mean_diff),0+2*sd(boot_mean_diff))
CI
