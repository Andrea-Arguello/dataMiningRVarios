require("twitteR")
require("RCurl")
require("tm")
require("wordcloud")

consumer_key = "GVMQcbrWncgAfBAgQpWt6obQl"
consumer_secret = "mNWLnRPkpC8Gr30FTbkhiDHkzb6LmsRHuOWETHQXtwhMCXDHZT"
access_token = "626914825-A1w9dfIFAHaYjzzvakCnMgMcAkbR1989injyIjqC"
access_secret = "bKUwASnfQphuvlyVKNwY07zYJpZ4s8qmtBkyCzZVyWHBO"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#-----------------------------------------------------------------------------------------------
#
#
# Get Tweets
#
#-----------------------------------------------------------------------------------------------

tweets10 = searchTwitter("NoMásViolenciaSexual",n=5000,lang="es")
tweets9 = searchTwitter("NoMasFemicidio",n=5000,lang="es")
tweets8 = searchTwitter("ElMachismoMata",n=5000,lang="es")
tweets7 = searchTwitter("VivasNosQueremos",n=5000,lang="es")
tweets6 = searchTwitter("Guatemala",n=5000,lang="es")
tweets5 = searchTwitteR("NiUnaMenos",n=5000,lang="es")
tweets4 = searchTwitter("EscuchameTambien",n=5000,lang="es")
tweets3 = searchTwitter("Femicidio",n=5000,lang="es")
tweets2 = searchTwitter("Violencia",n=5000,lang="es")
tweets1 = searchTwitter("Protejamosalasniñas",n=5000,lang="es")
tweets0 = searchTwitter("MeToo",n=5000,lang="es")



df10 = twListToDF(tweets7)
df9 = twListToDF(tweets7)
df8 = twListToDF(tweets7)
df7 = twListToDF(tweets7)
df6 = twListToDF(tweets6)
df5 = twListToDF(tweets5)
df4 = twListToDF(tweets4) 
df3 = twListToDF(tweets3)
df2 = twListToDF(tweets2) 
df1 = twListToDF(tweets1)
df0 = twListToDF(tweets0)
#colocando hora local
#diferencia entre UTC – The World's Time Standard y Guatemala (6 horas convertidas en segundos)
dif_UTC_gt = 6*60*60


df10$categoria = "NoMásViolenciaSexual"
df9$categoria = "NoMásFemicidio"
df8$categoria = "ElMachismoMata"
df7$categoria = "VivasNosQueremos"
df6$categoria = "Guatemala"
df5$categoria = "NiUnaMenos"
df4$categoria = "EscuchameTambien"
df3$categoria = "Femicidio"
df2$categoria = "Violencia"
df1$categoria = "ProtejamosALasNinas"
df0$categoria = "MeToo"



df0$created2=df0$created-dif_UTC_gt
df1$created2=df1$created-dif_UTC_gt
df2$created2=df2$created-dif_UTC_gt
df3$created2=df3$created-dif_UTC_gt
df4$created2=df4$created-dif_UTC_gt
df5$created2=df5$created-dif_UTC_gt
df6$created2=df6$created-dif_UTC_gt
df7$created2=df7$created-dif_UTC_gt
df8$created2=df8$created-dif_UTC_gt
df9$created2=df9$created-dif_UTC_gt
df10$created2=df10$created-dif_UTC_gt

df_total <- rbind(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)

write.csv(df_total,"TodaLaInfo2.csv")

write.csv(df5,"NiUnaMenos.csv")
write.csv(df2,"Francisco.csv")

