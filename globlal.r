################################################################
# Load all packeges

## set of packages used in the project
load.lib<-c("easypackages","tm","zoom","stringr","Rstem","ldatuning",
            "parallel","SnowballC","koRpus","wordcloud","topicmodels",
            "tokenizers","R.matlab","reshape2","ggplot2","newsanchor","readxl",
            "dplyr","tidyr","plotly","hrbrthemes","lubridate","TTR","ggfortify",
            "forecast","tseries","normtest","lmtest","TSA","urca","TSstudio")
print("<<<< Validate Packages installed >>>>")
install.lib<-load.lib[!load.lib %in% installed.packages()]
# install necessary packages
for(lib in install.lib) install.packages(lib,dependencies=TRUE, verbose=F)
sapply(load.lib,require,character=TRUE)

print(">>>>>loading packages")
library("easypackages")
libraries(load.lib)


## FINISH LOAD LIB
##################################################################

## set API KEY

api_key   = Sys.getenv()

Sys.setenv(api_key = "c703e5d02dc74cdaab1994c3c4451c83")
a<-Sys.getenv("api_key")

results <- get_headlines(category = "sports", page = 2,api_key)

results <- get_headlines(query = "Trump",api_key)

results <- get_headlines_all(category = "sports",api_key)

results <- get_everything(query = "Trump",api_key)

df <- get_headlines(category = "business")


df <- get_everything_all(query = "COVID", language = "en",api_key= a)

results <- get_everything(query = "ECUADOR", sort_by = "relevancy",api_key=a)

results <- get_headlines_all(category = "sports", api_key = a)

results$results_df$title


results$results_df$title

hPOelp(get_everything_all)



