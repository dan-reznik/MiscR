library("DBI")
library("rJava")
library("RJDBC")
#hive.class.path = list.files(path=c("/usr/lib/gphd/hive/lib"), pattern="jar", full.names=T);
#hadoop.lib.path = list.files(path=c("/usr/lib/gphd/hadoop/lib"), pattern="jar", full.names=T);
#hadoop.class.path = list.files(path=c("/usr/lib/gphd/hadoop"), pattern="jar", full.names=T);
#class.path = c(hive.class.path, hadoop.lib.path, hadoop.class.path);
options(java.parameters = "-Xmx8g")
drv <- JDBC("org.apache.hive.jdbc.HiveDriver", "C:/Users/dareznik/Downloads/hive-jdbc-2.0.0.jar")
hive.master <- "bda1node04.grupoamil.com.br:10000"
url.dbc <- paste0("jdbc:hive://", hive.master,"/default")
conn <- dbConnect(drv,url.dbc)
dbListTables(conn)