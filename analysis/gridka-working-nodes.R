nodes <- read.table("../data/gridka-benchmarks-2017.csv", sep=",", header=TRUE)
nodes <- nodes[c("hs06",
                 "interconnect",
                 "cores"
                 )]
library(sqldf)
distinct_nodes = sqldf("SELECT hs06,
                          cast(SUBSTR(interconnect,1,length(interconnect)-1) as integer )*1000  as interconnect,
                          cores,
                          COUNT(*) as number,
                          round((cast(COUNT(*) as float)/(Select count(*) from nodes)),5) as probability
                       FROM nodes group by hs06, interconnect, cores")

distinct_hs06 = sqldf("SELECT hs06, 
                         COUNT(*) as number,
                         round((cast(COUNT(*) as float)/(Select count(*) from nodes)),5) as probability
                       FROM nodes group by hs06")

distinct_cpu_cores = sqldf("SELECT cores, 
                              COUNT(*) as number,
                              round((cast(COUNT(*) as float)/(Select count(*) from nodes)),5) as probability
                            FROM nodes group by cores")

distinct_hdd = sqldf("SELECT cast(SUBSTR(interconnect,1,length(interconnect)-1) as integer )*1000  as interconnect, 
                        COUNT(*) as number,
                        round((cast(COUNT(*) as float)/(Select count(*) from nodes)),5) as probability
                      FROM nodes group by interconnect")

# create doublePMF distribution string for resource environment model

create_env_string = function (table, column){
  res_string = "DoublePMF["
  
  for( i in rownames(table)){
    res_string = paste(res_string, "(", table[i,column], ";", table[i,"probability"], ")")
  }
  
  res_string = paste(res_string, "]")
  return (res_string)
}

print (paste("CPU processing rate:", create_env_string(distinct_hs06, "hs06")))
print (paste("HDD processing rate:", create_env_string(distinct_hdd, "interconnect")))
print (paste("CPU number cores:", create_env_string(distinct_cpu_cores, "cores")))

