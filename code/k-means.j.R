
# ref: https://www.datacamp.com/community/tutorials/k-means-clustering-r

# objective: classify (determine clusters of) the trap locations containing crab spiders
# for a week/transect

kmReduce <- function(df, ft, fw, fs, intWeek, daytime) {

# filter data based on transect, week, and species presence

# agruments ft, fw, and fw are pre-built "formulae" that dplyr::filter_() can tolerate

#t <- "oakMargin"
#w <- 26
#formula.t <- (~ transect == "oakMargin" )
#formula.w <- (~ week == 24 )
#formula.s <- (~ Thomisidae..crab.spider. > 0 )
#s<- "Thomisidae..crab.spider."
#df <- bugs.df
#intWeek <- 26


#t <- enquo(t)
#w <- enquo(w)
#s<- enquo(s)



	data <- df %>%
		dplyr::filter(time==daytime) %>%
      	#dplyr::filter(transect == !! t, week == !! w) %>%    # get all traps with spiders for transect/week
	  	dplyr::filter_(ft) %>%
	  	dplyr::filter_(fw) %>%
      	dplyr::filter_(fs) %>%  # https://stackoverflow.com/questions/36647468/creating-a-function-with-an-argument-passed-to-dplyrfilter-what-is-the-best-wa
      	dplyr::select(row, position, week, transect)

      		#  !! transect
			# The bang bang says we want the expression that item is referring to, not item itself.
			# http://r.danal.com/tutorials/quosures_inside_out.html
      		#


    data$row <- as.character(data$row)  			# replace the row number with a week number  **** CHECK LOGIC ****
    data$row <- as.factor(as.character(intWeek))  	# replace the row number with a week number
    #data$row <- as.factor(data$row)
                                     				# this effectively combines the row data and
                                     				# enables the collection of weekly cluster info
                                     				# on one plot
    #https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement


	return(data)

}

kmAssignClusters <- function(list, cn) {

	# find clusters in weekly data and then standardize the cluster numbers
	# so multiple weeks can be compare visually

	kdata <- list  # kdata is a df that is an element of a large list

	# find clusters
	set.seed(20)	 									            # cn is the pre-defined number of clusters
	clusters <- kmeans(kdata[,1:2], cn, iter.max=100)    			# row and position are columns 1 and 2 (see kmReduce() )
	kdata$cluster <- as.factor(clusters$cluster)		            # 'replace' is handed to base::sample(), TRUE allows
																	# sample smaller than 'cn' (but kmeans() does not pass it)
								# https://www.r-bloggers.com/finding-optimal-number-of-clusters/

	# now every row / position pair with species occurrances is assigned a cluster number

	# problem: the cluster numbers assigned by kdata() are not necessarily in order of ascending trap position
	# (so, to make the cluster fill colors match in the graphics, the cluster numbers need to be synchronized
	#  across weeks)

	# for each cluster, create a list of the positions
	v.clusterVector <- getClusters(kdata)

	v.position.list <- list()
	v.mean.list <- list()
	v.cluster.df <- tibble(cluster=integer(), mean=integer())

	for (i in 1:length(v.clusterVector)) {

			positionVector <- kdata %>%
							dplyr::filter(cluster == i) %>%
   							dplyr::select(position) %>%
   							unique() %>%
   							.$position

   			v.position.list[[i]] <- positionVector

   			v.mean.list[[i]] <- mean(v.position.list[[i]])
	}

	# create a df that associates the mean of the positions with the cluster number
	# (this will allow the row groups to be sorted)
	for (i in 1:length(v.clusterVector)) {
		v.cluster.df <- v.cluster.df %>% tibble::add_row(cluster = i, mean = v.mean.list[[i]])
	}
	# now use the mean to sort the rows
	v.cluster.df <- dplyr::arrange(v.cluster.df, mean)
	# build a vector of row numbers
	# this is the new order for the cluster assignments
	v.rowVector <- list()
	for (i in 1:nrow(v.cluster.df)) {
		v.rowVector[[i]] <- i
	}
	# create a new column representing a revised cluster number
	v.cluster.df <- v.cluster.df %>% dplyr::mutate(newCluster = as.list(v.rowVector))

	# add an empty column to the target data
	kdata$newCluster <- NULL
	#
	# walk through data; compare data$cluster to v.cluster.df$cluster
	#
	for (i in 1:nrow(kdata)) {

		for (j in 1:nrow(v.cluster.df)) {
			if ( kdata$cluster[[i]] == v.cluster.df$cluster[[j]] ) {
				kdata$newCluster[[i]] <- v.cluster.df$newCluster[[j]]
			}
		}
	}

	kdata$newCluster <- as.factor(kdata$newCluster)


	return(kdata)

}


getClusters <- function(data) {

	# return a list of the clusters occurring in the dataset
	# https://stackoverflow.com/questions/29832411/use-dplyr-to-get-values-of-a-column

	library(dplyr)
	clusterVector <- data %>%
   					select(cluster) %>%
   					unique() %>%
   					.$cluster

   	return(clusterVector)


}

kmPlot <- function(list, transectText, time) {

	#list <- datalist

  colours = c("3" = "blue", "2" = "green", "1" = "red")

	ggplot() +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),    # x = row[]
  			data = list[[1]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[2]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[3]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[4]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[5]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[6]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[7]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[8]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[9]], shape = 21, size=5) +

  		geom_point(aes(y = position[], x = week[], fill = newCluster),
  			data = list[[10]], shape = 21, size=5) +

  		#ylim(c(1, 10)) +
      	expand_limits(y=c(1,10)) +
      	scale_y_continuous(breaks = seq(min(1), max(10), by = 1)) +

      	#xlim(c(22, 34)) +              # Set scale limits ('zoom' function; designed to exclude existing data)
      	expand_limits(x=c(22,34)) +    # enlarge the range of the axis
      	scale_x_continuous(breaks=seq(22,34,2)) +         # log, squart-root, reverse....

      	geom_hline(yintercept=4.5) +
      	geom_hline(yintercept=7.5) +

      	coord_fixed(ratio=1) + # control the aspect ratio

      	# guide_legend(title="clusters") +
      	guides(fill=guide_legend(title="clusters")) +

      	#labs(title=paste("crab spider clusters"),
        #subtitle=paste("transect: ", transectText, sep=""),
      	labs(x="week number",
          y="trap position",
          caption = paste("crab spider clusters\n", "transect: ", transectText, ", daytime: ", time,
          					"\n(data for weeks 33-34 is too sparse)", sep="") ) +

    # https://en.wikipedia.org/wiki/K-means_clustering

  		theme_bw() +

      scale_fill_manual(values = colours,
                      breaks = c("1", "2", "3"),
                      labels = c("cluster 1", "cluster 2", "cluster 3")) +

      theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "mm"),
          #legend.position=c(.9,.7),
          legend.justification=c(1,0),
          panel.border = element_rect(colour = "black", fill=NA),
          aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))


}

buildClustersByWeek <- function(df, t, species, cn, time) {

	#df <- bugs.df
	#i <- 1
	#species <- "Thomisidae..crab.spider."
	#cn <- 3
	#formula.t <- (~ transect == "oakMargin" )
	#formula.w <- (~ week == 25 )
	#formula.s <- (~ Thomisidae..crab.spider. > 0 )
	#t <- "control"

	transectString <- paste("~transect=='", t, "'", sep="")
	formula.t <- as.formula(transectString)
	#transectExp <- parse(text = transectString)

	speciesString <- paste("~", species, ">0", sep="")
	formula.s <- as.formula(speciesString)
	#speciesExp <- parse(text = speciesString)

	weeks.vector <- getWeeks(df)

	dataList <- NULL
	dataList <- list()

	for (i in 1:length(weeks.vector)) {

		if (weeks.vector[[i]] < 33) {    # data becomes too sparse to cluster

			w <- weeks.vector[[i]]

			weekString <- paste("~week==", weeks.vector[[i]], sep="") # dynamically create an expression to
			formula.w <- as.formula(weekString)
			#weekExp <- parse(text = weekString)                    # filter by week
																	 # http://adv-r.had.co.nz/Expressions.html#parsing-and-deparsing

			dataList[[i]] <- kmReduce(df, ft=formula.t, fw=formula.w, fs=formula.s, intWeek=w, daytime=time)

			clusterList <- dataList[[i]]

			dataList[[i]] <- kmAssignClusters(list=clusterList, cn=cn)

			# write data to disc for transect == "control", one file for each week
			if (t=="control") {

				f <- paste("clustersByWeek.", t, ".", time, ".", weeks.vector[[i]], sep="")

				singleDFtoDiscCL(df=data.frame(dataList[[i]]), transect=t, name=f)
			}

		}


	}

	return(dataList)

}
