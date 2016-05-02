distancia<-function(x,distance,type){
	switch(distance,
          euclidean = stats::dist(x),
          correlation = dcor(x),
          Bhattacharyya = dBhatta(x),
          Gower = dGower(x,type),
          Mahalanobis = dMahal(x),
          BrayCurtis = dBrayCurtis(x),
          Orloci = dOrloci(x),
          Hellinger = dHellinger(x),
          Prevosti = dPrevosti(x))
}
