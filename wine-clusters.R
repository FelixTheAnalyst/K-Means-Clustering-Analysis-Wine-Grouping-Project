#===========================================================
set.seed(2025)
dat_wine= read.table('Wine2025.txt', h = TRUE, stringsAsFactors = TRUE)
head(dat_wine)
attach(dat_wine)

X= as.matrix(dat_wine)

k_means= function(X,K,iterations)
{

  N = dim(X)[1]
  d = dim(X)[2]
  
  initial_index = sample(1:N,K,replace = FALSE)
  Centroids   =  X[initial_index,,drop=FALSE]
  
  ones  = matrix(1,N,1)
  dist_wine = matrix(0,N,K)
  cluster_sum= rep(0,K)
  error= rep(NA, iterations)
  
  for (i in 1:iterations)
  {
    dist_wine= dist_wine*0
    
    for (k in 1:K)
    {
      dist_wine[,k]= rowSums((X-ones%*%Centroids[k,])^2) #Distance to each centroid
      
    }
    assign_label = apply(dist_wine,1,which.min)# Centroid which you are closest to
    
    error_temp=0
    
    centroid_dist = apply(dist_wine,1,min) #Minimum distance from a centroid
    
    for (k in 1:K)
      {
        if (any(assign_label==k))
        {
              wh = which(assign_label==k)
              nk = length(wh)
              error_temp= error_temp + nk*sum(centroid_dist[wh])

         }
    }
    
    error[i]=error_temp
    
    for (k in 1:K)
    {
      if (any(assign_label==k))
       {
        wh= which(assign_label==k)
        nk= length(wh)
        Centroids[k,]=colSums(X[wh,,drop= FALSE])/nk
        
        
       }
      else
       {
        centroid_dist= apply(dist_wine,1,min)
        wh_worst=  which.max(centroid_dist)
        Centroids[k,]= X[wh_worst,]
        
       }
     }
  }
  
  
  
  
  
  return(list(Cluster= Centroids,Assignedlabels=assign_label, Distance=dist_wine, SSQ=error[iterations], N=N,K=K))
      
}
      
  
  
  SSC = function(res_k_means)
  {
    
    s_bar_overall=0
    silhouette_val= rep(NA,res_k_means$N)
    s_bar_k = rep(NA,res_k_means$K)
    
    for (i in 1:res_k_means$N)
    {
     squared_distance = sqrt(res_k_means$Distance[i,])
     squared_distance= sort(squared_distance)
     r_1 = squared_distance[1]# Minimum distance
     
     
     r_2=  squared_distance[2]# Second smallest distance
    
     silhouette_val[i]= (r_2-r_1)/r_2
    }
    
    for (k in 1:res_k_means$K)
    {
      if (any(res_k_means$Assignedlabels==k))
      {
        wh = which(res_k_means$Assignedlabels==k)
        nk = length(wh)
        s_bar_k[k]=(1/nk)*sum(silhouette_val[wh])
        
      }

    }
    s_bar_overall= (1/res_k_means$N)*sum(silhouette_val)
    
    
    return(list(s= silhouette_val,s_bar_k=s_bar_k, s_bar_overall=s_bar_overall))
    
    
  }
  # interim_result = k_means(X,6,100)
  # final_result= SSC(interim_result)
  # 
  
  #===========================================================
  # Plot Within-Sum-of-Squares & Simplified Silhouette values
  #===========================================================
  set.seed(2025)
  dat_wine <- read.table("Wine2025.txt", header = TRUE, stringsAsFactors = TRUE)
  X <- as.matrix(dat_wine)
  
  # --- Run k-means and silhouette calculations for K = 1 to 6 ---
  SSQ_vector <- rep(NA, 6)
  overall_s  <- rep(NA, 6)
  
  for (k in 1:6) {
    interim_result <- k_means(X, k, 1000)
    SSQ_vector[k] <- interim_result$SSQ
    
    if (k == 1) {
      overall_s[k] <- NA
    } else {
      final_result <- SSC(interim_result)
      overall_s[k] <- final_result$s_bar_overall
    }
  }
  
  # --- Plot 1: Within-Cluster Sum of Squares (Elbow Plot) ---
  plot(1:6, SSQ_vector, type = "b", pch = 19, lwd = 2, col = "black",
       xlab = "Number of Clusters (K)",
       ylab = "Within-Cluster Sum of Squares",
       main = "Within-Cluster Sum of Squares vs K")
  grid()
  
  # --- Plot 2: Simplified Silhouette Scores ---
  plot(1:6, overall_s, type = "b", pch = 19, lwd = 2, col = "black",
       xlab = "Number of Clusters (K)",
       ylab = expression(bar(s)[K]^overall),
       main = "Simplified Silhouette Scores vs K",
       ylim = c(0.45, 0.55))
  grid()
  
  #===========================================================
  # Silhouette Plot for Optimal K
  #===========================================================
  
  optimal_K <- 3  # chosen based on Q4(c)
  set.seed(2025)
  optimal_result <- k_means(X, optimal_K, 1000)
  sil_result <- SSC(optimal_result)
  
  cluster_df <- data.frame(
    Clusters = optimal_result$Assignedlabels,
    silhouette = sil_result$s
  )
  cluster_df <- cluster_df[order(cluster_df$Clusters, cluster_df$silhouette), ]
  
  M <- length(unique(cluster_df$Clusters))
  colors <- rainbow(M)
  
  # --- Create barplot (no x-axis yet) ---
  bar_centers <- barplot(cluster_df$silhouette,
                         col = colors[cluster_df$Clusters],
                         border = NA,
                         ylim = c(0, 1.15),
                         ylab = expression(s[i]),
                         xaxt = "n", # suppress x-axis
                         main = bquote(" " ~ bar(s)[overall]^3 == .(round(sil_result$s_bar_overall, 3)) ~ ""))
  
  # --- Add reference line for overall silhouette ---
  abline(h = sil_result$s_bar_overall, col = "black", lty = 2, lwd = 2)
  
  # --- Add gridlines ---
  grid(nx = NULL, ny = NULL)
  
  # --- Add x-axis with observation indices ---
  n_obs <- nrow(cluster_df)
  axis(side = 1,
       at = seq(0, max(bar_centers), length.out = 6),  # tick spacing similar to lecture
       labels = round(seq(0, n_obs, length.out = 6)),
       cex.axis = 0.8)
  mtext("Observations", side = 1, line = 2.5, cex = 0.9)
  
  # --- Add cluster mean silhouette labels above each cluster group ---
  cluster_means <- sil_result$s_bar_k
  cluster_positions <- sapply(unique(cluster_df$Clusters), function(k) {
    mean(bar_centers[cluster_df$Clusters == k])
  })
  
  # Add cluster average silhouette scores in mathematical notation (slide 69 format)
  for (k in 1:M) {
    text(x = cluster_positions[k], y = 1.05,
         labels = bquote(bar(s)[.(k)] == .(round(cluster_means[k], 3))),
         cex = 0.9, font = 2)
  }