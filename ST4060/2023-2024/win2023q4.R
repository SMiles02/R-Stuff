# Load required libraries and data
library(MASS)
library(cluster)

# Load the Pima.tr dataset
x <- Pima.tr
x$type <- NULL  # Remove the target variable (features only)
y <- Pima.tr$type  # Store the target variable separately

# Question (a)
# ---------------------------------------------------
# This is a classification problem because the goal is to predict `y`, 
# which is a categorical variable representing the "type" attribute. 
# Classification problems involve predicting categories (labels) 
# rather than continuous numerical values.
# ---------------------------------------------------

# Question (b): Perform k-means clustering on x with k = 2
# ---------------------------------------------------
set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(x, centers = 2, nstart = 10)

# (i) Confusion matrix between clusters and true labels
cluster_labels <- kmeans_result$cluster
confusion_matrix <- table(cluster_labels, y)
print(confusion_matrix)

# (ii) Scatterplot of the first two features with cluster color-coding
plot(
  x[, 1:2], 
  col = ifelse(cluster_labels == 1, "red", "black"), 
  pch = 20, 
  main = "K-Means Clustering Scatterplot"
)
legend("topright", legend = c("Cluster 1", "Cluster 2"), col = c("red", "black"), pch = 20)
# ---------------------------------------------------
# Explanation:
# The confusion matrix shows how the clusters (cluster_labels) 
# align with the true class labels (y). 
# The scatterplot visualizes how data points are grouped 
# in the first two feature dimensions.
# ---------------------------------------------------

# Question (c): Spatial distribution of clusters
# ---------------------------------------------------
# The scatterplot helps to assess the spatial distribution of points in terms of their cluster membership.
# If the clusters overlap significantly, it suggests that the features in `x` 
# do not fully separate the two classes. A clear separation indicates good clustering.
# Patterns like elongated or irregular shapes may signal issues with clustering assumptions.
# ---------------------------------------------------

# Question (d): Perform scaled PCA
# ---------------------------------------------------
scaled_pca <- prcomp(x, scale. = TRUE)

# Calculate cumulative variance explained by components
var_explained <- cumsum(scaled_pca$sdev^2) / sum(scaled_pca$sdev^2)

# Number of components needed to explain 90% variance
num_components <- which(var_explained >= 0.9)[1]
cat("Number of principal components to explain 90% variance:", num_components, "\n")
# ---------------------------------------------------
# Explanation:
# Scaled PCA standardizes the data before computing principal components.
# This ensures that features with large magnitudes do not dominate the analysis.
# The number of components output indicates how many are needed to capture 90% of the variance.
# ---------------------------------------------------

# Question (e): Perform unscaled PCA
# ---------------------------------------------------
unscaled_pca <- prcomp(x, scale. = FALSE)

# Inspect loadings for the first two principal components
loadings <- unscaled_pca$rotation[, 1:2]
print(loadings)
# ---------------------------------------------------
# Explanation:
# The loadings show the contribution of each original feature to the first two principal components.
# Features with the highest absolute values in the loadings influence the components the most.
# Unscaled PCA does not standardize the data, so the results may be dominated 
# by features with larger magnitudes or variances.
# ---------------------------------------------------
