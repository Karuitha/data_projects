##############################
# Segmentation Analysis in R #
##############################

## Sticks Kebob

# Load Packages (if needed) and set seed

set.seed(1)

# Import Data

seg <- read.csv("Case Data/M-0866X-Bases.csv") ## Choose M-0866X-Bases.csv file

# Run hierarchical clustering with bases variables

seg_hclust <- hclust(dist(scale(cbind(seg$B1, seg$B2, seg$B3, seg$B4))), method="complete")

# Elbow plot for first 10 segments

x <- c(1:10)
sort_height <- sort(seg_hclust$height,decreasing=TRUE)
y <- sort_height[1:10]
plot(x,y); lines(x,y,col="blue")

## Choose the K-Means with the number of segments you picked from the Elbow Plot

# Run k-means with 2 segments

seg_kmeans <- kmeans(x = data.frame(seg$B1, seg$B2, seg$B3, seg$B4), 2)
seg_kmeans$size

# Run k-means with 3 segments

seg_kmeans <- kmeans(x = data.frame(seg$B1, seg$B2, seg$B3, seg$B4), 3)
seg_kmeans$size

# Run k-means with 4 segments

seg_kmeans <- kmeans(x = data.frame(seg$B1, seg$B2, seg$B3, seg$B4), 4)
seg_kmeans$size

# Run k-means with 5 segments

seg_kmeans <- kmeans(x = data.frame(seg$B1, seg$B2, seg$B3, seg$B4), 5)
seg_kmeans$size

# Run k-means with 6 segments

seg_kmeans <- kmeans(x = data.frame(seg$B1, seg$B2, seg$B3, seg$B4), 6)
seg_kmeans$size

# Run k-means with 7 segments

seg_kmeans <- kmeans(x = data.frame(seg$B1, seg$B2, seg$B3, seg$B4), 7)
seg_kmeans$size

## Back to the regular code

# Add segment number back to original data

segment = seg_kmeans$cluster
segmentation_result <- cbind(seg, segment)

# Export data to a CSV file
write.csv(segmentation_result, file = "sticks_results.csv",
          row.names = FALSE) ## Name file sticks_result.csv

