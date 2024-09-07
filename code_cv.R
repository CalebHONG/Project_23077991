library(caret)


initial_train_size <- 245
validation_size <- 10
total_size <- 490

k_values <- 1:30


mean_accuracies <- numeric(length(k_values))


for (k in k_values) {
  
  accuracy_results <- c()  
  
  for (start in seq(initial_train_size + 1, total_size, by = validation_size)) {
    
    train_indices <- 1:(start - 1)
    train_data <- bbl[train_indices, ]
    

    validation_indices <- start:min(start + validation_size - 1, total_size)
    validation_data <- bbl[validation_indices, ]
    

    model <- train(new_winner ~ ., data = train_data, method = "knn", tuneGrid = data.frame(k = k))
    
    predictions <- predict(model, newdata = validation_data)
    
    accuracy <- sum(predictions == validation_data$new_winner) / length(predictions)
    accuracy_results <- c(accuracy_results, accuracy)
  }
  
  mean_accuracies[k] <- mean(accuracy_results)
  
  print(paste("Mean Validation Accuracy for k =", k, ":", round(mean_accuracies[k], 4)))
}

best_k <- which.max(mean_accuracies)
print(paste("Best k:", best_k))

plot(k_values, mean_accuracies * 100, type = "b", pch = 16, col = "blue",
     xlab = "Number of Neighbors (k)", ylab = "Validation Accuracy (%)",
     main = "Validation Accuracy for Different k Values")

abline(v = best_k, col = "red", lty = 2)
text(best_k, mean_accuracies[best_k] * 100-12, 
     labels = paste("Best k =", best_k, "\nAccuracy =", round(mean_accuracies[best_k] * 100, 2), "%"),
     pos = 4, col = "red")

#############################################################################################
#############################################svm

initial_train_size <- 245
validation_size <- 10
total_size <- 490


gamma_range = seq(0.1,1, length=10)
cost_range = seq(0.1,10,length=10)
#gamma_range <- 10^(-3:3)
#cost_range <- 10^(-3:3)


results <- expand.grid(gamma = gamma_range, cost = cost_range)
results$mean_accuracy <- NA


for (i in 1:nrow(results)) {
  
  gamma <- results$gamma[i]
  cost <- results$cost[i]
  
  accuracy_results <- c()  
  
  for (start in seq(initial_train_size + 1, total_size, by = validation_size)) {

    train_indices <- 1:(start - 1)
    train_data <- bbl[train_indices, ]
    

    validation_indices <- start:min(start + validation_size - 1, total_size)
    validation_data <- bbl[validation_indices, ]
    

    model <- svm(new_winner ~ ., data = train_data, type = "C", cost = cost, gamma = gamma)
    
    predictions <- predict(model, newdata = validation_data)
    
    accuracy <- sum(predictions == validation_data$new_winner) / length(predictions)
    accuracy_results <- c(accuracy_results, accuracy)
  }
  
  results$mean_accuracy[i] <- mean(accuracy_results)
}

best_index <- which.max(results$mean_accuracy)
best_gamma <- results$gamma[best_index]
best_cost <- results$cost[best_index]
best_accuracy <- results$mean_accuracy[best_index]

print(paste("Best Gamma:", best_gamma))
print(paste("Best Cost:", best_cost))
print(paste("Best Validation Accuracy:", round(best_accuracy, 4)))

ggplot(results, aes(x = log10(gamma), y = log10(cost), fill = mean_accuracy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Validation Accuracy for Different Gamma and Cost Values",
       x = "log10(Gamma)",
       y = "log10(Cost)",
       fill = "Accuracy") +
  theme_minimal()
library(htmltools)
library(plotly)
library(reshape2)  
library(ggplot2)

results_matrix <- acast(results, gamma ~ cost, value.var = "mean_accuracy")

#cost 1:100
#gamma 0.01:1
###############################################################
results_long <- melt(results_matrix)
colnames(results_long) <- c("gamma", "cost", "accuracy")

ggplot(results_long, aes(x = as.factor(cost), y = as.factor(gamma), fill = accuracy)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "SVM Accuracy Heatmap", x = "penalty parameter", y = "Gamma") +
  theme_minimal()



max_index <- which(results_matrix == max(results_matrix), arr.ind = TRUE)
max_gamma <- gamma_range[max_index[1]]
max_cost <- cost_range[max_index[2]]
max_accuracy <- results_matrix[max_index]

#3D
persp_plot <- persp(
  x = gamma_range, y = cost_range, z = results_matrix,
  theta = 45, phi = 20,
  xlab = "Gamma(G)", ylab = "penalty parameter(C)", zlab = "Accuracy(A)",
  main = "3D Surface Plot of Validation Accuracy",
  col = "lightblue", border = "black",ticktype = "detailed",nticks=3
)


trans_coords <- trans3d(max_gamma, max_cost, max_accuracy, persp_plot)

points(trans_coords, col = "red", pch = 16, cex = 1)
#text(trans_coords, labels = round(max_accuracy, 4), pos = 3, col = "red")
#text(trans_coords, labels = paste0("Gamma = 0.2\n", "Cost = 1\n", "Accuracy = ", round(max_accuracy, 3)), pos = 3, col = "red")

text(trans_coords[1],trans_coords[2], labels = paste0("G=0.2,C=1.2, A=0.593")
     , pos = 3, col = "red")
############################################################################################
library(rpart)


window_size <- 5  # 
cp_values <- seq(0, 0.01, by = 0.001)  # 
min_error <- Inf
best_cp <- 0

for (cp in cp_values) {
  errors <- c()
  for (i in seq(window_size + 1, nrow(training_validation_data))) {
    train_data <- training_validation_data[300:(i-1), ]
    test_data <- training_validation_data[i, , drop=FALSE]
    

    model_tree <- rpart(new_winner ~ ., data = train_data, method = "class", cp = cp)

    pred <- predict(model_tree, newdata = test_data, type = "class")
    error_rate <- mean(pred != test_data$new_winner)
    errors <- c(errors, error_rate)
  }
  

  avg_error <- mean(errors)
  
  if (avg_error < min_error) {
    min_error <- avg_error
    best_cp <- cp
  }
}


model_tree <- rpart(new_winner ~ ., data = training_validation_data, method = "class", cp = 0.009)
pfit <- prune(model_tree, cp = best_cp)

plot(pfit, uniform = TRUE, margin = 0.01, branch = 0.5)
text(pfit, cex = 0.8)

cat("Best cp:", best_cp, "\n")
