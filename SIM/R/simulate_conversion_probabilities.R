library(MASS)

simulate_conversion_probabilities=function(n_agents, conversion_mus, conversion_sigma, lambdas_in) {

  # Function 1: Generate `mu` and `Sigma` dynamically based on weights
  generate_mu_sigma <- function(weight_r, weight_s, weight_u, weight_nm, weight_matrix) {

    # Define mean preferences for each membership type
    mu <- c(r = weight_r, s = weight_s, u = weight_u, nm = weight_nm)

    # Define a **base covariance structure** that will be **scaled** by weight_matrix
    base_Sigma <- matrix(c(
      1.0,  0.5,  0.3, -0.6,   # r (high coworking + playground preference)
      0.5,  1.0,  0.4, -0.5,   # s (moderate tutoring + coworking)
      0.3,  0.4,  1.0, -0.4,   # u (strong tutoring)
      -0.6, -0.5, -0.4,  1.0    # nm (competes with all)
    ), nrow = 4, byrow = TRUE)

    # Apply weight scaling to base Sigma (makes it **dynamic**)
    Sigma <- base_Sigma * weight_matrix

    # Ensure diagonal is 1 for valid correlation structure
    diag(Sigma) <- 1

    return(list(mu = mu, Sigma = Sigma))
  }

  # Function 2: Generate the Correlation Matrix R

  #library(MASS)
  library(Matrix)

  generate_r_matrix <- function(n, mu, Sigma) {

    # Function to check if Sigma is positive definite
    is_positive_definite <- function(Sigma) {
      return(all(eigen(Sigma, only.values = TRUE)$values > 0))
    }

    # Keep trying until Sigma is positive definite
    attempt <- 1
    while (!is_positive_definite(Sigma)) {
      message(paste("Attempt", attempt, "- Sigma not positive definite, adjusting..."))
      Sigma <- Sigma + diag(ncol(Sigma)) * 0.01  # Add jitter
      attempt <- attempt + 1

      # As a fail-safe, force Sigma to be near positive definite if too many attempts
      if (attempt > 5) {
        message("Forcing Sigma to be near positive definite with Matrix::nearPD()")
        Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)
        break
      }
    }

    # Simulate R matrix using mvrnorm (random variation in correlations)
    sampled_r <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)

    # Normalize rows to ensure proportional influence
    sampled_r <- apply(sampled_r, 2, function(x) x / sum(abs(x)))

    return(sampled_r)
  }


  #generate_r_matrix <- function(n, mu, Sigma) {
    # Ensure Sigma is positive semi-definite
   # Sigma <- Sigma + diag(ncol(Sigma)) * 0.01  # Small jitter for numerical stability




    # Simulate R matrix using mvrnorm (random variation in correlations)
    #sampled_r <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)  # (n programs × memberships)

    # Normalize rows to ensure proportional influence
    #sampled_r <- apply(sampled_r, 2, function(x) x / sum(abs(x)))

    # Return R as a matrix (n programs × 4 memberships)
    #return(sampled_r)
  #}

  # Function 3: Compute Membership Probabilities
  calculate_membership_probabilities <- function(lambdas_df, r_matrix) {
    lambdas_mat <- as.matrix(lambdas_df[, -1])  # Remove 'id' column

    # Compute raw membership scores using generated r_matrix
    scores <- lambdas_mat %*% r_matrix

    # Softmax function for probability normalization
    softmax <- function(x) {
      exp_x <- exp(x - max(x))  # Stabilized exponentiation
      return(exp_x / sum(exp_x))
    }

    # Apply softmax row-wise
    membership_probs <- t(apply(scores, 1, softmax))

    # Convert to data frame and return
    colnames(membership_probs) <- c("r", "s", "u", "nm")
    return(as.data.frame(membership_probs))
  }

  # Step 1: Generate `mu` and `Sigma` based on provided weights
  mu_sigma <- generate_mu_sigma(
    weight_r = conversion_mus[1],
    weight_s = conversion_mus[2],
    weight_u = conversion_mus[3],
    weight_nm = conversion_mus[4],
    weight_matrix = conversion_sigma
  )

  # Step 2: Generate R matrix dynamically
  r_matrix <- generate_r_matrix(n = 8, mu = mu_sigma$mu, Sigma = mu_sigma$Sigma)

  # Step 3: Compute membership probabilities using the new R matrix
  membership_probs_df <- calculate_membership_probabilities(lambdas_in, r_matrix)

  membership_probs_df=data.frame(id=1:n_agents,membership_probs_df)

  return(membership_probs_df)
}
