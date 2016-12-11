calculate_ACMI_R <- function(
    dist_sum,
    colsums,
    rowsums,
    num_cols,
    column_contributions,
    row_index_counts,
    joint,
    total_non_zeros,
    full_MI){

    ACMI_contribution = rep(0,num_cols)

    for (i in 1:num_cols) {

        if (i %% 100000 == 0) {
            print(i)

        }

        if (colsums[i] > 0) {
            D <- dist_sum /(dist_sum - colsums[i])

            cur_non_zeros = 0

            x1 =  (rowsums[1] - joint[1,i])
            x2 =  (rowsums[2] - joint[2,i])

            x1 = x1 / (x1 + x2)
            x2 = x2 / (x1 + x2)

            D_1 = rowsums[1]/x1
            D_2 = rowsums[2]/x2

            sub_term_1 = 0
            if (joint[1,i] > 0) {
                cur_non_zeros =  cur_non_zeros + 1
                sub_term_1 = log(D_1) * (row_index_counts[1] - 1)
            } else {
                sub_term_1 = log(D_1) * row_index_counts[1]
            }

            sub_term_2 = 0
            if (joint[2,i] > 0) {
                cur_non_zeros =  cur_non_zeros + 1
                sub_term_2 = log(D_2) * (row_index_counts[2] - 1)
            } else {
                sub_term_2 = log(D_2) * row_index_counts[2]
            }

            term2 <- sub_term_1 + sub_term_2

            change <- D * (0 - term2 - column_contributions[i])

            if (i  < 3) {
                print(i)
                cat("sub_term_1", sub_term_1, "sub_term_2", sub_term_2, "term_2", term2, "change", change," D", D, "col contrib", column_contributions[i], "D_1", D_1, "D_2", D_2, "\n")
            }
            #

            ACMI_contribution[i] <- change
        }
    }

    return(ACMI_contribution)
}



slow_ACMI_contributions_R <- function(MI,
                                      joint_dist,
                                      colsums,
                                      num_cols) {


    ACMI_contribution = rep(0,num_cols)

    for (i in 1:num_cols) {

        if (i %% 100 == 0) {
            print(i)
        }

        if (colsums[i] > 0) {
            log <- capture.output({
                ACMI_contribution[i] <- mutual_information(joint_dist[,i]) - MI
            })
        }

    }
    return(ACMI_contribution)
}


calculate_mutual_info_contributions <- function(
    normalized_column_sums,
    normalized_row_sums,
    column_contributions,
    dense,
    num_cols,
    dist_sum){

    contribution <- rep(0,num_cols)

    # get the unique column contributions
    unique_contributions <- unique(column_contributions)

    # remove the zero
    unique_contributions <- unique_contributions[-which(unique_contributions == 0)]

    cat("Number of unique column entries:", length(unique_contributions), "\n")
    # get the column indicies for each and the first ones for each.
    unique_contribution_column_inds <- rep(0,num_cols)

    # also store first column index associated with each
    first_col_index <- rep(0, length(unique_contributions))
    unique_contribution_column_counts <- rep(0,length(unique_contributions))

    non_zero_inds <- which(column_contributions > 0)
    nz_column_contributions <- column_contributions[non_zero_inds]
    inds_list <- vector(mode = "list", length = length(unique_contributions))

    # now populate
    for (i in 1:length(unique_contributions)) {
        nz_inds <- which(nz_column_contributions == unique_contributions[i])
        inds <- non_zero_inds[nz_inds]
        inds_list[[i]] <- i
        first_col_index[i] <- inds[1]
        unique_contribution_column_inds[inds] <- i
        unique_contribution_column_counts[i] <- length(inds)
    }

    cat("Calculating unique column contributions, this may take a while...\n")
    # now we need to get the effect on all other unique contributions of
    # removing one of the unique contributions
    cs <- normalized_column_sums[first_col_index]
    joint <- dense[,first_col_index]
    unique_contrib_MI <- calculate_unique_MI_contribution(
        cs,
        normalized_row_sums,
        length(cs),
        length(normalized_row_sums),
        joint,
        unique_contribution_column_counts,
        dist_sum)

    cat("Complete! Filling in full column vector...\n")
    # now populate contribution
    # for (i in 1:length(contribution)) {
    #     if (unique_contribution_column_inds[i] > 0) {
    #         contribution[i] <- unique_contrib_MI[unique_contribution_column_inds[i]]
    #     }
    # }

    for (i in 1:length(unique_contributions)) {
        contribution[inds_list[[i]]] <- unique_contrib_MI[i]
    }

    return(contribution)
}
