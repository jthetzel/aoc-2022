## 8.r

path <- "8-example.txt"


viewable <- function(tree_row) {
    tree_list <- list(tree_row[1])
    for (tree in tree_row[-1]) {
        if (all(tree > tree_list)) {
            tree_list <- append(tree_list, tree)
        } else {
            tree_list <- append(tree_list, 0)
        }
    }

    return (tree_list)
}


score <- function(row) {
    tree_scores <- list()
    n <- length(row)
    for (index in seq(1, n)) {
        if (index == length(row)) {
            tree_score <- 0
        } else if (length(row) - index == 1) {
            tree_score <- 1
        } else {
            trues <- row[(index+1):length(row)] < row[[index]]
            if (all(trues)) {
                tree_score = length(trues)
            } else {
                tree_score <- which(!row[(index+1):length(row)] < row[[index]])[1]
            }
        }
        tree_scores <- append(tree_scores, tree_score)
    }

    return (tree_scores)
}


## read data
lines <- readLines(path)
ncol <- nchar(lines[[1]])
data <- read.fwf(path, rep(1, ncol)) + 1


## part 1
row <- apply(data, 1, viewable)
column <- apply(data, 2, viewable)
row_rev <- apply(rev(data), 1, viewable)
column_rev <- apply(apply(data, 2, rev), 2, viewable)

row_matrix <- matrix(unlist(row), ncol=ncol, byrow=TRUE)
column_matrix <- t(matrix(unlist(column), ncol=ncol, byrow=TRUE))
row_rev_matrix <- t(apply(matrix(unlist(row_rev), ncol=ncol, byrow=TRUE), 1, rev))
column_rev_matrix <- apply(t(matrix(unlist(column_rev), ncol=ncol, byrow=TRUE)), 2,rev)

total_matrix <- row_matrix + column_matrix + row_rev_matrix + column_rev_matrix

answer1 <- sum(total_matrix > 0)
print(answer1)


## part 2
row <- apply(data, 1, score)
column <- apply(data, 2, score)
row_rev <- apply(rev(data), 1, score)
column_rev <- apply(apply(data, 2, rev), 2, score)

row_matrix <- matrix(unlist(row), ncol=ncol, byrow=TRUE)
column_matrix <- t(matrix(unlist(column), ncol=ncol, byrow=TRUE))
row_rev_matrix <- t(apply(matrix(unlist(row_rev), ncol=ncol, byrow=TRUE), 1, rev))
column_rev_matrix <- apply(t(matrix(unlist(column_rev), ncol=ncol, byrow=TRUE)), 2,rev)

answer2 <- max((row_matrix * row_rev_matrix) * (column_matrix * column_rev_matrix))
print(answer2)
