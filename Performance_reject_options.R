# load package:
require(FFTrees)


# build example tree with heartdisease: ----
heart.fft <- FFTrees(formula = diagnosis ~ .,
                     data = heartdisease,
                     main = "Heart Disease")

# plot results:
plot(heart.fft)


# get necessary variables: ----
# define summary statistic:
x_summary <- summary(heart.fft)

# number of cases:
n <- x_summary$stats$train$n[1]

# number of hits:
HIT <- x_summary$stats$train$hi[1]

# number of false alarms:
FA <- x_summary$stats$train$fa[1]

# number of misses:
MI <- x_summary$stats$train$mi[1]

# number of correct rejections:
CR <- x_summary$stats$train$cr[1]

# define number of nodes that are in the tree:
nodes <- x_summary$definitions$nodes[1]


# define percentage that is missing in every variable of the data: ----
pc <- 0.25 # for example



# In this variant all data with missings is passed on to the next node and processed there.
# Only missings in all variables (which are used for the FFTree) are then put in category IDK.

# number of that category:
p <- pc^nodes
p


# build table with new performance data:

# Define the values for each cell:
hit <- HIT - (HIT * p)
IDK_hit <- HIT * p
IDK_mi <- MI * p
mi <- MI - (MI * p)
fa <- FA - (FA * p)
IDK_fa <- FA * p
IDK_cr <- CR * p
cr <- CR - (CR * p)

# collect in vector:
values <- c(hit, IDK_hit, IDK_mi, mi, fa, IDK_fa, IDK_cr, cr)

# Define the column and row names:
col_names <- c("TRUE (reality)", "FALSE (reality)")
row_names <- c("TRUE (decision)", "IDK_TRUE (decision)", "IDK_FALSE (decision)", "FALSE (decision)")

# Create a matrix with the values:
matrix_values <- matrix(values, nrow = length(row_names), ncol = length(col_names))

# Create a data frame:
table <- data.frame(matrix_values, row.names = row_names)

# Rename the columns:
colnames(table) <- col_names

# Print the table:
print(table)


# check whether sum is correct:
sum(as.matrix(table))
