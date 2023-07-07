
```{r IDK_every_node}
# get necessary variables: ----
cost_unassigned <- cost_IDK_hi

# define summary statistics:
x_summary <- summary(heart.fft)
x_level_stats <- heart.fft$trees$level_stats$train

# number of cases:
n <- x_summary$stats$train$n[1]

# number of hits, false alarms, misses and correct rejections per level in pc:
# level 1:
HIT_1 <- x_level_stats$hi[1]
HIT_1_pc <- (HIT_1/ n)
FA_1 <- x_level_stats$fa[1]
FA_1_pc <- (FA_1/ n)
MI_1 <- x_level_stats$mi[1]
MI_1_pc <- (MI_1/ n)
CR_1 <- x_level_stats$cr[1]
CR_1_pc <- (CR_1/ n)

# calculate new n:
n_after_1 <- n - sum(HIT_1, FA_1, MI_1, CR_1)

# level 2:
HIT_2 <- x_level_stats$hi[2] - x_level_stats$hi[1]
HIT_2_pc <- (HIT_2/ n_after_1)
FA_2 <- x_level_stats$fa[2] - x_level_stats$fa[1]
FA_2_pc <- (FA_2/ n_after_1)
MI_2 <- x_level_stats$mi[2] - x_level_stats$mi[1]
MI_2_pc <- (MI_2/ n_after_1)
CR_2 <- x_level_stats$cr[2] - x_level_stats$cr[1]
CR_2_pc <- (CR_2/ n_after_1)

# calculate new n:
n_after_2 <- n_after_1 - sum(HIT_2, FA_2, MI_2, CR_2)

# level 3:
HIT_3 <- x_level_stats$hi[3] - (x_level_stats$hi[1] + (x_level_stats$hi[2]-x_level_stats$hi[1]))
HIT_3_pc <- (HIT_3/ n_after_2)
FA_3 <- x_level_stats$fa[3] - (x_level_stats$fa[1] + (x_level_stats$fa[2]-x_level_stats$fa[1]))
FA_3_pc <- (FA_3/ n_after_2)
MI_3 <- x_level_stats$mi[3] - (x_level_stats$mi[1] + (x_level_stats$mi[2]-x_level_stats$mi[1]))
MI_3_pc <- (MI_3/ n_after_2)
CR_3 <- x_level_stats$cr[3] - (x_level_stats$cr[1] + (x_level_stats$cr[2]-x_level_stats$cr[1]))
CR_3_pc <- (CR_3/ n_after_2)


# define the new values of hi, fa, mi, cr and IDK ----
# In this case p% in each variable will be excluded directly since they are missings and cannot be classified:

# first node:
IDK_1 <- n * pc
IDK_1_hi <- n * pc * HIT_1_pc
IDK_1_fa <- n * pc * FA_1_pc
IDK_1_mi <- n * pc * MI_1_pc
IDK_1_cr <- n * pc * CR_1_pc
IDK_1_unassigned <- IDK_1 - sum(IDK_1_hi, IDK_1_fa, IDK_1_mi, IDK_1_cr)

# new hi, fa, mi, cr values without excluded missings:
n1_pc_gone_only <- n - (n * pc)
hit_1 <- HIT_1_pc * n1_pc_gone_only
fa_1 <- FA_1_pc * n1_pc_gone_only
mi_1 <- MI_1_pc * n1_pc_gone_only
cr_1 <- CR_1_pc * n1_pc_gone_only


# second node:
n1 <- n1_pc_gone_only -(sum(hit_1, fa_1, mi_1, cr_1))
IDK_2<- n1 * pc
IDK_2_hi <- n1 * pc * HIT_2_pc
IDK_2_fa <- n1 * pc * FA_2_pc
IDK_2_mi <- n1 * pc * MI_2_pc
IDK_2_cr <- n1 * pc * CR_2_pc
IDK_2_unassigned <- IDK_2 -sum(IDK_2_hi, IDK_2_fa, IDK_2_mi, IDK_2_cr)


# new hi, fa, mi, cr values without excluded missings:
n2_pc_gone_only <- n1 - (n1 * pc)
hit_2 <- HIT_2_pc * n2_pc_gone_only
fa_2 <- FA_2_pc * n2_pc_gone_only
mi_2 <- MI_2_pc * n2_pc_gone_only
cr_2 <- CR_2_pc * n2_pc_gone_only

# third node:
n2 <- n2_pc_gone_only -(sum(hit_2, fa_2, mi_2, cr_2))
IDK_3<- n2 * pc
IDK_3_hi <- n2 * pc * HIT_3_pc
IDK_3_fa <- n2 * pc * FA_3_pc
IDK_3_mi <- n2 * pc * MI_3_pc
IDK_3_cr <- n2 * pc * CR_3_pc

# new hi, fa, mi, cr values without excluded missings:
n3 <- n2 - (n2 * pc)
hit_3 <- HIT_3_pc * n3
fa_3 <- FA_3_pc * n3
mi_3 <- MI_3_pc * n3
cr_3 <- CR_3_pc * n3


# build table with new performance data: ----

# Define the values for each cell:
hit <- sum(hit_1, hit_2, hit_3)
IDK_hit <- sum(IDK_1_hi, IDK_2_hi, IDK_3_hi)
IDK_mi <- sum(IDK_1_mi, IDK_2_mi, IDK_3_mi)
mi <- sum(mi_1, mi_2, mi_3)
fa <- sum(fa_1, fa_2, fa_3)
IDK_fa <- sum(IDK_1_fa, IDK_2_fa, IDK_3_fa)
IDK_cr <- sum(IDK_1_cr, IDK_2_cr, IDK_3_cr)
cr <- sum(cr_1, cr_2, cr_3)

IDK_else_unassigned <- sum(IDK_1, IDK_2, IDK_3) - sum(IDK_hit, IDK_mi, IDK_fa, IDK_cr)

# collect in vector:
values <- c(hit, IDK_hit, IDK_mi, mi, fa, IDK_fa, IDK_cr, cr, 0, IDK_else_unassigned, 0, 0)

# Define the column and row names:
col_names <- c("TRUE (reality)", "FALSE (reality)", "IDK_else_unassigned")
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


# Calculate the percentages:----
# Define the multiplication factor:
# Divide each cell by n:
pc_table <- table %>%
  mutate_all(~ . / n)

# new column names:
col_names <- c("TRUE (reality) %", "FALSE (reality) %", "IDK_else_unassigned %")

# Rename the columns:
colnames(pc_table) <- col_names

# Print the new data frame
print(pc_table)

# check whether sum is correct:
sum(as.matrix(pc_table))

# calculating costs: ----

# probabilities taken from the table above:
IDK_all_costs <- pc_table$`TRUE (reality) %`[1] * cost_hi +
  pc_table$`TRUE (reality) %`[2] * cost_IDK_hi +
  pc_table$`TRUE (reality) %`[3] * cost_IDK_mi +
  pc_table$`TRUE (reality) %`[4] * cost_mi +
  pc_table$`FALSE (reality) %`[1] * cost_fa +
  pc_table$`FALSE (reality) %`[2] * cost_IDK_fa +
  pc_table$`FALSE (reality) %`[3] * cost_IDK_cr +
  pc_table$`FALSE (reality) %`[4] * cost_cr
pc_table$`IDK_else_unassigned %`[2] * cost_unassigned

```

