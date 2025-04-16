# === Вхідні дані ===
set.seed(42)
n <- 100 min_val <- 1
max_val <- 1000
alphabet_power <- 10 alphabet <- LETTERS[1:alphabet_power]
# Кількість чисел
# Потужність алфавіту
# === Крок 1: Генерація числового ряду ===
num_list <- sample(min_val:max_val, n, replace = TRUE)
sorted_list <- sort(num_list)
# === Крок 2: Розбиття на рівномірні інтервали ===
breaks <- seq(min(sorted_list), max(sorted_list), length.out =
alphabet_power + 1)
# === Крок 3: Перетворення чисел у літери алфавіту ===
get_letter <- function(x, breaks, alphabet) {
idx <- findInterval(x, breaks, rightmost.closed = TRUE)
alphabet[idx]
}
linguistic_seq <- sapply(num_list, get_letter, breaks = breaks,
alphabet = alphabet)
# === Крок 4: Виведення лінгвістичного ряду ===
cat("Лінгвістичний ряд:\n")
cat(paste(linguistic_seq, collapse = " "), "\n\n")
# === Крок 5: Побудова матриці передування ===
matrix_size <- length(alphabet)
transition_matrix <- matrix(0, nrow = matrix_size, ncol =
matrix_size,
dimnames = list(alphabet, alphabet))
for (i in 1:(length(linguistic_seq) - 1)) {
row <- linguistic_seq[i]
col <- linguistic_seq[i + 1]
transition_matrix[row, col] <- transition_matrix[row, col] + 1
}
# === Вивід матриці передування ===
cat("Матриця передування:\n")
print(transition_matrix)