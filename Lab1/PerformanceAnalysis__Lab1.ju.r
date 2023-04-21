# %% [markdown]
# # Лабораторная работа 1. Формализация постановок задач исследования КИС
# Выполнил: Пакало Александр Сергеевич, студент РТ5-81

# Вариант 5

# %% [markdown]
# ### Загрузка датасета

# %%
if (!require("hash")) {
    install.packages("hash")
}
library(hash)

data_path <- "https://raw.githubusercontent.com/junaart/ForStudents/master/KIS/Lab_1/Computers.csv"

computers <- read.csv(data_path)
View(computers)

p_price <- lm(price ~ speed + hd + ram + screen, computers)
p_price

# %% [markdown]
# ### Вычисление среднего значения по датасету

# %%
mean_ram <- mean(computers$ram)
mean_speed <- mean(computers$speed)
mean_hd <- mean(computers$hd)
mean_screen <- mean(computers$screen)

# %% [markdown]
# ### Ограничения

# %%
# C1
c1_restrictions <- hash()
c1_restrictions[["price"]] <- 80000
c1_restrictions[["min_n"]] <- 4
c1_restrictions[["max_n"]] <- 10
c1_restrictions[["hd"]] <- 500
c1_restrictions[["ram"]] <- 24
c1_restrictions[["screen"]] <- 15
c1_restrictions[["multi"]] <- 0
c1_restrictions[["speed"]] <- 70

# C2
c2_restrictions <- hash()
c2_restrictions[["price"]] <- 60000
c2_restrictions[["min_n"]] <- 13
c2_restrictions[["max_n"]] <- 32
c2_restrictions[["hd"]] <- 250
c2_restrictions[["ram"]] <- 12
c2_restrictions[["screen"]] <- 15
c2_restrictions[["multi"]] <- "yes"
c2_restrictions[["speed"]] <- 30

# C3
c3_restrictions <- hash()
c3_restrictions[["price"]] <- 60000
c3_restrictions[["min_n"]] <- 15
c3_restrictions[["max_n"]] <- 15
c3_restrictions[["hd"]] <- 300
c3_restrictions[["ram"]] <- 16
c3_restrictions[["screen"]] <- 15
c3_restrictions[["multi"]] <- "yes"
c3_restrictions[["speed"]] <- 40

# C4
c4_restrictions <- hash()
c4_restrictions[["price"]] <- 50000
c4_restrictions[["min_n"]] <- 1
c4_restrictions[["max_n"]] <- 8
c4_restrictions[["hd"]] <- 400
c4_restrictions[["ram"]] <- 16
c4_restrictions[["screen"]] <- 15
c4_restrictions[["multi"]] <- "yes"
c4_restrictions[["speed"]] <- 30


# %% [markdown]
# ### Критерии оптимизации

# %%
w1 <- c(0.4, 0.6)
w2 <- c(0.3, 0.7)
w3 <- 1
w4 <- 1
h2 <- c(0.35, 0.35, 0.15, 0.15)
h2_goal <- w1[1] * h2[1] * mean_speed + w1[2] * h2[1] * mean_ram +
    w2[1] * h2[2] * mean_ram + w2[2] * h2[2] * mean_hd +
    w3 * h2[3] * mean_speed +
    w4 * h2[4] * mean_hd

mean_hd <- c(
    max(mean_hd, c1_restrictions[["hd"]]), max(mean_hd, c2_restrictions[["hd"]]),
    max(mean_hd, c3_restrictions[["hd"]]), max(mean_hd, c4_restrictions[["hd"]])
)


mean_ram <- c(
    max(mean_ram, c1_restrictions[["ram"]]), max(mean_ram, c2_restrictions[["ram"]]),
    max(mean_ram, c3_restrictions[["ram"]]), max(mean_ram, c4_restrictions[["ram"]])
)


mean_speed <- c(
    max(mean_speed, c1_restrictions[["speed"]]), max(mean_speed, c2_restrictions[["speed"]]),
    max(mean_speed, c3_restrictions[["speed"]]), max(mean_speed, c4_restrictions[["speed"]])
)


mean_screen <- c(
    max(mean_screen, c1_restrictions[["screen"]]), max(mean_screen, c2_restrictions[["screen"]]),
    max(mean_screen, c3_restrictions[["screen"]]), max(mean_screen, c4_restrictions[["screen"]])
)

objective.in <- c(
    1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4],
    p_price$coefficients[5], 1, p_price$coefficients[2], p_price$coefficients[3],
    p_price$coefficients[4], p_price$coefficients[5], 1, p_price$coefficients[2],
    p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5],
    1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4],
    p_price$coefficients[5]
)

const.mat <- matrix(c(
    0, w1[1] * h2[1], w1[2] * h2[1], 0, 0, 0, 0, w2[1] * h2[2], w2[2] * h2[2], 0, 0, w3 * h2[3], 0, 0, 0, 0, w4 * h2[4], 0, 0, 0,
    1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5], 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, p_price$coefficients[2], p_price$coefficients[3], p_price$coefficients[4], p_price$coefficients[5],
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
), nrow = 45, byrow = TRUE)

const.dir <- c(
    ">=", "<=", "<=", "<=", "<=",
    ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
    ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
    ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<="
)

const.rhs <- c(
    h2_goal, c1_restrictions[["price"]], c2_restrictions[["price"]], c3_restrictions[["price"]], c4_restrictions[["price"]],
    c1_restrictions[["min_n"]] * p_price$coefficients[1], c1_restrictions[["max_n"]] * p_price$coefficients[1],
    c1_restrictions[["min_n"]] * mean_speed[1], c1_restrictions[["max_n"]] * mean_speed[1],
    c1_restrictions[["min_n"]] * mean_hd[1], c1_restrictions[["max_n"]] * mean_hd[1],
    c1_restrictions[["min_n"]] * mean_ram[1], c1_restrictions[["max_n"]] * mean_ram[1],
    c1_restrictions[["min_n"]] * mean_screen[1], c1_restrictions[["max_n"]] * mean_screen[1],
    c2_restrictions[["min_n"]] * p_price$coefficients[1], c2_restrictions[["max_n"]] * p_price$coefficients[1],
    c2_restrictions[["min_n"]] * mean_speed[2], c2_restrictions[["max_n"]] * mean_speed[2],
    c2_restrictions[["min_n"]] * mean_hd[2], c2_restrictions[["max_n"]] * mean_hd[2],
    c2_restrictions[["min_n"]] * mean_ram[2], c2_restrictions[["max_n"]] * mean_ram[2],
    c2_restrictions[["min_n"]] * mean_screen[2], c2_restrictions[["max_n"]] * mean_screen[2],
    c3_restrictions[["min_n"]] * p_price$coefficients[1], c3_restrictions[["max_n"]] * p_price$coefficients[1],
    c3_restrictions[["min_n"]] * mean_speed[3], c3_restrictions[["max_n"]] * mean_speed[3],
    c3_restrictions[["min_n"]] * mean_hd[3], c3_restrictions[["max_n"]] * mean_hd[3],
    c3_restrictions[["min_n"]] * mean_ram[3], c3_restrictions[["max_n"]] * mean_ram[3],
    c3_restrictions[["min_n"]] * mean_screen[3], c3_restrictions[["max_n"]] * mean_screen[3],
    c4_restrictions[["min_n"]] * p_price$coefficients[1], c4_restrictions[["max_n"]] * p_price$coefficients[1],
    c4_restrictions[["min_n"]] * mean_speed[4], c4_restrictions[["max_n"]] * mean_speed[4],
    c4_restrictions[["min_n"]] * mean_hd[4], c4_restrictions[["max_n"]] * mean_hd[4],
    c4_restrictions[["min_n"]] * mean_ram[4], c4_restrictions[["max_n"]] * mean_ram[4],
    c4_restrictions[["min_n"]] * mean_screen[4], c4_restrictions[["max_n"]] * mean_screen[4]
)


# %%
if (!require("lpSolve")) {
    install.packages("lpSolve", repos = "http://cran.rstudio.com")
}
library(lpSolve)

res <- lp("min", objective.in, const.mat, const.dir, const.rhs)
res$constraints

# %% [markdown]
# ### Проверка
# Составим функции для расчета H1 и H2.

# %%
hh_1 <- function(p) {
    return(sum(c(
        1, p_price$coefficients[2], p_price$coefficients[3],
        p_price$coefficients[4], p_price$coefficients[5],
        1, p_price$coefficients[2], p_price$coefficients[3],
        p_price$coefficients[4], p_price$coefficients[5],
        1, p_price$coefficients[2], p_price$coefficients[3],
        p_price$coefficients[4], p_price$coefficients[5],
        1, p_price$coefficients[2], p_price$coefficients[3],
        p_price$coefficients[4], p_price$coefficients[5]
    ) * p))
}

hh_2 <- function(p) {
    return(sum(c(
        0, w1[1] * h2[1], w1[2] * h2[1], 0, 0, 0, 0, w2[1] * h2[2], w2[2] * h2[2],
        0, 0, w3 * h2[3], 0, 0, 0, 0, w4 * h2[4], 0, 0, 0
    ) * p))
}

hh_1(res$solution)
hh_2(res$solution)

# %% [markdown]
# ### Фильтрация записей в соответствии с задаными ограничениями

# %%
apply_restrictions <- function(data, restrictions) {
    new_data <- data.frame()

    for (i in seq_along(rownames(data))) {
        speed_restrictions_met <- data[i, 3] >= restrictions[["speed"]]
        hd_restrictions_met <- data[i, 4] >= restrictions[["hd"]]
        screen_restrictions_met <- data[i, 6] >= restrictions[["screen"]]
        ram_restrictions_met <- data[i, 5] >= restrictions[["ram"]]
        multi_restrictions_met <- restrictions[["multi"]] != 0 && data[i, 8] == restrictions[["multi"]] || restrictions[["multi"]] == 0

        if (speed_restrictions_met &&
            hd_restrictions_met &&
            screen_restrictions_met &&
            ram_restrictions_met &&
            multi_restrictions_met
        ) {
            new_data <- rbind(new_data, data[i, ])
        }
    }

    return(new_data)
}

# %% [markdown]
# ### Нормализация значений

# %%
normalize <- function(data) {
    new_data <- data

    mmax <- max(data$price)
    mmin <- min(data$price)
    price <- (mmax - new_data$price) * 100 / (mmax - mmin)

    mmax <- max(data$speed)
    mmin <- min(data$speed)
    speed <- (new_data$speed - mmin) * 100 / (mmax - mmin)

    mmax <- max(data$hd)
    mmin <- min(data$hd)
    hd <- (new_data$hd - mmin) * 100 / (mmax - mmin)

    mmax <- max(data$ram)
    mmin <- min(data$ram)
    ram <- (new_data$ram - mmin) * 100 / (mmax - mmin)

    mmax <- max(data$screen)
    mmin <- min(data$screen)
    screen <- (new_data$screen - mmin) * 100 / (mmax - mmin)

    result <- data.frame(price, speed, hd, ram, screen)
    rownames(result) <- rownames(new_data)

    return(result)
}

# %% [markdown]
# ### Нахождение парето множества

# %%
d_pareto <- function(X, Y) {
    p <- TRUE
    l <- FALSE
    i <- 1
    while (p && (i <= length(X))) {
        if (X[i] < Y[i]) p <- FALSE
        if (X[i] > Y[i]) l <- TRUE
        i <- i + 1
    }
    if (!p || !l) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

# %% [markdown]
# ### Нахождение парето оптимальных решений

# %%
pareto_opt <- function(data) {
    result <- c()
    for (i in seq_along(rownames(data))) {
        p <- TRUE

        for (j in seq_along(rownames(data))) {
            if (d_pareto(data[j, ], data[i, ])) {
                p <- FALSE
            }
        }

        if (p) {
            result <- c(result, rownames(data)[i])
        }
    }
    return(result)
}

distance <- function(A, B) {
    return(sqrt(sum((A - B)^2)))
}

# %%
new_computers <- list()
pareto <- list()
ideal <- list()

restrs <- c(c1_restrictions, c2_restrictions, c3_restrictions, c4_restrictions)
solutions <- c(2, 7, 12, 17)

for (i in 1:4) {
    restr_computers <- apply_restrictions(computers, restrs[[i]])
    new_computers[[i]] <- restr_computers

    norm_computers <- normalize(restr_computers)
    pareto[[i]] <- c(pareto_opt(norm_computers))

    n <- res$solution[i] / p_price$coefficients[i]
    s_i <- solutions[i]
    ideal[[i]] <- c(res$solution[s_i] / n, res$solution[s_i + 1] / n, res$solution[s_i + 2] / n, res$solution[s_i + 3] / n)
}

for (i in 1:4) {
    k <- pareto[[i]][1]
    k_min <- distance(ideal[[i]], new_computers[[i]][k, c(3:6)])
    print(paste("Для C", i, ":", sep = ""))
    for (j in pareto[[i]]) {
        print(paste(j, ":", as.character(distance(ideal[[i]], new_computers[[i]][j, c(3:6)]))))
        if (distance(ideal[[i]], new_computers[[i]][j, c(3:6)]) < k_min) {
            k_min <- distance(ideal[[i]], new_computers[[i]][j, c(3:6)])
            k <- j
        }
    }
    cat("\n")
    print(paste("Лучший для C", i, ": ", k, " - ", k_min, sep = ""))
    cat("\n")
}

# %% [markdown]
# ## Дополнительные задания

# %%
if (!require("combinat")) {
    install.packages("combinat")
}
library(combinat)


# %%
get_decision_from_probability <- function(prob) {
    res <- sample(c(1, 0), size = 1, replace = TRUE, prob = c(prob, 1 - prob))
    return(res)
}

# %% [markdown]
# ### Задача 1
# В комиссии из 5 человек 4 члена принимают независимо друг от друга правильное
# решение с вероятностью 0.9, а пятый для принятия решения бросает монету.
# Окончательное решенние принимается большинством голосов. Кто с большей
# вероятностью принимает правильное решение: комиссия или один человек из
# комиссии?

# %%
N <- 10000
persons_count <- 5
overall_true_count <- 0
probabilities <- c(0.9, 0.9, 0.9, 0.9, 0.5)

for (i in 1:N) {
    true_count <- 0

    for (probability in probabilities) {
        result <- get_decision_from_probability(probability)
        if (result == 1) {
            true_count <- true_count + 1
        }
    }

    if (true_count > persons_count %/% 2) {
        overall_true_count <- overall_true_count + 1
    }
}

sprintf("Комиссия: %s", overall_true_count / N)

one_person_probability <- sum(rep(0.9, 4), 0.5) / 5
sprintf("Один человек: %s", one_person_probability)

# %% [markdown]
# ### Задача 2
# Три стрелка производят по одному выстрелу. Вероятность попадания в цель
# каждого стрелка равна 0.9; 0.8; 0.85 соответственно. Найти вероятность того,
# что в цель попадут только два стрелка?

# %%
overall <- 0
probabilities <- c(0.9, 0.8, 0.85)

for (i in 1:N) {
    in_target <- 0
    for (probability in probabilities) {
        result <- get_decision_from_probability(probability)

        if (result == 1) {
            in_target <- in_target + 1
        }
    }

    if (in_target == 2) {
        overall <- overall + 1
    }
}

overall / N

# %% [markdown]
# ### Задача 3
# Василий попадает по мячу один раз из трех. Какова вероятность, что он попадет
# три раза из десяти?

# %%
overall <- 0
for (i in 1:N) {
    in_target <- 0
    for (probability in 1:10) {
        result <- get_decision_from_probability(1 / 3)
        if (result == 1) {
            in_target <- in_target + 1
        }
    }
    if (in_target == 3) {
        overall <- overall + 1
    }
}

overall / N

# %% [markdown]
# ### Задача 4
# Вероятность рождения девочки 0.51, а мальчика 0.49. Если в семье Василия
# пятеро детей,то какова вероятность, что в ней три девочки?

# %%
overall <- 0
for (i in 1:N) {
    in_target <- 0
    for (probability in 1:5) {
        result <- get_decision_from_probability(0.51)
        if (result == 1) {
            in_target <- in_target + 1
        }
    }
    if (in_target == 3) {
        overall <- overall + 1
    }
}

overall / N

# %% [markdown]
# ### Задача 5
# В двух коробках лежат зеленые и красные мячи: в первой - 4 зеленых
# и 5 красных, во второй 7 зеленых и 3 красных. Из второй коробки наугад взяли
# мяч и переложили его в первую коробку. Найти вероятность того, что наугад
# взятый после этого из первой коробки шар будет зеленым.

# %%
overall <- 0
for (i in 1:N) {
    g <- 4
    r <- 5

    from_second <- get_decision_from_probability(0.7)
    if (from_second == 1) {
        g <- g + 1
    } else {
        r <- r + 1
    }

    from_first <- get_decision_from_probability(g / (r + g))

    if (from_first == 1) {
        overall <- overall + 1
    }
}

overall / N
