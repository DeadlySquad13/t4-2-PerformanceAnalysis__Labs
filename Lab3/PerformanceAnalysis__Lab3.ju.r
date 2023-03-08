# %% [markdown]
# # Лабораторная работа 3. Модель однофазной одноканальной замкнутой системы обслуживания
# ## Задание 1

# %%
variant <- 5
set.seed(variant)

k <- sample(c(4:9), 1)

pp1 <- runif(4)
pp2 <- runif(3)
pp3 <- runif(2)

p1 <- pp1 / sum(pp1)
p2 <- c(c(0), pp2 / sum(pp2))
p3 <- c(c(0, 0), pp3 / sum(pp3))
p4 <- c(0, 0, 0, 1)

P <- data.frame()

P <- rbind(P, p1, p2, p3, p4)

rownames(P) <- c("p1", "p2", "p3", "p4")
colnames(P) <- c("", "", "", "")

View(P)

print(paste("k =", as.character(k)))

# %% [markdown]
# ### Численно
# Симулируем проход по матрице переходных вероятностей для трех сценариев:
# 1. k - 2 осмотров,
# 2. k - 1 осмотров,
# 3. k осмотров.

# Выполнив эту операцию N раз для каждого сценария, получим три вектора,
# содержащие N состояний. Это состояния, в которых оставалась модель после
# симуляции:
# $$
#   Scenario_i = \{State_1 ... State_N\}, i \in \{1, 2, 3\}, \\
#   State \in \{S1, S2, S3, S4\}
# $$

# %%
# Получаем значение, соответствующее отрезку.
# v - случайная величина. Если нужно сгенерировать распределение по `p`, нужно
# подать runif(1)
# X - величины, соответствующие отрезкам.
# За отрезки отвечает параметр p, хранящий последовательность длин.
get_value_in_range <- function(v, X, p) {
    border <- 0
    for (i in seq_along(p)) {
        border <- border + p[i]
        if (v < border) {
            return(X[i])
        }
    }
}


# %%
make_step <- function(current_state) {
    possible_states <- P[current_state, ]

    return(get_value_in_range(runif(1), 1:4, possible_states))
}

# %%
walk <- function(starting_state, times) {
    if (times < 0) {
        return(starting_state)
    }

    next_state <- make_step(starting_state)

    return(walk(next_state, times - 1))
}

# %%
N <- 10000
test_walk <- function(first_state, k) {
    results <- c()

    for (i in 1:N) {
        results <- append(results, walk(first_state, k))
    }

    return(results)
}

# %%
first_state <- 1 # Выбрали произвольное.

# %%
Scenario1 <- test_walk(first_state, k - 2)

# %%
Scenario2 <- test_walk(first_state, k - 1)

# %%
Scenario3 <- test_walk(first_state, k)

# %% [markdown]
# В полученных сценариях посчитаем вероятности получения каждого состояния.

# %%
get_probability <- function(States, state) {
    number_of_states <- length(States[States == state])
    number_of_all_states <- length(States)

    return(number_of_states / number_of_all_states)
}

# %%
get_probabilities_to_stay <- function(States) {
    return(
        unlist(
            lapply(seq_along(P), function(state) get_probability(States, state))
        )
    )
}

# %%
Scenario1Propabilities <- get_probabilities_to_stay(Scenario1)

# %%
Scenario2Propabilities <- get_probabilities_to_stay(Scenario2)

# %%
Scenario3Propabilities <- get_probabilities_to_stay(Scenario3)

# %%
results <- data.frame(
    Scenario1Propabilities,
    Scenario2Propabilities,
    Scenario3Propabilities
)
colnames(results) <- c("k - 2", "k - 1", "k")
rownames(results) <- rownames(P)

results

# %% [markdown]
# ### Теоретически

# %%
initial_state_probabilities <- c(1, 0, 0, 0)
transition_matrix <- data.matrix(P)
transition_matrix

# %%
if (!require("matrixcalc")) {
    install.packages("matrixcalc")
}

initial_state_probabilities %*% matrix.power(transition_matrix, k)

# %% [markdown]
# Как видно, теоретически вычисленная матрица с некоторой точностью совпадает
# со значениями, полученными теоретически. При увеличении количества
# экспериментов $N$ точность только увеличивается.

# %% [markdown]
# # Задание 2

# %%
variant <- 5
set.seed(variant)
k <- sample(c(10:25), 1)
t1 <- sample(c(14:20), 1)
t2 <- sample(c(2:5), 1)
View(data.frame(k, t1, t2))

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

set.seed(42)

env <- simmer("SuperDuperSim")
env

# %%
programmers <- trajectory("programmers' path") %>%
    ## add an intake activity
    seize("server", 1) %>%
    timeout(function() rnorm(1, 5)) %>%
    release("server", 1)

# %%
env %>%
    add_resource("server", 1) %>%
    add_generator("programmers", programmers, function() {
        return(10)
    })
# , function() rnorm(1, 0.1, 2))

# %%
env %>%
    run(50) %>%
    now()
env %>% peek(3)

# %%
env %>% get_n_generated("programmers")

# %%
env %>% get_queue_count("server")

# %%
env %>% get_mon_resources()
