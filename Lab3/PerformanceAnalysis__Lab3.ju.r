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
P <- rbind(P, p1)
P <- rbind(P, p2)
P <- rbind(P, p3)
P <- rbind(P, p4)
rownames(P) <- c("p1", "p2", "p3", "p4")
colnames(P) <- c("", "", "", "")
View(P)
print(paste("k =", as.character(k)))

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
N <- 1000
test_walk <- function(first_state, k) {
    results <- c()

    for (i in 1:N) {
        results <- append(results, walk(first_state, k))
    }

    return(results)
}

# %%
first_state <- 1

# %%
S1 <- test_walk(first_state, k - 2)

# %%
S2 <- test_walk(first_state, k - 1)

# %%
S3 <- test_walk(first_state, k)

# %%
get_probability <- function(States, state) {
    number_of_states <- length(States[States == state])
    number_of_all_states <- length(States)

    return(number_of_states / number_of_all_states)
}

# %%
get_probabilities_to_stay <- function(States) {
    return(
        lapply(seq_along(P), function(state) get_probability(States, state))
    )
}

# %%
get_probabilities_to_stay(S1)

# %%
get_probabilities_to_stay(S2)

# %%
get_probabilities_to_stay(S3)

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
