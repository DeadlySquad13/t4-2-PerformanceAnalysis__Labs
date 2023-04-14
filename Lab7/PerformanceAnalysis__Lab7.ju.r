# %% [markdown]
# # Лабораторная работа 7. Управление ресурсами в однопроцессорной системе с неоднородными заявками
# ## Задание 1

# %%
Variant <- 5
set.seed(Variant)
m <- sample(c(6:20), 1)
lambda <- runif(1, 0.1, 2)
Q <- rexp(m, 0.3)
q <- sample(c(1:4), 1)
View(data.frame(m, q, lambda))
print(Q)

# %%
N <- 10000
# N <- 10
programs <- sample(Q, N, replace = TRUE)
programs

# %%
s <- c(1, 2, 3)
tail(s, length(s) - 1)

# %% [markdown]
# Реализуем round robin.

# %%
time <- 0

task_schedule <- programs

while (length(task_schedule) > 0) {
    time <- time + q
    task_schedule[1] <- task_schedule[1] - q

    if (task_schedule[1] <= 0) {
        task_schedule <- tail(task_schedule, length(task_schedule) - 1)
    }
}

time / N

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

env <- simmer("SuperDuperSim")
env

# %%
programs <- trajectory("programs' path") %>%
    select(paste0("doctor", 1:3), "round-robin") %>%
    seize_selected(1) %>%
    timeout(5) %>%
    release_selected(1)

# %%
env %>%
    add_resource("server", 1) %>%
    add_generator("programmers", programs, function() rexp(1, k / t1))
