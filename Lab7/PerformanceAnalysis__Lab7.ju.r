# %% [markdown]
# # Лабораторная работа 7. Управление ресурсами в однопроцессорной системе с неоднородными заявками
# Выполнил: Пакало Александр Сергеевич, студент РТ5-81Б

# ## Задание 1
# В однопроцессорную систему случайным образом поступают на выполнение $m$
# разных типов программ, отличающихся известной трудоемкостью
# $Q_1, Q_2,\dots, Q_m$.
# Входящий поток простейший с интенсивностью $\lambda$.

# Представив данную систему как одноканальную СМО с неограниченной очередью,
# вычислить среднее время обслуживания программ, считая длительность
# обслуживания случайной величиной (теоретически и экспериментально).

# Реализовать алгоритм SPT, выбирая из очереди заявки в соответствии с их
# приоритетом по трудоемкости. Рассчитать среднее время обслуживания программ.
# Сравнить полученные результаты.

# Реализовать алгоритм RR при заданном кванте времени $q$. Оценить среднее время
# обслуживания программ. Сравнить полученные результаты. Выяснить, как влияет
# величина кванта на среднее время обслуживания программ.

# %%
Variant <- 5
set.seed(Variant)
m <- sample(c(6:20), 1)
lambda <- runif(1, 0.1, 2)
Q <- rexp(m, 0.3)
q <- sample(c(1:4), 1)
View(data.frame(m, q, lambda))
print(Q)

# %% [markdown]
# ### СМО вида $М/М/1/\infty$
# Представим данную систему как одноканальную СМО с неограниченной очередью.

# #### Теоретически
# #### Численно

# %% [markdown]
# ### Алгоритм SPT

# #### Теоретически
# Среднее время обслуживания складывается из ожидания в очереди и времени
# выполнения, усредненным по всем заявкам:
# $$
# T_{\text{сист}} = \frac{1}{m} \left( Q'_1 + \left( Q'_1 + Q'_2 \right) +
# \left(Q'_1 + Q'_2 + Q'_3 \right) + \dots + \sum^m_{i = 1}Q'_i \right)
# $$
# где $Q'_i$ - $i$-й элемент массива $Q'$,
# **отсортированного по возрастанию** массива Q.

# %%
Q_sorted <- sort(Q)
Q_sorted

# %% [markdown]
# Суммы первых i элементов:

# %%
Q_progression_sums <- lapply(
    seq_along(Q_sorted),
    function(i) sum(head(Q_sorted, i))
)
Q_progression_sums


# %% [markdown]
# Итоговая сумма

# %%
sum_of_Q_progression_sums <- sum(unlist(Q_progression_sums))
sum_of_Q_progression_sums

# %%
T_spt <- 1 / m * sum_of_Q_progression_sums
T_spt

# %% [markdown]
# #### Численно

# %% [markdown]
# ### Алгоритм Round Robin
# Реализуем round robin.

# %%
N <- 10000
programs <- sample(Q, N, replace = TRUE)
programs


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
add_generator("programmers", programs, function() rexp(1, k / t1))
