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
# Заведем таблицу результатов

# %%
results <- data.frame(0, 0, 0, 0)
colnames(results) <- c("M/M/1/infty theoretical", "M/M/1/infty practical", "SPT", "Round Robin")
results

# %% [markdown]
# ### СМО вида $М/М/1/\infty$
# Представим данную систему как одноканальную СМО с неограниченной очередью.

# %% [markdown]
# #### Теоретически

# %%
t2 <- mean(Q)
mu <- 1 / t2
mu

# %%
y <- lambda / mu
y

# %% [markdown]
# Так как $y > 1$, поменяем $\lambda$.

# %%
lambda <- 0.3

# %%
t2 <- mean(Q)
mu <- 1 / t2
mu

# %%
y <- lambda / mu
y

# %%
results[1] <- 1 / mu / (1 - y)
results

# %% [markdown]
# #### Численно

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

env <- simmer("SuperDuperSim")
env

# %%
programs <- trajectory("programs' path") %>%
    seize("server", amount = 1) %>%
    timeout(function() rexp(1, mu)) %>%
    release("server", amount = 1)

# %%
SIMULATION_TIME <- 10000

env %>%
    add_resource("server", 1) %>%
    add_generator("programs", programs, function() rexp(1, lambda)) %>%
    run(until = SIMULATION_TIME)

# %%
arrivals <- env %>%
    get_mon_arrivals()
arrivals

# %%
results[2] <- mean(arrivals %>% with(end_time - start_time))
results

# %% [markdown]
# ### Алгоритм SPT

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

spt.env <- simmer("SuperDuperSptSim")
spt.env

# %% [markdown]
# Добавим $m$ генераторов. Каждый будет иметь приоритет в зависимости от
# скорости выполнения программы. Генератор, создающий программы с наибольшей
# длительностью выполнения, будет иметь наименьший приоритет.

# %%
SIMULATION_TIME <- 100000

spt.env %>%
    add_resource("server", 1)

Q_sorted_decr <- sort(Q, decreasing = TRUE)

spt.programs_trajectory <- function(time_to_execute) {
    return(
        trajectory("programs' path") %>%
            seize("server", 1) %>%
            timeout(function() rexp(1, 1 / time_to_execute)) %>%
            release("server", 1)
    )
}

for (Q_i in seq_along(Q_sorted_decr)) {
    priority <- Q_i

    name <- paste0("programs", Q_i)

    spt.env %>% add_generator(
        name,
        spt.programs_trajectory(Q_sorted_decr[Q_i]),
        priority = priority,
        preemptible = priority,
        distribution = function() rexp(1, lambda / m)
    )
}

spt.env %>%
    run(until = SIMULATION_TIME)

# %%
spt.arrivals <- spt.env %>%
    get_mon_arrivals()
spt.arrivals

# %%
results[3] <- mean(spt.arrivals %>% with(end_time - start_time))
results


# %% [markdown]
# ### Алгоритм Round Robin
# Реализуем Round Robin с помощью simmer, воспользовавшись механизмом select,
# выбирающим из очереди значение по определенной стратегии.

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

env <- simmer("SuperDuperSim")
env

# %%
programs <- trajectory("programs' path") %>%
    select("server", "round-robin") %>%
    seize_selected(1) %>%
    timeout(function() rexp(1, mu)) %>%
    release_selected(1)

# %%
SIMULATION_TIME <- 10000

env %>%
    add_resource("server", 1) %>%
    add_generator("programs", programs, function() rexp(1, lambda)) %>%
    run(until = SIMULATION_TIME)

# %%
arrivals <- env %>%
    get_mon_arrivals()
arrivals

# %%
results[5] <- mean(arrivals %>% with(end_time - start_time))
results

# %% [markdown]
# Как видно, практические вычисления совпадают теоретическими с некоторой
# погрешностью, которая уменьшается при увеличении числа экспериментов.

# При этом система, выполненная с помощью алгоритма Round Robin оказалась
# быстрее обычной системы, а система, реализованная с алгоритмом SPT - самой
# быстрой.
