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
results <- data.frame("-", "-", "-", "-")
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
# Так как $y > 1$, поменяем $\lambda$, чтобы в системе не образовывалась
# бесконечная очередь.

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
            timeout(time_to_execute) %>%
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
# Реализуем Round Robin с помощью simmer. Для этого воспользуемся механизмом
# `leave` и `handle_unfinished`.
# Алгоритм реализации метода Round Robin:

# 1. Устанавливаем каждой программе, появившейся в системе, атрибут
# `execution_time` $\in Q$. В последующем этот атрибут будет обновляться, храня
# значение времени, которое осталось до выполнения программы.

# 2. Занимаем ресурс (сервер) на время $ = min(q, \text{execution_time})$.

# 3. Освобождаем ресурс (сервер).

# 4. Уменьшаем `execution_time` на `q`.

# 5. Если $\text{execution_time} <= 0$, программа выполнилась.

# 6. Если $\text{execution_time} > 0$, выводим программу из траектории с помощью
# `leave`.

# 7. Перехватываем программу, покинувшую траекторию, с помощью
# `handle_unfinished`, и отправляем её снова в эту же траекторию. Так, мы по факту
# освобождаем ресурс для следующих программ, а текущую программу отправляем
# в конец очереди, как это и нужно в Round Robin.

# Выполняем пункты 2-7 для каждой программы. Замечу, что при переходе программы
# в `handle_unfinished`, у нее будет обновленное значение `execution_time` (с
# вычтенным квантом).
# Следовательно, программа, проходя цикл, будет потихонечку исполняться.

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

if (!require("parallel")) {
    install.packages("parallel")
}
library(parallel)

# %% [markdown]
# Round Robin показывает очень колеблющиеся результаты в зависимости от порядка
# заявок, приходящих в систему. Поэтому воспользуемся пакетом parallel,
# позволяющим запустить параллельно N симуляций, используя всю мощь механизма
# `fork` операционных систем на базе Unix.

# Также запустим эти N симуляций для ряда $q$, чтобы посмотреть, как меняется
# эффективность алгоритма в зависимости от значения кванта.

# %%
SIMULATION_TIME <- 10000

rr.simulate <- function(simulation_time, q) {
    return(function(i) {
        rr.env <- simmer("SuperDuperRoundRobinSim")

        rr.execute_program_for_quant <- trajectory() %>%
            seize("server", 1) %>%
            timeout(function() min(get_attribute(rr.env, "execution_time"), q)) %>%
            set_attribute("execution_time", -q, mod = "+") %>%
            release("server", 1) %>%
            leave(
                function() get_attribute(rr.env, "execution_time") > 0
            )

        rr.programs <- trajectory() %>%
            set_attribute("execution_time", function() sample(Q, 1)) %>%
            handle_unfinished(
                trajectory() %>%
                    log_(
                        function() paste0("preempteed with execution_time: ", get_attribute(rr.env, "execution_time")),
                        level = 1
                    ) %>%
                    join(rr.execute_program_for_quant)
            ) %>%
            join(rr.execute_program_for_quant)

        rr.env %>%
            add_resource("server", 1, preemptive = TRUE) %>%
            add_generator(
                "programs",
                rr.programs,
                priority = 1,
                preemptible = 1,
                restart = TRUE,
                distribution = function() rexp(1, lambda)
            ) %>%
            run(until = simulation_time) %>%
            wrap()
    })
}

# %%
quants <- c(0.01 * q, 0.1 * q, q, 2 * q, 3 * q, 4 * q, 5 * q)

rr.results <- mclapply(quants, function(quant) {
    rr.envs <- mclapply(1:200, rr.simulate(SIMULATION_TIME, quant))

    rr.arrivals <- rr.envs %>%
        get_mon_arrivals()

    return(mean(rr.arrivals %>% with(end_time - start_time)))
})

# %%
unlist(rr.results)

# %%
rr.experiments <- data.frame(quant = quants, result = unlist(rr.results))
rr.experiments

# %%
results[4] <- unlist(rr.results)[1]
results

# %% [markdown]
## Вывод по Round Robin для разных $q$
# По полученным данным можно заметить, что система демонстрирует лучшие
# значения при малых $q \le 1$, причем чем квант меньше, тем ближе результат
# к SPT. При $q > 1$ система начинает стремиться к значениям, полученных при
# симуляции обычной $M/M/1/\infty$, и колебаться около них.

# Смею предположить, что данные также сильно зависят от вектора $Q$.
# Чем больше среднее значение $Q$, тем "неприхотливее" будет система к величине
# $q$, а значит критическое значение кванта, при котором система будет
# становиться эффективнее, будет выше.

# %% [markdown]
## Вывод
# Как видно, система, выполненная с помощью алгоритма Round Robin оказалась
# эффективнее обычной системы, а система, реализованная с алгоритмом SPT - самой
# эффективной (с точки зрения среднего времени пребывания в системе).
# эффективной (с точки зрения среднего времени пребывания в системе).
# эффективной (с точки зрения среднего времени пребывания в системе).
