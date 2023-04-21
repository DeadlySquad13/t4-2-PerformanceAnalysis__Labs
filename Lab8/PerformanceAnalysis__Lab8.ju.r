# %% [markdown]
# # Лабораторная работа 8. Анализ одноканальной системы массового обслуживания с учетом приоритета заявок
# Выполнил студент РТ5-81Б Пакало Александр Сергеевич

# %% [markdown]
# Задание 1.
# Система электронного документооборота с автоматическим классификатором
# поступающей корреспонденции должна учитывать различную важность документов,
# количество типов документов $P$,
# средняя интенсивность поступления заявок
# $\lambda_1, \lambda_2, \dots, \lambda_P$,
# средняя интенсивность обслуживания
# $\mu_1, \mu_2, \dots, \mu_P$ соответственно, очередь неограничена.
# Приоритеты заявок
# $p_1 < p_2 < \dots < p_P$.

# Оценить среднее время нахождения каждого типа заявки в очереди (теоретически
# и экспериментально) для одного из 4 случаев:

# - относительный фиксированный приоритет (V1);
# - абсолютный фиксированный приоритет (V2);
# - приоритет с зависимой задержкой без прерывания (V3);
# - приоритет с зависимой задержкой с прерываниями (V4);

# в соответствии с вариантом.
# %%
Variant <- 5
set.seed(Variant)
V <- sample(c("V1", "V2", "V3", "V4"), 1)
P <- sample(c(4, 6), 1)
if ((V == "V3") | (V == "V4")) {
    b <- sort(sample(c(1:10), P))
}
lambda <- runif(P)
mu <- runif(P, 1, 3)
View(data.frame(P, V))
if ((V == "V3") | (V == "V4")) {
    View(data.frame(lambda, mu, b))
}
if ((V == "V1") | (V == "V2")) {
    View(data.frame(lambda, mu))
}

# %%
sum(lambda / mu)

# %% [markdown]
# Как видно, суммарная нагрузка на систему превышает 1, следовательно будет
# накапливаться бесконечная очередь. Немного изменим
# исходные данные.

# %%
lambda[1] <- 0.1
lambda[2] <- 0.1
ro <- sum(lambda / mu)
ro

# %% [markdown]
# ### Теоретически
# $$\lambda=\sum_{p=1}^P\lambda_p$$

# %%
lambda_ <- sum(lambda)
lambda_

# %% [markdown]
# $$\frac{1}{\mu}=\sum_{p=1}^P \frac{\lambda_p}{\lambda}\cdot \frac{1}{\mu_p}$$

# %%
ro <- lambda / mu

mu_ <- sum(ro) / lambda_
mu_


# %% [markdown]
# $$\rho=\frac{\lambda}{\mu}$$

# %%
ro_ <- lambda_ / mu_
ro_

# %% [markdown]
# $$W_0=\sum_{p=1}^P\frac{\rho_p}{\mu_p}$$

# %%
W0 <- sum(ro / mu)
W0


# %% [markdown]
# Для получения $W_p$ воспользуемся формулой

# $$W_p= \frac{\frac{\rho_p}{\mu_p}+\sum_{i=p+1}^P \rho_i\cdot \left(\frac{1}{\mu_p}+\frac{1}{\mu_i}\right)+\sum_{i=p+1}^P\rho_i\cdot W_i}{1-\sum_{i=p}^P \rho_i}$$

# %%
get_Wqueue <- function(p) {
    numerator <- ro[p] / mu[p]
    denominator <- 1 - sum(ro[p:P])

    if (p == P) {
        return(numerator / denominator)
    }

    sum_part1 <- sum(unlist(
        lapply(
            c((p + 1):P),
            function(i) ro[i] * (1 / mu[p] + 1 / mu[i])
        )
    ))

    sum_part2 <- sum(unlist(
        lapply(
            c((p + 1):P),
            function(i) ro[i] * get_Wqueue(i)
        )
    ))

    result <- (numerator + sum_part1 + sum_part2) / denominator


    if (result < 0) {
        return(Inf)
    }

    return(result)
}

results <- data.frame(theoretical = unlist(lapply(1:P, get_Wqueue)))
row.names(results) <- 1:P
results

# %% [markdown]
# ### Численно

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

env <- simmer("SuperDuperSim")
env

# %%
create_trajectory <- function(mu, name) {
    return(trajectory(name) %>%
        seize("server", 1) %>%
        timeout(function() rexp(1, mu)) %>%
        release("server", 1))
}

create_application_generator <- function(env, i) {
    name <- gsub(" ", "", paste("documents", i))

    env %>%
        add_generator(
            name_prefix = name,
            trajectory = create_trajectory(mu[i], name),
            distribution = function() rexp(1, lambda[i]),
            priority = i,
            preemptible = i + 1,
        )
}

add_generators <- function(env, n) {
    for (i in 1:n) {
        create_application_generator(env, i)
    }
}

# %%
SIMULATION_TIME <- 10000

env %>%
    add_resource("server", preemptive = TRUE)

add_generators(env, P)

env %>% run(until = SIMULATION_TIME)

# %%
programmers <- trajectory("programmers' path") %>%
    ## add an intake activity
    seize("server", 1) %>%
    timeout(function() rexp(1, 1 / t2)) %>%
    release("server", 1)

# %%
arrivals <- env %>% get_mon_arrivals(ongoing = TRUE) # Show ongoing tasks too.
resources <- env %>% get_mon_resources()
arrivals
resources

# %% [markdown]
# Отфильтруем NA значения:

# %%
arrivals["end_time"][arrivals["start_time"] == -1] <- SIMULATION_TIME
arrivals["start_time"][arrivals["start_time"] == -1] <- SIMULATION_TIME
arrivals["end_time"][is.na(arrivals["end_time"])] <- SIMULATION_TIME
arrivals["activity_time"][is.na(arrivals["activity_time"])] <- 0

# %% [markdown]
# Разделим заявки по приоритету и подсчитаем для каждого набора данных время:

# %%
patterns <- lapply(1:P, function(i) paste("documents", i, sep = ""))

results$practical <- lapply(patterns, function(pattern) {
    arrivals[grepl(arrivals$name, pattern = pattern, fixed = TRUE), ] %>%
        with(mean(end_time - activity_time - start_time))
})
patterns
results

# %% [markdown]
# ### Вывод
# Как видно, теоретически вычисленные значения с некоторой точностью совпадают
# со значениями, полученными теоретически. При увеличении количества
# экспериментов 𝑁 точность только увеличивается.
