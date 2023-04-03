# %% [markdown]
# # Лабораторная работа 5. Рекомендации по формализации и расчетам характеристик подсистем КИС в виде разомкнутых или замкнутых стохастических моделей
# ## Задание 1

# Для одноканальной системы массового обслуживания с ограничением на длину
# очереди $m$ составьте дифференциальные уравнения для вероятностей нахождения
# в заданных состояниях в зависимости от времени. Найдите эти вероятности при
# определенном в соответствии с вариантом значении $t$, а также при $t
# \xrightarrow{} \infty$. Канал иногда может выходить из строя. Заявка, которая
# обслуживается в момент отказа канала ставится в очередь, если там есть места,
# в противном случае она покидает систему необслуженной. Входящий поток, поток
# обслуживания, поток отказов и поток восстановления простейшие
# с соответствующими интенсивностями $\lambda, \mu, \nu, \gamma$. Количество
# клиентов, от которых могут поступать заявки на обслуживание $k$. Начальные
# условия $P_0(0) = 1$.

# %%
Variant <- 5
set.seed(Variant)
m <- sample(c(4:18), 1)

mu <- runif(1)

lambda <- runif(1)
if (lambda > mu) {
    current <- lambda
    lambda <- mu
    mu <- current
}

gamma <- runif(1)

nu <- runif(1)

if (gamma < nu) {
    current <- nu
    nu <- gamma
    gamma <- current
}

if (sample(c(0:1), 1)) {
    k <- sample(c(4:7), 1)
} else {
    k <- "inf"
}
t <- runif(1)
View(data.frame(lambda, mu, nu, gamma, k, m, t))

# %% [markdown]
# Введем следующие состояния:
# - $S_0$ - нет выполняемых задач, СМО готова выполнять внешние задачи;
# - $S_1$ - СМО занята выполнением внешней задачи, очереди нет;
# - $S_2$ - СМО занята выполнением внешней задачи, в очереди одна задача;
# - ...
# - $S_m$ - СМО занята выполнением внешней задачи, в очереди $m$ задач;
# - $S_{m+1}$ - СМО вышла из строя, в очереди нет задач;
# - $S_{m+2}$ - СМО вышла из строя, в очереди одна задача;
# - ...
# - $S_{2m}$ - СМО вышла из строя, в очереди m задач.

# %% [markdown]
# Тогда граф состояний будет выглядеть:
# ![graph](./State_graph.png)
# В бирюзовом кластере находятся состояния, когда СМО в рабочем состоянии.
# В зеленом - когда вышла из строя.

# %% [markdown]
# ### Теоретически

# %% [markdown]
# По графу составим уравнения Колмогорова:
# $$
# \begin{cases}
# \frac{dP_0(t)}{dt} = -(\lambda + \nu)P_0(t) + \mu\cdot P_1(t) + \gamma \cdot P_6(t) \\
# \frac{dP_1(t)}{dt} = -(\lambda + \nu + \mu)P_1(t) + \lambda \cdot P_0(t) + \mu \cdot P_2(t) + \gamma \cdot P_7(t) \\
# \frac{dP_2(t)}{dt} = -(\lambda + \nu + \mu)P_2(t) + \lambda \cdot P_1(t) + \mu \cdot P_3(t) + \gamma \cdot P_8(t) \\
# \frac{dP_3(t)}{dt} = -(\lambda + \nu + \mu)P_3(t) + \lambda \cdot P_2(t) + \mu \cdot P_4(t) + \gamma \cdot P_9(t) \\
# \frac{dP_4(t)}{dt} = -(\lambda + \nu + \mu)P_4(t) + \lambda \cdot P_3(t) + \mu \cdot P_5(t) + \gamma \cdot P_{10}(t) \\
# \frac{dP_5(t)}{dt} = -(\nu + \mu)P_5(t) + \lambda \cdot P_4(t) \\
# \frac{dP_6(t)}{dt}  = -(\gamma + \lambda)P_6(t) + \nu \cdot P_0(t) \\
# \frac{dP_7(t)}{dt}  = -(\gamma + \lambda)P_7(t) + \lambda \cdot P_6(t) + \nu \cdot P_1(t) \\
# \frac{dP_8(t)}{dt}  = -(\gamma + \lambda)P_8(t) + \lambda \cdot P_7(t) + \nu \cdot P_2(t) \\
# \frac{dP_9(t)}{dt}  = -(\gamma + \lambda)P_9(t) + \lambda \cdot P_{8}(t) + \nu \cdot P_3(t) \\
# \frac{dP_{10}(t)}{dt} = -\gamma \cdot P_{10}(t) + \lambda \cdot P_{9}(t) + \nu \cdot P_4(t) + \nu \cdot P_5(t)
# \end{cases}
# $$

# и уравнение нормировки:
# $$
# P_0(t) + P_1(t) + P_2(t) + P_3(t) + P_4(t) + P_5(t) + P_6(t) + P_7(t) + P_8(t) + P_9(t) + P_{10}(t) = 1
# $$

# %% [markdown]
# Решим эти уравнения с помощью пакета deSolve
# (см. 75 стр. [документации](chrome-extension://gfbliohnnapiefjpjlpjnehglfpaknnc/pages/pdf_viewer.html?r=https://cran.r-project.org/web/packages/deSolve/deSolve.pdf)):

# %%
if (!require("deSolve")) {
    install.packages("deSolve")
}
library(deSolve)

ode_system_equations <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        dP_0 <- -(lambda + nu) * abs(P_0) + mu * abs(P_1) + gamma * abs(P_6)
        dP_1 <- -(lambda + nu + mu) * abs(P_1) + lambda * abs(P_0) + mu * abs(P_2) + gamma * abs(P_7)
        dP_2 <- -(lambda + nu + mu) * abs(P_2) + lambda * abs(P_1) + mu * abs(P_3) + gamma * abs(P_8)
        dP_3 <- -(lambda + nu + mu) * abs(P_3) + lambda * abs(P_2) + mu * abs(P_4) + gamma * abs(P_9)
        dP_4 <- -(lambda + nu + mu) * abs(P_4) + lambda * abs(P_3) + mu * abs(P_5) + gamma * abs(P_10)
        dP_5 <- -(nu + mu) * abs(P_5) + lambda * abs(P_4)
        dP_6 <- -(gamma + lambda) * abs(P_6) + nu * abs(P_0)
        dP_7 <- -(gamma + lambda) * abs(P_7) + lambda * abs(P_6) + nu * abs(P_1)
        dP_8 <- -(gamma + lambda) * abs(P_8) + lambda * abs(P_7) + nu * abs(P_2)
        dP_9 <- -(gamma + lambda) * abs(P_9) + lambda * abs(P_8) + nu * abs(P_3)
        dP_10 <- -gamma * abs(P_10) + lambda * abs(P_9) + nu * abs(P_4) + nu * abs(P_5)
        norm <- P_0 + P_1 + P_2 + P_3 + P_4 + P_5 + P_6 + P_7 + P_8 + P_9 + P_10

        # Specifying list of derivatives.
        return(
            list(c(
                dP_0,
                dP_1,
                dP_2,
                dP_3,
                dP_4,
                dP_5,
                dP_6,
                dP_7,
                dP_8,
                dP_9,
                dP_10,
                norm
            ))
        )
    })
}

# %% [markdown]
# Уравнение нормировки в данном пакете задать напрямую нельзя, оно будет
# восприниматься как дифференциальное уравнение. Однако если его упустить,
# значения $P_i$ будут выходить за пределы $[0, 1]$.

# Также, если его оставить и задать в начальных условиях равным 1, оно все
# равно будет постепенно расти до $\infty$, дестабилизируя при этом $P_i$ (
# они тоже будут выходить за адекватные пределы).
# 
# Поэтому было принято решение оставить уравнение нормировки как
# дифференциальное, воспользовавшись при этом дополнительным параметром
# `events`, позволяющим задать частичные частные решения в любой момент
# времени. Так, "обнуляя" значение уравнения нормировки до 1 каждые 10 тактов,
# мы стабилизируем результаты вычисления системы ОДУ: по таблице `output`
# видно, что переменная `norm` часто становится выше 1, однако значения
# $P_i$ все равно находятся в $[0, 1]$ и их сумма равна 1. Именно этого
# мы и добивались.

# %%
pars <- NULL
yini <- c(
    P_0 = 1,
    P_1 = 0,
    P_2 = 0,
    P_3 = 0,
    P_4 = 0,
    P_5 = 0,
    P_6 = 0,
    P_7 = 0,
    P_8 = 0,
    P_9 = 0,
    P_10 = 0,
    norm = 1
)


INITIAL_TIME <- 0
FINISH_TIME <- 100000

norm_equation_values <- data.frame(var = "norm",
                             time = INITIAL_TIME:FINISH_TIME,
                             value = 1,
                             method = "replace"
                            )

ACCURACY <- 0.1
times <- seq(INITIAL_TIME, FINISH_TIME, by = ACCURACY)
output <- ode(yini, times, ode_system_equations, pars, events = list(data = norm_equation_values))
output

# %% [markdown]
# Вычислим индекс строки, которую нужно взять для получения результата при
# заданом в условии $t$:

# %%
PRECISION_OF_ACCURACY <- 1
t_index <- round(t, PRECISION_OF_ACCURACY) * 10^PRECISION_OF_ACCURACY + 1
t_index

# %% [markdown]
# По данному индексу имеем ближайший вектор значений:

# %%
results <- output[t_index, 1:12]
results

# %% [markdown]
# При $t \rightarrow{} \infty$:
# %%
results <- output[nrow(output), 2:12]
results

# %% [markdown]
# Подтвердим, что $\sum P_i = 1$:
# %%
sum(results)

# %%
get_P <- function(P_index) {
    return(as.numeric(results)[1 + P_index])
}

# %% [markdown]
# #### Вероятность простоя
# Вероятность простоя равна $P_0$:

# %%
get_P(0)

# %% [markdown]
# #### Вероятность образования очереди
# Вероятность образования очереди $P_{\text{оч}}$ равна обратной вероятности $P_{\overline{\text{оч}}}$.
# Та, в свою очередь, равна сумме вероятностей, соответствующих состояниям,
# в которых очередь пуста.
# $$
# P_{\text{оч}} = 1 - P_{\overline{\text{оч}}} = 1 - (P_0 + P_1 + P_{m + 1})
# $$

# %%
1 - (get_P(0) + get_P(1) + get_P(m + 1))

# %% [markdown]
# #### Абсолютную пропускную способность
#  Абсолютную пропускную способность вычислим по формуле:
# $$
# \lambda'=\lambda\cdot(1-P_m(t)-P_{2m}(t))
# $$

# %%
absolute_flow_capacity <- lambda * (1 - get_P(m) - get_P(2 * m))
absolute_flow_capacity

# %% [markdown]
# #### Среднюю длину очереди
# Для вычисления средней длины очереди просуммируем произведения вероятностей
# на соответствующие этим вероятностям длины очередей.
# $$
# L_{\text{оч}} = 1 \cdot P_2(t) + 2 \cdot P_3(t) + 3 \cdot P_4(t) +
# \text{...} + (m-1) \cdot P_m + 1 \cdot P_{m+2}(t) + 2 \cdot P_{m+3}(t) +
# \text{...} + (m - 1) \cdot P_{2m}(t)
# $$

# %%
# Получаем длину очереди в системе для заданного индекса вероятности.
P.get_queue_length <- function(P_index) {
    if (P_index < 1) {
        return(0)
    }

    if (P_index <= m) {
        return(P_index - 1)
    }

    return(P_index - m - 1)
}

P.get_product <- function(P_index) {
    return(get_P(P_index) * P.get_queue_length(P_index))
}

mean_length <- sum(unlist(
    lapply(c(2:m), P.get_product)
)) + sum(unlist(
    lapply(c((m + 2):(2 * m)), P.get_product)
))
mean_length

# %% [markdown]
# #### Среднее время нахождения в очереди
# $$
# W_{\text{оч}}=\frac{L_{\text{оч}}}{\lambda'}
# $$

# %%
W <- mean_length / absolute_flow_capacity
W

# %% [markdown]
# #### Среднее число заявок в системе
# Для вычисления средней длины очереди просуммируем произведения вероятностей
# на соответствующие этим вероятностям значения заявок в системе
# (длины очередей + количество заявок на обслуживании):
# $$
# L = (1 + 0) \cdot P_1(t) + (1 + 1) \cdot P_2(t) + (1 + 2) \cdot P_3(t) +
# (1 + 3) \cdot P_4(t) + \text{...} + (1 + m - 1) \cdot P_m +
# 1 \cdot P_{m+2}(t) + 2 \cdot P_{m+3}(t) + \text{...} + (m - 1) \cdot P_{2m}(t)
# $$

# %%
# Получаем количество заявок на обслуживании для заданного индекса вероятности.
P.get_number_of_requests_proccessed <- function(P_index) {
    if (P_index > 0 && P_index <= m) {
        return(1)
    }

    return(0)
}

P.get_product1 <- function(P_index) {
    P.number_of_requests <- P.get_queue_length(P_index) + P.get_number_of_requests_proccessed(P_index)

    return(get_P(P_index) * P.number_of_requests)
}

mean_number_of_requests <- sum(unlist(
    lapply(c(2:m), P.get_product1)
)) + sum(unlist(
    lapply(c((m + 2):(2 * m)), P.get_product1)
))
mean_number_of_requests

# %% [markdown]
# #### Среднее время нахождения заявок в системе
# $$
# T =\frac{L}{\lambda'}
# $$

# %%
T <- mean_number_of_requests / absolute_flow_capacity
T

# %% [markdown]
# ### Численно
# Построим с помощью пакета simmer симуляцию системы $M/M/1/m$.


# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)
if (!require("simmer.plot")) {
    install.packages("simmer.plot")
}
library(simmer.plot)

MM1m.env <- simmer("SuperDuperSim")
MM1m.env

# %%
m.queue <- trajectory("clients' path") %>%
    ## add an intake activity
    seize("server", amount = 1) %>%
    timeout(function() rexp(1, mu)) %>%
    release("server", amount = 1) %>%
    set_capacity("server", 0)

# %% [markdown]
# Добавим симуляцию отказов

# %%
# Transform intervals timetable of chronologic points.
# Example:
# 1, 2, 0.5, 0.3, 0.2 -> 1, 3, 3.5, 3.8, 4
accumulate <- function(intervals) {
    timetable <- c()

    for (interval_index in seq_along(intervals)) {
        point <- intervals[interval_index]

        if (interval_index > 1) {
            point <- point + timetable[interval_index - 1]
        }

        timetable <- append(timetable, point)
    }

    return(timetable)
}

# %%
create_timetable <- function(number_of_points, break_intensity, repair_intensity) {
    intervals <- c()

    for (interval_index in 1:number_of_points) {
        intensity <- break_intensity

        if (interval_index %% 2 == 0) {
            intensity <- repair_intensity
        }

        intervals <- append(intervals, rexp(1, 1 / intensity))
    }


    return(accumulate(intervals))
}

create_capacity_schedule <- function(number_of_points) {
    stopifnot(number_of_points %% 2 == 0)

    timetable <- create_timetable(
                                  number_of_points,
                                  break_intensity = gamma,
                                  repair_intensity = nu
    )

    capacity_sequence <- rep(c(1, 0), times = number_of_points / 2)

    period <- sum(timetable)

    return(
       schedule(
                timetable,
                capacity_sequence,
                period
        )
    )
}

capacity_schedule <- create_capacity_schedule(1000)

# %%
MM1m.env %>%
    add_resource("server", capacity = capacity_schedule, queue_size = m) %>%
    add_generator("clients", m.queue, function() rexp(1, lambda)) %>%
    run(until = FINISH_TIME)

# %%
arrivals <- get_mon_arrivals(MM1m.env)
arrivals

# %%
get_mon_arrivals(MM1m.env) %>%
    with(sum(!finished) / length(finished))

# %%
resources <- get_mon_resources(MM1m.env)
resources

# %%
plot(resources, metric = "usage", names = "server", items = "system") +
    geom_hline(yintercept = mean_number_of_requests)

# %%
plot(arrivals, metric = "waiting_time", names = "server", items = "system") +
    geom_hline(yintercept = W)


# %% [markdown]
# #### Вероятность простоя
# Подсчитаем отношение количества состояний, когда система не загружена,
# к общему количеству состояний.

# %%
free_states <- resources %>% subset(server == 0 & capacity > 0)
free_states

# %%
nrow(free_states) / nrow(resources)

# %% [markdown]
# #### Вероятность образования очереди
# Вероятность образования очереди $P_{\text{оч}}$ равна обратной вероятности $P_{\overline{\text{оч}}}$.
# Та, в свою очередь, равна сумме вероятностей, соответствующих состояниям,
# в которых очередь пуста.
# $$
# P_{\text{оч}} = 1 - P_{\overline{\text{оч}}} = 1 - (P_0 + P_1 + P_{m + 1})
# $$

# %%
1 - get_P(0) + get_P(1) + get_P(m + 1)

# %% [markdown]
# #### Абсолютную пропускную способность
#  Абсолютную пропускную способность вычислим по формуле:
# $$
# \lambda'=\lambda\cdot(1-P_m(t)-P_{2m}(t))
# $$

# %%
absolute_flow_capacity <- lambda * (1 - get_P(m) - get_P(2 * m))
absolute_flow_capacity

# %% [markdown]
# #### Среднюю длину очереди
# Для вычисления средней длины очереди просуммируем произведения вероятностей
# на соответствующие этим вероятностям длины очередей.
# $$
# L_{\text{оч}} = 1 \cdot P_2(t) + 2 \cdot P_3(t) + 3 \cdot P_4(t) +
# \text{...} + (m-1) \cdot P_m + 1 \cdot P_{m+2}(t) + 2 \cdot P_{m+3}(t) +
# \text{...} + (m - 1) \cdot P_{2m}(t)
# $$

# %%
# Получаем длину очереди в системе для заданного индекса вероятности.
P.get_queue_length <- function(P_index) {
    if (P_index < 1) {
        return(0)
    }

    if (P_index <= m) {
        return(P_index - 1)
    }

    return(P_index - m - 1)
}

P.get_product <- function(P_index) {
    return(get_P(P_index) * P.get_queue_length(P_index))
}

mean_length <- sum(unlist(
    lapply(c(2:m), P.get_product)
)) + sum(unlist(
    lapply(c((m + 2):(2 * m)), P.get_product)
))
mean_length

# %% [markdown]
# #### Среднее время нахождения в очереди
# $$
# W_{\text{оч}}=\frac{L_{\text{оч}}}{\lambda'}
# $$

# %%
mean_length / absolute_flow_capacity

# %% [markdown]
# #### Среднее число заявок в системе
# Для вычисления средней длины очереди просуммируем произведения вероятностей
# на соответствующие этим вероятностям значения заявок в системе
# (длины очередей + количество заявок на обслуживании):
# $$
# L = (1 + 0) \cdot P_1(t) + (1 + 1) \cdot P_2(t) + (1 + 2) \cdot P_3(t) +
# (1 + 3) \cdot P_4(t) + \text{...} + (1 + m - 1) \cdot P_m +
# 1 \cdot P_{m+2}(t) + 2 \cdot P_{m+3}(t) + \text{...} + (m - 1) \cdot P_{2m}(t)
# $$

# %%
# Получаем количество заявок на обслуживании для заданного индекса вероятности.
P.get_number_of_requests_proccessed <- function(P_index) {
    if (P_index > 0 && P_index <= m) {
        return(1)
    }

    return(0)
}

P.get_product1 <- function(P_index) {
    P.number_of_requests <- P.get_queue_length(P_index) + P.get_number_of_requests_proccessed(P_index)

    return(get_P(P_index) * P.number_of_requests)
}

mean_number_of_requests <- sum(unlist(
    lapply(c(2:m), P.get_product1)
)) + sum(unlist(
    lapply(c((m + 2):(2 * m)), P.get_product1)
))
mean_number_of_requests

# %% [markdown]
# #### Среднее время нахождения заявок в системе
# $$
# T =\frac{L}{\lambda'}
# $$

# %%
mean_number_of_requests / absolute_flow_capacity

# %%
MM1m.env %>%
    reset() %>%
    run(1000000)

# %%
activities <- MM1m.env %>% get_mon_arrivals()
activities

# %%
resources <- MM1m.env %>% get_mon_resources()
resources

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал
# она  же обратная вероятность того, что
# программа **будет выполнена** сразу же, то есть:

# %%
EPS <- 0.0001 # Должно быть 0, но в модели присутствуют некоторые погрешности.
queue <- resources$queue
income_count <- length(activities$name)
programs_starts <- length(
    subset(activities, (activities$end_time - activities$start_time - activities$activity_time) > EPS)$name
)

programs_starts
income_count

# %%
program_wont_be_executed_immediately <- programs_starts / income_count
program_wont_be_executed_immediately

# %% [markdown]
# #### 2. Среднее время до получения пользователем результатов реализации.

# %%
finished_activity_time <- mean(activities$end_time - activities$start_time)
finished_activity_time

# %% [markdown]
# #### 3. Среднее количество программ, ожидающих выполнения на сервере.

# %%
mean_queue <- program_wont_be_executed_immediately^2 / (1 - program_wont_be_executed_immediately)
mean_queue
mean_queue <- program_wont_be_executed_immediately^2 / (1 - program_wont_be_executed_immediately)
mean_queue
mean_queue
mean_queue
