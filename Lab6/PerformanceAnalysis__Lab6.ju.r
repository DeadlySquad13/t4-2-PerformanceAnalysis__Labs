# %% [markdown]
# # Лабораторная работа 6. Метод квазиэквивалентного укрупнения состояний многомерных марковских процессов размножения-гибели
# ## Задание 1

# $K$ программистов могут писать программы для выполнения на одном из $M$ серверов,
# при этом программа не попадает сразу на сервер,
# а обрабатывается на одном из $N$ специальных компьютеров,
# которые проверяют отсутствие вирусов.
# Интенсивность работы программистов $\lambda$,
# интенсивность работы компьютеров-антивирусов $\nu$,
# интенсивность работы основных серверов $\mu$,
# программа оказывается с вирусом с вероятностью $p$.
# Если программа с вирусом, она получает отказ обслуживания на основных
# серверах. Для компьютеров по проверке вирусов имеется ограничение по длине
# очереди $m_1$ , для основных серверов ограничение по длине очереди $m_2$.

# - Нарисовать граф состояний системы, учитывая количество программистов,
# которые пишут программу, количество программ на компьютерах-антивирусах,
# количество программ на серверах;
# - Написать уравнения Колмогорова для вероятностей состояний, финальных
# вероятностей;
# - Тремя способами (экспериментально, методом укрупнения состояний, по
# уравнениям Колмогорова) найти основные характеристики эффективности СМО:
# среднее время пребывания заявки в системе, абсолютную пропускную способность,
# среднее число заявок в системе.

# %%
Variant<-5
set.seed(Variant) 
K<-sample(c(3:6),1)
M<-sample(c(1:3),1)
N<-sample(c(1:3),1)
lambda<-runif(1)
mu<-runif(1)
nu<-runif(1)
p<-runif(1)
m2<-sample(c(0:2),1)
m1<-sample(c(0:2),1)
View(data.frame(K,M,N,lambda,mu,nu,p,m1,m2))

# %% [markdown]
# Введем следующие состояния:
# - $S_{4000}$ - все программисты пишут программу, сервера и компьютеры
# свободны;
# - $S_{3010}$ - три программиста пишут программу, одна программа проверяется на
# вирус;
# - $S_{3001}$ - три программиста пишут программу, одна программа выполняется
# на сервере;
# - $S_{2110}$ - два программиста пишут программу, одна программа проверяется на
# - $S_{2110}$ - два программиста пишут программу, одна программа проверяется на
# вирус, одна стоит в очереди;
# - $S_{2011}$ - два программиста пишут программу, одна программа проверяется на
# вирус, одна выполняется на сервере;
# - $S_{2002}$ - два программиста пишут программу, две выполняются на сервере;
# - $S_{1111}$ - один программист пишет программу, одна программа стоит
# в очереди, одна программа проверяется на вирус, две выполняются на сервере;
# - $S_{1003}$ - один программист пишет программу, три выполняются на
# - $S_{1012}$ - один программист пишет программу, одна программа проверяется
# на вирус, две выполняются на сервере;
# - $S_{1003}$ - один программист пишет программу, три выполняются на
# сервере;
# - $S_{0112}$ - все программисты ожидают ответа, одна программа стоит
# в очереди, одна программа проверяется
# на вирус, две выполняются на сервере;
# - $S_{0013}$ - все программисты ожидают ответа, одна программа проверяется на
# вирус, три выполняются на сервере;


# %% [markdown]
# Тогда граф состояний будет выглядеть:
# ![graph](./State_graph.png)

# %% [markdown]
# ### Теоретически по уравнениям Колмогорова

# %% [markdown]
# По графу составим уравнения Колмогорова:
# $$
# \begin{cases}
# \frac{dP_{4000}(t)}{dt} = -4 \lambda P_{4000}(t) + p \nu P_{3010}(t) + \mu P_{3001}(t) \\
# \frac{dP_{3010}(t)}{dt} = -(3 \lambda + p\nu + (1-p)\nu) P_{3010}(t) + 4 \lambda P_{4000}(t) + p \nu P_{2110}(t) + \mu P_{2011}(t) \\
# \frac{dP_{2110}(t)}{dt} = -(p\nu + (1-p)\nu) P_{2110}(t) + 3 \lambda P_{3010}(t) + \mu P_{1111} \\
# \frac{dP_{3001}(t)}{dt} = -(3 \lambda + \mu) P_{3001}(t) + p \nu P_{2011} + 2 \mu P_{2002} + (1-p) \nu P_{3010}(t) \\
# \frac{dP_{2011}(t)}{dt} = -(2 \lambda + p\nu + (1-p)\nu + \mu) P_{2011}(t) + 3 \lambda P_{3001}(t) + p \nu P_{1111}(t) + 2 \mu P_{1012}(t) + (1-p)\nu P_{2110}(t) \\
# \frac{dP_{1111}(t)}{dt} = -(p\nu + (1-p)\nu + \mu) P_{1111}(t) + 2 \lambda P_{2011}(t) + 2 \mu P_{0112} \\
# \frac{dP_{2002}(t)}{dt} = -(2 \lambda + 2 \mu) P_{2002}(t) + p \nu P_{1012} + 3 \mu P_{1003} + (1-p) \nu P_{2011}(t) \\
# \frac{dP_{1012}(t)}{dt} = -(\lambda + p\nu + (1-p)\nu + 2 \mu) P_{1012}(t) + 2 \lambda P_{2002}(t) + p \nu P_{0112}(t) + 3 \mu P_{0013}(t) + (1-p)\nu P_{1111}(t) \\
# \frac{dP_{0112}(t)}{dt} = -(p\nu + (1-p)\nu + 2 \mu) P_{0112}(t) + \lambda P_{1012}(t) \\
# \frac{dP_{1003}(t)}{dt} = -(\lambda + 3 \mu) P_{1003}(t) + p\nu P_{0013}(t) + (1-p)\nu P_{1012}(t) \\
# \frac{dP_{0013}(t)}{dt} = -(p\nu + 3 \mu) P_{0013}(t) + \lambda P_{1003}(t) + (1-p)\nu P_{0112}(t)
# \end{cases}
# $$

# и уравнение нормировки:
# $$
# P_{4000}(t) + P_{3010}(t) + P_{2110}(t) + P_{3001}(t) + P_{2011}(t) + P_{1111}(t) + P_{2002}(t) + P_{1012}(t) + P_{0112}(t) + P_{1003}(t) + P_{0013}(t) = 1
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
        dP_4000 <- -4 * lambda * abs(P_4000) + p * nu * abs(P_3010) + mu * abs(P_3001)
        dP_3010 <- -(3 * lambda + p * nu + (1-p) * nu) * abs(P_3010) + 4 * lambda * abs(P_4000) + p * nu * abs(P_2110) + mu * abs(P_2011)
        dP_2110 <- -(p * nu + (1-p) * nu) * abs(P_2110) + 3 * lambda * abs(P_3010) + mu * abs(P_1111)
        dP_3001 <- -(3 * lambda + mu) * abs(P_3001) + p * nu * abs(P_2011) + 2 * mu * abs(P_2002) + (1-p) * nu * abs(P_3010)
        dP_2011 <- -(2 * lambda + p* nu + (1-p) * nu + mu) * abs(P_2011) + 3 * lambda * abs(P_3001) + p * nu * abs(P_1111) + 2 * mu * abs(P_1012) + (1-p) * nu * abs(P_2110)
        dP_1111 <- -(p * nu + (1-p) * nu + mu) * abs(P_1111) + 2 * lambda * abs(P_2011) + 2 * mu * abs(P_0112)
        dP_2002 <- -(2 * lambda + 2 * mu) * abs(P_2002) + p * nu * abs(P_1012) + 3 * mu * abs(P_1003) + (1-p) * nu * abs(P_2011)
        dP_1012 <- -(lambda + p * nu + (1-p) * nu + 2 * mu) * abs(P_1012) + 2 * lambda * abs(P_2002) + p * nu * abs(P_0112) + 3 * mu * abs(P_0013) + (1-p) * nu * abs(P_1111)
        dP_0112 <- -(p * nu + (1-p) * nu + 2 * mu) * abs(P_0112) + lambda * abs(P_1012)
        dP_1003 <- -(lambda + 3 * mu) * abs(P_1003) + p * nu * abs(P_0013) + (1-p) * nu * abs(P_1012)
        dP_0013 <- -(p * nu + 3 * mu) * abs(P_0013) + lambda * abs(P_1003) + (1-p) * nu * abs(P_0112)
        norm <- abs(P_4000) + abs(P_3010) + abs(P_2110) + abs(P_3001) + abs(P_2011) + abs(P_1111) + abs(P_2002) + abs(P_1012) + abs(P_0112) + abs(P_1003) + abs(P_0013)

        # Specifying list of derivatives.
        return(
            list(c(
                dP_4000,
                dP_3010,
                dP_2110,
                dP_3001,
                dP_2011,
                dP_1111,
                dP_2002,
                dP_1012,
                dP_0112,
                dP_1003,
                dP_0013,
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
    P_4000 = 1,
    P_3010 = 0,
    P_2110 = 0,
    P_3001 = 0,
    P_2011 = 0,
    P_1111 = 0,
    P_2002 = 0,
    P_1012 = 0,
    P_0112 = 0,
    P_1003 = 0,
    P_0013 = 0,
    norm = 1
)


INITIAL_TIME <- 0
FINISH_TIME <- 100000

norm_equation_values <- data.frame(
    var = "norm",
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
# При $t \rightarrow{} \infty$ (взяли последний результат, брать значения $t$
# выше нет смысла, так как видно, что значения $\forall P_i$ в конце таблицы
# сходятся):

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

# %% [markdown]
Зададим траекторию отказа в случае полной очереди:

# %%
m.queue <- trajectory("clients' path") %>%
    ## add an intake activity
    seize("server", amount = 1) %>%
    timeout(function() rexp(1, 1 / mu)) %>%
    release("server", amount = 1)

# %% [markdown]
# Добавим симуляцию поломки системы, задав механизм смены поля $capacity$ ресурса
# $server$ с 1 на 0 и обратно с интенсивностью $\gamma$ и $\nu$ соответственно.

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
SIMULATION_TIME <- FINISH_TIME

MM1m.env %>%
    add_resource(
        "server",
        capacity = capacity_schedule,
        queue_size = m
    ) %>%
    add_generator("clients", m.queue, function() rexp(1, lambda)) %>%
    run(until = SIMULATION_TIME)

# %%
arrivals <- get_mon_arrivals(MM1m.env)
arrivals

# %% [markdown]
# Логи симуляции:

# %%
resources <- get_mon_resources(MM1m.env)
resources

# %% [markdown]
# #### Вероятность простоя
# Подсчитаем отношение количества состояний, когда система не загружена,
# к общему количеству состояний.

# %%
free_states <- resources %>% subset(server == 0) %>% subset( capacity > 0)
free_states

# %%
nrow(free_states) / nrow(resources)

# %% [markdown]
# #### Вероятность образования очереди
# Вычислим вероятность образования очереди, подсчитав отношение количества записей,
# в которых очередь не пуста, к общему количеству записей.

# %%
queue_unempty <- resources %>% subset(queue != 0)
queue_unempty

# %%
nrow(queue_unempty) / nrow(resources)

# %% [markdown]
# #### Абсолютную пропускную способность
#  Абсолютную пропускную способность вычислим по формуле:
# $$
# \lambda'=\lambda\cdot(1-P_m(t)-P_{2m}(t))
# $$

# %%
number_of_unfinished <- arrivals %>% with(sum(!finished))
number_of_unfinished

# %%
finish_probability <- number_of_unfinished / nrow(arrivals)
finish_probability

# %%
absolute_flow_capacity <- lambda * finish_probability
absolute_flow_capacity

# %% [markdown]
# #### Среднюю длину очереди

# %%
mean_length <- resources %>% with(mean(queue))
mean_length

# %% [markdown]
# #### Среднее время нахождения в очереди
# Построим график и сравним его со значением $W_\text{оч}$ (черная горизонтальная прямая), найденным теоретически:

# %%
plot(arrivals, metric = "waiting_time", names = "server", items = "system") +
    coord_cartesian(xlim = c(0, SIMULATION_TIME), ylim = c(0, 100)) +
    geom_hline(yintercept = W)


# %% [markdown]
# #### Среднее число заявок в системе
# Среднее число заявок в системе сравним с теоретическим, проанализируя график использования
# ресурса `server`:

# %%
plot(resources, metric = "usage", names = "server", items = "system") +
    geom_hline(yintercept = mean_number_of_requests)

# %% [markdown]
# По этому графику сложно судить, поэтому дополнительно рассчитаем значение,
# воспользовавшись таблицей использования ресурсов:

# %%
resources %>% with(mean(queue) + mean(server))

# %% [markdown]
# #### Среднее время нахождения заявок в системе $T$
# Поступим со средним временем нахождения заявок в системе $T$ по аналогии
# с $W$ - сравним два графика:

# %%
plot(arrivals, metric = "flow_time", names = "server", items = "system") +
    coord_cartesian(xlim = c(0, 80000), ylim = c(0, 100)) +
    geom_hline(yintercept = T)

# %% [markdown]
# ### Выводы
# Как видно, теоретически вычисленные значения с некоторой точностью совпадают
# со значениями, полученным численным методом. При увеличении количества
# экспериментов 𝑁 точность только увеличивается.
