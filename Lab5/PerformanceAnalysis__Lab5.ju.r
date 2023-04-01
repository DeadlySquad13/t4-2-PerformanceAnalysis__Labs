# %% [markdown]
# # Лабораторная работа 5. Рекомендации по формализации и расчетам характеристик подсистем КИС в виде разомкнутых или замкнутых стохастических моделей
# ## Задание 1

# Для одноканальной системы массового обслуживания с ограничением на длину
# очереди $m$ составьте дифференциальные уравнения для вероятностей нахождения
# в заданных состояниях в зависимости от времени. Найдите эти вероятности при
# определенном в соответствии с вариантом значении $t$, а также при $t
# \xrightarrow{} 0$. Канал иногда может выходить из строя. Заявка, которая
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
# ![graph](./StateGraph.png)

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
        dP_0 <- -(lambda + nu) * P_0 + mu * P_1 + gamma * P_6
        dP_1 <- -(lambda + nu + mu) * P_1 + lambda * P_0 + mu * P_2 + gamma * P_7
        dP_2 <- -(lambda + nu + mu) * P_2 + lambda * P_1 + mu * P_3 + gamma * P_8
        dP_3 <- -(lambda + nu + mu) * P_3 + lambda * P_2 + mu * P_4 + gamma * P_9
        dP_4 <- -(lambda + nu + mu) * P_4 + lambda * P_3 + mu * P_5 + gamma * P_10
        dP_5 <- -(nu + mu) * P_5 + lambda * P_4
        dP_6 <- -(gamma + lambda) * P_6 + nu * P_0
        dP_7 <- -(gamma + lambda) * P_6 + lambda * P_5 + nu * P_0
        dP_8 <- -(gamma + lambda) * P_7 + lambda * P_6 + nu * P_1
        dP_8 <- -(gamma + lambda) * P_8 + lambda * P_7 + nu * P_2
        dP_9 <- -(gamma + lambda) * P_9 + lambda * P_8 + nu * P_3
        dP_10 <- -gamma * P_10 + lambda * P_9 + nu * P_4 + nu * P_5

        # Specifying list of derivatives.
        return(
            list(
                c(dP_0, dP_1, dP_2, dP_3, dP_4, dP_5, dP_6, dP_7, dP_8, dP_9, dP_10)
            )
        )
    })
}

# %%
pars <- c(norm_equation = 1)
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
    P_10 = 0
)

accuracy <- 0.001
times <- seq(0, 1, by = accuracy)
output <- ode(yini, times, ode_system_equations, pars)
output

# %% [markdown]
# Вычислим индекс строки, которую нужно взять для получения результата при
# заданом в условии $t$:

# %%
precision_of_accuracy <- 3
t_index <- round(t, precision_of_accuracy) * 10^precision_of_accuracy + 1
print(t_index)
results <- output[t_index, 1:12]
results

# %%
# index - index of P.
get_P <- function(index) {
    return(as.numeric(results)[2 + index])
}


# %% [markdown]
# ### Вероятность простоя
# Вероятность простоя равна $P_0$:

# %%
get_P(0)

# %% [markdown]
# ### Вероятность образования очереди
# Вероятность образования очереди обратной величине от суммы $P_i$, соответствующим состояниям,
# в которых очередь пуста.
# $$
# 1 - (P_0 + P_1 + P_{m + 1})
# $$

# %%
1 - get_P(0) + get_P(1) + get_P(m + 1)

# %% [markdown]
# ### Абсолютную пропускную способность
# $\lambda'=\lambda\cdot(1-P_m(t)-P_{2m}(t))$

# %%
absolute_intensity <- lambda * (1 - get_P(m) - get_P(2 * m))
absolute_intensity

# %% [markdown]
# ### Среднюю длину очереди
# Для вычисления средней длины очереди просуммируем произведения вероятностей
# на соответствующие этим вероятностям длины очередей.
# $$
# L_{\text{оч}} = 1 \cdot P_2(t) + 2 \cdot P_3(t) + 3 \cdot P_4(t) + \text{...} + (m-1) \cdot P_m
# + 1 \cdot P_{m+2}(t) + 2 \cdot P_{m+3}(t) + \text{...} + (m - 1) \cdot P_{2m}(t)
# $$

# %%
get_queue_length_for_P <- function(index) {
    if (index < 1) {
        return(0)
    }

    return(index - 1)
}

get_P_product <- function(index) {
    return(get_P(index) * get_queue_length_for_P(index))
}

mean_length <- sum(unlist(
    lapply(c(2:m), get_P_product)
)) + sum(unlist(
    lapply(c((m + 2):(2 * m)), get_P_product)
))
mean_length

# %% [markdown]
# ### Среднее время нахождения заявок в системе
# $$
# W_{\text{оч}}=\frac{L_{\text{оч}}}{\lambda'}
# $$

# %%
mean_length / absolute_intensity

# %% [markdown]
# ### Среднее число заявок в системе
# Для вычисления средней длины очереди просуммируем произведения вероятностей
# на соответствующие этим вероятностям длины очередей + количество заявок на
# обслуживании.
# $$
# L_{\text{оч}} = 1 \cdot P_1(t) + 2 \cdot P_2(t) + 3 \cdot P_3(t) + 4 \cdot P_4(t) + \text{...} + m \cdot P_m
# + 1 \cdot P_{m+2}(t) + 2 \cdot P_{m+3}(t) + \text{...} + (m) \cdot P_{2m}(t)
# $$

# %%
get_queue_length_for_P <- function(index) {
    if (index < 1) {
        return(0)
    }

    return(index - 1)
}

get_P_product <- function(index) {
    return(get_P(index) * get_queue_length_for_P(index))
}

mean_length <- sum(unlist(
    lapply(c(2:m), get_P_product)
)) + sum(unlist(
    lapply(c((m + 2):(2 * m)), get_P_product)
))
mean_length

# %%

# ### Среднее время нахождения в очереди

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
# ### Граф состояний
# С точностью до второго знака после запятой
# ![graph](./TransitionMatrix_graph--dot.png)


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
make_step <- function(current_state, transition_matrix) {
    possible_states <- transition_matrix[current_state, ]

    return(get_value_in_range(runif(1), seq_along(transition_matrix), possible_states))
}

# %%
walk <- function(starting_state, times, transition_matrix) {
    if (times == 0) {
        return(starting_state)
    }

    next_state <- make_step(starting_state, transition_matrix)

    return(walk(next_state, times - 1, transition_matrix))
}

# %%
N <- 10000
test_walk <- function(first_state, k, transition_matrix) {
    results <- c()

    for (i in 1:N) {
        results <- append(results, walk(first_state, k, transition_matrix))
    }

    return(results)
}

# %%
first_state <- 1 # Выбрали произвольное.

# %%
Scenario1 <- test_walk(first_state, k - 2, P)

# %%
Scenario2 <- test_walk(first_state, k - 1, P)

# %%
Scenario3 <- test_walk(first_state, k, P)

# %% [markdown]
# В полученных сценариях посчитаем вероятности получения каждого состояния.

# %%
get_probability <- function(States, state) {
    number_of_states <- length(States[States == state])
    number_of_all_states <- length(States)

    return(number_of_states / number_of_all_states)
}

# %%
get_probabilities_to_stay <- function(States, transition_matrix) {
    return(
        unlist(
            lapply(
                seq_along(transition_matrix), function(state) get_probability(States, state)
            )
        )
    )
}

# %%
Scenario1Propabilities <- get_probabilities_to_stay(Scenario1, P)

# %%
Scenario2Propabilities <- get_probabilities_to_stay(Scenario2, P)

# %%
Scenario3Propabilities <- get_probabilities_to_stay(Scenario3, P)

# %% [markdown]
# Получим следующие результаты. Каждый столбец хранит вероятности остаться
# в той или иной вершине. Первый столбец для $k - 2$, второй - $k - 1$, третий - для $k$ проходов.

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

initial_state_probabilities %*% matrix.power(transition_matrix, k - 2)

# %%
initial_state_probabilities %*% matrix.power(transition_matrix, k - 1)

# %%
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
# Интесивность получалась больше 1, поэтому заменил значение.
# t1 <- sample(c(14:20), 1)
t1 <- 100
t2 <- sample(c(2:5), 1)
View(data.frame(k, t1, t2))

# %% [markdown]
# ### Численно

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал
# она  же обратная вероятность того, что
# программа **будет выполнена** сразу же, то есть:

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

env <- simmer("SuperDuperSim")
env

# %%
programmers <- trajectory("programmers' path") %>%
    ## add an intake activity
    seize("server", 1) %>%
    timeout(function() rexp(1, 1 / t2)) %>%
    release("server", 1)

# %%
env %>%
    add_resource("server", 1) %>%
    add_generator("programmers", programmers, function() rexp(1, k / t1))

# %%
env %>%
    reset() %>%
    run(1000000)

# %%
activities <- env %>% get_mon_arrivals()
activities

# %%
resources <- env %>% get_mon_resources()
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

# %% [markdown]
# ### Теоретически

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал
# она  же обратная вероятность того, что
# программа **будет выполнена** сразу же, то есть:
# $$
# 1 - P_0 = 1 - (1 - \rho) = \rho = \frac{\lambda}{\mu}
# $$
# где $\lambda$ - интенсивность, с которой заявки приходят:
# $$
# \lambda = \frac{k}{t_1} \\
# $$

# %%
income_intensity <- k / t1
income_intensity

# %% [markdown]
# а $\mu$ - интенсивность обслуживания:
# $$
# \mu = \frac{1}{t_2}
# $$

# %%
process_intensity <- 1 / t2
process_intensity

# %%
program_wont_be_executed_immediately <- income_intensity / process_intensity
program_wont_be_executed_immediately

# %% [markdown]
# #### 2. Среднее время до получения пользователем результатов реализации.

# Оно же среднее время пребывания заявки в системе по формуле Литтла:
# $$
# T_{\text{сист}} = \frac{1}{\mu(1 - \rho)}
# $$

# %%
time_to_get <- 1 / process_intensity / (1 - program_wont_be_executed_immediately)
time_to_get

# %% [markdown]
# #### 3. Среднее количество программ, ожидающих выполнения на сервере.

# Она же средняя длина очереди $L_{\text{оч}} = \frac{\rho ^ 2}{1 - \rho}$:

# %%
mean_queue <- program_wont_be_executed_immediately^2 / (1 - program_wont_be_executed_immediately)
mean_queue

# %% [markdown]
# Как видно, все значения сошлись.

# ## Дополнительное задание
# Найти вероятность  того, что точка после $k$
# шагов окажется от начала координат не дальше, чем на расстоянии, равном $m$.

# %% [markdown]
# $$
# P P_0
# $$

# %%
variant <- 5
set.seed(variant)

pp <- runif(3)

S <- pp[1] / sum(pp)
R <- pp[2] / sum(pp)
L <- pp[3] / sum(pp)

k <- sample(c(4:8), 1)
m <- sample(c(1:k), 1)

View(data.frame(p1, p2, p3, k, m))

# %% [markdown]
# Иными словами, она не должна остаться в 0, 1 или -1.
# Для этого составим матрицу переходов. Мы можем максимум уйти за k переходов либо на k единиц влево, либо
# на k единиц вправо. Добавляя к этому еще точку 0, с которой мы начинаем,
# получаем максимум $1 + 2 \cdot k$ состояний (строк и столбцов).

# %% [markdown]
# \begin{pmatrix}
# S & R & L & 0 & 0 & ... & 0 \\
# L & S & 0 & R & 0 & ... & 0 \\
# R & 0 & S & 0 & L & ... & 0 \\
# 0 & L & 0 & S &  ... & & ... \\
# 0 & 0 & R & ... &  ... & &  L \text{ или } R^1 \\
# ... & ... & ... & & & & 0 \\
# 0 & 0 & ... & 0 & R \text{ или } L^1 & 0 & S \\
# 0 & 0 & ... & ... & 0 & 1 & 0 \\
# 0 & 0 & ... & ... & 0 & 0 & 1
# \end{pmatrix}

# ---
# S - вероятность остаться,
# L - вероятность переместиться влево,
# R - вероятность переместиться вправо,
# 1. Зависит от четности количества столбцов

# %% [markdown]
# ### Теоретически
# По составленной матрице аналогично первому заданию можно подсчитать
# вероятность остаться в той или иной точке прямой.

# %%
initial_state_probabilities <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# %%
transition_matrix <- data.frame()

transition_matrix <- rbind(
    c(S, R, L, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(L, S, 0, R, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(R, 0, S, 0, L, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, L, 0, S, 0, R, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, R, 0, S, 0, L, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, L, 0, S, 0, R, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, R, 0, S, 0, L, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, L, 0, S, 0, R, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, R, 0, S, 0, L, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, L, 0, S, 0, R, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, R, 0, S, 0, L),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
)
transition_matrix

# %%
if (!require("matrixcalc")) {
    install.packages("matrixcalc")
}

probabilities <- initial_state_probabilities %*% matrix.power(data.matrix(transition_matrix), k)
probabilities

# %% [markdown]
# Вероятность не выйти за m единиц - вероятность остаться в 0 или 1, то есть
# сумма первых трёх ячеек полученной матрицы (они соответствуют вероятностям попасть в 0, 1 и -1 соответственно):

# %%
probabilities[1] + probabilities[2] + probabilities[3]

# %% [markdown]
# ### Численно
# Численный метод тоже не отличается от основного задания лабораторной работы.
# Воспользуемся уже ранее задаными функциями для вычисления вероятностей.

# %%
Scenario <- test_walk(initial_state_probabilities, k, transition_matrix)
# seq_along works differently with matrix.
probabilities <- get_probabilities_to_stay(Scenario, data.frame(transition_matrix))

probabilities

# %%
probabilities[1] + probabilities[2] + probabilities[3]

# %% [markdown]
# ### Итого
# Как видно, теоретически вычисленное значение с некоторой точностью совпадает
# со значением, полученным теоретически. При увеличении количества
# экспериментов 𝑁 точность только увеличивается.
