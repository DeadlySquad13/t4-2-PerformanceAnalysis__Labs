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
#   - среднее время пребывания заявки в системе,
#   - абсолютную пропускную способность,
#   - среднее число заявок в системе.

# %%
Variant <- 5
set.seed(Variant)
K <- sample(c(3:6), 1)
M <- sample(c(1:3), 1)
N <- sample(c(1:3), 1)
lambda <- runif(1)
mu <- runif(1)
nu <- runif(1)
p <- runif(1)
m2 <- sample(c(0:2), 1)
m1 <- sample(c(0:2), 1)
View(data.frame(K, M, N, lambda, mu, nu, p, m1, m2))

# %% [markdown]
# Введем следующие состояния, приняв $S_{k,q,n,m}$ за состояние с $k$ активных
# программистов, $q$ заявок в очереди, $n$ заявок на проверке на вирусы и $m$
# заявок на основном сервере:
# - $S_{4000}$ - все программисты пишут программу, сервера и компьютеры
# свободны;
# - $S_{3010}$ - три программиста пишут программу, одна программа проверяется на
# вирус;
# - $S_{3001}$ - три программиста пишут программу, одна программа выполняется
# на сервере;
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
                dP_0013
            ))
        )
    })
}

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
    P_0013 = 0
)


INITIAL_TIME <- 0
FINISH_TIME <- 10000

ACCURACY <- 0.1
times <- seq(INITIAL_TIME, FINISH_TIME, by = ACCURACY)
output <- ode(yini, times, ode_system_equations, pars)
output

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

# %% [markdown]
Создадим вспомогательную функцию для взаимодействия с результатами.

# %%
if (!require("hash")) {
    install.packages("hash")
}
library(hash)
probabilities <- hash(results)

# %%
P <- function(P_index) {
    hash_index <- paste("P_", P_index, sep = "")

    try(
        if (!has.key(hash_index, probabilities)) {
            stop("No P value with index: ", P_index)
        }
    )

    return(probabilities[[hash_index]])
}

# %% [markdown]
# #### Среднее число заявок в системе
# Для вычисления среднего числа заявок в системе просуммируем произведения вероятностей
# на соответствующие этим вероятностям значения заявок в системе
# (длина очереди + количество заявок на проверке антивируса + количество
# заявок на обслуживании):
# $$
# L = 0 \cdot P_{4000}(t) + 1 \cdot P_{3010}(t) + (1 + 1) \cdot P_{2110}(t) +
# 1 \cdot P_{3001}(t) + (1 + 1) \cdot P_{2011}(t) + (1 + 1 + 1) \cdot P_{1111}(t) +
# 2 \cdot P_{2002}(t) + (1 + 2) \cdot P_{1012}(t) + (1 + 1 + 2) \cdot P_{0112}(t) +
# 3 \cdot P_{1003}(t) + (1 + 3) \cdot P_{0013}(t)
# $$

Упростив, получаем:
# $$
# L = P_{3010}(t) + 2 \cdot P_{2110}(t) +
# P_{3001}(t) + 2 \cdot P_{2011}(t) + 3 \cdot P_{1111}(t) +
# 2 \cdot P_{2002}(t) + 3 \cdot P_{1012}(t) + 4 \cdot P_{0112}(t) +
# 3 \cdot P_{1003}(t) + 4 \cdot P_{0013}(t)
# $$

# %%
mean_number_of_requests <- P("3010") + 2 * P("2110") + P("3001") + 2 * P("2011") +
    3 * P("1111") + 2 * P("2002") + 3 * P("1012") + 4 * P("0112") + 3 * P("1003") +
    4 * P("0013")
mean_number_of_requests

# %% [markdown]
# #### Абсолютная пропускная способность
# Для определения абсолютной пропускной способности учтем, что
# заявки-программы, в которых обнаружен вирус не получают полного обслуживания
# (отклоняются), т.е. нас интересует среднее число заявок получивших полное
# обслуживание в единицу времени. Для замкнутых систем абсолютная пропускная
# способность выражается как
# $$
# \lambda'=P_{\text{зан}}\cdot \mu
# $$

# %%
absolute_flow_capacity <- mu * (
    3 * (P("1003") + P("0013")) +
    2 * (P("2002") + P("1012") + P("0112")) +
    P("3001") + P("2011") + P("1111")
)
absolute_flow_capacity


# %% [markdown]
# #### Среднее время пребывания заявки в системе
# $$
# T =\frac{L}{\lambda'}
# $$

# %%
T <- mean_number_of_requests / absolute_flow_capacity
T

# %% [markdown]
# ### Численно
# Построим с помощью пакета simmer симуляцию системы.

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
Зададим траекторию отказа в случае полной очереди:

# %%
antivirus_server_path <- trajectory("antivrus server path") %>%
    seize("antivirus_server", amount = 1) %>%
    timeout(function() rexp(1, nu)) %>%
    release("antivirus_server", amount = 1) %>%
    leave(
          function() runif(1) < p,
          out = trajectory() %>% log_("Antivirus found", level = 3)
    ) %>%
    log_("No antivirus found", level = 3)

main_server <- trajectory("main server path") %>%
    seize("server", amount = 1) %>%
    timeout(function() rexp(1, mu)) %>%
    release("server", amount = 1)

# %%
SIMULATION_TIME <- 10000

envs <- mclapply(1:30, function(i) {
    env <- simmer("SuperDuperSim", log_level = 2)

    programmers_source <- trajectory("programmers source path") %>%
            set_source("program", function() {
                number_of_free_programmers <- get_global(env, "number_of_free_programmers")

                if (number_of_free_programmers <= 0) {
                    return(0.5)
                } else {
                    return(rexp(1, lambda * number_of_free_programmers))
                }
            })

    programmers_path <- trajectory("program's path") %>%
            join(antivirus_server_path) %>%
            set_global("number_of_free_programmers", function() {
                on_server <- get_server_count(env, resources = c('antivirus_server', 'server'))
                in_queue <- get_queue_count(env, resources = c('antivirus_server', 'server'))

                in_system <- sum(on_server) + sum(in_queue)

                number_of_free_programmers <- K - in_system

                return(number_of_free_programmers)
            }) %>%
            join(programmers_source) %>%
            join(main_server) %>%
            set_global("number_of_free_programmers", value = 1, mod = "+") %>%
            join(programmers_source)

    return(env %>%
        add_resource(
            "antivirus_server",
            capacity = N,
            queue_size = m1
        ) %>%
        add_resource(
            "server",
            capacity = M,
            queue_size = m2
        ) %>%
        add_generator(
            "program",
            programmers_path,
            function() rexp(1, K * lambda)
        ) %>%
        run(until = SIMULATION_TIME) %>%
        wrap()
    )
})

# %%
arrivals <- get_mon_arrivals(envs)
arrivals

# %% [markdown]
# Логи симуляции:

# %%
resources <- get_mon_resources(envs)
resources

# %% [markdown]
# #### Абсолютную пропускную способность
# Абсолютную пропускную способность найдем следующим образом:

# %%
number_of_finished <- arrivals %>% with(sum(finished))
number_of_finished

# %%
finish_probability <- number_of_finished / nrow(arrivals)
finish_probability

# %%
absolute_flow_capacity <- lambda * finish_probability
absolute_flow_capacity

# %% [markdown]
# #### Среднее число заявок в системе
# Рассчитаем значение, воспользовавшись таблицей использования ресурсов:

# %%
mean_number_of_requests <- resources %>% with(
    mean(queue) + mean(server) + mean(system)
)
mean_number_of_requests

# %% [markdown]
# #### Среднее время нахождения заявок в системе
# Найдем на основании практически величин среднее время нахождения заявок
# в системе

# %%
T <- mean_number_of_requests / absolute_flow_capacity
T

# %% [markdown]
# ## Вывод
# Как видно, теоретически вычисленное значение с некоторой точностью совпадает
# со значением, полученным теоретически. При увеличении количества
# экспериментов 𝑁 точность только увеличивается.

# %% [markdown]
# ### Теоретически методом укрупненных состояний
# Проанализируем сложную систему по частям, с помощью укрупненных (агрегированных) моделей.
# В каждой модели подробно представим только некоторую часть системы, а влияние
# остальных частей отразим некоторым обобщенным параметром (параметром связи).

# Предложим декомпозицию нашей задачи в виде двух моделей.

# В первой модели совокупность серверов и компьютеров по проверке наличия
# вирусов заменим только одним обобщенный параметром - интенсивностью
# обслуживания общ $\mu_{\text{общ}}$.

# ![Programmers graph](./Programmers_graph.png)

# Как видно, модель будет обычной моделью
# рождения гибели для замкнутой системы

# %% [markdown]
# $$
# P_0 = \left(1 + \frac{K\lambda}{\mu_{\text{общ}}} + \frac{K(K-1)\lambda^2}{\mu_{\text{общ}}^2} + \ldots + \frac{K!\lambda^K}{\mu_{\text{общ}}^K}\right)^{-1}
# $$

# %% [markdown]
# #### Итерация 1
# Пусть $\mu_{\text{общ}} = 0.15 < \lambda'$. Тогда:

# %%
mu_ <- 0.15
y_ <- lambda / mu_
y_

# %% [markdown]
# Каждый член суммы можно также представить:
# $$
# \text{sum_part}_i =\frac{K!}{(K-i)!} \cdot \frac{\lambda}{\mu_{\text{общ}}}
# $$

# %%
i <- 0:K
sum_parts <- c(factorial(K) / factorial(K - i) * y_^i)
sum_parts

# %%
P0 <- (sum(sum_parts)
)^(-1)
P0

# %% [markdown]
# $$
# P_1 = P_0 \frac{K\lambda}{\mu_{\text{общ}}} = P_0 \cdot \text{sum_part}_1 \\
# P_2 = P_1 \frac{(K - 1)\lambda}{\mu_{\text{общ}}} = P_0 \cdot \text{sum_part}_2 \\
# P_3 = P_2\frac{(K - 2)\lambda}{\mu_{\text{общ}}} = P_0 \cdot \text{sum_part}_3 \\
# P_i = P_{i-1}\frac{(K - (i - 1))\lambda}{\mu_{\text{общ}}} = P_0 \cdot
# \text{sum_part}_i
# $$


# %%
Pi <- unlist(lapply(sum_parts, function(sum_part) P0 * sum_part))
Pi

# %% [markdown]
# Проверим, что вероятности имеют адекватные значения.

# %%
sum(Pi)

# %% [markdown]
# $$
# L_{\text{сист}} = \sum_{k=1}^K k \cdot P_k
# $$

# %%
mean_number_of_requests1 <- sum(c(1:K) * Pi[2:(K+1)])
mean_number_of_requests1

# %% [markdown]
# Во второй модели будем считать, что в системе постоянно циркулируют
# $L_{\text{сист}}$ заявок. В качестве состояний системы возьмем количество
# заявок, обрабатываемых на основных серверах:
# $$
# S_0, S_1, S_2, \ldots, S_{L_{\text{сист}}}
# $$

# %% [markdown]
# ![Servers graph](./Servers_graph.png)

# %% [markdown]
# В данной итерации, округляя, мы получили $[L_{\text{сист}} \approx 3.47] = 3$

# %% [markdown]
# $$
# \nu_n = (1-p)\nu \cdot \min{(N,L_{\text{сист}}-n)}, n=0, \ldots, L_{\text{сист}} - 1 \\
# $$

# %%
mean_number_of_requests1 <- round(mean_number_of_requests1)
n <- 0:(mean_number_of_requests1 - 1)

nu_n <- unlist(lapply(n, function(n) (1 - p) * nu * min(N, mean_number_of_requests1 - n)))
nu_n

# %% [markdown]
# $$
# \mu_k = \mu \cdot \min{(M, k)}, k=1, 2, \ldots, L_{\text{сист}}
# $$

# %%
k <- 1:mean_number_of_requests1

mu_k <- unlist(lapply(k, function(k) mu * min(M, k)))
mu_k

# %% [markdown]
# Аналогичным образом посчитаем финальные вероятности:
# $$
# \pi_0 = \left(1+\frac{\nu_0}{\mu_1}+\frac{\nu_0\nu_1}{\mu_1\mu_2}+\ldots\right)^{-1} \\
# \pi_1=\pi_0\cdot \frac{\nu_0}{\mu_1} \\
# \pi_2=\pi_0\cdot \frac{\nu_0\nu_1}{\mu_1\mu_2} \\
# \dots
# $$

# %%
sum_parts <- c()
sum_parts[1] <- 1
sum_parts[2] <- sum_parts[1] * nu_n[1] / mu_k[1]
sum_parts[3] <- sum_parts[2] * nu_n[2] / mu_k[2]
sum_parts[4] <- sum_parts[3] * nu_n[3] / mu_k[3]

pi_0 <- (sum(sum_parts))^(-1)
pi_0

# %%
pi_i <- unlist(lapply(sum_parts, function(sum_part) pi_0 * sum_part))
pi_i

# %% [markdown]
# Проверим, что вероятности имеют адекватные значения.

# %%
sum(pi_i)

# %% [markdown]
# Тогда:
# $$
# \mu_{\text{общ}} = \sum_{n=1}^{L_{\text{сист}}}\pi_n\cdot \mu_n
# $$

# %%
mu_ <- sum(mu_k[1:(mean_number_of_requests1)] * pi_i[2:(mean_number_of_requests1 + 1)])
mu_

# %% [markdown]
# #### Итерация 2

# %%
y_ <- lambda / mu_
y_

# %% [markdown]
# Каждый член суммы можно также представить:
# $$
# \text{sum_part}_i =\frac{K!}{(K-i)!} \cdot \frac{\lambda}{\mu_{\text{общ}}}
# $$

# %%
i <- 0:K
sum_parts <- c(factorial(K) / factorial(K - i) * y_^i)
sum_parts

# %%
P0 <- (sum(sum_parts)
)^(-1)
P0

# %% [markdown]
# $$
# P_1 = P_0 \frac{K\lambda}{\mu_{\text{общ}}} = P_0 \cdot \text{sum_part}_1 \\
# P_2 = P_1 \frac{(K - 1)\lambda}{\mu_{\text{общ}}} = P_0 \cdot \text{sum_part}_2 \\
# P_3 = P_2\frac{(K - 2)\lambda}{\mu_{\text{общ}}} = P_0 \cdot \text{sum_part}_3 \\
# P_i = P_{i-1}\frac{(K - (i - 1))\lambda}{\mu_{\text{общ}}} = P_0 \cdot
# \text{sum_part}_i
# $$


# %%
Pi <- unlist(lapply(sum_parts, function(sum_part) P0 * sum_part))
Pi

# %% [markdown]
# Проверим, что вероятности имеют адекватные значения.

# %%
sum(Pi)

# %% [markdown]
# $$
# L_{\text{сист}} = \sum_{k=1}^K k \cdot P_k
# $$

# %%
mean_number_of_requests1 <- sum(c(1:K) * Pi[2:(K+1)])
mean_number_of_requests1

# %% [markdown]
# В данной итерации, округляя, мы получили $[L_{\text{сист}} \approx 3.47]
# = 3$. Ясно, что снова выбрав 3, ничего не поменяется, поэтому попробуем
# $L_{\text{сист}} = 2$.

# %% [markdown]
# $$
# \nu_n = (1-p)\nu \cdot \min{(N,L_{\text{сист}}-n)}, n=0, \ldots, L_{\text{сист}} - 1 \\
# $$

# %%
mean_number_of_requests1 <- 2
n <- 0:(mean_number_of_requests1 - 1)

nu_n <- unlist(lapply(n, function(n) (1 - p) * nu * min(N, mean_number_of_requests1 - n)))
nu_n

# %% [markdown]
# $$
# \mu_k = \mu \cdot \min{(M, k)}, k=1, 2, \ldots, L_{\text{сист}}
# $$

# %%
k <- 1:mean_number_of_requests1

mu_k <- unlist(lapply(k, function(k) mu * min(M, k)))
mu_k

# %% [markdown]
# Аналогичным образом посчитаем финальные вероятности:
# $$
# \pi_0 = \left(1+\frac{\nu_0}{\mu_1}+\frac{\nu_0\nu_1}{\mu_1\mu_2}+\ldots\right)^{-1} \\
# \pi_1=\pi_0\cdot \frac{\nu_0}{\mu_1} \\
# \pi_2=\pi_0\cdot \frac{\nu_0\nu_1}{\mu_1\mu_2} \\
# \dots
# $$

# %%
sum_parts <- c()
sum_parts[1] <- 1
sum_parts[2] <- sum_parts[1] * nu_n[1] / mu_k[1]
sum_parts[3] <- sum_parts[2] * nu_n[2] / mu_k[2]

pi_0 <- (sum(sum_parts))^(-1)
pi_0

# %%
pi_i <- unlist(lapply(sum_parts, function(sum_part) pi_0 * sum_part))
pi_i
# %% [markdown]
# Проверим, что вероятности имеют адекватные значения.

# %%
sum(pi_i)

# %% [markdown]
# Тогда:
# $$
# \mu_{\text{общ}} = \sum_{n=1}^{L_{\text{сист}}}\pi_n\cdot \mu_n
# $$

# %%
mu_ <- sum(mu_k[1:(mean_number_of_requests1)] * pi_i[2:(mean_number_of_requests1 + 1)])
mu_

# %% [markdown]
# $$
# T_{\text{сист}} = \frac{L_{\text{сист}}}{\lambda(K-L_{\text{сист}})}
# $$

# %%
T <- mean_number_of_requests1 / (lambda * (K - mean_number_of_requests1))
T

# %% [markdown]
# #### Итого

# %%
c(mu_, T)

# %% [markdown]
# ### Выводы
# Как видно, метод укрупнённых состояний позволяет упростить вычисления в очень
# больших системах. Однако приходится пожертвовать точностью вычислений, что
# связано с округлением $L_{\text{сист}}$ до целых.
