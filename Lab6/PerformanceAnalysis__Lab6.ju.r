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
# Введем следующие состояния:
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
if (!require("simmer.plot")) {
    install.packages("simmer.plot")
}
library(simmer.plot)

env <- simmer("SuperDuperSim")
env

# %% [markdown]
Зададим траекторию отказа в случае полной очереди:

# %%
queue <- trajectory("program's path") %>%
    # set_attribute("number_of_free_programmers", function() get_global(env, "number_of_free_programmers") - 1) %>%
    set_source("program", function() rexp(1, 1 / lambda )) %>%
    seize("antivirus_server", amount = 1) %>%
    timeout(function() rexp(1, 1 / nu)) %>%
    log_(function() {
        seized <- get_seized(env, resources = c('antivirus_server', 'server'))

        return(paste0(seized[1], seized[2]))
    }) %>%
    release("antivirus_server", amount = 1) %>%
    leave(
          function() runif(1) < p,
          out = trajectory() %>% log_("Antivirus found")
    ) %>%
    log_("No antivirus found") %>%
    seize("server", amount = 1) %>%
    timeout(function() rexp(1, 1 / mu)) %>%
    release("server", amount = 1)

# %%
SIMULATION_TIME <- FINISH_TIME

env %>%
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
    add_generator("program", queue, function() rexp(1, 1 / (K * lambda))) %>%
    run(until = SIMULATION_TIME)

# %%
arrivals <- get_mon_arrivals(env)
arrivals

# %% [markdown]
# Логи симуляции:

# %%
resources <- get_mon_resources(env)
resources

# %% [markdown]
# #### Абсолютную пропускную способность
# Абсолютную пропускную способность найдем следующим образом:
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
# Пусть $\mu_{\text{общ}} = \frac{\lambda'}{2}$. Тогда:

# %%
mu_ <- 0.1
y_ <- lambda / mu_
y_

# %%

P0 <- (1 +
    K * y_ +
    K * (K-1) * y_^2 +
    K * (K-1) * (K-2) * y_^3 +
    K * (K-1) * (K-2) * (K-3) * y_^K
)^(-1)
P0

# %% [markdown]
# $$
# P_1 = P_0 \frac{K\lambda}{\mu_{\text{общ}}}
# $$

# %%
P1 <- P0 * K * y_
P1

# %% [markdown]
# $$
# P_2 = P_0 \frac{K\lambda^2}{\mu_{\text{общ}}^2}
# $$

# %%
P2 <- P1 * (K - 1) *  y_
P2

# %% [markdown]
# $$
# P_3 = P_0\frac{K\lambda^3}{\mu_{\text{общ}}^3}
# $$

# %%
P3 <- P2 * (K - 2) * y_
P3

# %% [markdown]
# $$
# P_K = P_0\frac{K\lambda^K}{\mu_{\text{общ}}^K}
# $$

# %%
P4 <- P3 * (K - 3) * y_
P4

# %%
P0 + P1 + P2 + P3 + P4

# %% [markdown]
# $$
# L_{\text{сист}} = \sum_{k=1}^K k \cdot P_k
# $$

# %% [markdown]
# $$
# T_{\text{сист}} = \frac{L_{\text{сист}}}{\lambda(4-L_{\text{сист}})}
# $$

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
# $$
# \nu_n = (1-p)\nu \cdot \min{(N,L_{\text{сист}}-n)}, n=0, \ldots, L_{\text{сист}} - 1 \\
# \mu_n = \mu \cdot \min{(M, n)}, n=1, 2, \ldots, L_{\text{сист}}
# $$

# %% [markdown]
# Аналогичным образом посчитаем финальные вероятности:
# $$
# \pi_0 = \left(1+\frac{\nu_0}{\mu_1}+\frac{\nu_0\nu_1}{\mu_1\mu_2}+\ldots\right)^{-1} \\
# \pi_1=\pi_0\cdot \frac{\nu_0}{\mu_1} \\
# \pi_2=\pi_0\cdot \frac{\nu_0\nu_1}{\mu_1\mu_2} \\
# $$

# %% [markdown]
# Тогда:
# $$
# \mu_{\text{общ}}=\sum_{n=1}^{L_{\text{сист}}}\pi_n\cdot \mu_n\tag{11}
# $$

# %% [markdown]
# ### Выводы
# Как видно, теоретически вычисленные значения с некоторой точностью совпадают
# со значениями, полученным численным методом. При увеличении количества
# экспериментов 𝑁 точность только увеличивается.
