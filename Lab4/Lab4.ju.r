# %% [markdown]
# # Лабораторная работа № 4. Модель однофазной многоканальной замкнутой системы обслуживания
# Выполнил Пакало Александр Сергеевич, студент РТ5-81Б

# Вариант № 5

# ## Задание № 1
# Вычислительный центр фирмы состоит из $m$
#  главных серверов коллективного пользования. Число работающих в центре
#  программистов в любой момент времени равно
# $k$. Каждый программист готовит свою программу и через терминал передает ее
# на сервер для выполнения, куда она сразу попадает. Время подготовки программ
# имеет экспоненциальное распределение со средним значением
# $t_1$ мин. Время выполнения программы на любом из серверов имеет экспоненциальное
#  распределение со средним значением
#  $t_2$ мин. Каждый программист ожидает ответа от сервера, прежде, чем начнет писать следующую программу.

# Найти (теоретически и экспериментально):
# 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал.
# 2. Среднее время до получения пользователем результатов реализации.
# 3. Среднее количество программ, ожидающих выполнения на сервере.

# Требуется выполнить расчет при заданном по варианту значении
# $m$, а также при $m = 1$.

# %%
Variant <- 5
set.seed(Variant)
k <- sample(c(10:25), 1)
m <- sample(c(3:6), 1)
t1 <- sample(c(14:20), 1)
t2 <- sample(c(2:6), 1)
View(data.frame(k, m, t1, t2))

# %% [markdown]
# Найдем интенсивность выполнения программ:

# %%
lambda_1 <- 1 / t1
lambda_1

# %% [markdown]
# Интенсивность поступления программ от $k$ программистов:

# %%
lambda_k <- k * lambda_1
lambda_k

# %% [markdown]
# Интенсивность выполнения программ:

# %%
miu <- 1 / t2
miu

# %% [markdown]
# Интенсивность выполнения программ меньше интенсивности поступления,
# следовательно очередь в нашей системе будет бесконечно расти. Согласно
# указаниям преподавателя, изменим начальные параметры так, чтобы неравенство
# обратилось:

# %%
t1 <- 45
lambda_k <- k / t1
lambda_k

# %% [markdown]
# Итого имеем следующие начальные величины:

# %%
View(data.frame(k, m, t1, t2))

# %%
# Зададим таблицу результатов.

# %%
results <- data.frame(probability = c("-", "-"), T = c("-", "-"), Lqueue = c("-", "-"))
row.names(results) <- c('theoretical', 'practical')
results

# %% [markdown]
# ### Теоретически

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал
# Мы имеем дело с многоканальной **замкнутой** СМО с неограниченной очередью
# $M / M / m / \infty$. Следовательно:

# $$
# P_0 = \left(
#   \sum_{i = 0}^m \frac{k! \rho^i}{\left(k - i \right)!i!}
#   +
#   \sum_{i = 1}^{k - m} \frac{k! \rho^{m + i}}{m! \cdot (k - m - i)!m^i}
# \right) ^ {-1}
# $$
# где $\rho = \frac{\lambda}{\mu}$

# %%
ro <- lambda_1 / miu
ro

# %%
i1 <- 0:m
parts_of_sum1 <- c(factorial(k) * ro^i1 / (factorial((k - i1)) * factorial(i1)))

i2 <- 1:(k - m)
parts_of_sum2 <- c(factorial(k) * ro^(m + i2) / (factorial(m) * factorial((k - m - i2)) * m^i2))

P0 <- (sum(parts_of_sum1) + sum(parts_of_sum2))^(-1)
P0

# %%
P0 * k * ro

# %% [markdown]
# Тогда вероятность того, что $i$ серверов занято:
# $$
# P_i = P_0 \cdot \frac{k! \rho^i}{\left(k - i \right)!i!}
# $$

# Для этого воспользуемся уже созданным массивом частей суммы:

# %%
get_Pi <- function(part) part * P0

# %%
Pi <- lapply(parts_of_sum1[1:m], get_Pi)
Pi

# %% [markdown]
# Тогда $\sum_{i = 0}^{m} P_i$ -  вероятность того, что хотя бы один сервер
# будет доступен. Следовательно, обратная вероятность и будет нашим ответом.

# %%
probability <- 1 - sum(unlist(Pi))
results$probability[1] <- probability
probability

# %% [markdown]
# #### 2. Среднее время до получения пользователем результатов реализации.
# По формуле Литтла, подходящей для СМО любого вида имеем:
# $$
# T_{\text{сист}} = \frac{L_{\text{сист}}}{\Lambda} \\
# \Lambda = (k - L_{\text{сист}}) \cdot \lambda \\
# L_{\text{сист}} = \sum_{i = 1}^k \left(P_i \cdot i \right)
# $$

# %%
Pi_queue <- mapply(function(Pi, i) Pi * i, Pi[1:k], 1:k)
Lsys <- sum(unlist(Pi))
Lsys

# %%
Lambda <- (k - Lsys) * lambda_1
Lambda

# %%
T <- Lsys / Lambda
results$T[1] <- T
T

# %% [markdown]
# #### 3. Среднее количество программ, ожидающих выполнения на сервере.
# Она же средняя длина очереди:
# $$
# L_{\text{оч}} = \frac{\rho^{m+1}}{m! \cdot m} \cdot P_0 \cdot
# \frac{1}{\left( 1 - \frac{\rho}{m} \right)^2}
# $$

# %%
Lqueue <- ro^(m + 1) / (factorial(m) * m) * P0 * 1 / (1 - ro / m)^2
results$Lqueue[1] <- Lqueue
Lqueue

# %%
results

# %% [markdown]
# Как видно, очередь система работает достаточно быстро, чтобы не накапливать очередь.
# Это подтверждается и при сравнении интенсивностей поступления / обработки программ.

# %% [markdown]
# ### Численно

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

SIMULATION_TIME <- 10000

env <- simmer("SupaDupaSim")

programs <- trajectory() %>%
    seize("server", 1) %>%
    timeout(function() rexp(1, 1 / t1)) %>%
    release("server", 1)

env %>%
    add_resource("server", capacity = m, queue_size = k - m) %>%
    add_generator(
        "programs",
        programs,
        distribution = function() rexp(1, lambda_k)
    ) %>%
    run(until = SIMULATION_TIME)

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал.

# %%
resources <- get_mon_resources(env)
resources

# %%
probability <- resources %>% with(sum(server == m) / length(server)) / 2
results$probability[2] <- probability

# %% [markdown]
# #### 2. Среднее время до получения пользователем результатов реализации.

# %%
arrivals <- get_mon_arrivals(env)
arrivals

# %%
T <-arrivals %>% subset(finished) %>% with(mean(end_time - start_time))
results$T[2] <- T

# %% [markdown]
# #### 3. Среднее количество программ, ожидающих выполнения на сервере.

# %%
Lqueue <- resources %>% with(mean(queue))
results$Lqueue[2] <- Lqueue

# %%
results

# %% [markdown]
# ### Для $m = 1$
# Зададим таблицу результатов для $m = 1$.

# %%
mm1.results <- data.frame(probability = c("-", "-"), T = c("-", "-"), Lqueue = c("-", "-"))
row.names(mm1.results) <- c('theoretical', 'practical')
mm1.results

# %% [markdown]
# ### Теоретически для $m = 1$

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал.

# $$
# 1 - P_0 = \frac{\lambda}{\mu}
# $$

# %%
mm1.probability <- lambda_1 / miu
mm1.results$probability[1] <- mm1.probability
mm1.probability

# %% [markdown]
# #### 2. Среднее время до получения пользователем результатов реализации.
# Вычислим среднее время до получения пользователем результатов реализации:

# $$
# T_{\text{сист}} = \frac{1}{\mu(1 - \rho)}
# $$

# %%
mm1.T <- 1 / (miu * (1 - ro))
mm1.results$T[1] <- mm1.T
mm1.T

# %% [markdown]
# #### 3. Среднее количество программ, ожидающих выполнения на сервере.
# Она же средняя длина очереди:
# $$
# L_{\text{оч}} = \frac{\rho^{2}}{1 - \rho}
# $$

# %%
mm1.Lqueue <- ro^2 / (1 - ro)
mm1.results$Lqueue[1] <- mm1.Lqueue
mm1.Lqueue

# %%
mm1.results

# %% [markdown]
# Как видно, очередь система работает достаточно быстро, чтобы не накапливать очередь.
# Это подтверждается и при сравнении интенсивностей поступления / обработки программ.

# %% [markdown]
# ### Численно для $m = 1$

# %%
if (!require("simmer")) {
    install.packages("simmer")
}
library(simmer)

SIMULATION_TIME <- 10000

env <- simmer("SupaDupaSim")

programs <- trajectory() %>%
    seize("server", 1) %>%
    timeout(function() rexp(1, 1 / t1)) %>%
    release("server", 1)

env %>%
    add_resource("server", capacity = 1, queue_size = k - 1) %>%
    add_generator(
        "programs",
        programs,
        distribution = function() rexp(1, lambda_k)
    ) %>%
    run(until = SIMULATION_TIME)

# %% [markdown]
# #### 1. Вероятность того, что программа не будет выполнена сразу же, как только она поступила на терминал.

# %%
mm1.resources <- get_mon_resources(env)

# %%
mm1.probability <- mm1.resources %>% with(sum(server == 1) / length(server))
mm1.results$probability[2] <- mm1.probability

# %% [markdown]
# #### 2. Среднее время до получения пользователем результатов реализации.

# %%
mm1.arrivals <- get_mon_arrivals(env)

# %%
mm1.T <- mm1.arrivals %>% subset(finished) %>% with(mean(end_time - start_time))
mm1.results$T[2] <- T

# %% [markdown]
# #### 3. Среднее количество программ, ожидающих выполнения на сервере.

# %%
mm1.Lqueue <- mm1.resources %>% with(mean(queue))
mm1.results$Lqueue[2] <- mm1.Lqueue

# %%
mm1.results
