# %% [markdown]
# Контрольная работа № 1

Выполнил: Пакало Александр Сергеевич, студент РТ5-81Б

Вариант № 2

# %% [markdown]
## Задание № 1

# %% [markdown]
Решение методом производящих функций.

Составим функции отвечающие вероятностям израсходования патронов для первого и второго стрелка соответственно:
$$
P_1(z) = 0.4 \cdot z^1 + 0.4 \cdot 0.6 \cdot z^2 + 0.4 \cdot 0.6 \cdot 0.6 \cdot z^3 \\
P_2(z) = 0.6 \cdot z^1 + 0.6 \cdot 0.4 \cdot z^2
$$

# %% [markdown]
Так как события независимы, $P_{1 + 2}(z) = P_1 \cdot P_2$

Посчитаем с помощью Maxima:

# %%
P1: 0.4 * z^1 + 0.4 * 0.6 * z^2 + 0.4 * 0.6 * 0.6 * z^3;
P2: 0.6 * z^1 + 0.6 * 0.4 * z^2;

# %% [markdown]
Получили закон распределения для величины $Z = X + Y$:

# %%
P: expand(P1 * P2);

# %% [markdown]
Тогда:
$$
M(Z) = P'_{1+2}(1) \\
D(Z)=P''_{1+2}(1) + P'_{1+2}(1) - \left(P'_{1+2}(1)\right)^2
$$

# %%
MZ: diff(P, z, 1);
DZ: diff(P, z, 2) + MZ - MZ^2;

# %% [markdown]
Получаем следующие M(Z) и D(Z):

# %%
at(MZ, [z=1]);
at(DZ, [z=1]);

# %% [markdown]
## Задание № 2
Чтобы понять, какая система выгоднее, следует вычислить характеристики для каждой из систем.

### Для $M/M/n/\infty$
Количество каналов: $n = 3$,

Интенсивность: $\lambda = 4$,

Среднее время обслуживания: $t = 0.5$, следовательно интенсивность обслуживания $\mu = \frac{1}{t} = 2$

#### a) Средняя длина очереди
$$
L_{\text{оч}}=\frac{y^{n+1}}{n!\cdot n}\cdot P_0\cdot \frac{1}{(1-y/n)^2} \\
$$
где
$$
y = \frac{\lambda}{\mu} \\
P_0=\left(\sum_{i=0}^n\frac{y^i}{i!}+\frac{y^{n+1}}{n!(n-y)}\right)^{-1}
$$

# %%
n: 3;
y: 4 / 2;
P0: (
    sum(y^i / i!, i, 0, n) +
    y^(n+1) / (n! * (n - y))
)^-1;

# %% [markdown]
Тогда $L_{\text{оч}}$:
# %%
L: y^(n+1) / (n!*n) * P0 * 1/(1 - y / n)^2;

# %% [markdown]
#### б) Среднее время пребывания в очереди
$$W_{\text{оч}}=\frac{L_{\text{оч}}}{\lambda}$$

# %%
a: 4;
L / a;

# %% [markdown]
#### в) Среднее время пребывания в СМО
$$
L_{\text{сист}}=L_{\text{оч}}+y \\
$$

# %%
Lsys: L + y;

# %% [markdown]
Тогда $W_{\text{сист}}=\frac{L_{\text{сист}}}{\lambda}$:

# %%
Lsys / a;

# %% [markdown]
### Для СМО $M/M/1/\infty$
Время обслуживания в три раза меньше, следовательно интенсивность обслуживания в три раза больше:
$$
\mu = 6
$$

#### a) Средняя длина очереди
$$
L_{\text{оч}}=\frac{y^2}{(1-y)}
$$
где
$$
y = \frac{\lambda}{\mu} \\
$$

# %%
y: 4 / 6;

# %% [markdown]
Тогда $L_{\text{оч}}$:

# %%
L: y^2/(1-y);

# %% [markdown]
#### б) Среднее время пребывания в очереди
$$W_{\text{оч}}=\frac{y^2}{\lambda(1-y)}$$

# %%
y^2 / (a * (1 - y));

# %% [markdown]
#### в) Среднее время пребывания в СМО
$$
W_{\text{сист}}=\frac{y}{\lambda(1-y)}
$$

# %%
y / (a * (1 - y));

# %% [markdown]
## Сравним СМО
a) Первая СМО выгоднее, так как $\frac{8}{9} < \frac{4}{3}$

б) Вторая СМО выгоднее, так как $\frac{2}{9} > \frac{1}{3}$

в) Вторая СМО выгоднее, так как $\frac{13}{18} > \frac{1}{2}$

# %% [markdown]
### Задание № 3
Перечислиим состояния СМО:
- $P_0$ - все каналы свободны,
- $P_1$ - один канал занят,
- $P_2$ - два канала заняты,
- $P_3$ - три канала заняты,
- $P_4$ - три канала заняты, 1 в очереди;
- $P_5$ - три канала заняты, 2 в очереди;
- ...
- $P_{k+3}$ - три канала заняты, k в очереди;


# %% [markdown]
Тогда граф:
![](./Task3States1.png)

# %% [markdown]
Как можно заметить, будут работать те же формулы, то и в задании №2, только $y = \frac{\lambda}{\mu p}$

# %% [markdown]
$$
L_{\text{оч}}=\frac{y^{n+1}}{n!\cdot n}\cdot P_0\cdot \frac{1}{(1-y/n)^2} \\
$$
где
$$
y = \frac{\lambda}{\mu p} \\
P_0=\left(\sum_{i=0}^n\frac{y^i}{i!}+\frac{y^{n+1}}{n!(n-y)}\right)^{-1}
$$

# %% [markdown]
#### Средняя длина очереди

# %%
n: 3;
a: 0.7;
m: 0.8;
p: 0.5;
y: a / m / p;

# %%
P0: (
    sum(y^i / i!, i, 0, n) +
    y^(n+1) / (n! * (n - y))
)^-1;

# %% [markdown]
Тогда $L_{\text{оч}}$:
# %%
L: y^(n+1) / (n!*n) * P0 * 1/(1 - y / n)^2;

# %% [markdown]
#### Среднее время пребывания в очереди
$$W_{\text{оч}}=\frac{L_{\text{оч}}}{\lambda}$$

# %%
W: L / a;

# %% [markdown]
#### Среднее время пребывания в СМО
$$
L_{\text{сист}}=L_{\text{оч}}+y \\
$$

# %%
Lsys: L + y;

# %% [markdown]
Тогда $T_{\text{сист}}=\frac{L_{\text{сист}}}{\lambda}$:

# %%
Lsys / a;

# %% [markdown]
#### Абсолютная пропускная способность
$$\lambda'=\lambda\cdot \left(1-\frac{y^n}{n!}\cdot P_0\right)$$

# %%
a * (1 - y^n / n! * P0);

# %% [markdown]
#### Среднее время пребывания заявки в системе
$$
T_{\text{сист}}=W_{\text{оч}}+\frac{1}{\mu}
$$

# %%
W + 1 / m;

# %% [markdown]
# ## Задание № 4
# ### Граф состояний
# ![](./Task4States.png)

# %% [markdown]
# Выведем уравнения Колмогорова и выполним преобразования Лапласа:
$$sP_{0}(s) - 1 = \mu_{1}P_{1}(s) + \mu_{2}P_{4}(s) - \lambda_{1}P_{0}(s) - \lambda_{2}P_{0}(s)$$ 


$$sP_{1}(s) = \mu_{1}P_{2}(s) + \mu_{2}P_{5}(s) + \lambda_{1}P_{0}(s) - \mu_{1}P_{1}(s) - \lambda_{1}P_{1}(s)  - \lambda_{2}P_{1}(s)$$


$$sP_{2}(s) = \mu_{1}P_{3}(s) + \mu_{2}P_{6}(s) + \lambda_{1}P_{1}(s) - \mu_{1}P_{2}(s) - \lambda_{1}P_{2}(s)  - \lambda_{2}P_{2}(s)$$


$$sP_{3}(s) = \lambda_{1}P_{2}(s) + \lambda_{1}P_{6}(s) - \mu_{1}P_{3}(s)$$


$$sP_{4}(s) = \mu_{1}P_{5}(s) + \mu_{2}P_{7}(s) + \lambda_{2}P_{0}(s) - \mu_{2}P_{4}(s) - \lambda_{1}P_{4}(s)  - \lambda_{2}P_{4}(s)$$


$$sP_{5}(s) = \mu_{1}P_{6}(s) + \mu_{2}P_{8}(s) + \lambda_{1}P_{4}(s) + \lambda_{2}P_{1}(s) - \mu_{2}P_{5}(s) - \lambda_{1}P_{5}(s)  - \lambda_{2}P_{5}(s) - \mu_{1}P_{5}(s)$$


$$sP_{6}(s) = \lambda_{1}P_{5}(s) + \lambda_{2}P_{2}(s) + \lambda_{1}P_{8}(s) - \mu_{2}P_{6}(s) - \mu_{1}P_{6}(s) - \lambda_{1}P_{6}(s) $$


$$sP_{7}(s) = \mu_{1}P_{8}(s) + \mu_{2}P_{9}(s) + \lambda_{2}P_{4}(s) - \mu_{2}P_{7}(s) - \lambda_{1}P_{7}(s)  - \lambda_{2}P_{7}(s)$$


$$sP_{8}(s) = \lambda_{1}P_{7}(s) + \lambda_{2}P_{5}(s) + \lambda_{1}P_{9}(s) - \mu_{2}P_{8}(s) - \mu_{1}P_{8}(s) - \lambda_{1}P_{8}(s) $$


$$sP_{9}(s) = \lambda_{2}P_{7}(s) - \mu_{2}P_{9}(s) - \lambda_{1}P_{9}(s)$$

# %%
mu1: 1.3;
mu2: 2.2;
lambda1: 1.1;
lambda2: 1.4;
s: 0;

solution: float(solve(
    [
        P0+P1+P2+P3+P4+P5+P6+P7+P8+P9=1, 
        s*P1=mu1*P2 + mu2*P5 + lambda1*P0 - mu1*P1 - lambda1*P1 - lambda2*P1, 
        s*P2=mu1*P3 + mu2*P6 + lambda1*P1 - mu1*P2 - lambda1*P2 - lambda2*P2, 
        s*P3=lambda1*P2 + lambda1*P6 - mu1*P3, 
        s*P4=mu1*P5 + mu2*P7 + lambda2*P0 - mu2*P4 - lambda1*P4 - lambda2*P4, 
        s*P5=mu1*P6 + mu2*P8 + lambda1*P4 + lambda2*P1 - mu2*P5 - lambda1*P5 - lambda2*P5 - mu1*P5, 
        s*P6=lambda1*P5 + lambda2*P2 + lambda1*P8 - mu2*P6 - mu1*P6 - lambda1*P6, 
        s*P7=mu1*P8 + mu2*P9 + lambda2*P4 - mu2*P7 - lambda1*P7 - lambda2*P7, 
        s*P8=lambda1*P7 + lambda2*P5 + lambda1*P9 - mu2*P8 - mu1*P8 - lambda1*P8, 
        s*P9=lambda2*P7 - mu2*P9 - lambda1*P9
    ],
    [P0, P1, P2, P3, P4, P5, P6, P7, P8, P9]
))[1];

# %% [markdown]
Вероятность отказа в момент поступления заявки 1:

# %%
solution[4];

# %% [markdown]
Вероятность отказа в момент поступления заявки 2:

# %%
solution[4] + solution[7] + solution[9] + solution[10];

# %% [markdown]
Среднеее число каналов, обслуживающих задачу 1:

# %%
solution[2] + solution[6] + solution[7] + 2 * (solution[8] + solution[9]) + 3 * solution[4];

# %% [markdown]
Среднеее число каналов, обслуживающих задачу 2:

# %%
solution[5] + solution[6] + solution[7] + 2 * (solution[8] + solution[9]) + 3 * solution[10];

# %% [markdown]
Вероятность отказа:

# %%
lambda1 / (lambda1 + lambda2) * solution[4] + 
lambda2 / (lambda1 + lambda2) * (solution[4] + solution[7] + solution[9] + solution[10]);

# %% [markdown]
Среднее число каналов, обслуживающих задачи:

# %%
solution[5] + solution[2] + 2 * (solution[3] + solution[6] + solution[8]) + 3 * (solution[4] + solution[7] + solution[9] + solution[10]);

# %% [markdown]
Среднее число заявок 1 и 2 и для системы в целом равно среднему числу каналов 1 и 2 и системы в целом, так как очередь отсутсвует.
