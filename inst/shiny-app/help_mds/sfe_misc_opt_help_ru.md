## Оптимизация смешиваемости

Этот модуль оценивает, какие со-растворители лучше всего повышают смешиваемость целевого вещества в сверхкритическом CO₂, на основе **параметров растворимости Хансена (HSP)** и работ **Tirado et al. (2018, 2019)**.

### Метод

HSP вещества (оценённые в модуле характеризации) сравниваются с HSP чистого CO₂ и смесей CO₂ + со-растворитель по сетке давлений и температур. Расстояние HSP *Rₐ* между растворителем (индекс 1) и веществом (индекс 2):

$$R_a = \sqrt{4 \times (\delta_{d1} - \delta_{d2})^2 + (\delta_{p1} - \delta_{p2})^2 + (\delta_{HB1} - \delta_{HB2})^2}$$

**Улучшение смешиваемости (ME%)** рассчитывается как:

$$ME\ (\%) = \left(1 - \frac{R_{a,\ scCO_2 + cosolvent}}{R_{a,\ pure\ scCO_2}}\right) \times 100$$

Положительное ME% означает, что со-растворитель приближает смесь к веществу в пространстве HSP, улучшая смешиваемость.

### Смеси растворителей

Пользовательские смеси со-растворителей создаются комбинированием двух или более растворителей в заданных объёмных долях.

### Литература

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
