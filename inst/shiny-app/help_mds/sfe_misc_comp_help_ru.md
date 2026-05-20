## Сравнение смешиваемости

Этот модуль сравнивает смешиваемость **нескольких веществ** с заданным со-растворителем или пользовательской смесью, используя ту же методологию **параметров растворимости Хансена (HSP)**, что и модуль оптимизации. В отличие от оптимизации (множество со-растворителей для одного вещества), сравнение оценивает один со-растворитель для множества веществ одновременно.

### Рабочий процесс

Выберите одну или несколько сохранённых характеризаций из модуля характеризации, выберите со-растворитель (чистый или пользовательская смесь) и укажите давление(я), температуру и объёмную(ые) долю(и). Модуль рассчитывает расстояния HSP и улучшение смешиваемости (ME%) для каждого вещества при каждой комбинации давление–объёмная доля, формируя сравнительные тепловые карты и столбчатые диаграммы.

### Расстояние HSP и улучшение смешиваемости

Расстояние HSP *Rₐ* между смесью растворителей (индекс 1) и веществом (индекс 2):

$$R_a = \sqrt{4 \times (\delta_{d1} - \delta_{d2})^2 + (\delta_{p1} - \delta_{p2})^2 + (\delta_{HB1} - \delta_{HB2})^2}$$

Улучшение смешиваемости:

$$ME\ (\%) = \left(1 - \frac{R_{a,\ scCO_2 + cosolvent}}{R_{a,\ pure\ scCO_2}}\right) \times 100$$

Положительное значение ME% указывает на улучшение смешиваемости по сравнению с чистым CO₂.

### Литература

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), 'Pure and Pseudo-pure Fluid Thermophysical Property Evaluation and the Open-Source Thermophysical Property Library CoolProp', *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London, United Kingdom.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), 'Prediction of the best cosolvents to solubilise fatty acids in supercritical CO2 using the Hansen solubility theory', *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), 'The Selective Supercritical Extraction of High-value Fatty Acids from Tetraselmis suecica using the Hansen Solubility Theory', *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), 'The Hansen theory to choose the best cosolvent for supercritical CO2 extraction of beta-carotene from *Dunaliella salina*', *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
