## Karışabilirlik Optimizasyonu

Bu modül, **Hansen Çözünürlük Parametreleri (HSP)** ve **Tirado et al. (2018, 2019)** çalışmalarına dayanarak hedef çözünenin süperkritik CO₂'deki karışabilirliğini en iyi artıran ko-çözücüleri değerlendirir.

### Yöntem

Çözünen HSP'leri (Çözünen Karakterizasyonu modülünde tahmin edilir) saf CO₂ ve CO₂ + ko-çözücü karışımlarının HSP'leri ile basınç ve sıcaklık aralığında karşılaştırılır. Çözücü (indis 1) ile çözünen (indis 2) arasındaki HSP mesafesi *Rₐ*:

$$R_a = \sqrt{4 \times (\delta_{d1} - \delta_{d2})^2 + (\delta_{p1} - \delta_{p2})^2 + (\delta_{HB1} - \delta_{HB2})^2}$$

**Karışabilirlik İyileştirmesi (ME%)**:

$$ME\ (\%) = \left(1 - \frac{R_{a,\ scCO_2 + koçözücü}}{R_{a,\ saf\ scCO_2}}\right) \times 100$$

Pozitif ME%, ko-çözücünün çözücü karışımını HSP uzayında çözünene yaklaştırdığını ve karışabilirliği artırdığını gösterir.

### Çözücü Karışımları

Özel ko-çözücü karışımları, belirlenen hacim fraksiyonlarında iki veya daha fazla çözücü birleştirilerek oluşturulabilir.

### Kaynaklar

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
