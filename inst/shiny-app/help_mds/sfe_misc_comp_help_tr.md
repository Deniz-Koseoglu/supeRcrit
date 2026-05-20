## Karışabilirlik Karşılaştırması

Bu modül, **birden fazla çözünenin** belirli bir ko-çözücü veya özel karışım ile karışabilirliğini, Karışabilirlik Optimizasyonu modülüyle aynı **Hansen Çözünürlük Parametresi (HSP)** metodolojisini kullanarak karşılaştırır. Optimizasyon tek bir çözünen için birden fazla ko-çözücüyü değerlendirirken, Karşılaştırma tek bir ko-çözücüyü birden fazla çözünen için aynı anda değerlendirir.

### İş Akışı

Çözünen Karakterizasyonu modülünden bir veya daha fazla kaydedilmiş karakterizasyon seçin, bir ko-çözücü (saf veya özel karışım) seçin ve değerlendirilecek basınç(lar), sıcaklık ve hacim fraksiyonu(ları)nı belirtin. Modül, her bir basınç–hacim fraksiyonu kombinasyonunda her çözünen için HSP mesafelerini ve Karışabilirlik İyileştirmesini (ME%) hesaplayarak karşılaştırma ısı haritaları ve çubuk grafikleri üretir.

### HSP Mesafesi ve Karışabilirlik İyileştirmesi

Çözücü karışımı (alt indis 1) ile çözünen (alt indis 2) arasındaki HSP mesafesi *Rₐ*:

$$R_a = \sqrt{4 \times (\delta_{d1} - \delta_{d2})^2 + (\delta_{p1} - \delta_{p2})^2 + (\delta_{HB1} - \delta_{HB2})^2}$$

Karışabilirlik İyileştirmesi:

$$ME\ (\%) = \left(1 - \frac{R_{a,\ scCO_2 + cosolvent}}{R_{a,\ pure\ scCO_2}}\right) \times 100$$

Pozitif ME% değeri, saf CO₂'ye kıyasla iyileştirilmiş karışabilirliği gösterir.

### Kaynaklar

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), 'Pure and Pseudo-pure Fluid Thermophysical Property Evaluation and the Open-Source Thermophysical Property Library CoolProp', *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London, United Kingdom.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), 'Prediction of the best cosolvents to solubilise fatty acids in supercritical CO2 using the Hansen solubility theory', *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), 'The Selective Supercritical Extraction of High-value Fatty Acids from Tetraselmis suecica using the Hansen Solubility Theory', *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), 'The Hansen theory to choose the best cosolvent for supercritical CO2 extraction of beta-carotene from *Dunaliella salina*', *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
