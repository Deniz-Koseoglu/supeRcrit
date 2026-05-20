## DOE Arzu Edilirlik Optimizasyonu

Bu modül, **Derringer-Suich arzu edilirlik fonksiyonu** yaklaşımını (**Derringer & Suich, 1980**) kullanarak çoklu yanıt optimizasyonu gerçekleştirir. Daha önce analiz edilmiş DOE modellerinden birden fazla yanıt değişkeni için hedefleri aynı anda karşılayan faktör ayarlarını bulur.

### İş Akışı

**1. Analiz Seçimi:** Kaydedilmiş DOE Analiz sonuçlarını seçin.

**2. Arzu Edilirlik Ayarları:** Her yanıt için hedefi (maksimize, minimize veya hedef değer), kabul edilebilir aralığı ve önem ağırlığını ayarlayın.

**3. Faktör Aralıkları:** Her faktör için arama alanını tanımlayın.

**4. Optimizasyon:** Optimizer **genel arzu edilirliği** *D*'yi maksimize eden ayarları arar:

$$D = \left(\prod_{i=1}^{n} d_i^{w_i}\right)^{1/\sum w_i}$$

burada *d*ᵢ bireysel arzu edilirlik (0–1) ve *w*ᵢ *i*. yanıtın ağırlığıdır.

### Kaynaklar

**Cardoso, R.P., et al.** (2023), *Management and Administrative Professional Review* **14** (1), pp. 564-578. [DOI](http://dx.doi.org/10.7769/gesec.v14i1.1536)

**Cojocaru, C., et al.** (2009), *Journal of Hazardous Materials* **167**, pp. 52-63. [DOI](http://dx.doi.org/10.1016/j.jhazmat.2008.12.078)

**Derringer, G., Suich, R.** (1980), *Journal of Quality Technology* **12** (4), pp. 214-219. [DOI](https://doi.org/10.1080/00224065.1980.11980968)
