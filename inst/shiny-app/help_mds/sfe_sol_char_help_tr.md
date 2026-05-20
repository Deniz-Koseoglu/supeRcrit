## Çözünen Karakterizasyonu

Bu modül, süperkritik ekstraksiyon proses tasarımı için gerekli termofiziksel özellikleri tahmin etmek üzere **Grup Katkı Yöntemleri (GKY)** kullanarak hedef çözünenleri karakterize eder. Tahmin edilen özellikler, **Hansen Çözünürlük Parametreleri (HSP)** aracılığıyla ko-çözücü uygunluğunu değerlendirmek için Karışabilirlik Optimizasyonu ve Karşılaştırma modüllerinde kullanılır.

### İş Akışı

Çözünenin SMILES dizesi ve moleküler geometrisi (MOL dosyası) verildiğinde, modül kaynama noktasını, kritik sıcaklığı ve Hansen Çözünürlük Parametrelerini GKY ile tahmin eder. Sıcaklığın çözünen HSP'leri üzerindeki etkisi indirgenmiş sıcaklıklardan hesaplanır. Çözücü ile çözünen arasındaki HSP mesafesi *Rₐ* karışabilirliği değerlendirmek için kullanılır.

### Tahmin Edilen Özellikler

**Normal kaynama noktası (Tᵦ)** Joback-Reid, Stein-Brown, Nannoolal (2004) veya Hukkerikar yöntemleri ile tahmin edilir. **Kritik sıcaklık (Tꞓ), basınç (Pꞓ) ve hacim (Vꞓ)** Joback-Reid, Nannoolal (2007) veya Hukkerikar yöntemleri ile tahmin edilir. **Hansen Çözünürlük Parametreleri (δD, δP, δH)** dispersiyon, polarite ve hidrojen bağı bileşenleri. Stefanis-Panayiotou (2008, 2012) veya Hukkerikar yöntemleri ile tahmin edilir. **Van der Waals hacmi** Zhao (2003), Bondi (1964) veya Slonimskii (1970) yöntemleri ile tahmin edilir.

### Parçalanma Basitliği

Molekülün GKY gruplarına nasıl ayrıştırıldığını kontrol eder. **Otomatik** her yöntem için en iyi ayarı seçer. **Basit** örtüşen gruplara izin verir. **Normal** örtüşmeleri engeller. **Karmaşık** tüm olası parçalanma kalıplarını değerlendirir (daha yavaş ama daha kapsamlı).

### Kaynaklar

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
