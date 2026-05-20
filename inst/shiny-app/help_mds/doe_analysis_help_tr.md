## DOE Analizi

Bu modül, deneysel tasarım verilerinden polinom regresyon modelleri oluşturur, sadeleştirir ve analiz eder. Birinci derece (tarama), etkileşim ve ikinci derece (yanıt yüzeyi) modellerini, otomatik veya manuel model sadeleştirme ve kanonik analiz ile destekler.

### İş Akışı

**1. Veri Yükleme:** Kaydedilmiş DOE tasarımından içe aktarma veya kodlanmış faktörler (A, B, C...), kodlanmamış faktörler, yanıt sütunu ve çalışma sırası sütunu içeren CSV dosyası yükleme.

**2. Model Oluşturma:** Seçilen derecede (doğrusal, doğrusal + etkileşimler veya kuadratik) polinom model uydurma.

**3. Model Sadeleştirme:** Anlamsız terimleri adım adım geriye doğru eleme (AIC tabanlı), p-değeri eşiği, her iki yöntem veya tam modeli koruma ile kaldırma.

**4. Model Teşhis:** ANOVA tabloları, R², uyum eksikliği testleri, artık grafikleri.

**5. Optimizasyon:** Durağan noktayı belirlemek ve yüzey şeklini karakterize etmek için kanonik analiz. Yanıtı en aza indiren veya en üst düzeye çıkaran faktör ayarlarını bulmak için sayısal optimizasyon.

### Kaynaklar

**Kiratu, J., Raynie, D.E.** (2015), *LCGC North America* **33** (7), pp. 104-111.

**NIST/SEMATECH** (2013), *Engineering Statistics Handbook*. [Link](https://www.itl.nist.gov/div898/handbook/index.htm)

**Sharif, K.M., et al.** (2014), *Journal of Food Engineering* **124**, pp. 105-116. [DOI](https://doi.org/10.1016/j.jfoodeng.2013.10.003)

**Yolmeh, M., Jafari, S.M.** (2017), *Food and Bioprocess Technology* **10**, pp. 413-433. [DOI](https://www.doi.org/10.1007/s11947-016-1855-2)
