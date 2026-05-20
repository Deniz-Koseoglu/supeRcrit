## Deney Tasarımı (DOE) — Tasarım Oluşturma

Bu modül, süreç parametrelerinin sistematik incelenmesi için deneysel tasarımlar oluşturur. Oluşturulan tasarım matrisi, her bir deney için koşulları belirtir; bunlar laboratuvarda uygulanarak DOE Analiz modülünde analiz edilebilir.

### Mevcut Tasarım Türleri

**Box-Behnken Tasarımı (BBD)** — 3–4 faktör için yanıt yüzey tasarımı. Faktör başına üç seviye, köşe noktaları içermez; aşırı faktör kombinasyonlarından kaçınılması gerektiğinde etkilidir.

**Merkezi Bileşik Tasarım (CCD)** — 2–4 faktör için yanıt yüzey tasarımı. Çevrelenmiş (CCC) — yıldız noktaları faktör aralığının ötesine uzanır (±1.414, 5 seviye). Yüz Merkezli (CCF) — yıldız noktaları faktör yüzlerinde yer alır (±1.0, 3 seviye).

**Tam Faktöriyel Tasarım (FFD)** — faktör seviyelerinin tüm olası kombinasyonlarını araştırır. 2 seviyede 2–5 faktör veya 3 seviyede 2–3 faktör. Üç seviyeli tasarımlar varsayılan olarak 3 merkez noktası içerir.

**Kesirli Faktöriyel Tasarım (FrFD)** — ana etkileri tahmin edebilme yeteneğini korurken daha az deney gerektiren tam faktöriyelin alt kümesi. Ayarlanabilir fraksiyonlama ve karıştırma ile 3–5 faktör.

**Taguchi Yöntemi (TM)** — 2–4 seviyede 3–5 faktör ile gürbüz parametre tasarımı için ortogonal diziler. Ortalama yanıtı optimize etmek yerine varyasyonu minimize etmeye odaklanır.

### Merkez Noktaları

Eğriliği (doğrusal olmayan etkileri) tespit etmeye ve saf deneysel hatayı tahmin etmeye yardımcı olmak için herhangi bir tasarıma ek merkez noktaları eklenebilir. Bunlar, tüm faktör aralıklarının orta noktasında gerçekleştirilen deneylerdir.

### Kaynaklar

**NIST/SEMATECH** (2012), *e-Handbook of Statistical Methods*. [DOI](https://doi.org/10.18434/M32189)

**Weese, M.L., Ramsey, P.J., Montgomery, D.C.** (2025), 'Response Surface Methodology: Past, Present, and Future Perspectives', *Applied System Innovation* **8** (4), article 99. [DOI](https://doi.org/10.3390/asi8040099)
