## Üretim Maliyeti (COM) Analizi

Bu modül, süperkritik akışkan ekstraksiyonu (SFE), karşı akımlı süperkritik ekstraksiyon (CC-SFE) veya subkritik su ekstraksiyonu (SWE) süreçleri için **Üretim Maliyetini (COM)** **Turton et al. (1998)** metodolojisine göre hesaplar.

### Proses Türleri

**SFE** — Katı ham maddeden kesikli süperkritik CO₂ ekstraksiyonu. Materyal bir kaba yüklenir, CO₂ sabit yataktan geçer ve ayrık ekstraksiyon döngüleri sayılır.

**CC-SFE** — Sıvı beslemeden karşı akımlı süperkritik CO₂ ekstraksiyonu. Besleme ve CO₂ zıt yönlerde sürekli akar. Ayrık döngü yoktur; aylık üretim sürekli çalışma saatlerinden hesaplanır.

**SWE** — Katı ham maddeden subkritik su ekstraksiyonu. SFE'ye benzer ancak ana çözücü olarak basınçlı su kullanır.

### COM Formülü

Turton et al. katsayıları etkinleştirildiğinde:

$$COM = 0.304 \times FMC + 2.73 \times COL + 1.23 \times (CRM + CUT)$$

Aksi takdirde COM, kullanıcı tanımlı katsayılarla tüm maliyet bileşenlerinin ağırlıklı toplamıdır.

### Hammadde Maliyeti (CRM)

Kesikli modlarda aylık ekstraksiyon döngü sayısı:

$$N_{ex} = \frac{W_{sh} \times W_{hr}}{(T_{ex} + T_{aux})/60} \times W_{days}$$

CC-SFE için aylık besleme hacmi: *M*<sub>feed</sub> = *F*<sub>feed</sub> × *T*<sub>monthly</sub> / 1000 (kg)

### Enerji Maliyeti (CUT)

Ana güç artı kurutma, öğütme ve buharlaştırma için yardımcı güç.

### İşçilik Maliyeti (COL)

$$COL = W_{sh} \times W_{pers} \times W_{pay}$$

### Ekonomik Göstergeler

$$SC = COM / Y_{month} \qquad GPr = (Y_{month} \times SP_{kg}) - COM$$

$$NPr = GPr \times (1 - taxrate) \qquad PBK = CAPEX / (NPr \times 12)$$

### Kaynaklar

**Turton, R., Bailie, R.C., Whiting, W.B., Shaeiwitz, J.A.** (1998), *Analysis, Synthesis and Design of Chemical Process, PTR*, Prentice Hall, Upper Saddle River, NJ, USA.
