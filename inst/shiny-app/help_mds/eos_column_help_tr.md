## Fraksiyonlama Kolonu

GC(A)-EOS ile **karşı akışlı süperkritik-CO₂ fraksiyonlama kolonu** simülasyonu — deterpenasyon, deasidifikasyon, skualen geri kazanımı ve benzeri yüksek basınçlı ayırmalar için proses tasarım aracı. Model, Naphtali–Sandholm denge-kademe çözücüsüdür (SFE'nin çok yüksek çözücü oranlarında sağlam olan, kesin eşzamanlı düzeltme yöntemi).

**Besleme çözünenleri**

Oturum listesinden **1–3** çözünen işaretleyin (fraksiyonlanacak yağ); CO₂ örtük çözücüdür. **Örnekleri Yükle** / **Diskten Yükle** / **Kaldır** oturum bileşenlerini yönetir. Tüm çözünenler aynı EOS modelini kullanmalıdır.

**Besleme bileşimi (yağ)**

Yağ çözünenlerinin mol (veya onay kutusuyla **kütle**) kesirleri, CO₂'siz bazda; toplamları **1** olmalıdır. Aksi hâlde rozet kırmızıya döner ve çalıştırmayı engeller.

**Kolon parametreleri**

- **Kademeler** — denge kademelerinin sayısı (kademe 1 = üst).
- **Besleme kademesi** — sıvı yağ beslemesinin girdiği yer; altındaki kademeler zenginleştirme bölümünü oluşturur.
- **Geri akış oranı** — dış geri akış R/E (0 = geri akış yok). Yüksek değer ayrımı keskinleştirir.
- **S/F (mol)** — çözücü/besleme mol oranı (CO₂ akışı ÷ yağ besleme akışı); ana ekstraksiyon ayarı.
- **Sıcaklık** (°C) ve **basınç** (bar). **Sıcaklık gradyanı**nı işaretleyerek üstten alta doğrusal bir profil uygulayın (iç geri akış oluşturur).

**Sonuçlar**

- **Ürünler** — yakınsama durumu, çözücüsüz **ekstrakt** (üst) ve **rafinat** (alt) bileşimleri, her ürünün saflığı ve akışı ve çözünen başına **geri kazanım** (ekstrakta giden kesir).
- **Kademe Profilleri** — her çözünenin kolon boyunca çözücüsüz sıvı mol kesri (etkileşimli; dışa aktarılabilir).
- **Boyutlandırma** — besleme hızından (kg/sa), paketli SFE kolonu taşma modeliyle (Diaz et al. 2005) **kolon çapı** ve **paket yüksekliği**; faz yoğunlukları GC-EOS veya Budich–Brunner'dan.

**Notlar**

- Karşı akış kuralı: yağ aşağı, CO₂ yukarı akar; **ekstrakt** üstten, **rafinat** alttan ayrılır.
- Genel (verilen) parametrelerle belirli bir ayırma, belirli bir parametre setine bağlı literatür değerinden daha yüksek bir S/F gerektirir — daha keskin bir ayrım için S/F ve/veya geri akışı artırın.
