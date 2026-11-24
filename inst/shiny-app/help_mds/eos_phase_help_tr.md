## Faz Davranışı

GC(A)-EOS ile **CO2 + çözünen** faz davranışını hesaplayın. CO2 her zaman hafif bileşendir; **çözünenler**, **Bileşenler ve Parametreler** aracında oluşturduğunuz bileşenlerdir.

**Çözünenler**

Oturum listesinden **1 veya 2** çözünen işaretleyin. **Örnekleri Yükle**, yerleşik örnek bileşenleri ekler; **Seçilenleri Oturumdan Kaldır**, işaretli olanları siler. Seçilen tüm çözünenler aynı EOS modelini kullanmalıdır (listenin altında gösterilir) — bir GC-EOS bileşenini bir GCA-EOS bileşeniyle birleştirmek engellenir.

**Koşullar**

- **Sıcaklık** (°C) ve **basınç** (bar) — flaş ve P-x,y diyagramı için. Aralık rozetleri, bir değer geçerli aralığın dışında olduğunda **kırmızıya** döner ve çalıştırmayı engeller.
- **Besleme bileşimi** — CO2 ve çözünen(ler)in toplam **mol** (veya onay kutusuyla **kütle**) kesirleri, flaş için kullanılır; otomatik olarak normalize edilir.
- **Kritik eğri sıcaklık aralığı** — kritik eğri sekmesi için sıcaklık taraması.

**Sonuçlar (sekmeler)**

Üç hesaplama koşulları paylaşır ve sekmeler hâlinde gösterilir; her biri ilk görüntülemede hesaplanır:

- **Flaş** — CO2 + çözünen(ler) beslemesini denge fazlarına (1, 2 veya 3) ayırır ve dağılım katsayılarını *K = y/x* raporlar. İki çözünenle bağıl uçuculuk *K*₂/*K*₃ gösterilir. Bir kararlılık testi, var olduğunda üç fazlı buhar–sıvı–sıvı ayrımına geçer.
- **P–x,y Diyagramı** *(yalnızca ikili)* — sabit sıcaklıkta, basınca karşı kabarcık (sıvı *x*) ve çiy (buhar *y*) eğrileri, karışımın **kritik noktası** işaretlenmiş olarak.
- **Kritik Eğri** *(yalnızca ikili)* — karışımın kritik basıncının sıcaklığa bağlı değişimi.

P–x,y ve kritik eğri sekmeleri tek bir çözünen gerektirir (ikili CO2 + çözünen sistemi); iki çözünenle bir not gösterirler.

**Notlar**

- Sıcaklıklar °C, basınçlar bar cinsindendir; diyagramlar etkileşimlidir (tam nokta için fareyle üzerine gelin).
- "Yakınsamadı" sonucu genellikle koşulların iki fazlı bölgenin dışında olduğu anlamına gelir (örneğin kritik eğrinin üzerinde).
