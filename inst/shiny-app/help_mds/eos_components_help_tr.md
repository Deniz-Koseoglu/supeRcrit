## Bileşenler ve Parametreler

Moleküler yapıdan GC-EOS / GCA-EOS faz dengesi araçları için bir bileşen oluşturun. Her başarılı analiz otomatik olarak oturuma eklenir ve flaş, fraksiyonlama kolonu ve faz diyagramı araçları için hazır olur.

**Molekülün tanımlanması**

- **EOS Modeli** — hidrokarbonlar ve terpenler için **GC-EOS** (deterpenasyon, skualen) ya da yağ asitleri, su ve alkoller için hidrojen bağı *ilişkilendirmesi* ekleyen **GCA-EOS**.
- **Örnek Yükle** — yerleşik bir molekül seçmek için bir iletişim kutusu açar; verileri alanları doldurur ve alanlar düzenlenebilir kalır.
- **CAS Numarası** — birincil tanımlayıcı; yapı ve SMILES otomatik olarak alınır. *T*c veya *T*b GKM ile tahmin edildiğinde bir CAS numarası gereklidir.
- **Görünen Ad** ve **SMILES** — isteğe bağlıdır, kendi anahtarlarıyla etkinleştirilir. *T*c ve *T*b'nin her ikisi de elle girildiğinde çevrimdışı çalışmak için doğrudan bir SMILES girin.

**Kritik sıcaklık ve kaynama noktası**

*T*c ve *T*b değerlerinin her biri **elle girilebilir** veya bir **Grup Katkı Yöntemi** ile tahmin edilebilir (Joback-Reid, Nannoolal, Stein-Brown, Hukkerikar, …). Sert küre kritik çapı **dc**, durum denkleminin normal kaynama noktasını (*T*b'de *P*sat = 1 atm) vermesi için regresyonla bulunur. Bir GKM seçildiğinde, **Grup Mertebesi** denetimi maksimum grup mertebesini belirler ve bir **Görselleştirmeyi Göster** düğmesi kullanılan parçaları gösterir.

**Buhar basıncı**

Bir veya daha fazla sıcaklığı (en fazla 10) etiket olarak girin; saf bileşenin buhar basıncı her birinde raporlanır ve ikiden fazla değer verildiğinde bir grafik çizilir.

**Sonuçlar ve görselleştirme**

Sonuç paneli, EOS gruplarına ayrılmış molekülü (solda), grup ayrıştırmasını, hesaplanan **dc** değerini, *T*c, *T*b, buhar basınçlarını ve — GCA-EOS için — **ilişkilendirici grupları** gösterir.

**Kaydetme**

Bileşen otomatik olarak oturuma eklenir. **Diske Kaydet**, bir dosya adı ve klasör seçmek için bir iletişim kutusu açar ve bileşeni JSON olarak yazar.

**Notlar**

- Aralık rozetleri, bir değer geçerli aralığının dışında olduğunda **kırmızıya** döner ve çalıştırmayı engeller.
- GKM tahmini ve CAS araması bir internet bağlantısı gerektirir (PubChem); elle girilen *T*c / *T*b ile birlikte bir SMILES çevrimdışı çalışır.
