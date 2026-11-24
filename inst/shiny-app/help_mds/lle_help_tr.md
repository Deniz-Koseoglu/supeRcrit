## Sıvı-Sıvı Ekstraksiyon

Bu araç, **hedef bir çözüneni besleme (seyreltici) fazından çekip almak** için çözücüleri tasarlar ve sıralar — örneğin sulu bir akıştan bir aroma, ilaç ya da doğal ürün molekülünü ayırmak gibi. Her şey moleküler yapıdan, yerel **COSMO-SAC-dsp** aktiflik katsayısı modeliyle hesaplanır (deneysel dağılım verisi gerekmez) ve yerleşik σ-profil tahmincisi sayesinde **çizebildiğiniz herhangi bir moleküle** uygulanabilir.

**İki analiz modu** sunar: çözücüleri dağılım katsayısına göre sıralayan hızlı bir **tarama** ve gerçek sıvı-sıvı dengesini çözen kesin bir **iki fazlı flaş** — beslemeyi iki gerçek faza ayırır, her çözüneni bunlar arasında dağıtır, iki çözücünün karşılıklı çözünürlüğünü verir ve sıcaklık üzerinde optimize eder (AMS tarzı "Optimize LLE").

### Fizik

Bir çözünen, temas hâlindeki iki sıvı faz arasında **aktiflik katsayısının** daha düşük olduğu faza dağılır. Seyreltik limitte bu γ∞'dur. Her aday çözücü için araç şunları verir:

- **D — dağılım katsayısı**: `D = γ∞(besleme) / γ∞(çözücü)` — çözücünün çözüneni ne kadar güçlü çektiği. **Yüksek olması iyidir.**
- **Kapasite** = `1 / γ∞(çözücü)` — çözücü fazının ne kadar çözünen tutabileceği.
- **γ∞ (çözücü)** — sonsuz seyreltme aktiflik katsayısı.
- **Seçicilik** (opsiyonel) = `D(çözünen) / D(yan-ekstrakt)` — çözücünün hedefi safsızlığa göre ne kadar temiz tercih ettiği. `S > 1` hedefi tercih eder.

D bir aktiflik katsayısı *oranı* olduğundan, çözünenin erime noktasından bağımsızdır: katı-erime terimi iki faz arasında sadeleşir. Bu nedenle tarama yalnızca σ-profillerine ihtiyaç duyar, Te'ye değil.

### Motor

Aktiflik katsayıları **COSMO-SAC-dsp** (Hsieh, Lin & Vrabec 2014) ile hesaplanır — dağılım düzeltmeli ve 2010 hidrojen-bağı ayrımlı (bağsız / hidroksil / diğer) COSMO-SAC modeli. Bu yerel uygulama NIST referans uygulamasına (Bell et al. 2020) bit-bit doğrulanmıştır.

### İki analiz modu

- **Hızlı tarama** *(varsayılan)* — tüm çözücü kütüphanesini sonsuz seyreltme aktiflik katsayılarından (γ∞) tek geçişte sıralar. Hızlıdır, onlarca çözücüye ilk bakış için idealdir. Yukarıdaki "Fizik" bölümünde anlatılan mod budur.
- **İki fazlı flaş** — gerçek bir besleme (seyreltici + çözücü + çözünenler) için *gerçek* sıvı-sıvı dengesini çözer. γ∞ sıralamasının veremeyeceği yanıtları verir: gerçek bir iki fazlı ayrışma oluşuyor mu? Her çözünenin sonlu-derişimdeki gerçek dağılımı nedir? Her çözücünün ne kadarı diğer fazda çözünür? Ve ayırma hangi sıcaklıkta en iyidir?
- **Karışım optimizasyonu** — seçilen 2–4 çözücülük bir karışımın *bileşimini* (simpleks-ızgaradan çoklu-başlangıç + yerel iyileştirme, isteğe bağlı olarak sıcaklık boyunca sıcak-başlangıç) ayırma seçiciliğini veya hedefin dağılımını en üst düzeye çıkaran reçete için arar. Amaç tam flaştan geldiği için faz ayrışması yapacak karışımlar otomatik olarak reddedilir. İyi ayarlanmış bir karışım çoğunlukla herhangi bir tek çözücüyü geçer (AMS "Optimize LLE" MINLP karışım-tasarımının karşılığı).

### İki fazlı flaş (kesin SSE)

Flaş modunda araç, seyreltici + her aday çözücüden (seçilen mol oranında) ve çözünenlerinizden (seyreltik tutulan) bir besleme oluşturur ve taranan aralıktaki her çözücü ve her sıcaklık için:

1. karışımın iki sıvı faza ayrışıp ayrışmadığına karar vermek için bir **teğet-düzlem kararlılık testi** (Michelsen) çalıştırır — seyrelticiyle tamamen karışan çözücüler *tek faz* olarak işaretlenir (ekstraksiyon mümkün değil);
2. **her iki** denge fazının bileşimi için **eş-aktivite flaşını** (Rachford–Rice + ardışık yerine koyma, `K_i = γ_i(rafinat) / γ_i(ekstrakt)`) çözer;
3. her çözünenin **dağılımını** `D = x(ekstrakt) / x(rafinat)`, hedefin ilk ek çözünene göre ikili **seçiciliğini** `S = D₁ / D₂` ve iki çözücünün **karşılıklı çözünürlüğünü** (rafinatta-çözücü ve ekstraktta-seyreltici) raporlar.

Sonuçlar en iyiden başlayarak sıralanır — iki veya daha fazla çözünen verildiğinde seçiciliğe, aksi hâlde D'ye göre. **İki fazlı optimizasyon** sekmesi sıralanmış (çözücü × sıcaklık) tablosunu ve en iyi noktada her iki fazın tam mol-kesri bileşimini gösterir; **Dağılım–T** sekmesi D veya seçiciliğin sıcaklıkla nasıl değiştiğini çizer, böylece en uygun ekstraksiyon sıcaklığını okuyabilirsiniz. **İki veya daha fazla** çözüneni ayırmak için bunları *Ayrılacak ek çözünen(ler)* alanına ekleyin.

### Bileşik veritabanları

Her bileşiğin **σ-profili** temel girdidir. İki gömülü, kendi içinde tutarlı veritabanı mevcuttur:

- **Seçilmiş (UD)** — 2.259 yaygın çözücü ve çözünen, **bilinen adlarla** (DMol3 kuantum kimyası). Günlük çözücülerle rutin tarama için.
- **CHAOS** — 53.088 molekül (Gond et al. 2026, ωB97X-D/def2-TZVP), **SMILES ile** seçim. Tasarım, ağır ve özel bileşikler dahil çok daha geniş kapsam.

> İki veritabanı farklı kuantum-kimyasal yöntemler kullanır; aktiflik katsayıları biraz farklı ölçeklerdedir ve **veritabanları arasında karşılaştırılamaz** — sıralama başına bir tane seçin. Her veritabanı kendi içinde tamamen tutarlıdır.

### FastSigma — veritabanında olmasa bile herhangi bir molekül

**CHAOS** veritabanında, katalogda olmayan **yepyeni bir SMILES yazabilirsiniz**. Yerleşik **FastSigma** modeli σ-profilini doğrudan moleküler yapıdan tahmin eder (53 binlik tüm CHAOS profilleri üzerinde eğitilmiş bir makine öğrenmesi modeli; ayrılmış doğruluk ≈ 0,98–0,99 kosinüs benzerliği). Böylece hiçbir veritabanında bulunmayan tasarım veya türevlendirilmiş hedefler bile taranabilir. Dağılım enerjisi ve hidrojen-bağı sınıfı yapıdan deterministik olarak hesaplanır.

### "Çözünen σ-Profili" sekmesi

Seçilen çözünenin COSMO-SAC σ-profilini gösterir:

- **Yeni bir SMILES** için FastSigma tahmini çizilir.
- **CHAOS veritabanında zaten bulunan** bir bileşik için FastSigma tahmini ikinci bir renkte **gerçek profilin üzerine bindirilir** — tahminin ne kadar doğru olduğunun görsel kontrolü.

Hidrojen-bağı verici/alıcı eşik çizgileri referans için gösterilir.

### Nasıl kullanılır

1. **Bileşik veritabanı** — Seçilmiş (adla) veya CHAOS (SMILES ile).
2. **Hedef çözünen** — listeden seçin veya (CHAOS) herhangi bir SMILES yazın.
3. **Besleme / seyreltici faz** — çözünenin bulunduğu faz (varsayılan **su**).
4. **Sıcaklık** — ekstraksiyon sıcaklığı (°C).
5. **Birlikte ekstrakte edilen çözünen** *(opsiyonel)* — geride bırakılacak safsızlık; seçicilik sütununu etkinleştirir.
6. **Çözücü taraması** — ~75 yaygın ekstraksiyon çözücüsünden oluşan yerleşik bir kütüphane **otomatik** olarak taranır (manuel seçim gerekmez). İsteğe bağlı olarak çözücü sınıflarıyla sınırlayın veya — yalnızca Seçilmiş veritabanı — tüm 2.259 bileşiği tarayın.
7. **Taramayı Başlat.** Sonuçlar **D**'ye göre sıralanır: değer kutuları, tablo, çubuk grafik ve çözünenin σ-profili. **Sonuçları Dışa Aktar** ile tam CSV.

### Notlar ve sınırlamalar

- COSMO-SAC *öngörücü* bir modeldir: eğilimleri iyi yakalar, ancak güçlü çağrışımlı / sulu sistemlerde mutlak γ∞ olduğundan yüksek tahmin edilebilir. Birincil çıktı **sıralamadır**.
- **Hızlı tarama** yalnızca seyreltik-limit (γ∞) davranışını modeller; gerçek bir iki fazlı ayrışma ve sonlu-derişim dağılımı için **iki fazlı flaş** modunu kullanın. Flaş, çözünenleri beslemede seyreltik kabul eder (*çözünen mol kesrini* düşürerek sonsuz seyreltme limitine yaklaşın).
- FastSigma profilleri CHAOS ölçeğindedir; bu nedenle σ-profil karşılaştırması/bindirmesi **CHAOS** modunda anlamlıdır; Seçilmiş (UD) modunda sekme yalnızca veritabanı profilini gösterir.

### Kaynaklar

- **Gond, D. vd. (2026)**, *CHAOS — A Large-scale Database for σ-Profiles…*, arXiv:2511.19002; Zenodo 10.5281/zenodo.17691924.
- **Bell, I. H. vd. (2020)**, J. Chem. Theory Comput. 16, 2635–2646.
- **Hsieh, C.-M.; Lin, S.-T.; Vrabec, J. (2014)**, Fluid Phase Equilibria 367, 109–116.
- **Hsieh, C.-M.; Sandler, S. I.; Lin, S.-T. (2010)**, Fluid Phase Equilibria 297, 90–97.
