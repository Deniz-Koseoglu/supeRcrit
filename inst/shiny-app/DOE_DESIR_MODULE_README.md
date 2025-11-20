# DOE Desirability Function Module

## Genel Bakış

Bu modül, birden fazla DOE analiz sonucunu Desirability Function kullanarak birleştirmeyi sağlar. Kullanıcı 2-6 farklı response için optimizasyon yapabilir.

## Dosya Yapısı

```
inst/shiny-app/
├── ui_modules/
│   └── doe_desir_ui.R              # UI modülü
├── server_modules/
│   └── doe_desir_server.R          # Server modülü
└── config/
    ├── default-settings.json       # Varsayılan ayarlar
    └── user-settings/
        └── doe_desir/              # Kullanıcı ayarları klasörü
```

## Özellikler

### 1. Analysis Selection
- Kaydedilmiş DOE analysis setup'larından seçim
- Minimum 2, maksimum 6 analiz seçilebilir
- Her seçilen analiz için özet bilgi gösterimi

### 2. Desirability Settings
Her response için:
- **Objective**: Maximize, Minimize veya Target
- **Lower/Upper Limits**: Response sınırları
- **Target Value**: (sadece Target objective için)
- **Weights**: Desirability ağırlıkları (0.1-10 arası)

### 3. Factor Range Settings
- Varsayılan range kullanımı (coded: -1 to 1)
- Özel range tanımlama seçeneği

### 4. Global Parameters
- **dtype**: Coded veya Uncoded faktörler
- **modbase**: Initial veya Final model kullanımı
- **optmet**: Optimizasyon metodu (NLopt veya Optim)
- **kmed**: Clustering (None, Auto, 2-5)
- **spts**: Starting points (random + data)

## Kullanım Akışı

### Adım 1: Analysis Seçimi
1. Kaydedilmiş DOE analysis'lerden 2-6 tanesini seçin
2. "Load Selected Analyses" butonuna tıklayın
3. Seçilen analizlerin önizlemesini görün

### Adım 2: Desirability Ayarları
1. Her response için objective belirleyin (max/min/target)
2. Lower ve upper limitleri girin
3. Target seçtiyseniz, target değerini girin
4. Weight değerlerini ayarlayın (varsayılan: 1)

### Adım 3: Factor Ranges (Opsiyonel)
- Varsayılan range'leri kullanabilir veya
- Özel range'ler tanımlayabilirsiniz

### Adım 4: Hesaplama
1. "Calculate" butonuna tıklayın
2. Progress bar ile ilerlemeyi takip edin
3. Sonuçları inceleyin

## İş Mantığı

### Calculate İşlemi Adımları:

1. **DOE Analyses Çalıştırma**
   - Her seçili setup için doe_analyze() çağrılır
   - Setup parametreleri kullanılır
   - Sonuçlar mods listesine eklenir

2. **Parametre Hazırlama**
   - dsrng: Response ranges ve targets
   - obj: Objectives vektörü
   - wts: Weights listesi
   - frng: Factor ranges

3. **Desirability Optimizasyonu**
   - doe_desir() fonksiyonu çağrılır
   - Tüm parametreler geçilir
   - Sonuçlar kaydedilir

## Sonuçlar

### Summary Tab
- **Factor Limits**: Faktör sınırları tablosu
- **Response Limits**: Response sınırları ve desirability ayarları
- **Model Summaries**: Model performans metrikleri

### Optimization Results Tab
- **Unique Solutions**: İstatistiksel olarak benzersiz çözümler
- **All Optimization Outputs**: Tüm optimizasyon çıktıları
- **Original Data**: Orijinal veri + desirability değerleri

### Visualizations Tab
- Desirability plotları (gelecekte eklenecek)

### Export Tab
- ZIP olarak tüm sonuçları indirme
- Bireysel tabloları CSV olarak indirme

## Kayıt Sistemi

### Settings Kaydetme
Kullanıcı ayarları şunları içerir:
- Seçilen analysis'ler
- Desirability parametreleri (dsrng, obj, wts)
- Global parametreler
- Metadata (tarih, versiyon)

Dosya formatı:
```json
{
  "selected_analyses": ["analysis1", "analysis2"],
  "parameters": {
    "dsrng": {...},
    "obj": [...],
    "wts": [...],
    "dtype": "coded",
    ...
  },
  "metadata": {
    "created_date": "2025-10-23 15:00:00",
    "app_version": "0.9.0"
  }
}
```

## Performans Notları

- Her Calculate işleminde analizler yeniden çalıştırılır
- 2-3 analiz için ~5-10 saniye
- 5-6 analiz için ~15-20 saniye
- Progress bar ile kullanıcı bilgilendirilir

## Hata Yönetimi

- Analysis seçim sayısı kontrolü (2-6)
- Setup yükleme hataları yakalanır
- doe_analyze hataları gösterilir
- doe_desir hataları kullanıcıya bildirilir

## Gelecek Geliştirmeler

1. **Visualizations Tab**
   - Desirability contour plots
   - Response surface plots
   - Trade-off analysis plots

2. **Advanced Features**
   - Saved settings'ten yükleme
   - Batch processing
   - Sensitivity analysis

3. **UI Improvements**
   - Response range'leri otomatik önerme
   - Factor correlation gösterimi
   - Interactive plots

## Bağımlılıklar

### R Fonksiyonları
- `doe_analyze()`: DOE analizi
- `doe_desir()`: Desirability optimizasyonu
- `desir_export()`: Sonuç export

### Shiny Modülleri
- `list_saved_settings()`: Kayıtlı ayarları listeleme
- `load_settings()`: Ayar yükleme
- `save_settings()`: Ayar kaydetme
- `generate_filename_with_timestamp()`: Dosya adı oluşturma

### UI Kütüphaneleri
- shinydashboard
- DT
- shinyWidgets

## Örnek Kullanım Senaryosu

### Senaryo: Rosemary Ekstraksiyon Optimizasyonu

**Amaç**: 3 farklı response'u optimize etmek
- CarnosicAcid: Maksimize
- Carnosol: Maksimize  
- ExtYield: Maksimize

**Adımlar**:
1. Her response için ayrı DOE analysis yapılmış ve kaydedilmiş
2. Desirability modülünde 3 analiz seçilir
3. Her response için:
   - Objective: max
   - Lower: 0
   - Upper: Deneysel maksimum değer
   - Weight: 1 (eşit önem)
4. Calculate ile optimal koşullar bulunur
5. Unique Solutions tablosundan en iyi çözüm seçilir

## Teknik Detaylar

### Reactive Values
```r
desir_data <- reactiveValues(
  available_setups = NULL,      # Mevcut setup'lar
  selected_setups = NULL,        # Seçili setup isimleri
  loaded_setups_data = list(),   # Yüklenmiş setup verileri
  analysis_results = list(),     # doe_analyze sonuçları
  desir_result = NULL,           # doe_desir sonucu
  response_info = list()         # Response metadata
)
```

### Dynamic UI Rendering
- Response sayısına göre input'lar oluşturulur
- Factor sayısına göre range input'ları oluşturulur
- Conditional panels ile target/weight input'ları gösterilir

## Sorun Giderme

### "No saved DOE analyses found"
- DOE Analysis modülünde analiz yapın ve kaydedin

### "Please select at least 2 analyses"
- Minimum 2 analiz seçmelisiniz

### "Error in desirability calculation"
- Response limit değerlerini kontrol edin
- Tüm gerekli parametrelerin doldurulduğundan emin olun
- Setup'ların doğru yüklendiğini kontrol edin

## Versiyon Geçmişi

- **v0.9.0** (2025-10-23): İlk implementasyon
  - Temel desirability fonksiyonalitesi
  - Multi-response optimizasyonu
  - Settings kaydetme/yükleme
  - Export özellikleri
