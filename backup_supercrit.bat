@echo off
SETLOCAL EnableDelayedExpansion

:: Get the directory where the script is located
SET "SourceDir=%~dp0"
IF "%SourceDir:~-1%"=="\" SET "SourceDir=%SourceDir:~0,-1%"

:: Get the user's Desktop path
FOR /F "tokens=2*" %%I IN ('REG QUERY "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" /v Desktop 2^>nul') DO SET "DesktopPath=%%J"
IF NOT DEFINED DesktopPath (
    echo Hata: Masaüstü yolu bulunamadı. Lütfen Registry ayarlarınızı kontrol edin.
    goto :eof
)

:: Generate timestamp for the zip file
FOR /F "tokens=1-4 delims=/ " %%i IN ('date /t') DO (SET "CurrentDate=%%i/%%j/%%k")
FOR /F "tokens=1-2 delims=:. " %%i IN ('time /t') DO (SET "CurrentTime=%%i:%%j")

SET "Timestamp=%date:~-4%%date:~-7,2%%date:~-10,2%_%time:~0,2%%time:~3,2%%time:~6,2%"
SET "Timestamp=%Timestamp: =0%"

SET "TempBaseDir=%TEMP%\supeRcrit_backup_temp_%RANDOM%"
SET "ZipFileName=supeRcrit_backup_%Timestamp%.zip"
SET "DestinationZipFile=%DesktopPath%\%ZipFileName%"
SET "TempProjectDir=%TempBaseDir%\supeRcrit"

echo.
echo +-----------------------------------------------+
echo ^|            supeRcrit Yedekleme Scripti        ^|
echo +-----------------------------------------------+
echo.
echo Başlangıç: supeRcrit klasörünü yedekleme işlemi...
echo.
echo Kaynak Dizin: "%SourceDir%"
echo.
echo Hedef Zip Dosyası: "%DestinationZipFile%"
echo.

:: Create temporary base directory
echo Geçici baz dizini oluşturuluyor: "%TempBaseDir%"
mkdir "%TempBaseDir%" >NUL 2>&1
IF NOT EXIST "%TempBaseDir%\" (
    echo Hata: Geçici baz dizini oluşturulamadı VEYA dizin zaten mevcut değil. Çıkılıyor.
    goto :eof
)

:: Copy supeRcrit contents excluding .git, .claude, .continue using robocopy
echo supeRcrit klasörü kopyalanıyor (hariç tutulanlar: .git, .claude, .continue)...
robocopy "%SourceDir%" "%TempProjectDir%" /E /XD ".git" ".claude" ".continue"  ".temp" /NFL /NDL /NJH /NJS /nc /ns /np /r:1 /w:1
IF %ERRORLEVEL% GEQ 8 (
    echo Hata: Dosyalar kopyalanırken bir hata oluştu. Yönetici ayrıcalıkları gerekebilir.
    rmdir /S /Q "%TempBaseDir%" >NUL 2>&1
    goto :eof
)
echo Kopyalama tamamlandı.

:: Compress the folder using PowerShell
echo Dosyalar ZIP'leniyor...
powershell -Command "Compress-Archive -Path '%TempProjectDir%' -DestinationPath '%DestinationZipFile%'"
IF %ERRORLEVEL% NEQ 0 (
    echo Hata: ZIP dosyası oluşturulurken bir hata oluştu. PowerShell'in yüklü ve erişilebilir olduğundan emin olun.
    rmdir /S /Q "%TempBaseDir%" >NUL 2>&1
    goto :eof
)

echo.
echo +-----------------------------------------------+
echo ^|          Yedekleme Başarıyla Tamamlandı!      ^|
echo +-----------------------------------------------+
echo.
echo Yedekleme dosyası burada oluşturuldu:
echo "%DestinationZipFile%"
echo.

:: Clean up temporary directory
echo Geçici dizin siliniyor: "%TempBaseDir%"
rmdir /S /Q "%TempBaseDir%" >NUL 2>&1

echo.
echo İşlem tamamlandı.
echo Scripti kapatmak için herhangi bir tuşa basın.
pause > NUL

ENDLOCAL
