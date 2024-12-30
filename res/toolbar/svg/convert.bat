@echo off
:: Batch script to convert SVG files to PNG with different sizes
for %%f in (*.svg) do (
    set filename=%%~nf
    inkscape --export-width=16 --export-height=16 "%%f" --export-filename="%%~nf_16x16.png"
    inkscape --export-width=24 --export-height=24 "%%f" --export-filename="%%~nf_24x24.png"
    inkscape --export-width=32 --export-height=32 "%%f" --export-filename="%%~nf_32x32.png"
)
echo Done! All PNG files generated.
