# FX Data: Source, Attribution, and Transformation Notice

This directory contains daily bilateral exchange rates for the ASEAN-5
economies (IDR, MYR, PHP, SGD, THB) against the U.S. dollar, 2006-01 to
2020-05, compiled by the thesis author from IMF data.

## Source and attribution
Source: International Monetary Fund, Exchange Rate Data,
https://www.imf.org/external/data.htm#exchange

The series were downloaded from the IMF and assembled into a single
country-date panel by the author for non-commercial academic research.

## Redistribution basis
Under the IMF "Use of IMF Data" terms (Copyright and Usage, effective
2024-10-11; https://www.imf.org/en/about/copyright-and-terms), published IMF
statistical data — which explicitly includes Exchange Rate Data — may be
downloaded, copied, used to create derivative works, published, and
distributed, subject to accurate attribution to the IMF as source and explicit
disclosure of any material transformation of the data. This file provides both.

## Material transformation (required disclosure)
The Philippine peso (PHP) series was backfilled for 2006–2010 using data from
the Bangko Sentral ng Pilipinas (BSP), the Philippine central bank, because
IMF coverage for that period was incomplete. This is a material transformation
of the original IMF series and is disclosed here as required. The fill is
documented in the `PHP_fill_log` sheet of `fx_asean5_filled.xlsx`. All other
series are as obtained from the IMF.

## Note on reuse
This data is made available here for transparency and reproducibility of the
thesis. Anyone reusing it remains bound by the IMF terms linked above,
including the attribution requirement and the restriction that IMF data is
free of charge and not for commercial resale.
