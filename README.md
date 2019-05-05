# FloodAttitude_DDES
Social-media attitude after the floods using Sentimental Analysis

## Description
The script `scripts/TweetAnalysis.R` performs the sentimental Analysis using `TidyText` library in R. It takes csv file as input created from the bash file `utils/twint_scrapper.sh`. twint_scrapper.sh scraps tweets from twitter in a geographically controlled area and time period.

```bash

.
├── LICENSE
├── README.md
├── dat
│   ├── FullMontreal_data.csv
│   └── MontrealFlood_Pilot.csv
├── figs
│   ├── AfinnSpectrum.svg
│   ├── AfinnSpectrum.tiff
│   ├── Bing_diff.svg
│   ├── Bing_diff.tiff
│   ├── Bing_sentiments.svg
│   ├── Bing_sentiments.tiff
│   ├── NRC.svg
│   └── NRC.tiff
├── scripts
│   ├── TweetAnalysis.R
│   └── twint_scrapper.sh
└── utils
    └── twint_environment.yml

4 directories, 15 files

```

### Group Members 
- Muhammad Zohaib Anwar
- Tim Alamenciak
- Sandrine Soeharjono
- Paola Galloso
- Anna Quenneville


