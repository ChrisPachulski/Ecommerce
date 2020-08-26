# Ecommerce
MTG Collectibles Data Pipeline
All scripts written in R (Base and dplyr styles available)

Major trading card game websites like [tcg player](http://tcgplayer.com) and [card kingdom](http://cardkingdom.com) publish pricing and buylist information of _Magic, the Gathering_ cards in [secondary markets](https://en.wikipedia.org/wiki/Secondary_market). These scripts scrape and analyze their publications to forecast retail, buylist, and inventory movements of _Magic, the Gathering_ trading cards. Automating the collection of price discrepancies between different market's valuation of specific _Magic, the Gathering_ trading cards creates opportunities for consistenet monetary profit.

# Script Order

### Data_Pipeline.R
   |Purpose    |                 Acquire Market Kingpins Storefront information and provide basic market comparison      |

### DB_Buylist_Forecast.R
   |Purpose    |                 Forecast CardKingdom's standing buylist offer creating monetary safety net              |

### DB_Vendor_Forecast.R
   |Purpose    |                 Forecast Market Demand well in advance of all other potential competitors               |

### MTGBAN_Data_Transfer.R
   |Purpose    |                 Consolidate and transfer findings to transitory DB at MTGBAN for end users              |
