
# chinesestudy

<!-- badges: start -->
<!-- badges: end -->

This is a small and (shall we say) "organic"" R package for converting flashcards exported from Pleco into a somewhat-nice format for Anki. The code is pretty specific to my settings and assumes

- Pleco export is saved in iCloud Drive (only relevant for `read_pleco()`), so iphone and mac are used

- flash cards use the ABC dictionary

- all decks are saved into a `decks` subfolder in this repo.

Organization is hodge-podge and documentation is sketchy but should be generally readable. 

## Installation

You can install the development version of chinesestudy from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("amytildazhang/chinesestudy")
```

If there are any errors it's because I usually just `devtools::load_all()` and run the functions. 

## Example

Steps:

1. Export flash cards from within Pleco to a .txt file.

2. Move the flash card file to a shared drive. This is done within Pleco, it's "Step 2" in the "Export" interface.

3. See below:

``` r
library(chinesestudy)
# if "file does not exist", may need to manually download it from iCloud within Finder
pleco_deck <- read_pleco("flash.txt") |> 
  separate_lists() |>
  reformat_abc() |>
  merge_dupes()

save_anki(pleco_deck, "tianguancifu")
```

4. Upload to Anki using the "import file" button. This needs to be done on desktop; mobile versions don't seem to allow import.
