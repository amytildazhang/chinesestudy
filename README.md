
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

## Example: Using default Chinese notetype

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
  merge_dupes() |>
  split_examples()

save_anki(pleco_deck, "tianguancifu")
```

4. Upload to Anki using the "import file" button. This needs to be done on desktop; mobile versions don't seem to allow import.

## Example: Using custom notetype with traditional and simplified

In this example, I made a new notetype with the following fields:

- Traditional
- Reading
- Definition
- Simplified

I named it "Chinese (Trad and Simp)"

```r
# same as previous example
library(chinesestudy)
pleco_deck <- read_pleco("flash.txt") |> 
  unique() |>
  separate_lists() |>
  reformat_abc() |>
  merge_dupes() |>
  split_examples() 

# difference

pleco_deck <- anno_both(pleco_deck) |>
  tag_hsk()

colnames(pleco_deck) # check column order before defining headers


save_anki(pleco_deck, "tianguancifu",
          columns="Traditional;Reading;Definition;Simplified;tag",
          notetype="Chinese (Trad and Simp)")

pleco_deck |>
  dplyr::filter(nchar(traditional) == 1) |>
  dplyr::select(traditional, tag) |>
  save_anki("writing", columns="Hanzi;tag")
```


```r
simp_trad <- pleco_deck |>
  dplyr::filter(simplified != "") |>
  merge_dupes(word_col = "simplified", reading_col = "pinyin", defn_col = "traditional") |>
  dplyr::select(simplified, traditional, pinyin)
save_anki(simp_trad, "simp_vs_trad", columns="Simplified;Traditional;Reading",
notetype = "Simplified")
```

