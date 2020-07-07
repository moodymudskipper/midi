
<!-- README.md is generated from README.Rmd. Please edit that file -->

# midi

JUST A DRAFT\!\!\!

This is a rewrite of `TuneR::readMidi()`, with a counterpart to write
midi.

We don’t create a data.frame but a R6 object.

The structure is not stable yet.

I’d like to be able to change scales/modes, apply arpeggios etc, maybe
do some analysis on big amounts of midi files to detect patterns in
styles of music or specific artists, but we’re far from there.

I’d like to do the same with guitar pro files too.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/midi")
```

## Example

Load a simple music file with 2 instruments (that’s 3 tracks here)

``` r
library(midi)

mid <- parse_midi("https://www.8notes.com/school/midi/violin/vivaldi_spring.mid")
#> parsing header
#> parsing tracks

# it prints nicely
mid
#> header:
#> # A tibble: 1 x 3
#>   format n_tracks n_ticks_per_quarter_note
#>    <int>    <int>                    <int>
#> 1      1        3                      960
#> tracks:
#> $`Spring from the Four Seasons`
#> # A tibble: 7 x 4
#>    time event channel
#>   <dbl> <chr>   <int>
#> 1     0 Time~      NA
#> 2     0 Key ~      NA
#> 3     0 Sequ~      NA
#> 4     0 Text~      NA
#> 5  2880 Set ~      NA
#> 6 99840 Time~      NA
#> 7 99841 End ~      NA
#> # ... with 1 more variable: params <mid_prms>
#> 
#> $Violin
#> # A tibble: 204 x 4
#>     time event               channel                       params
#>    <dbl> <chr>                 <int>                   <mid_prms>
#>  1  2880 Program Change            1                           40
#>  2  2880 Control Change            1    0, controller_number: 121
#>  3  2880 Control Change            1    0, controller_number:  64
#>  4  2880 Control Change            1   42, controller_number:  91
#>  5  2880 Control Change            1   38, controller_number:  10
#>  6  2880 Control Change            1  115, controller_number:   7
#>  7  2880 Sequence/Track Name      NA                       Violin
#>  8  2880 Note On                   1            69, velocity:  84
#>  9  3743 Note Off                  1            69, velocity:   0
#> 10  3840 Note On                   1            73, velocity: 105
#> # ... with 194 more rows
#> 
#> $Piano
#> # A tibble: 597 x 4
#>     time event          channel                       params
#>    <dbl> <chr>            <int>                   <mid_prms>
#>  1  2880 Program Change       0                            0
#>  2  2880 Control Change       0    0, controller_number: 121
#>  3  2880 Control Change       0    0, controller_number:  64
#>  4  2880 Control Change       0   48, controller_number:  91
#>  5  2880 Control Change       0   51, controller_number:  10
#>  6  2880 Control Change       0  100, controller_number:   7
#>  7  2880 Note On              0            69, velocity:  92
#>  8  2880 Control Change       0    0, controller_number: 121
#>  9  2880 Control Change       0    0, controller_number:  64
#> 10  2880 Control Change       0   48, controller_number:  91
#> # ... with 587 more rows

# we can show separately header or tracks
mid$header
#> # A tibble: 1 x 3
#>   format n_tracks n_ticks_per_quarter_note
#>    <int>    <int>                    <int>
#> 1      1        3                      960

# methods will be available for extraction and transformations, for now only: 
mid$track_names()
#> [1] "Spring from the Four Seasons" "Violin"                      
#> [3] "Piano"

# we can save locally
local_file <- tempfile(fileext = ".mid")
encode_midi(mid, local_file)

# and reimport to check if our back and forth transformation was reliable
mid_reimported <- parse_midi(local_file)
#> parsing header
#> parsing tracks
all.equal(mid, mid_reimported) 
#> [1] TRUE

# listen to it : 
cat(local_file)
#> C:\Users\afabri\AppData\Local\Temp\RtmpusmViH\file214878f9610c.mid
```
