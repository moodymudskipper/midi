
<!-- README.md is generated from README.Rmd. Please edit that file -->

# midi

JUST A DRAFT\!\!\!

This is a rewrite of `TuneR::readMidi()`, with a counterpart to write
midi.

We don’t create a data.frame but a list.

It should probably be an R6 object, and the structure should evolve.

I’d like to be able to change scales/modes, apply arpeggios etc, maybe
do some analysis on big amounts of midi files to detect patterns in
styles of music or specific artists, but we’re far from there.

I’d like to do the same with guitar pro files too.

## Installation

You can install the released version of midi from
[CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("moodymudskipper/midi")
```

## Example

Load file (a single piano note), check out the parsed data, write it
back and check that it is the same.

``` r
library(midi)

file <- "data/test.mid"

parsed <- parse_midi(file)
str(parsed)
#> List of 2
#>  $ header:List of 3
#>   ..$ format                  : int 1
#>   ..$ n_tracks                : int 2
#>   ..$ n_ticks_per_quarter_note: int 384
#>  $ tracks:List of 2
#>   ..$ :List of 3
#>   .. ..$ :List of 6
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "meta_event"
#>   .. .. ..$ event       : chr "Time Signature"
#>   .. .. ..$ type        : chr "58"
#>   .. .. ..$ params      :List of 4
#>   .. .. .. ..$ time_signature                 : chr "4/4"
#>   .. .. .. ..$ clocks_per_tick                : int 24
#>   .. .. .. ..$ n_32nd_notes_per_24_midi_clocks: int 8
#>   .. .. .. ..$ length                         : num 4
#>   .. .. ..$ EventChannel: raw ff
#>   .. ..$ :List of 6
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "meta_event"
#>   .. .. ..$ event       : chr "Set Tempo"
#>   .. .. ..$ type        : chr "51"
#>   .. .. ..$ params      :List of 2
#>   .. .. .. ..$ value : num 545454
#>   .. .. .. ..$ length: num 3
#>   .. .. ..$ EventChannel: raw ff
#>   .. ..$ :List of 6
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "meta_event"
#>   .. .. ..$ event       : chr "End of Track"
#>   .. .. ..$ type        : chr "2f"
#>   .. .. ..$ params      :List of 2
#>   .. .. .. ..$ value : raw(0) 
#>   .. .. .. ..$ length: num 0
#>   .. .. ..$ EventChannel: raw ff
#>   .. ..- attr(*, "length")= int 19
#>   ..$ :List of 5
#>   .. ..$ :List of 6
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "meta_event"
#>   .. .. ..$ event       : chr "Sequence/Track Name"
#>   .. .. ..$ type        : chr "03"
#>   .. .. ..$ params      :List of 2
#>   .. .. .. ..$ value : chr "Electric Piano"
#>   .. .. .. ..$ length: num 14
#>   .. .. ..$ EventChannel: raw ff
#>   .. ..$ :List of 5
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "channel_voice"
#>   .. .. ..$ event       : chr "Program Change"
#>   .. .. ..$ params      :List of 2
#>   .. .. .. ..$ channel: num 0
#>   .. .. .. ..$ program: int 0
#>   .. .. ..$ EventChannel: raw c0
#>   .. ..$ :List of 5
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "channel_voice"
#>   .. .. ..$ event       : chr "Note On"
#>   .. .. ..$ params      :List of 3
#>   .. .. .. ..$ channel   : num 0
#>   .. .. .. ..$ key_number: int 48
#>   .. .. .. ..$ velocity  : int 50
#>   .. .. ..$ EventChannel: raw 90
#>   .. ..$ :List of 5
#>   .. .. ..$ deltatime   : num 96
#>   .. .. ..$ message_type: chr "channel_voice"
#>   .. .. ..$ event       : chr "Note Off"
#>   .. .. ..$ params      :List of 3
#>   .. .. .. ..$ channel   : num 0
#>   .. .. .. ..$ key_number: int 48
#>   .. .. .. ..$ velocity  : int 0
#>   .. .. ..$ EventChannel: raw 80
#>   .. ..$ :List of 6
#>   .. .. ..$ deltatime   : num 0
#>   .. .. ..$ message_type: chr "meta_event"
#>   .. .. ..$ event       : chr "End of Track"
#>   .. .. ..$ type        : chr "2f"
#>   .. .. ..$ params      :List of 2
#>   .. .. .. ..$ value : raw(0) 
#>   .. .. .. ..$ length: num 0
#>   .. .. ..$ EventChannel: raw ff
#>   .. ..- attr(*, "length")= int 33

file2 <- tempfile(fileext = ".mid")
encode_midi(parsed, file2)

hexView::viewRaw(file)
#>  0  :  4d 54 68 64 00 00 00 06 00 01 00 02 01 80 4d 54 72  |  MThd..........MTr
#> 17  :  6b 00 00 00 13 00 ff 58 04 04 02 18 08 00 ff 51 03  |  k......X.......Q.
#> 34  :  08 52 ae 00 ff 2f 00 4d 54 72 6b 00 00 00 21 00 ff  |  .R.../.MTrk...!..
#> 51  :  03 0e 45 6c 65 63 74 72 69 63 20 50 69 61 6e 6f 00  |  ..Electric Piano.
#> 68  :  c0 00 00 90 30 32 60 80 30 00 00 ff 2f 00           |  ....02`.0.../.
hexView::viewRaw(file2)
#>  0  :  4d 54 68 64 00 00 00 06 00 01 00 02 01 80 4d 54 72  |  MThd..........MTr
#> 17  :  6b 00 00 00 13 00 ff 58 04 04 02 18 08 00 ff 51 03  |  k......X.......Q.
#> 34  :  08 52 ae 00 ff 2f 00 4d 54 72 6b 00 00 00 21 00 ff  |  .R.../.MTrk...!..
#> 51  :  03 0e 45 6c 65 63 74 72 69 63 20 50 69 61 6e 6f 00  |  ..Electric Piano.
#> 68  :  c0 00 00 90 30 32 60 80 30 00 00 ff 2f 00           |  ....02`.0.../.
```
