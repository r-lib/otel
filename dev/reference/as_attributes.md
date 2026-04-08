# R objects as OpenTelemetry attributes

Convert a list of R objects to a form that is suitable as OpenTelemetry
attributes.

## Usage

``` r
as_attributes(x)
```

## Arguments

- x:

  A list of R objects, to be used as OpenTelemetry attributes.

## Value

A named list that can be used as the `attributes` argument to the
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md)
method of
[otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md), the
[`log()`](https://otel.r-lib.org/dev/reference/log.md) method of
[otel_logger](https://otel.r-lib.org/dev/reference/otel_logger.md), etc.

If `x` is not named, or some names are the empty string or `NA`, then
integer numbers as used for the missing or invalid names.

If some elements in `x` are not of the natively supported R types in
OpenTelemetry (character, logical, double, integer), then their printed
form is captured using
[`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html).

### Limits

The number of attributes can be limited with the
OTEL_ATTRIBUTE_COUNT_LIMIT environment variable. The default is 128.

The length of the each attribute (vector) can be limited with the
OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT environment variable. The default is
`Inf`. Note that this is applied to the length of each attribute as an R
vector. E.g. it does *not* currently limit the number of characters in
individual strings.

## Examples

``` r
as_attributes(list(
  number = 1.0,
  vector = 1:10,
  string = "otel",
  string_vector = letters,
  object = mtcars
))
#> $number
#> [1] 1
#> 
#> $vector
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $string
#> [1] "otel"
#> 
#> $string_vector
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#> 
#> $object
#>  [1] "                     mpg cyl  disp  hp drat    wt  qsec vs am gear"
#>  [2] "Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4"
#>  [3] "Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4"
#>  [4] "Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4"
#>  [5] "Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3"
#>  [6] "Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3"
#>  [7] "Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3"
#>  [8] "Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3"
#>  [9] "Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4"
#> [10] "Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4"
#> [11] "Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4"
#> [12] "Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4"
#> [13] "Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3"
#> [14] "Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3"
#> [15] "Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3"
#> [16] "Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3"
#> [17] "Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3"
#> [18] "Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3"
#> [19] "Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4"
#> [20] "Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4"
#> [21] "Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4"
#> [22] "Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3"
#> [23] "Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3"
#> [24] "AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3"
#> [25] "Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3"
#> [26] "Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3"
#> [27] "Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4"
#> [28] "Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5"
#> [29] "Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5"
#> [30] "Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5"
#> [31] "Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5"
#> [32] "Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5"
#> [33] "Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4"
#> [34] "                    carb"                                          
#> [35] "Mazda RX4              4"                                          
#> [36] "Mazda RX4 Wag          4"                                          
#> [37] "Datsun 710             1"                                          
#> [38] "Hornet 4 Drive         1"                                          
#> [39] "Hornet Sportabout      2"                                          
#> [40] "Valiant                1"                                          
#> [41] "Duster 360             4"                                          
#> [42] "Merc 240D              2"                                          
#> [43] "Merc 230               2"                                          
#> [44] "Merc 280               4"                                          
#> [45] "Merc 280C              4"                                          
#> [46] "Merc 450SE             3"                                          
#> [47] "Merc 450SL             3"                                          
#> [48] "Merc 450SLC            3"                                          
#> [49] "Cadillac Fleetwood     4"                                          
#> [50] "Lincoln Continental    4"                                          
#> [51] "Chrysler Imperial      4"                                          
#> [52] "Fiat 128               1"                                          
#> [53] "Honda Civic            2"                                          
#> [54] "Toyota Corolla         1"                                          
#> [55] "Toyota Corona          1"                                          
#> [56] "Dodge Challenger       2"                                          
#> [57] "AMC Javelin            2"                                          
#> [58] "Camaro Z28             4"                                          
#> [59] "Pontiac Firebird       2"                                          
#> [60] "Fiat X1-9              1"                                          
#> [61] "Porsche 914-2          2"                                          
#> [62] "Lotus Europa           2"                                          
#> [63] "Ford Pantera L         4"                                          
#> [64] "Ferrari Dino           6"                                          
#> [65] "Maserati Bora          8"                                          
#> [66] "Volvo 142E             2"                                          
#> 
```
