# Change month encoding

Take month from text to number or form number to text. This work with
any month name (Spanish or English) to create a number. From number to
text only creates Spanish months

## Usage

``` r
crear_mes(mes, type = "text_to_number")
```

## Arguments

- mes:

  a number or character with the month

- type:

  a character indicating the type of conversion, can be any of these:
  `c("text_to_number", "number_to_text", "number_to_shorttext")`

## Examples

``` r
crear_mes("Enero", "text_to_number")
#> [1] 1
```
