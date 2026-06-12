# Pick the first matching name from a candidate list

Internal helper used to resolve dimension or column names by matching
candidate names against a set of available names in a case-insensitive
way.

## Usage

``` r
pick_name(targets, available_names)
```

## Arguments

- targets:

  Character vector of candidate names, typically already lower-cased.

- available_names:

  Character vector of names available in the current object.

## Value

A single matching name using the original case from `available_names`,
or `NULL` if no match is found.
