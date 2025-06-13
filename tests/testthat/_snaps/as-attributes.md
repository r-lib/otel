# not a list

    Code
      as_attributes(1:10)
    Condition
      Error in `as_attributes()`:
      ! Invalid argument: `x` must be a list in `as_attributes()`.

# print unsupported types

    Code
      as_attributes(list(mtcars[1:2, 1:4]))
    Output
      $`1`
      [1] "              mpg cyl disp  hp" "Mazda RX4      21   6  160 110"
      [3] "Mazda RX4 Wag  21   6  160 110"
      

