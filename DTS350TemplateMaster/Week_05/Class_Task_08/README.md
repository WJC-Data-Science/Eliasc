 Chapter 18 - Pipes
 - The use of assign with the pipe does not work because it assigns it to a temporary environment used by %>%. If you do want to use assign with the pipe, you must be explicit about the environment:
 - Dont use when operation is over 10 steps
 - You have multiple inputs or outputs. If there isn’t one primary object being transformed, but two or more objects being combined together, don’t use the pipe.
 - You are starting to think about a directed graph with a complex dependency structure. Pipes are fundamentally linear and expressing complex relationships with them will typically yield confusing code. -- Dont use the pipe
 
 Chapter 20 - Vectors
 - 2 Types of Vecters
      - Atomic vectors, of which there are six types: logical, integer, double, character, complex, and raw. Integer and double vectors are collectively known as numeric vectors.
      - Lists, which are sometimes called recursive vectors because lists can contain other lists.
- Note that each type of atomic vector has its own missing value:
      NA            # logical
#> [1] NA
NA_integer_   # integer
#> [1] NA
NA_real_      # double
#> [1] NA
NA_character_ # character
