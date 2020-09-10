 Chapter 3
 - The shape of the data can be determined by a number in CH. 3 Figure 3.1
 - Google is a great tool to use when trying to figure out problems in R
 - Facets are subplots that display one subset of the data
 - When using geom_smooth(), it is good to color coat the data to be able to tell the difference
 
 Chapter 28
 - labs() can be used to create title, subtitles, and captions of graphs
 - hjust and vjust measure the alignment of the labels
 
 Chapter 6
 - Instead of running expression-by-expression, you can also execute the complete script in one step: Cmd/Ctrl + Shift + S.
 - I recommend that you always start your script with the packages that you need. That way, if you share your code with others, they can easily see what packages they need to install. 
 
 Chapter 11
 - read_csv() reads comma delimited files, read_csv2() reads semicolon separated files (common in countries where , is used as the decimal place), read_tsv() reads tab delimited files, and read_delim() reads in files with any delimiter.
 - f there are many parsing failures, you’ll need to use problems() to get the complete set. This returns a tibble, which you can then manipulate with dplyr.
 - Every parse_xyz() function has a corresponding col_xyz() function.
 
