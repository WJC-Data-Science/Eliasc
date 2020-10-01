-  The EDA cycle is:
   - Generate questions about your data.

   - Search for answers by visualising, transforming, and modelling your data.

   - Use what you learn to refine your questions and/or generate new questions.
   
- Variation is the tendency of the values of a variable to change from measurement to measurement
- A variable is categorical if it can only take one of a small set of values
- A variable is continuous if it can take any of an infinite set of ordered values.

- If you wish to overlay multiple histograms in the same plot, I recommend using geom_freqpoly() instead of geom_histogram(). geom_freqpoly() performs the same calculation as geom_histogram(), but instead of displaying the counts with bars, uses lines instead. It’s much easier to understand overlapping lines than bars.

- To make it easy to see the unusual values, we need to zoom to small values of the y-axis with coord_cartesian()

- To visualise the covariation between categorical variables, you’ll need to count the number of observations for each combination. One way to do that is to rely on the built-in geom_count()

- Use geom_tile() for the tile aesthletic
