# chart News

## chart version 1.3.1

-   Code in `chart.default()` choked `R CMD Check` (variable `y` not defined in scope) is commented out. Expect a slightly different behavior with automatic `geom_point()` with only one variable.

## chart version 1.3.0

-   Two bugs in chart.default() that appeared with ggplot 3.0 are solved.

-   ggplot2 3.0 viridis palettes are not needed any more and are thus eliminated.

## chart version 1.2.0

-   The functions to use viridis palettes in ggplot2 are cloned here from ggplot2
    v.  3.0.0, in order to use them also with older ggplot2 plots.

## chart version 1.1.0

-   First implementation of base and lattice versions of `chart()`s.

-   Addition of `combine_charts()`.

## chart version 1.0.1

-   A bug in `chart()` produced an error when labels are not defined and `auto.labs = TRUE`.

## chart version 1.0.0

First version of the package.
