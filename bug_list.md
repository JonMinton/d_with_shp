#List of bugs/quirks identified
1. Error generated when numerator includes many zeros (e.g. Chinese population counts) - seems to be in the CARBayes function itself; 
likely to do with the choice of prior; *option to use an alternative set of priors worth considering*
2. App can crash even on desktop if run twice in a row, due to memory limits being reached. 
3. On some machines the older CARBayes function names work but not the newer one, even though the newer ones (beginning with an 
`S.` prefix) are listed in the most recent version of the PDF for the package.
4. R may not recognise that shiny app has been installed and so app will not deplay. *think this is due to not giving packrat 
enough time to load configuration onto new machine, then not restarting R session*
5. Warning message when distribution generated: `Warning in dta$denominator * abs(p.current - p.current.overall) :
  longer object length is not a multiple of shorter object length` *investigate further*
6. Final output should report probability that value is within threshold. Currently returns `NULL` *investigate further*
7. **action**: *Make threshold slider non-reactive, so later processes are only updated once a button has been pressed* 
