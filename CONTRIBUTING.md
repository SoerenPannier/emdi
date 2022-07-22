# Guidelines for contributing to the R package emdi

The R package emdi is an open source project. We are not directly funded to maintain it. Therefore, we are grateful for your consideration to contribute to this project.

## Ways to contribute

Why repeat what others already explained much better, so if you are looking for general information on how and why to contribute to open source projects see: [Open Source Guide](https://opensource.guide/how-to-contribute/)

### Questions, Bugs and Enhancements

If you are stuck with a question that you can not solve yourself by browsing through the documentation feel free to use the github issues to let us know. Please remember to use labels to let us know what kind of an issue it is.

#### Reporting a bug

Did you notice a bug while using emdi? Please let us know so we can fix it and improve yours and others experience with emdi. To make sure we fully understand the issue and can reproduce the bug: Please include the following details in your report.

-   Your operating system
-   The R version you are using
-   The emdi version you are using
-   The steps necessary to reproduce the bug (if possible, in form of a minium working example)

#### Suggesting an enhancement

Please, first study the documentation and the accompanying vignettes to see if
the suggested feature is not already included. If you are sure your feature is not 
already there please provide us with a
-   detailed description of the desired feature. 
-   focus on what you want to achieve and less how you would implement it.
-   keep the scope narrow. (Chances for small features to be implemented are much 
higher ;-))


## Contributing to the code and documentation

As you might have already noticed while browsing our repo we use branches to implement features or fix bugs. Our `master` branch corresponds to the current emdi version on cran. This allows us to develop larger features on the `dev` branch while
maintaining the possibility of providing hotfixes to emdi on cran. Such hotfixes 
on the `master` branch will ONLY be done by emdi's core development team. All other contributions should be done via branches from the `dev` branch. Any kind of enhancement, e.g. an improvement of the documentation or a new feature should be 
done in branches starting with `feature\`  while `bug\` signals us a bugfix.

If you contribute to code and documentation please also add a bullet point at the 
`news.md` file, and tell people what has changed. Please, prepare your pull request 
thoroughly. Work on an up-to-date version of `dev`. If there are merge conflicts 
due to a very out-of-date version of `dev` we will ask you to prepare an updated
version before considering merging it. Also `devtools::check()` should yield no 
errors, no warnings and no notes. New code also needs to be covered with tests. 
(Hint, we use the testthat package, and tests are loYou cated at: 
`emdi/tests/testthat/`. Both, the check and the tests are automatically checked 
by github actions.

Also please please try to keep the scope of a pull request as narrow as possible!
It is really hard and time consuming to review multiple hundreds of lines of code
at once!

### On adding documentation

THANKS! You have read all the above and still want to contribute GREAT! Please note, 
we use `roxygen2` for our documentation, hence you will need to use the `#'` escape
to add your documentation directly in the R file! All changes in the markdown files 
in the `man` folder are overwritten when building the package.

### On enhancing the code

Still here? Amazing! 
Just some notes regarding our style we (try to) use snake_case for practically everything,
and try to stay within the 80 characters per line boundary! Make sure a new function
is documented (see above) and contains both tests, and an example. 

Fixing a bug? Great! Please make sure to add a (small) test that would have detected
it in the first place.

If your enhancement of bug fix corresponds to a github issue, make sure to link 
it in the merge request.

### But I really want to add something big! 

Contact us (Soeren.Pannier@fu-berlin.de or Ann-Kristin.Kreutzmann@fu-berlin.de) 
and we will figure it out. It will probably include you forking our repo for development purposes. 


## Anything else I need to know?

Yes! If you think about implementing a whole new statistical method, please note
we ONLY include methods into emdi that have previously been published in an peer
reviewed journal.



