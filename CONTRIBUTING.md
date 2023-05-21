# 1. Asking Questions

Kaiaulu uses [GitHub's Discussions](https://github.com/sailuh/kaiaulu/discussions) instead of Mailing Lists. If you have general questions, please post there instead of sending direct e-mails, as others may be able to reply/benefit from the response. Please feel free to ask any questions: If it is confusing to you, it means there is room for improvement.

# 2. Contributing Code

First of all, thank you for your willingness to help making changes to the code. I really appreciate your time understanding the codebase to propose changes, and it is always reassuring to know others reviewed the core functionality. When contributing code you have to familiarize yourself with two things: What files in an R package are modified (on top of the one you wish to modify), and how to make the request for change. The formalities add a bit of upfront overhead but make searching for information and usage much easier. If you are not familiar with R package development or making Pull Requests, the following sections will brief you through it.  

## 2.1 Do I need to edit any additional files when proposing changes?

Yes. Since Kaiaulu is an R package, we follow some R package development conventions so R users know where to find information. This varies depending on the intended change. The following sub-sections are not mutually exclusive, but many can be skipped depending on your proposed changes.

### 2.1.1 For any change you make

Please add a brief note to the `NEWS.md` file in the appropriate section to the most current milestone (top of the file) so others are aware of your change. Include the issue number as it is done to all other new notes. When the documentation is generated, this generates a [changelog](http://itm0.shidler.hawaii.edu/kaiaulu/news/index.html). 

### 2.1.2 If you are a new contributor

Please add your name to the `DESCRIPTION` file in the root folder so I can give you credit! You can also add your ORCID. This information is reflected when the package docs are documented, and will also be listed on `CRAN` (R Package manager) when this package is released there. GitHub will automatically list you as a contributor when I accept your pull request. Please build the package and the documentation when doing so in RStudio and observe if no warnings are generated or errors due to bad formatting. In OS X, Cmd + Shift + D will re-build the docs, and Cmd + Shift + B will re-build the package.

### 2.1.3 If your changes add new R package dependencies

Please include the new package dependencies on the `DESCRIPTION` file **under Imports** (see it for examples). You can find the package versions you are using sessionInfo() on an R session. If given choice, please opt for more permissive licenses, so it is clear Kaiaulu is MPL instead of GPL as an aggregate. If unclear, please ask about the issue associated with the change request. Also, please use a package Kaiaulu already uses if it can accomplish the same functionality for your needs to keep dependencies to a minimum.

### 2.1.4 If you are proposing a new function 

Kaiaulu uses Rdoxygen to automatically generate the package documentation (see http://itm0.shidler.hawaii.edu/kaiaulu). Please observe the formatting of function documentation in the various R/ functions in the package if you are creating one. Moreover, if the feature is expressive, you may want to include a new vignette (i.e. R Notebook) in your pull request showcasing the functionality.  Make sure you keep the `#' @export` line, that's what makes the function "public" for users to use. In addition, update the `_pkgdown.yml` file with the function(s) you added. Finally, please compile both the package and documentation before pushing the commit (the NAMESPACE file will be updated automatically).

### 2.1.5 If you are proposing a new 3rd party software integration

Kaiaulu interacts with 3rd party software by system calls and by passing the tool's binary path to an R function. In general, this interaction is kept simple: The function responsibility is just to pass a request, and format the response as a table which can be easy to join to other data in Kaiaulu. If obtaining the 3rd party software binary during setup is relatively simple, please open an issue so we can discuss how to add an interface function. 

## 3. Making Pull Requests

If you are unsure on how to perform the below steps or are new to Git and Github, please see the **Learning Resources** (Section 5.) at the bottom of this document for some learning material.

The step-by-step process is as follows:

1. **Create an issue** describing what is wrong, or with a new feature proposal you wish to implement. 
   1. You can use the issue to discuss and agree on **what** files will be submitted, and **where** in the repository it will be added **before** submitting a Pull Request.
   1. It is ok to attach to the issue example files or images to clarify your point during an issue discussion, but it is **not** ok to submit entire datasets to either issue or as pull requests. Data should be hosted separately and discussed in the issue.
1. **Fork the repo**, develop and test your code changes.
   1. After cloning your fork, **before starting to modify any file**, please create a topic branch. A topic branch has the following format: `<issue-id>-<meaningful name associated to **what** you are trying to do>`.
      * Example: If the issue `#27` is about creating histograms of full disclosure word counts, then your branch should be `27-full-disclosure-word-count-histogram` (note the # symbol is **not** included in the branch name).
   1. Please **include the issue ID in all commits** (i.e. `i #27`). Your commits should follow the format `i <issue-id> <meaningful commit name>`. Note the leading `i` followed by a single whitespace is required so GitHub can parse it. 
       * Example: Continuing the example above, as you work in branch `27-full-disclosure-word-count-histogram`, one of your commits may be `i #27 parse input data into data frames`, followed by `i #27 plot and specify histogram ranges`. Note that, different from topic branches, **the i and # symbol must be included** in all commit labels.
   1. Please strive for good commit messages. A quick google search will provide many examples of good and bad commits.  
   1. Keep commit message titles short (50 characters or less). Use the commit body for details, and wrap commit body lines at 72 characters. **Please avoid titles such as "modified R/file.R"**. 
1. **Submit a pull request** using the created topic branch in step 3.2.1.
   1. Git will prompt the commit message to be used as the title of your Pull Request. **Please remove the issue number from the pull request title**, as it becomes confusing to read with the Pull Request own number.
   1. Ensure that after clicking `New Pull Request` you select the correct branch of your fork. Ensure you are **not** using your fork's master branch, but instead the topic branch.
1. Please note the concept behind Github Pull Requests is synchronizing the branch from your fork to the Pull Request interface. As such, please refrain from making modifications to the associated topic branch after submitted. I will review your pull request and let you know if any further modifications are necessary. If you wish to work on something else, git checkout to the master branch, and then create a separate topic branch. 
   1. Please avoid **deleting your fork once a Pull Request is submitted**. Doing so will forcefully close the Pull Request, breaking the discussion about the same Pull Request in several different new ones. This makes it much more difficult in the future for new contributors to follow-up a related contribution discussion. If needed, please contact me in the associated issue if you need help instead of deleting it.
   1. I will review your proposed code changes in the Pull Request. If code changes are required, it will be noted directly in the Pull Request. You can make the necessary requested changes by simplying making another commit to your existing fork topic branch (please do **not** open a new pull request). GitHub will automatically show the new commit of your fork's topic branch on the Pull Request so it can be reviewed.
      1. You do not need to worry about git squashing as GitHub now provide a "Squash and Merge" feature. However, please keep the proper formatting of the commits in your fork (e.g. `i #27 plot and specify histogram ranges`).  
   


# 4. Use `git commit -s`

Kaiāulu is licensed under MPL 2.0 (see LICENSE.md). We chose this license in the same spirit as the package R data.table, which decided to move from GPL to MPL 2.0 (see Rdatatable/data.table 
issue 2456), and used other major project examples, such as XGBoost as a basis to 
respect other project licenses when using them (see dmlc/xgboost issue 1338, and 1401).

Specifically, the motivation of this project is to facilitate interoperability between different interfaces to facilitate research in software engineering. It makes sense to us, that in leveraging interfaces from both private and public sources, MPL 2.0 provides a balance between open contribution and allowing you to use this software, provided any improvements to this package code remains open-source if distributed.

To ensure this project respects the license of the projects it interfaces with and their
respective owners intentions, we require any contribution follows the [Developer's 
Certificate of Origin](http://developercertificate.org) process:

```
Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the
    best of my knowledge, is covered under an appropriate open
    source license and I have the right under that license to
    submit that work with modifications, whether created in whole
    or in part by me, under the same open source license (unless
    I am permitted to submit under a different license), as
    Indicated in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including
    all personal information I submit with it, including my
    sign-off) is maintained indefinitely and may be redistributed
    consistent with this project or the open source license(s)
    involved.
```

You acknowledge the above by signing off all commit messages submitted by Pull Request or directly to the repo, e.g.:

```
Signed-off-by: Carlos Paradis <carlosviansi@gmail.com>
```

Git provides you with the above if you do the following to `git commit`:

 * `-s`
 * `--signoff`
 
If you forget, after a commit, you can amend:
 
 * `git commit --amend -s`
  
If it was already pushed to the branch, please force push after the amend: `git push -f`

This does not constitute legal advice. The aforementioned decisions were done in agreement with the licenses based on our best efforts.

# 5. Learning Resources

If you are new to collaborating on GitHub, or need to brush up, see the [Wiki's](https://github.com/sailuh/kaiaulu/wiki) `Learning Resources` section.

