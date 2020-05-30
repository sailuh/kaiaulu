# 1. Open a Pull Request 


# 2. Use `git commit -s`

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
