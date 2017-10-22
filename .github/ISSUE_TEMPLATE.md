# Please follow the general troubleshooting steps first:

You may erase parts of this template not applicable to your Issue. Deleting
_all_ points is considered rude and may result in closing the issue as invalid.

- [ ] Have you read the [CONTRIBUTING](CONTRIBUTING.md) document?
- [ ] Have you tested with a **fresh fork source checkout into a new directory
      with a full rebuild there**?
- [ ] If this was working before, do you know which one was the latest working
      revision?
- [ ] Does it also happen with latest
      [mainline (3.2)](https://github.com/harbour/core)?
- [ ] If this is an issue with a Cl\*pper legacy functionality, have you
      compared it against the original behavior of Cl\*pper itself?
- [ ] If this is an issue with mainline or a Cl\*pper incompatibility, have
      you reported this to
      [mainline](https://groups.google.com/forum/#!forum/harbour-devel)
      already?
- [ ] Have you included the top of the output (lines starting with `!`) and the
      area where the issue occurred **first** from your `HB_BUILD_VERBOSE=yes`,
      STDOUT/STDERR (`make > log.txt 2>&1`) build output?

### Bug reports:

Please replace this section with a brief summary of your issue, including
self-contained example code, build output, platform/compiler and
version/revision details.

### Notes:

In case of inquiries, questions, usage issues and general or feature specific
discussion, please use available public forums. Popular ones are listed in
the [README](../README.md#external-links).

Consider creating a [Pull Request](https://github.com/vszakats/harbour-core/pulls)
to address any problem found. This is in particular welcome or even expected
for issues falling outside the focus of this fork, e.g. any C compiler except
mingw/gcc/clang or regarding certain, non-priority or deprecated components
(e.g. Windows CE, `hbtip`, `rddads`, `gtwvg`/`gtwvw`, `xhb`) and feature requests
in general.
