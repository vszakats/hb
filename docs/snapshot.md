---
layout: default
title: "Nightly Builds"
---
{% if site.fork %}
# Snapshot Build
{% else %}
# Nightly Builds
{% endif %}

{% if site.fork %}
Latest source code can be downloaded from the repository page or directly from
our homepage. Snapshot binaries for the Windows platform are also built after
each commit and are made available from the releases page and the homepage.
These Windows binaries require a 64-bit OS to run and is able to produce both
32-bit and 64-bit applications. The C compiler used in these snapshot builds
is the latest release of MinGW.

Binary builds for other platforms can be built directly from source. See more
on how to that in the README.

While these packages are _not_ intended for production use; they are most of
the time solid to use in production. It's worth to keep an eye on the latest
commits and/or `ChangeLog.txt` to see if there is any work in progress and
what changes to expect.
{% else %}
You can find the latest development sources for Harbour directly on our website.
Source code is checked out of version repository and packaged every night and
made available along with the binaries built for Windows.

These packages are _not_ intended for production use; please use the packages at
the download page.
{% endif %}

If you do encounter a bug, please test the latest snapshot build to see if it
has already been fixed before reporting it.

To see what developers have added, you can view `ChangeLog.txt` directly in the
source repository.

# Download Links

To download latest development sources, go to the
[Project Status]({{ site.baseurl }}/#project-status) section on the main page.
{% if site.fork %}
To download snapshot or stable binaries of Harbour, visit [here]({{ site.dl_url }}).
{% else %}
To download nightly or stable release of Harbour, visit [here]({{ site.dl_url }}).
{% endif %}
