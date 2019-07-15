---
layout: default
title: "Forks"
---
<div markdown="1" class="components">

{%- assign forkurl = 'https://vszakats.gitlab.io/hb/' %}

# Harbour Forks

## [Harbour]({% if site.fork %}{{ site.baseurl }}{% else %}{{ forkurl }}{% endif %}) by [vszakats](https://vszakats.net/) (version 3.4)

Forked in April of 2013 in an attempt to focus on a narrower set of platforms
and contrib libraries while also freeing up resources by distancing from
project/community management tasks. This fork focuses on Linux, Windows (64-bit)
and macOS, and some selected C compilers (clang, gcc/MinGW). It has the stated
goal to adapt all mainline (non-fork) updates with minimal delay. To ease
maintenance, this fork has the policy to disable all deprecated components by
default (they can be enabled though, with no promises). Other goals are
compatibility with mainline core, continuous maintenance, build automation and
security ([`hbcrypto`]({% if site.fork %}{{ site.baseurl }}{% else %}{{ forkurl }}{% endif %}/contribs{{ site.ilink_suffix }}#hbcrypto)
contrib and [curl/OpenSSL builds](https://github.com/curl/curl-for-win)).
It also features a large number of fixes and cleanups, many of which are
eventually retrofitted to mainline.

This fork accepts
[donations]({% if site.fork %}{{ site.baseurl }}{% else %}{{ forkurl }}{% endif %}/#this-fork).

* [Repository](https://github.com/vszakats/harbour-core)

This fork is largely compatible with mainline, with the notable exception
for deprecated parts, which are disabled by default.

{% if site.fork %}
## [Mainline](https://harbour.github.io/) (non-fork) Harbour

This is the non-fork, original version of Harbour, started in 1999.

* [Repository](https://github.com/harbour/core)
* [Community](https://groups.google.com/group/harbour-users/)
{% endif %}

## xHarbour

Forked in 2001 with the goal of providing a more aggressive development path
with a different approach to language extensions and compatibility and more
focus on the Windows platform along with commercial offerings.

* [Repository](https://sourceforge.net/projects/xharbour/)
* [Community](https://groups.google.com/forum/#!forum/comp.lang.xharbour)

Technical details on how this fork differs from the other variations,
[here](https://raw.githubusercontent.com/{{ site.repo_slug }}/master/doc/xhb-diff.txt).

## MiniGUI forks

Harbour MiniGUI (HMG) is a Windows GUI development environment consisting
of a GUI library, an IDE, a Harbour binary distribution and a C compiler.
It implements a semi-OOP model and is ready to use by including all required
components. There are a number of semi-connected forks found on the internet:

* [HMG Extended](https://sourceforge.net/projects/hmgs-minigui/)
* [HMG](https://sourceforge.net/projects/hmg/) / <https://sites.google.com/site/hmgweb/>
* [HMG3](https://sourceforge.net/projects/harbourminigui/) / hmgforum.com

Each use altered Harbour sources and may use non-standard toolchain elements
(e.g. `hbmk2.bat` wrapper for standard `hbmk2.exe`). They feature certain
libraries with names overlapping with Harbour contribs, but with slightly
incompatible or completely different functionality. Some distros come in
binary form only, making it difficult/impossible to find out what was the exact
source code / patches they were built from. In practice it means that general
Harbour support forums can't help with issues.

</div>
