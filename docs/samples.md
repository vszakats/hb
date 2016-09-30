---
layout: default
title: "Code Examples"
permalink: /examples
---
# Code Examples

This is the list of some examples in Harbour's test directory.
The examples demonstrate Harbour features in small programs.

## Categories

{% for cat in site.sample_categories %}* [{{ cat | capitalize }}](#{{ cat }})
{% endfor %}* [How to build/run applications](#how-to-buildrun-applications)

---

{% for cat in site.sample_categories %}
## {{ cat | capitalize }}

{% for sample in site.samples %}{% if sample.category == cat %}* [{{ sample.title_long }}](samples/{{ sample.slug }}.html)
{% endif %}{% endfor %}
{% endfor %}

## More examples

More examples can be provided with the installation package of the Harbour.
Look the [`/tests`](https://github.com/{{ site.repo_slug }}/tree/master/tests) and
[`/extras`](https://github.com/{{ site.repo_slug }}/tree/master/extras) folders to
find other examples, but the best example is the source code of Harbour
itself.

---

## How to build/run applications

```
$ hbmk2 app.prg
$ ./app
```

or run them as scripts:

```
$ hbrun app.prg
```
