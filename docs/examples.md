---
layout: default
title: "Code Examples"
---
# Code Examples

This is a small collection of short examples from Harbour's test directory.

## Categories

{% for cat in site.example_categories %}* [{{ cat | capitalize }}](#{{ cat }})
{% endfor %}* [How to build/run applications](#how-to-buildrun-applications)

---

{% for cat in site.example_categories %}
## {{ cat | capitalize }}

{% for example in site.example %}{% if example.category == cat %}* [{{ example.title_long }}]({{ site.baseurl }}/example/{{ example.slug }}{{ site.ilink_suffix }})
{% endif %}{% endfor %}
{% endfor %}

## More examples

You can find many more examples in the source package of Harbour.
Look into the [`tests`](https://github.com/{{ site.repo_slug }}/tree/master/tests)
and [`extras`](https://github.com/{{ site.repo_slug }}/tree/master/extras)
directories to find them, but the best example is the source code of
Harbour itself.

---

## How to build/run applications

[Build](https://github.com/{{ site.repo_slug }}/blob/master/utils/hbmk2/doc/hbmk2.en.md):

```shell
$ hbmk2 app.prg
$ ./app
```

or [run](https://github.com/{{ site.repo_slug }}/blob/master/contrib/hbrun/doc/hbrun.en.md) them as scripts:

```shell
$ hbrun app.prg
```
