---
layout: default
title: "News Archive"
---
# News Archive

This page gives you some of the main changes introduced in recent versions of
Harbour. To view the latest [`ChangeLog.txt`]({{ site.baseurl }}/changelog)
or [download]({{ site.dl_url }}) lastest stable release of Harbour.

You can find the detailed list of changes
[here](https://raw.githubusercontent.com/{{ site.repo_slug }}/master/doc/oldnews.txt).

---

<div markdown="1" class="news news-full">
{% for story in site.data.news %}
  {{ story.date }}
  : **{{ story.title }}**<br>
    <p>{{ story.text }}
{% endfor %}
</div>
