---
layout: about
title: About
permalink: /
subtitle: 

profile:
  align: center
  image: 
  image_circular: false # crops the image to make it circular
  address: false

news: false  # includes a list of news items
latest_posts: false  # includes a list of the newest posts
selected_papers: false # includes a list of papers marked as "selected={true}"
social: true  # includes social icons at the bottom of the page

---

![jmpic]({{ site.url }}/assets/img/mypic_web.jpg){:width="100%"}

I am an Assistant Professor at the Universidad de Santiago de Chile (USACH), working in labor, gender and health in developing countries. 

You can contact me at joao.garcia@usach.cl, and you can find my CV [here][3]. I am also on [Google Scholar][5] and [ORCID][6].

# Working Papers

{% assign working_papers = site.papers | sort: "order" %}
{% for paper in working_papers %}
{% assign paper_url = paper.url | relative_url %}
<div class="paper-entry">
  <h2><a href="{{ paper_url }}">{{ paper.title }}</a></h2>
  {%- if paper.coauthors %}
  <h3>with {{ paper.coauthors }}</h3>
  {%- endif %}
  <p>{{ paper.abstract }}</p>
  {% include paper_links.html paper=paper page_url=paper_url %}
</div>
{% endfor %}

[3]:{{ site.url }}/assets/pdf/CV.pdf
[5]:https://scholar.google.com/citations?user=2ETYvogAAAAJ
[6]:https://orcid.org/0000-0001-6691-5260
