---
layout: cv
permalink: /cv/
title: CV
nav: true
nav_order: 4
cv_pdf: CV.pdf
description: 
---

You can download my CV [here]({{ '/assets/pdf/CV.pdf' | relative_url }}).

<!--
  The inline preview is hidden on narrow screens: most mobile browsers cannot
  render an embedded PDF and show an empty box instead. The download link above
  is always available.
-->
<div class="cv-preview">
  <object data="{{ '/assets/pdf/CV.pdf' | relative_url }}" type="application/pdf">
    <p>
      Your browser cannot display PDFs inline.
      <a href="{{ '/assets/pdf/CV.pdf' | relative_url }}">Download my CV</a> instead.
    </p>
  </object>
</div>

<style>
  .cv-preview object {
    width: 100%;
    height: 800px;
    border: 1px solid rgba(0, 0, 0, 0.1);
  }

  @media (max-width: 768px) {
    .cv-preview {
      display: none;
    }
  }
</style>
