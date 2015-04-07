{% block content %}
{% for id in ids %}
	<h2><a href="{{ id.page_url }}">{{ id.title }}</a></h2>

	<p>
	    {% if id.summary %}{{ id.summary }}
	    {% else %}{{ id.body|striptags|truncate:120 }}
	    {% endif %}
	</p>

	<p><a href="{{ id.page_url }}">{_ Read more _}</a></p>
{% endfor %}
{% endblock %}

{% javascript %}
    GeoMapCountry._popup.updateSize();
{% endjavascript %}
